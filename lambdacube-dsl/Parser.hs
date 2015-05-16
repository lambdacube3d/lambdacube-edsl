{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Parser where

import Data.Function
import Data.Char
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Monoid
import Control.Applicative (some,liftA2,Alternative())
import Control.Arrow
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import qualified Text.Parsec.Indentation.Char as I
import Text.Parsec.Indentation
import Text.Parsec hiding (optional)

import qualified Pretty as P
import Type
import ParserUtil

-------------------------------------------------------------------------------- parser specific types

type P = P_ ()

type PreDefinitionR = DefinitionR
type PrecExpR = ExpR

---------------------

void_ a = a >> return ()

typeConstraint :: P ClassName
typeConstraint = do
  i <- ident lcIdents
  if isUpper $ head i then return $ TypeN' i (P.text i) else fail "type constraint must start with capital letter"

typeConstructor :: P N
typeConstructor = do
  i <- ident lcIdents
  if isUpper $ head i then return $ TypeN' i (P.text i) else fail "type name must start with capital letter"

upperCaseIdent :: P N
upperCaseIdent = do
  i <- ident lcIdents
  if isUpper $ head i then return $ ExpN i else fail "upper case ident expected"

-- see http://blog.ezyang.com/2014/05/parsec-try-a-or-b-considered-harmful/comment-page-1/#comment-6602
try' s m = try m <?> s

typeVar :: P N
typeVar = try' "type variable" $ do
  p <- position
  i <- ident lcIdents
  if isUpper $ head i then fail "type variable name must start with lower case letter" else return $ TypeN' i (P.text $ i ++ show p)

dataConstructor :: P N
dataConstructor = try' "data constructor" $ do
  i <- ident lcIdents
  if isUpper $ head i then return $ ExpN i else fail "data constructor must start with capital letter"

var :: P N
var = try' "variable" $ do
  p <- position
  i <- ident lcIdents
  if isUpper $ head i then fail "variable name must start with lower case letter" else return $ ExpN' i (P.text $ i ++ show p)

-- qualified variable
qVar :: P N    -- TODO
qVar = var <|> {-runUnspaced-} (try' "qualified var" $ sepBy ({-Unspaced-} upperCaseIdent) dot *> dot *> {-Unspaced-} var)

operator' :: P N
operator' = try' "operator" (do
                  p <- position
                  i <- ident lcOps
                  if head i == ':' then fail "operator cannot start with ':'" else return $ ExpN' i $ P.text $ show p)
        <|> (operator "." *> pure (ExpN "."))
        <|> conoperator'

conoperator' = try' "constructor operator" (do
                  p <- position
                  i <- ident lcOps
                  if head i /= ':' then fail "constructor operator should start with ':'" else return $ ExpN' (if i == ":" then "Cons" else i) $ P.text $ show p)

varId :: P N
varId = var <|> parens operator'

--------------------------------------------------------------------------------

sepBy2 a b = (:) <$> a <* b <*> sepBy1 a b

alts :: Int -> [PrecExpR] -> PrecExpR
alts _ [e] = e
alts i es = EAltsR' (foldMap getTag es) i es

compileWhereRHS :: WhereRHS -> PrecExpR
compileWhereRHS (WhereRHS r md) = maybe x (flip eLets x) md where
    x = compileGuardedRHS r

compileGuardedRHS :: GuardedRHS -> PrecExpR
compileGuardedRHS (NoGuards e) = e
compileGuardedRHS (Guards p gs) = foldr addGuard (ExpR p{-TODO-} (ENext_ ())) gs
  where
    addGuard (b, x) y = eApp (eApp (eApp (eVar p{-TODO-} (ExpN "ifThenElse")) b) x) y

compileCases :: Range -> PrecExpR -> [(PatR, WhereRHS)] -> PrecExpR
compileCases r e rs = eApp (alts 1 [eLam p $ compileWhereRHS r | (p, r) <- rs]) e

compileRHS :: [PreDefinitionR] -> PreDefinitionR
compileRHS ds = case ds of
    ((r1, DTypeSig (TypeSig _ t)): ds@((r2, PreValueDef{}): _)) -> (r1 `mappend` r2, mkAlts (`eTyping` t) ds)
    ds@((r, PreValueDef{}): _) -> (r, mkAlts id ds)
    [x] -> x
  where
    mkAlts f ds@( (_, PreValueDef (r, n) _ _): _)
        = DValueDef $ ValueDef (PVar' r n) $ f $ alts i als
      where
        i = allSame is
        (als, is) = unzip [(foldr eLam (compileWhereRHS rhs) pats, length pats) |  (_, PreValueDef _ pats rhs) <- ds]

allSame (n:ns) | all (==n) ns = n

groupDefinitions :: [PreDefinitionR] -> [DefinitionR]
groupDefinitions defs = concatMap mkDef . map compileRHS . groupBy (f `on` snd) $ defs
  where
    f (h -> Just x) (h -> Just y) = x == y
    f _ _ = False

    h ( (PreValueDef (_, n) _ _)) = Just n
    h ( (DValueDef (ValueDef p _))) = name p        -- TODO
    h ( (DTypeSig (TypeSig n _))) = Just n
    h _ = Nothing

    name (PVar' _ n) = Just n
    name _ = Nothing

    mkDef = \case
         (r, PreInstanceDef c t ds) -> [(r, InstanceDef c t [v | (r, DValueDef v) <- groupDefinitions ds])]
         x -> [x]

--------------------------------------------------------------------------------

moduleName :: P Name
moduleName = do
  l <- sepBy1 (ident lcIdents) dot
  when (any (isLower . head) l) $ fail "module name must start with capital letter"
  return $ N ExpNS (init l) (last l) $ NameInfo Nothing "module"

moduleDef :: FilePath -> P ModuleR
moduleDef fname = do
  modn <- optional $ do
    keyword "module"
    modn <- moduleName
    optional $ parens (commaSep varId)
    keyword "where"
    return modn
  localAbsoluteIndentation $ do
    idefs <- many importDef
    -- TODO: unordered definitions
    defs <- groupDefinitions . concat <$> many (choice
        [ (:[]) <$> dataDef
        , concat <$ keyword "axioms" <*> localIndentation Gt (localAbsoluteIndentation $ many axiom)
        , typeSignature
        , (:[]) <$> typeFamily
        , const [] <$> typeSynonym
        , (:[]) <$> typeClassDef
        , (:[]) <$> valueDef
        , fixityDef
        , (:[]) <$> typeClassInstanceDef
        ])
    return $ Module
      { moduleImports = (if modn == Just (ExpN "Prelude") then id else (ExpN "Prelude":)) idefs
      , moduleExports = mempty
      , definitions   = defs
      }

importDef :: P Name
importDef = do
  keyword "import"
  optional $ keyword "qualified"
  n <- moduleName
  let importlist = parens (commaSep (varId <|> dataConstructor))
  optional $
        (keyword "hiding" >> importlist)
    <|> importlist
  optional $ do
    keyword "as"
    moduleName
  return n

typeSynonym :: P ()
typeSynonym = void_ $ do
  keyword "type"
  localIndentation Gt $ do
    typeConstructor
    many typeVar
    operator "="
    void_ typeExp

typeSignature :: P [PreDefinitionR]
typeSignature = do
  ns <- try' "type signature" $ do
    ns <- sepBy1 varId comma
    localIndentation Gt $ operator "::"
    return ns
  t <- localIndentation Gt $ do
    optional (operator "!") *> typeExp
  return [(mempty, DTypeSig $ TypeSig n t) | n <- ns]

axiom :: P [PreDefinitionR]
axiom = do
  ns <- try' "axiom" $ do
    ns <- sepBy1 (varId <|> dataConstructor) comma
    localIndentation Gt $ operator "::"
    return ns
  t <- localIndentation Gt $ do
    optional (operator "!") *> typeExp
  return [(mempty, DAxiom $ TypeSig n t) | n <- ns]

tcExp :: P (TyR -> TyR)   -- TODO
tcExp = try' "type context" $ do
  let tyC = addPos addC (eqC <$> try (ty <* operator "~") <*> ty)
        <|> addPos addC (CClass <$> typeConstraint <*> typeAtom)
      addC :: Range -> ConstraintR -> TyR -> TyR
      addC r c = ExpR r . Forall_ Nothing (ExpR r $ ConstraintKind_ c)
      eqC t1 t2 = CEq t1 (mkTypeFun t2)
  t <- tyC <|> parens (foldr (.) id <$> sepBy tyC comma)
  operator "=>"
  return t

pattern Tyy a <- ExpR _ a
pattern TyApp1 s t <- Tyy (EApp_ () (Tyy (TCon_ () (TypeN s))) t)
pattern TyApp2 s t t' <- Tyy (EApp_ () (TyApp1 s t) t')

mkTypeFun :: TyR -> TypeFunR
mkTypeFun = \case
    TyApp2 "TFMat" a b -> TFMat a b
    TyApp1 "MatVecElem" a -> TFMatVecElem a
    TyApp1 "MatVecScalarElem" a -> TFMatVecScalarElem a
    TyApp2 "TFVec" a b -> TFVec a b               -- may be data family
    TyApp2 "VecScalar" a b -> TFVecScalar a b
    TyApp1 "FTRepr'" a -> TFFTRepr' a
    TyApp1 "ColorRepr" a -> TFColorRepr a
    TyApp1 "TFFrameBuffer" a -> TFFrameBuffer a
    TyApp1 "FragOps" a -> TFFragOps a
    TyApp2 "JoinTupleType" a b -> TFJoinTupleType a b
    x -> error $ "mkTypeFun: " ++ P.ppShow x

typeExp :: P TyR
typeExp = choice
  [ do
        keyword "forall"
        choice
            [ addPos ExpR $ do
                (v, k) <- parens ((,) <$> typeVar <* operator "::" <*> ty)
                operator "."
                t <- typeExp
                return $ Forall_ (Just v) k t
            , do
                some typeVar
                operator "."
                typeExp
            ]
  , tcExp <*> typeExp
  , ty
  ]


ty :: P TyR
ty = do
    t <- tyApp
    maybe t (tArr t) <$> optional (operator "->" *> typeExp)
  where
    tArr t a = ExpR (t <-> a) $ Forall_ Nothing t a

tyApp :: P TyR
tyApp = typeAtom >>= f
  where
    f t = do
        a <- typeAtom
        f $ ExpR (t <-> a) $ EApp_ () t a
      <|> return t

typeAtom :: P TyR
typeAtom = typeRecord
    <|> addTPos (Star_ <$ operator "*")
    <|> addTPos (EVar_ () <$> try' "type var" typeVar)
    <|> addTPos (ELit_ <$> (LNat . fromIntegral <$> natural <|> literal))
    <|> addTPos (TCon_ () <$> typeConstructor)
    <|> addPos tTuple (parens (sepBy ty comma))
    <|> addPos (\p -> ExpR p . EApp_ () (ExpR p $ TCon_ () (TypeN' "List" "List"))) (brackets ty)

tTuple :: Range -> [TyR] -> TyR
tTuple p [t] = t
tTuple p ts = ExpR p $ TTuple_ ts

addTPos = addPos ExpR

addDPos m = addPos (,) m

typeFamily :: P PreDefinitionR
typeFamily = addDPos $ do
    try $ keyword "type" >> keyword "family"
    tc <- typeConstructor
    tvs <- many typeVarKind
    res <- optional $ do
        operator "::"
        ty
    return $ TypeFamilyDef tc tvs $ fromMaybe (ExpR mempty Star_) res

dataDef :: P PreDefinitionR
dataDef = addDPos $ do
 keyword "data"
 localIndentation Gt $ do
  tc <- typeConstructor
  tvs <- many typeVarKind
  let dataConDef = addDPos $ do
        tc <- dataConstructor
        tys <-   braces (sepBy (FieldTy <$> (Just <$> varId) <*> (keyword "::" *> optional (operator "!") *> typeExp)) comma)
            <|>  many (optional (operator "!") *> (FieldTy Nothing <$> typeAtom))
        return $ ConDef tc tys
  do
    do
      keyword "where"
      ds <- localIndentation Ge $ localAbsoluteIndentation $ many $ do
        cs <- do
            cs <- sepBy1 dataConstructor comma
            localIndentation Gt $ do
                operator "::"
            return cs
        localIndentation Gt $ do
            t <- typeExp
            return [(c, t) | c <- cs]
      return $ GADT tc tvs $ concat ds
   <|>
    do
      operator "="
      ds <- sepBy dataConDef $ operator "|"
      derivingStm
      return $ DDataDef tc tvs ds


derivingStm = optional $ keyword "deriving" <* (void_ typeConstraint <|> void_ (parens $ sepBy typeConstraint comma))

typeRecord :: P TyR
typeRecord = undef "trec" $ do
  braces (commaSep1 typeSignature >> optional (operator "|" >> void_ typeVar))

-- compose ranges through getTag
infixl 9 <->
a <-> b = getTag a `mappend` getTag b

addPPos = addPos PatR
addEPos = addPos ExpR

addPos :: (Range -> a -> b) -> P a -> P b
addPos f m = do
    p1 <- position
    a <- m
    p2 <- position
    return $ f (Range p1 p2) a

typeClassDef :: P PreDefinitionR
typeClassDef = addDPos $ do
  keyword "class"
  localIndentation Gt $ do
    optional tcExp
    c <- typeConstraint
    tvs <- many typeVarKind
    ds <- optional $ do
      keyword "where"
      localIndentation Ge $ localAbsoluteIndentation $ many $ do
        typeSignature
    return $ ClassDef c tvs [d | (_, DTypeSig d) <- maybe [] concat ds]

typeVarKind =
      parens ((,) <$> typeVar <* operator "::" <*> ty)
  <|> (,) <$> typeVar <*> addTPos (pure Star_)

typeClassInstanceDef :: P PreDefinitionR
typeClassInstanceDef = addDPos $ do
  keyword "instance"
  localIndentation Gt $ do
    optional tcExp
    c <- typeConstraint
    t <- typeAtom
    ds <- optional $ do
      keyword "where"
      localIndentation Ge $ localAbsoluteIndentation $ many $ do
        valueDef
    return $ PreInstanceDef c t $ fromMaybe [] ds

fixityDef :: P [PreDefinitionR]
fixityDef = do
  dir <-    Nothing      <$ keyword "infix" 
        <|> Just FDLeft  <$ keyword "infixl"
        <|> Just FDRight <$ keyword "infixr"
  localIndentation Gt $ do
    i <- natural
    ns <- sepBy1 (addPos (,) (operator' <|> backquoteOp)) comma
    return [(p, PrecDef n (dir, fromIntegral i)) | (p, n) <- ns]

undef msg = (const (error $ "not implemented: " ++ msg) <$>)

valuePattern :: P PatR
valuePattern
    = appP <$> some valuePatternOpAtom

appP :: [PatR] -> PatR
appP [p] = p
appP (PCon' r n xs: ps) = PCon' r n $ xs ++ ps
appP xs = error $ "appP: " ++ P.ppShow xs

valuePatternOpAtom :: P PatR
valuePatternOpAtom = do
    e <- appP <$> some valuePatternAtom
    f e <$> op <*> valuePattern  <|>  return e
  where
    f e op e' = appP [op, e, e']

    op :: P PatR
    op = addPPos $ (\x -> PCon_ () x []) <$> conoperator'

valuePatternAtom :: P PatR
valuePatternAtom
    =   addPPos (Wildcard_ () <$ operator "_")
    <|> addPPos (PLit_ <$> literal)
    <|> addPPos (PAt_ <$> try' "at pattern" (var <* operator "@") <*> valuePatternAtom)
    <|> addPPos (PVar_ () <$> var)
    <|> addPPos ((\c -> PCon_ () c []) <$> try dataConstructor)
    <|> tuplePattern
    <|> recordPat
    <|> listPat
    <|> parens valuePattern
 where
  tuplePattern :: P PatR
  tuplePattern = try' "tuple" $ addPPos $ PTuple_ <$> parens (sepBy2 valuePattern comma)

  recordPat :: P PatR
  recordPat = addPPos $ PRecord_ <$> braces (sepBy ((,) <$> var <* colon <*> valuePattern) comma)

  listPat :: P PatR
  listPat = addPos (\p -> foldr cons (nil p)) $ brackets $ commaSep valuePattern
    where
      nil r = PCon' r{-TODO-} (ExpN "Nil") []
      cons a b = PCon' mempty (ExpN "Cons") [a, b]

eLam p e = ELamR' (p <-> e) p e

valueDef :: P PreDefinitionR
valueDef = addDPos $ do
  f <- 
    try' "definition" (do
      n <- addPos (,) varId
      localIndentation Gt $ do
        pats <- many valuePatternAtom
        lookAhead $ operator "=" <|> operator "|"
        return $ PreValueDef n pats
    )
   <|>
    try' "node definition" (do
      n <- valuePattern
      n2 <- optional $ do
          op <- addPos (,) operator'
          n2 <- valuePattern
          return (op, n2)
      localIndentation Gt $ do
        lookAhead $ operator "=" <|> operator "|"
        return $ case n2 of
            Nothing -> \e -> DValueDef $ ValueDef n $ alts 0 [compileWhereRHS e]
            Just (op, n2) -> PreValueDef op [n, n2]
    )
  localIndentation Gt $ do
    e <- whereRHS $ operator "="
    return $ f e

whereRHS :: P () -> P WhereRHS
whereRHS delim = do
    d <- rhs delim
    do
        do
          keyword "where"
          l <- localIndentation Ge $ localAbsoluteIndentation $ some ((:[]) <$> valueDef <|> typeSignature)
          return (WhereRHS d $ Just $ concat l)
      <|> return (WhereRHS d Nothing)

rhs :: P () -> P GuardedRHS
rhs delim = NoGuards <$> xx
  <|> addPos Guards (many ((,) <$> (operator "|" *> expression) <*> xx))
  where
    xx = delim *> expression

application :: [PrecExpR] -> PrecExpR
application [e] = e
application es = eApp (application $ init es) (last es)

eApp :: PrecExpR -> PrecExpR -> PrecExpR
eApp = eApp'

eApp' :: ExpR -> ExpR -> ExpR
eApp' a b = EAppR' (a <-> b) a b

expression :: P PrecExpR
expression = do
  e <-
      ifthenelse <|>
      caseof <|>
      letin <|>
      lambda <|>
      eApp <$> addPos eVar (const (ExpN "negate") <$> operator "-") <*> expressionOpAtom <|> -- TODO: precedence
      expressionOpAtom
  do
      do
        operator "::"
        t <- typeExp
        return $ eTyping e t
    <|> return e
 where
  lambda :: P PrecExpR
  lambda = (\(ps, e) -> foldr eLam e ps) <$> (operator "\\" *> ((,) <$> many valuePatternAtom <* operator "->" <*> expression))

  ifthenelse :: P PrecExpR
  ifthenelse = addPos (\r (a, b, c) -> eApp (eApp (eApp (eVar r (ExpN "ifThenElse")) a) b) c) $
        (,,) <$ keyword "if" <*> expression <* keyword "then" <*> expression <* keyword "else" <*> expression

  caseof :: P PrecExpR
  caseof = addPos (uncurry . compileCases) $ do
    keyword "case"
    e <- expression
    keyword "of"
    pds <- localIndentation Ge $ localAbsoluteIndentation $ some $
        (,) <$> valuePattern <*> localIndentation Gt (whereRHS $ operator "->")
    return (e, pds)

  letin :: P PrecExpR
  letin = do
      keyword "let"
      l <- localIndentation Ge $ localAbsoluteIndentation $ some valueDef
      keyword "in"
      a <- expression
      return $ eLets l a

eLets :: [PreDefinitionR] -> PrecExpR -> PrecExpR
eLets l a = foldr ($) a $ map eLet $ groupDefinitions l
  where
    eLet (r, DValueDef (ValueDef a b)) = ELetR' r a b

eTyping :: PrecExpR -> TyR -> PrecExpR
eTyping a b = ETypeSigR' (a <-> b) a b

expressionOpAtom :: P PrecExpR
expressionOpAtom = addEPos $ EPrec_ <$> exp <*> opExps
  where
    opExps = do
        do
            o <- op
            expression' o
      <|> return []

    expression' o = do
            (e, o') <- try $ (,) <$> exp <*> op
            es <- expression' o'
            return $ (o, e): es
        <|> do
            e <- expression
            return [(o, e)]

    exp = application <$> some expressionAtom
    --         a * b + c * d     -->     |$| a;  |$| a |*| b;  |$| a*b |+| c;  |$| a*b |+| c |*| d;  |$| a*b |+| c*d;  a*b+c*d
    op = addPos eVar $ operator'
        <|> backquoteOp

backquoteOp = try' "backquote operator" ({-runUnspaced-} ({-Unspaced-} (operator "`") *> {-Unspaced-} (var <|> upperCaseIdent) <* {-Unspaced-} (operator "`")))

expressionAtom :: P PrecExpR
expressionAtom = do
    e <- expressionAtom_
    sw <- optional $ do
        operator "%"
        ident lcIdents
    ts <- many $ do
        operator "@"
        typeAtom
    return $ foldl eTyApp (maybe id desugarSwizzling sw e) ts

desugarSwizzling :: [Char] -> ExpR -> ExpR
desugarSwizzling cs e = eTuple mempty [eApp (EFieldProjR' mempty $ ExpN [c]) e | c <- cs]

eTyApp a b = ETyAppR (a <-> b) a b

expressionAtom_ :: P PrecExpR
expressionAtom_ =
  listExp <|>
  addPos eLit literal <|>
  recordExp <|>
  recordExp' <|>
  recordFieldProjection <|>
  addPos eVar qVar <|>
  addPos eVar dataConstructor <|>
  tuple
 where
  tuple :: P PrecExpR
  tuple = addPos eTuple $ parens $ sepBy expression comma

  recordExp :: P PrecExpR
  recordExp = addPos eRecord $ braces $ sepBy ((,) <$> var <* colon <*> expression) comma

  recordExp' :: P PrecExpR
  recordExp' = try $ addPos (uncurry . eNamedRecord) ((,) <$> dataConstructor <*> braces (sepBy ((,) <$> var <* keyword "=" <*> expression) comma))

  recordFieldProjection :: P PrecExpR
  recordFieldProjection = try $ flip eApp <$> addPos eVar var <*>
        addPos EFieldProjR' ({-runUnspaced $-} dot *> {-Unspaced-} var)

  eLit p l@LInt{} = eApp' (eVar p (ExpN "fromInt")) $ ELitR' p l
  eLit p l = ELitR' p l

  listExp :: P PrecExpR
  listExp = addPos (\p -> foldr cons (nil p)) $ brackets $ commaSep expression
    where
      nil r = eVar (r{-TODO-}) $ ExpN "Nil"
      cons a b = eApp (eApp (eVar mempty{-TODO-} (ExpN "Cons")) a) b

literal :: P Lit
literal =
    LFloat <$> try double <|>
    LInt <$> try natural <|>
    LChar <$> charLiteral <|>
    LString <$> stringLiteral

eTuple _ [x] = x
eTuple p xs = ExpR p $ ETuple_ xs
eRecord p xs = ERecordR' p xs
eNamedRecord p n xs = ENamedRecordR' p n xs
eVar p n = EVarR' p n

parseLC :: FilePath -> ErrorT IO (String, ModuleR)
parseLC fname = do
  src <- lift $ readFile fname
  let p = do
        setPosition =<< flip setSourceName fname <$> getPosition
        whiteSpace
        moduleDef fname <* eof
      res = runParser p mempty "" $ mkIndentStream 0 infIndentation True Ge $ I.mkCharIndentStream src
  case res of
    Left err -> throwParseError err
    Right e  -> return (src, e)
