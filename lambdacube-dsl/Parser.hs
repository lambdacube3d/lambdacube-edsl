{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Parser where

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

import qualified Text.Parsec.Indentation.Char as I
import Text.Parsec.Indentation
import Text.Parsec hiding (optional)

import Type
import ParserUtil

void_ a = a >> return ()

typeConstraint :: P ClassName
typeConstraint = do
  i <- ident lcIdents
  if isUpper $ head i then return i else fail "type constraint must start with capital letter"

typeConstructor :: P String
typeConstructor = do
  i <- ident lcIdents
  if isUpper $ head i then return i else fail "type name must start with capital letter"

upperCaseIdent :: P String
upperCaseIdent = do
  i <- ident lcIdents
  if isUpper $ head i then return i else fail "upper case ident expected"

-- see http://blog.ezyang.com/2014/05/parsec-try-a-or-b-considered-harmful/comment-page-1/#comment-6602
try' s m = try m <?> s

typeVar :: P String
typeVar = try' "type variable" $ do
  i <- ident lcIdents
  if isUpper $ head i then fail "type variable name must start with lower case letter" else return i

dataConstructor :: P String
dataConstructor = try' "data constructor" $ do
  i <- ident lcIdents
  if isUpper $ head i then return i else fail "data constructor must start with capital letter"

var :: P String
var = try' "variable" $ do
  i <- ident lcIdents
  if isUpper $ head i then fail "variable name must start with lower case letter" else return i

-- qualified variable
qVar :: P String    -- TODO
qVar = var <|> {-runUnspaced-} (try' "qualified var" $ sepBy ({-Unspaced-} upperCaseIdent) dot *> dot *> {-Unspaced-} var)

operator' :: P String
operator' = try' "operator" (do
                  i <- ident lcOps
                  if head i == ':' then fail "operator cannot start with ':'" else return i)
        <|> (operator "." *> pure ".")
        <|> (operator ":" *> pure "Cons")

varId :: P String
varId = var <|> parens operator'

--------------------------------------------------------------------------------

alts :: Int -> [ExpR] -> ExpR
alts i es_ ps = EAlts (foldr1 (<-->) $ map getTag es) i es  where es = map ($ ps) es_

compileWhereRHS :: WhereRHSR -> ExpR
compileWhereRHS (WhereRHS r md) = maybe x (flip eLets x) md where
    x = compileGuardedRHS r

compileGuardedRHS :: GuardedRHSR -> ExpR
compileGuardedRHS (NoGuards e) = e
compileGuardedRHS (Guards gs) = foldr addGuard (\ps -> Exp mempty ENext_) gs
  where
    addGuard (b, x) y = eApp (eApp (eApp (eVar mempty "ifThenElse") b) x) y

compileCases :: Range -> ExpR -> [(PatR, WhereRHSR)] -> ExpR
compileCases r e rs = eApp (alts 1 [eLam p $ compileWhereRHS r | (p, r) <- rs]) e

compileRHS :: [DefinitionR] -> DefinitionR
compileRHS ds = case ds of
    (TypeSig (_, t): ds@(PreValueDef{}: _)) -> mkAlts (`eTyping` t) ds
    ds@(PreValueDef{}: _) -> mkAlts id ds
    [x] -> x
  where
    mkAlts f ds@(PreValueDef (r, n) _ _: _)
        = ValueDef (PVar r n, f $ alts i als)
      where
        i = allSame is
        (als, is) = unzip [(foldr eLam (compileWhereRHS rhs) pats, length pats) | PreValueDef _ pats rhs <- ds]

allSame (n:ns) | all (==n) ns = n

groupDefinitions :: [DefinitionR] -> [DefinitionR]
groupDefinitions = map compileRHS . groupBy f
  where
    f (h -> Just x) (h -> Just y) = x == y
    f _ _ = False

    h (PreValueDef (_, n) _ _) = Just n
    h (ValueDef (p, _)) = name p        -- TODO
    h (TypeSig (n, _)) = Just n
    h _ = Nothing

    name (PVar _ n) = Just n
    name _ = Nothing

--------------------------------------------------------------------------------

moduleName :: P (Q Name)
moduleName = do
  l <- sepBy1 (ident lcIdents) dot
  when (any (isLower . head) l) $ fail "module name must start with capital letter"
  return $ Q (init l) (last l)

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
        , const [] <$> typeSynonym
        , (:[]) <$> typeClassDef
        , (:[]) <$> valueDef
        , fixityDef
        , (:[]) <$> typeClassInstanceDef
        ])
    let ps = Map.fromList [(n, p) | DFixity n p <- defs]
        mkDef = \case
            ValueDef d -> [ValueDef $ id *** ($ ps) $ d]
            DDataDef d -> [DDataDef d]
            GADT a b c -> [GADT a b c]
            InstanceDef c t ds -> [InstanceDef c t $ concatMap mkDef ds]
            TypeSig d -> [TypeSig d]
            ClassDef a b c -> [ClassDef a b $ concatMap mkDef c]
            _ -> []
    return $ Module
      { moduleImports = (if modn == Just (Q [] "Prelude") then id else (Q [] "Prelude":)) idefs
      , moduleExports = mempty
      , typeAliases   = mempty
      , definitions   = concatMap mkDef defs
--      , instances     = Map.unionsWith (<>) [Map.singleton c $ Set.singleton t | InstanceDef c t <- defs] -- TODO: check clash
      , precedences   = ps     -- TODO: check multiple definitions
--      , axioms        = [d | TypeSig d <- defs]
      , moduleFile    = fname
      }


importDef :: P (Q Name)
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

typeSignature :: P [DefinitionR]
typeSignature = do
  ns <- try' "type signature" $ do
    ns <- sepBy1 varId comma
    localIndentation Gt $ operator "::"
    return ns
  t <- localIndentation Gt $ do
    optional (operator "!") *> typeExp
  return [TypeSig (n, t) | n <- ns]

axiom :: P [DefinitionR]
axiom = do
  ns <- try' "axiom" $ do
    ns <- sepBy1 (varId <|> dataConstructor) comma
    localIndentation Gt $ operator "::"
    return ns
  t <- localIndentation Gt $ do
    optional (operator "!") *> typeExp
  return [TypeSig (n, t) | n <- ns]

tcExp :: P (TyR -> TyR)   -- TODO
tcExp = try' "type context" $ do
  let tyC = addC <$> (eqC <$> try (ty <* operator "~") <*> ty)
        <|> addC <$> (CClass <$> typeConstraint <*> typeAtom)
      addC :: Constraint TyR -> TyR -> TyR
      addC c = Ty' mempty . TConstraintArg_ c
      eqC t1 t2 = CEq t1 (mkTypeFun t2)
  t <- tyC <|> parens (foldr (.) id <$> sepBy tyC comma)
  operator "=>"
  return t

pattern Tyy a <- Ty' _ a
pattern TyApp1 s t <- Tyy (TApp_ (Tyy (TCon_ s)) t)
pattern TyApp2 s t t' <- Tyy (TApp_ (TyApp1 s t) t')

mkTypeFun :: TyR -> TypeFun TyR
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
    x -> error $ "mkTypeFun: " ++ ppShow x

typeExp :: P TyR
typeExp = choice
  [ do
        keyword "forall"
        choice
            [ addPos Ty' $ do
                (v, k) <- parens ((,) <$> typeVar <* operator "::" <*> ty)
                operator "."
                t <- typeExp
                return $ Forall_ v k t
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
    tArr t a = Ty' (t <-> a) $ TArr_ t a

tyApp :: P TyR
tyApp = typeAtom >>= f
  where
    f t = do
        a <- typeAtom
        f $ Ty' (t <-> a) $ TApp_ t a
      <|> return t

typeAtom :: P TyR
typeAtom = typeRecord
    <|> addPos Ty' (Star_ C <$ operator "*")
    <|> addPos Ty' (TVar_ <$> try' "type var" typeVar)
    <|> addPos Ty' (TLit_ <$> (LNat . fromIntegral <$> natural <|> literal))
    <|> addPos Ty' (TCon_ <$> typeConstructor)
    <|> addPos tTuple (parens (sepBy ty comma))
    <|> addPos (\p -> Ty' p . TApp_ (Ty' p $ TCon_ "List")) (brackets ty)

tTuple :: Range -> [TyR] -> TyR
tTuple p [t] = t
tTuple p ts = Ty' p $ TTuple_ ts

dataDef :: P DefinitionR
dataDef = do
 keyword "data"
 localIndentation Gt $ do
  tc <- typeConstructor
  tvs <- many typeVarKind
  let dataConDef = do
        tc <- upperCaseIdent
        tys <-   braces (sepBy (FieldTy <$> (Just <$> varId) <*> (keyword "::" *> optional (operator "!") *> typeExp)) comma)
            <|>  many (optional (operator "!") *> (FieldTy Nothing <$> typeAtom))
        return $ ConDef tc tys
  do
    do
      keyword "where"
      ds <- localIndentation Ge $ localAbsoluteIndentation $ many $ do
        cs <- do
            cs <- sepBy1 upperCaseIdent comma
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
      return $ DDataDef $ DataDef tc tvs ds


derivingStm = optional $ keyword "deriving" <* (void_ typeConstructor <|> void_ (parens $ sepBy typeConstructor comma))

typeRecord :: P TyR
typeRecord = undef "trec" $ do
  braces (commaSep1 typeSignature >> optional (operator "|" >> void_ typeVar))

-- compose ranges through getTag
infixl 9 <->, <-->
a <-> b = getTag a <--> getTag b
(<-->) :: Range -> Range -> Range
(a1, a2) <--> (b1, b2) = (min a1 a2, max b1 b2)

addPos f m = do
    p1 <- position
    a <- m
    p2 <- position
    return $ f (p1, p2) a

typeClassDef :: P DefinitionR
typeClassDef = do
  keyword "class"
  localIndentation Gt $ do
    optional tcExp
    c <- typeConstraint
    tvs <- many typeVarKind
    ds <- optional $ do
      keyword "where"
      localIndentation Ge $ localAbsoluteIndentation $ many $ do
        typeSignature
    return $ ClassDef c tvs $ maybe [] concat ds

typeVarKind =
      parens ((,) <$> typeVar <* operator "::" <*> ty)
  <|> (,) <$> typeVar <*> pure (Ty' mempty $ Star_ C)

typeClassInstanceDef :: P DefinitionR
typeClassInstanceDef = do
  keyword "instance"
  localIndentation Gt $ do
    optional tcExp
    c <- typeConstraint
    t <- typeAtom
    ds <- optional $ do
      keyword "where"
      localIndentation Ge $ localAbsoluteIndentation $ many $ do
        valueDef
    return $ InstanceDef c t $ fromMaybe [] ds

fixityDef :: P [DefinitionR]
fixityDef = do
  dir <-    FNoDir  <$ keyword "infix" 
        <|> FDLeft  <$ keyword "infixl"
        <|> FDRight <$ keyword "infixr"
  localIndentation Gt $ do
    i <- natural
    ns <- sepBy1 operator' comma
    return [DFixity n (dir, fromIntegral i) | n <- ns]

undef msg = (const (error $ "not implemented: " ++ msg) <$>)

valuePattern :: P PatR
valuePattern
    =   appP <$> some valuePatternOpAtom

appP :: [PatR] -> PatR
appP [p] = p
appP (PCon r n xs: ps) = PCon r n $ xs ++ ps
appP xs = error $ "appP: " ++ ppShow xs

valuePatternOpAtom :: P PatR
valuePatternOpAtom = do
    e <- appP <$> some valuePatternAtom
    f e <$> op <*> valuePattern  <|>  return e
  where
    f e op e' = appP [op, e, e']

    op :: P PatR
    op = addPos (\p x -> PCon p x []) operator'
--        <|> try' "backquote operator" (runUnspaced (Unspaced (operator "`") *> Unspaced (var <|> upperCaseIdent) <* Unspaced (operator "`")))

valuePatternAtom :: P PatR
valuePatternAtom
    =   addPos Pat (const Wildcard_ <$> operator "_")
    <|> addPos Pat (PAt_ <$> try' "at pattern" (var <* operator "@") <*> valuePatternAtom)
    <|> addPos PVar var
    <|> addPos (\p c -> PCon p c []) (try dataConstructor)
    <|> tuplePattern
    <|> recordPat
    <|> listPat
    <|> parens valuePattern
 where
  tuplePattern :: P PatR
  tuplePattern = try' "tuple" $ addPos PTuple $ parens ((:) <$> valuePattern <*> some (comma *> valuePattern))

  recordPat :: P PatR
  recordPat = addPos PRecord $ braces (sepBy ((,) <$> var <* colon <*> valuePattern) comma)

  listPat :: P PatR
  listPat = addPos (\p -> foldr cons (nil p)) $ brackets $ commaSep valuePattern
    where
      nil (p1, p2) = PCon (p2 {- - 1 -}, p2) "Nil" []
      cons a b = PCon mempty "Cons" [a, b]

eLam p e_ ps = ELam (p <-> e) p e where e = e_ ps

valueDef :: P DefinitionR
valueDef = do
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
      localIndentation Gt $ do
        lookAhead $ operator "=" <|> operator "|"
        return $ \e -> ValueDef (n, alts 0 [compileWhereRHS e])
    )
  localIndentation Gt $ do
    e <- whereRHS $ operator "="
    return $ f e

whereRHS :: P () -> P WhereRHSR
whereRHS delim = do
    d <- rhs delim
    do
        do
          keyword "where"
          l <- localIndentation Ge $ localAbsoluteIndentation $ some ((:[]) <$> valueDef <|> typeSignature)
          return (WhereRHS d $ Just $ concat l)
      <|> return (WhereRHS d Nothing)

rhs :: P () -> P GuardedRHSR
rhs delim = NoGuards <$> xx
  <|> Guards <$> many ((,) <$> (operator "|" *> expression) <*> xx)
  where
    xx = delim *> expression

application :: [ExpR] -> ExpR
application [e] = e
application es = eApp (application $ init es) (last es)

eApp :: ExpR -> ExpR -> ExpR
eApp = liftA2 eApp'

eApp' :: ExpR' -> ExpR' -> ExpR'
eApp' a b = EApp (a <-> b) a b

expression :: P ExpR
expression = do
  e <-
      ifthenelse <|>
      caseof <|>
      letin <|>
      lambda <|>
      eApp <$> addPos eVar (const "negate" <$> operator "-") <*> expressionOpAtom <|> -- TODO: precedence
      expressionOpAtom
  do
      do
        operator "::"
        t <- typeExp
        return $ eTyping e t
    <|> return e
 where
  lambda :: P ExpR
  lambda = (\(ps, e) -> foldr eLam e ps) <$> (operator "\\" *> ((,) <$> many valuePatternAtom <* operator "->" <*> expression))

  ifthenelse :: P ExpR
  ifthenelse = addPos (\r (a, b, c) -> eApp (eApp (eApp (eVar r "ifThenElse") a) b) c) $
        (,,) <$ keyword "if" <*> expression <* keyword "then" <*> expression <* keyword "else" <*> expression

  caseof :: P ExpR
  caseof = addPos (uncurry . compileCases) $ do
    keyword "case"
    e <- expression
    keyword "of"
    pds <- localIndentation Ge $ localAbsoluteIndentation $ some $
        (,) <$> valuePattern <*> localIndentation Gt (whereRHS $ operator "->")
    return (e, pds)

  letin :: P ExpR
  letin = do
      keyword "let"
      l <- localIndentation Ge $ localAbsoluteIndentation $ some valueDef
      keyword "in"
      a <- expression
      return $ eLets l a

eLets :: [DefinitionR] -> ExpR -> ExpR
eLets l a ps = foldr ($) (a ps) . map eLet $ groupDefinitions l
  where
    eLet (ValueDef (a, b_)) = ELet (a <-> b) a b where b = b_ ps

eTyping :: ExpR -> TyR -> ExpR
eTyping a_ b ps = ETyping (a <-> b) a b  where a = a_ ps

expressionOpAtom :: P ExpR
expressionOpAtom = do
    e <- application <$> some expressionAtom
    f e <$> op <*> expression  <|>  return e
  where
    f e op e' = application [op, e, e']

    op = addPos eVar $ operator'
        <|> try' "backquote operator" ({-runUnspaced-} ({-Unspaced-} (operator "`") *> {-Unspaced-} (var <|> upperCaseIdent) <* {-Unspaced-} (operator "`")))

expressionAtom :: P ExpR
expressionAtom = do
    e <- expressionAtom_
    ts <- many $ do
        operator "@"
        typeAtom
    return $ foldl eTyApp e ts

eTyApp a_ b ps = ETyApp (a <-> b) a b where a = a_ ps

expressionAtom_ :: P ExpR
expressionAtom_ =
  listExp <|>
  addPos (ret eLit) literal <|>
  recordExp <|>
  recordExp' <|>
  recordFieldProjection <|>
  addPos eVar qVar <|>
  addPos eVar dataConstructor <|>
  tuple
 where
  tuple :: P ExpR
  tuple = addPos eTuple $ parens $ sepBy expression comma

  recordExp :: P ExpR
  recordExp = addPos eRecord $ braces $ sepBy ((,) <$> var <* colon <*> expression) comma

  recordExp' :: P ExpR
  recordExp' = try $ addPos (uncurry . eNamedRecord) ((,) <$> dataConstructor <*> braces (sepBy ((,) <$> var <* keyword "=" <*> expression) comma))

  recordFieldProjection :: P ExpR
  recordFieldProjection = try $ flip eApp <$> addPos eVar var <*>
        addPos (ret EFieldProj) ({-runUnspaced $-} dot *> {-Unspaced-} var)

  eLit p l@LInt{} = eApp' (EVar p "fromInt") $ ELit p l
  eLit p l = ELit p l

  listExp :: P ExpR
  listExp = addPos (\p -> foldr cons (nil p)) $ brackets $ commaSep expression
    where
      nil (p1, p2) = eVar (p2 {- - 1 -}, p2) "Nil"
      cons a b = eApp (eApp (eVar mempty{-TODO-} "Cons") a) b

literal :: P Lit
literal =
    LFloat <$> try double <|>
    LInt <$> try integer <|>
    LChar <$> charLiteral <|>
    LString <$> stringLiteral

eTuple _ [x] ps = x ps
eTuple p xs ps = ETuple p $ map ($ ps) xs
eRecord p xs ps = ERecord p (map (id *** ($ ps)) xs)
eNamedRecord p n xs ps = ENamedRecord p n (map (id *** ($ ps)) xs)
ret f x y = const $ f x y
ret' f x y ps = f x (y ps)
eVar p n = \ps -> EVar p n

parseLC :: String -> IO (Either String (String, ModuleR))
parseLC fname = do
  src <- readFile fname
  let setName = setPosition =<< flip setSourceName fname <$> getPosition
  case runParser (setName *> whiteSpace *> moduleDef fname <* eof) () "" (mkIndentStream 0 infIndentation True Ge $ I.mkCharIndentStream src) of
    Left err -> return $ Left $ show err
    Right e  -> return $ Right (src,e)
