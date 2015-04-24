{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Parser where

import Data.ByteString.Char8 (unpack,pack)
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Control.Applicative
import Control.Arrow
import Control.Monad
import Text.Parser.Expression
import Text.Parser.Token.Style
import Text.Parser.LookAhead
import Text.PrettyPrint.ANSI.Leijen (pretty)
import Text.Trifecta
import Text.Trifecta.Delta
import Text.Trifecta.Indentation (IndentationRel (Ge, Gt), localIndentation, localAbsoluteIndentation, mkIndentationState, infIndentation, IndentationParserT, evalIndentationParserT)
import Text.Show.Pretty
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashSet as HashSet

import Type

type P a = IndentationParserT Char (LCParser Parser) a

newtype LCParser p a = LCParser { runLCParser :: p a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, Parsing, CharParsing, LookAheadParsing, DeltaParsing)

instance TokenParsing p => TokenParsing (LCParser p) where
  someSpace = LCParser $ buildSomeSpaceParser someSpace lcCommentStyle
  nesting = LCParser . nesting . runLCParser
  highlight h = LCParser . highlight h . runLCParser
  token p = p <* whiteSpace

lcCommentStyle = haskellCommentStyle

lcOps = haskell98Ops
  { _styleReserved = HashSet.fromList ["=","\\","#","::",".","@","_","|","->"]
  }

lcIdents = haskell98Idents { _styleReserved = HashSet.fromList reservedIdents }
  where
    reservedIdents =
      [ "module", "import"
      , "let", "in"
      , "where"
      , "if", "then", "else"
      , "class", "instance"
      , "case", "of"
      , "type", "data"
      , "forall"
      , "infix", "infixl", "infixr"
      , "deriving"
      ]

keyword :: String -> P ()
keyword = reserve lcIdents

operator :: String -> P ()
operator = reserve lcOps

void_ a = a >> return ()

typeConstraint :: P ()
typeConstraint = void_ $ do
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
qVar = var <|> runUnspaced (try' "qualified var" $ sepBy (Unspaced upperCaseIdent) dot *> dot *> Unspaced var)

operator' :: P String
operator' = try' "operator" $ do
  i <- ident lcOps
  if head i == ':' then fail "operator cannot start with ':'" else return i

varId :: P String
varId = var <|> parens (ident lcOps :: P String)

--------------------------------------------------------------------------------

type PatR = Pat Range
type ExpR' = Exp Range
type ExpR = Prec -> ExpR'

data WhereRHS = WhereRHS GuardedRHS (Maybe [Definition])
data GuardedRHS
    = Guards [(ExpR, ExpR)]
    | NoGuards ExpR

data Definition
  = ValueDef (PatR, ExpR)
  | PreValueDef (Range, EName) [PatR] WhereRHS --(PatR, Prec -> Exp Range -> Exp Range)  -- before group
  | TypeSig (String, TyR)
  | DDataDef (DataDef Range)
  | DFixity EName (FixityDir, Int)

--------------------------------------------------------------------------------

alts :: [ExpR] -> ExpR
alts es_ ps = Exp (foldr1 (<-->) $ map getTag es) $ EAlts_ es  where es = map ($ ps) es_

compileWhereRHS :: WhereRHS -> ExpR
compileWhereRHS (WhereRHS r md) = maybe x (flip eLets x) md where
    x = compileGuardedRHS r

compileGuardedRHS :: GuardedRHS -> ExpR
compileGuardedRHS (NoGuards e) = e
compileGuardedRHS (Guards gs) = foldr addGuard (\ps -> Exp mempty ENext_) gs
  where
    addGuard (b, x) y = eApp (eApp (eApp (eVar mempty "ifThenElse") b) x) y

compileCases :: Range -> ExpR -> [(PatR, WhereRHS)] -> ExpR
compileCases r e rs = eApp (alts [eLam p $ compileWhereRHS r | (p, r) <- rs]) e

compileRHS :: [Definition] -> Definition
compileRHS ds = case ds of
    (TypeSig (_, t): PreValueDef (r, n) _ _: _)
        -> ValueDef (PVar r n, eTyping (alts [foldr eLam (compileWhereRHS rhs) pats | PreValueDef _ pats rhs <- ds]) $ [] ==> t)
    (PreValueDef (r, n) _ _: _)
        -> ValueDef (PVar r n, alts [foldr eLam (compileWhereRHS rhs) pats | PreValueDef _ pats rhs <- ds])
    [x] -> x

groupDefinitions :: [Definition] -> [Definition]
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

moduleDef :: P (Module Range)
moduleDef = do
  optional $ do
    keyword "module"
    moduleName
    optional $ parens (commaSep varId)
    keyword "where"
  localAbsoluteIndentation $ do
    idefs <- many importDef
    -- TODO: unordered definitions
    defs <- groupDefinitions . concat <$> many (choice
        [ (:[]) . DDataDef <$> dataDef
        , (:[]) <$> typeSignature
        , const [] <$> typeSynonym
        , const [] <$> typeClassDef
        , (:[]) <$> valueDef
        , fixityDef
        , const [] <$> typeClassInstanceDef
        ])
    let ps = Map.fromList [(n, p) | DFixity n p <- defs]
    return $ Module
      { moduleImports = idefs
      , moduleExports = ()
      , typeAliases   = ()
      , definitions   = [id *** ($ ps) $ d | ValueDef d <- defs]
      , dataDefs      = [d | DDataDef d <- defs]
      , typeClasses   = ()
      , instances     = ()
      , precedences   = ps     -- TODO: check multiple definitions
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

typeSignature :: P Definition
typeSignature = TypeSig <$> do
  n <- try' "type signature" $ do
    n <- varId
    localIndentation Gt $ operator "::"
    return n
  t <- localIndentation Gt $ do
    optional (operator "!") *> typeExp
  return (n, t)

typePattern :: P ()  -- TODO
typePattern = choice [void_ (try' "type var" typeVar), void_ typeConstructor, void_ $ parens ((void_ typeConstructor <* some typePattern) <|> typePattern)]

tcExp :: P ()   -- TODO
tcExp = void_ $ try $ do
  let tyC = void_ $ typeConstraint >> typePattern
  tyC <|> (void_ $ parens (sepBy1 tyC comma))
  operator "=>"

typeExp :: P TyR
typeExp = do
  optional (keyword "forall" >> some typeVar >> operator ".")
  optional (tcExp <?> "type context")
  ty

dataDef :: P (DataDef Range)
dataDef = do
 keyword "data"
 localIndentation Gt $ do
  tc <- typeConstructor
  tvs <- many typeVar
  let dataConDef = do
        tc <- typeConstructor
        tys <-   braces (sepBy (FieldTy <$> (Just <$> varId) <*> (keyword "::" *> optional (operator "!") *> typeExp)) comma)
            <|>  many (optional (operator "!") *> (FieldTy Nothing <$> typeExp))
        return $ ConDef tc tys
  operator "="
  ds <- sepBy dataConDef $ operator "|"
  derivingStm
  return $ DataDef tc tvs ds

derivingStm = optional $ keyword "deriving" <* (void_ typeConstructor <|> void_ (parens $ sepBy typeConstructor comma))

typeRecord :: P TyR
typeRecord = undef "trec" $ do
  braces (commaSep1 typeSignature >> optional (operator "|" >> void_ typeVar))

type TyR = Ty' Range

ty :: P TyR
ty = foldr1 tArr <$> sepBy1 tyApp (operator "->")
  where
    tArr t a = Ty' (t <-> a) $ TArr_ t a

-- compose ranges through getTag
infixl 9 <->, <-->
a <-> b = getTag a <--> getTag b
(<-->) :: Range -> Range -> Range
(a1, a2) <--> (b1, b2) = (min a1 a2, max b1 b2)

tyApp :: P TyR
tyApp = typeAtom >>= f
  where
    f t = do
        a <- typeAtom
        f $ Ty' (t <-> a) $ TApp_ t a
      <|> return t

addPos f m = do
    p1 <- position
    a <- m
    p2 <- position
    return $ f (p1, p2) a

typeAtom :: P TyR
typeAtom = typeRecord
    <|> addPos Ty' (TVar_ <$> try' "type var" typeVar)
    <|> addPos Ty' (TNat_ . fromIntegral <$> natural)
    <|> addPos Ty' (TCon_ <$> typeConstructor)
    <|> addPos tTuple (parens (sepBy ty comma))
    <|> addPos (\p -> Ty' p . TApp_ (Ty' p $ TCon_ "[]")) (brackets ty)

tTuple :: Range -> [TyR] -> TyR
tTuple p [t] = t
tTuple p ts = Ty' p $ TTuple_ ts

typeClassDef :: P ()
typeClassDef = void_ $ do
  keyword "class"
  localIndentation Gt $ do
    optional tcExp
    typeConstraint
    typeVar
    optional $ do
      keyword "where"
      localAbsoluteIndentation $ do
        many typeSignature

typeClassInstanceDef :: P ()
typeClassInstanceDef = void_ $ do
  keyword "instance"
  localIndentation Gt $ do
    optional tcExp
    typeConstraint
    typePattern
    optional $ do
      keyword "where"
      localAbsoluteIndentation $ do
        many valueDef

fixityDef :: P [Definition]
fixityDef = do
  dir <-    FNoDir  <$ keyword "infix" 
        <|> FDLeft  <$ keyword "infixl"
        <|> FDRight <$ keyword "infixr"
  localIndentation Gt $ do
    i <- natural
    ns <- sepBy1 (ident lcOps) comma
    return [DFixity n (dir, fromIntegral i) | n <- ns]

undef msg = (const (error $ "not implemented: " ++ msg) <$>)

valuePattern :: P PatR
valuePattern
    =   appP <$> some valuePatternAtom

appP :: [PatR] -> PatR
appP [p] = p
appP (PCon r n xs: ps) = PCon r n $ xs ++ ps
appP xs = error $ "appP: " ++ ppShow xs

valuePatternAtom :: P PatR
valuePatternAtom
    =   addPos Pat (const Wildcard_ <$> operator "_")
    <|> addPos Pat (PAt_ <$> try' "at pattern" (var <* operator "@") <*> valuePatternAtom)
    <|> addPos PVar var
    <|> addPos (\p c -> PCon p c []) (try dataConstructor)
    <|> tuplePattern
    <|> recordPat
    <|> parens valuePattern
 where
  tuplePattern :: P PatR
  tuplePattern = try' "tuple" $ addPos PTuple $ parens ((:) <$> valuePattern <*> some (comma *> valuePattern))

  recordPat :: P PatR
  recordPat = addPos PRecord $ braces (sepBy ((,) <$> var <* colon <*> valuePattern) comma)

eLam p e_ ps = ELam (p <-> e) p e where e = e_ ps

valueDef :: P Definition
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
        return $ \e -> ValueDef (n, alts [compileWhereRHS e])
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
          l <- localIndentation Ge $ localAbsoluteIndentation $ some (valueDef <|> typeSignature)
          return (WhereRHS d $ Just l)
      <|> return (WhereRHS d Nothing)

rhs :: P () -> P GuardedRHS
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
        return $ eTyping e $ [] ==> t
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

eLets :: [Definition] -> ExpR -> ExpR
eLets l a ps = foldr ($) (a ps) . map eLet $ groupDefinitions l
  where
    eLet (ValueDef (a, b_)) = ELet (a <-> b) a b where b = b_ ps

eTyping :: ExpR -> Typing_ TyR -> ExpR
eTyping a_ b ps = ETyping (a <-> typingType{-!-} b) a b  where a = a_ ps

expressionOpAtom :: P ExpR
expressionOpAtom = do
    e <- application <$> some expressionAtom
    f e <$> op <*> expression  <|>  return e
  where
    f e op e' = application [op, e, e']

    op = addPos eVar $
            ident lcOps
        <|> try' "backquote operator" (runUnspaced (Unspaced (operator "`") *> Unspaced (var <|> upperCaseIdent) <* Unspaced (operator "`")))
        <|> (dot *> pure ".")

expressionAtom :: P ExpR
expressionAtom =
  listExp <|>
  addPos (ret eLit) literalExp <|>
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
        addPos (ret EFieldProj) (runUnspaced $ dot *> Unspaced var)

  eLit p l@LInt{} = eApp' (EVar p "fromInt") $ ELit p l
  eLit p l = ELit p l

  literalExp :: P Lit
  literalExp =
      LFloat <$> try double <|>
      LInt <$> try integer <|>
      LChar <$> charLiteral <|>
      LString <$> stringLiteral <|>
      (LNat . fromIntegral <$ operator "#" <*> integer) <?> "type level nat"

  listExp :: P ExpR
  listExp = addPos (\p -> foldr cons (nil p)) $ brackets $ commaSep expression
    where
      nil (p1, p2) = eVar (p2 {- - 1 -}, p2) "[]"
      cons a b = eApp (eApp (eVar mempty{-TODO-} ":") a) b

eTuple _ [x] ps = x ps
eTuple p xs ps = ETuple p $ map ($ ps) xs
eRecord p xs ps = ERecord p (map (id *** ($ ps)) xs)
eNamedRecord p n xs ps = ENamedRecord p n (map (id *** ($ ps)) xs)
ret f x y = const $ f x y
ret' f x y ps = f x (y ps)
eVar p n = \ps -> EVar p n

indentState = mkIndentationState 0 infIndentation True Ge

parseLC :: String -> IO (Either String (BS.ByteString, Module Range))
parseLC fname = do
  src <- BS.readFile fname
  case parseByteString (runLCParser $ evalIndentationParserT (whiteSpace *> moduleDef <* eof) indentState) (Directed (pack fname) 0 0 0 0) src of
    Failure m -> return $ Left $ show m
    Success e -> return $ Right (src, e)

