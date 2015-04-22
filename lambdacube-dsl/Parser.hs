{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
module Parser where

import Data.ByteString.Char8 (unpack,pack)
import Data.Char
import Data.Monoid
import Control.Applicative
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
qVar = var <|> runUnspaced (try $ sepBy (Unspaced upperCaseIdent) (Unspaced dot) *> Unspaced dot *> Unspaced var)

operator' :: P String
operator' = try' "operator" $ do
  i <- ident lcOps
  if head i == ':' then fail "operator cannot start with ':'" else return i

varId :: P String
varId = var <|> parens (ident lcOps :: P String)

moduleName :: P ()
moduleName = void_ $ do
  l <- sepBy1 (ident lcIdents) dot
  when (any (isLower . head) l) $ fail "module name must start with capital letter"

moduleDef :: P (Module Range)
moduleDef = do
  optional $ do
    keyword "module"
    moduleName
    optional $ parens (commaSep varId)
    keyword "where"
  defs <- localAbsoluteIndentation $ do
    many importDef
    -- TODO: unordered definitions
    concat <$> many (choice
        [ (:[]) . DDataDef <$> dataDef
        , (:[]) . TypeSig <$> typeSignature
        , const [] <$> typeSynonym
        , const [] <$> typeClassDef
        , (\(p, e) -> [ValueDef (p, e)]) <$> valueDef_
        , const [] <$> fixityDef
        , const [] <$> typeClassInstanceDef
        ])
  return $ Module
      { moduleImports = ()
      , moduleExports = ()
      , typeAliases   = ()
      , definitions   = [d | ValueDef d <- defs]
      , dataDefs      = [d | DDataDef d <- defs]
      , typeClasses   = ()
      , instances     = ()
      }


importDef :: P ()
importDef = void_ $ do
  keyword "import"
  optional $ keyword "qualified"
  moduleName
  let importlist = parens (commaSep (varId <|> dataConstructor))
  optional $
        (keyword "hiding" >> importlist)
    <|> importlist
  optional $ do
    keyword "as"
    moduleName

typeSynonym :: P ()
typeSynonym = void_ $ do
  keyword "type"
  localIndentation Gt $ do
    typeConstructor
    many typeVar
    operator "="
    void_ typeExp

typeSignature :: P ()
typeSignature = void_ $ do
  try' "type signature" $ do
    varId
    localIndentation Gt $ do
      operator "::"
  localIndentation Gt $ do
    optional (operator "!") <* typeExp

typePattern :: P ()
typePattern = choice [void_ (try typeVar), void_ typeConstructor, void_ $ parens ((void_ typeConstructor <* some typePattern) <|> typePattern)]

tcExp :: P ()
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
        tys <-   braces (sepBy (varId *> keyword "::" *> optional (operator "!") *> typeExp) comma)
            <|>  many (optional (operator "!") *> typeExp)
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
infixl 9 <->
a <-> b = (fst $ getTag a, snd $ getTag b)

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
    <|> addPos Ty' (TVar_ <$> try typeVar)
    <|> try (addPos Ty' $ parens $ pure $ TCon_ "()")
    <|> addPos Ty' (TNat_ . fromIntegral <$> natural)
    <|> addPos Ty' (TCon_ <$> typeConstructor)
    <|> addPos Ty' (TTuple_ <$> parens (sepBy ty comma))
    <|> addPos (\p -> Ty' p . TApp_ (Ty' p $ TCon_ "[]")) (brackets ty)

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

fixityDef :: P ()
fixityDef = void_ $ do
  keyword "infix" <|> keyword "infixl" <|> keyword "infixr"
  localIndentation Gt $ do
    natural
    ident lcOps :: P String

undef msg = (const (error $ "not implemented: " ++ msg) <$>)

valuePattern :: P (Pat Range)
valuePattern
    =   appP <$> some valuePatternAtom

appP :: [Pat Range] -> Pat Range
appP (PCon r n xs: ps) = PCon r n $ xs ++ ps

valuePatternAtom :: P (Pat Range)
valuePatternAtom
    =   addPos Pat (const Wildcard_ <$> operator "_")
    <|> (try $ undef "@" $ var *> operator "@" *> valuePatternAtom)
    <|> addPos PVar var
    <|> addPos (\p c -> PCon p c []) (try dataConstructor)
    <|> tuplePattern
    <|> recordPat
    <|> pat
 where
  tuplePattern :: P (Pat Range)
  tuplePattern = try' "tuple" $ addPos PTuple $ parens ((:) <$> valuePattern <*> some (comma *> valuePattern))

  recordPat :: P (Pat Range)
  recordPat = addPos PRecord $ braces (sepBy ((,) <$> var <* colon <*> valuePattern) comma)

  pat = parens ((try $ undef "data Const 2" $ dataConstructor *> some valuePatternAtom) <|> addPos PVar var)

valueDef :: P (Exp Range -> Exp Range)
valueDef = (\(p, e) -> ELet (p <-> e) p e) <$> valueDef_

valueDef_ :: P (Pat Range, Exp Range)
valueDef_ = do
  (n, f) <- 
    try' "definition" (do
      n <- addPos PVar varId
      localIndentation Gt $ do
        a <- many valuePatternAtom
        operator "="
        let args p e = ELam (p <-> e) p e
        return (n, \d -> foldr args d a)
    )
   <|>
    try' "node definition" (do
      n <- valuePattern
      localIndentation Gt $ do
        operator "="
        return (n, id)
    )
  localIndentation Gt $ do
    d <- expression
    optional $ do
      keyword "where"
      localIndentation Ge $ localAbsoluteIndentation $ some (valueDef <|> undef "ts" typeSignature)
    return (n, f d)

application :: [Exp Range] -> Exp Range
application [e] = e
application es = eApp (application $ init es) (last es)

eApp a b = EApp (a <-> b) a b

expression :: P (Exp Range)
expression = do
  e <-
      ifthenelse <|>
      caseof <|>
      letin <|>
      lambda <|>
      eApp <$> addPos EVar (const "negate" <$> operator "-") <*> expressionOpAtom <|> -- TODO: precedence
      expressionOpAtom
  optional (operator "::" *> void_ typeExp)  -- TODO
  return e
 where
  lambda :: P (Exp Range)
  lambda = addPos (\r (p: _ {-TODO-}, e) -> ELam r p e) $ operator "\\" *> ((,) <$> many valuePatternAtom <* operator "->" <*> expression)

  ifthenelse :: P (Exp Range)
  ifthenelse = undef "if" $ keyword "if" *> (expression *> keyword "then" *> expression *> keyword "else" *> expression)

  caseof :: P (Exp Range)
  caseof = undef "case" $ do
    keyword "case"
    expression
    keyword "of"
    localIndentation Ge $ localAbsoluteIndentation $ some $
        valuePattern *> localIndentation Gt rhs

  letin :: P (Exp Range)
  letin = do
      keyword "let"
      l <- localIndentation Ge $ localAbsoluteIndentation (some valueDef)
      keyword "in"
      a <- expression
      return $ foldr ($) a l

rhs = x
  <|> undef "guards" (many $ operator "|" *> expression *> x)
  where
    x = operator "->" *> expression

expressionOpAtom :: P (Exp Range)
expressionOpAtom = do
    e <- application <$> some expressionAtom
    f e <$> try op <*> expression <|> return e
  where
    f e op e' = application [op, e, e']

    op = addPos EVar $
            ident lcOps
        <|> runUnspaced (Unspaced (operator "`") *> Unspaced var <* Unspaced (operator "`"))

expressionAtom :: P (Exp Range)
expressionAtom =
  listExp <|>
  addPos ELit literalExp <|>
  recordExp <|>
  recordExp' <|>
  recordFieldProjection <|>
  addPos EVar qVar <|>
  addPos EVar dataConstructor <|>
  unit <|>
  try tuple <|>
  parens expression
 where
  tuple :: P (Exp Range)
  tuple = addPos ETuple $ parens $ (:) <$> expression <*> some (comma *> expression)

  unit :: P (Exp Range)
  unit = try $ addPos ETuple $ parens $ pure []

  recordExp :: P (Exp Range)
  recordExp = addPos ERecord $ braces $ sepBy ((,) <$> var <* colon <*> expression) comma

  recordExp' :: P (Exp Range)
  recordExp' = try $ dataConstructor {-TODO-} *> addPos ERecord (braces $ sepBy ((,) <$> var <* keyword "=" <*> expression) comma)

  recordFieldProjection :: P (Exp Range)
  recordFieldProjection = try $ addPos (uncurry . flip . EApp) $ (,) <$> addPos EVar var <*> addPos EFieldProj (dot *> var)

  literalExp :: P Lit
  literalExp =
      LFloat <$> try double <|>
      LInt <$> try integer <|>
      LChar <$> charLiteral <|>
      LString <$> stringLiteral <|>
      (LNat . fromIntegral <$ operator "#" <*> integer) <?> "type level nat"

  listExp :: P (Exp Range)
  listExp = addPos (\p -> foldr cons (nil p)) $ brackets $ commaSep expression
    where
      nil (p1, p2) = EVar (p2 {- - 1 -}, p2) "[]"
      cons a b = eApp (eApp (EVar mempty{-TODO-} ":") a) b

indentState = mkIndentationState 0 infIndentation True Ge

parseLC :: String -> IO (Either String (BS.ByteString, Module Range))
parseLC fname = do
  src <- BS.readFile fname
  case parseByteString (runLCParser $ evalIndentationParserT (whiteSpace *> moduleDef <* eof) indentState) (Directed (pack fname) 0 0 0 0) src of
    Failure m -> return $ Left $ show m
    Success e -> return $ Right (src, e)

data Definition a
  = ValueDef (ValueDef a)
  | TypeSig ()
  | DDataDef (DataDef a)
    deriving (Show)

