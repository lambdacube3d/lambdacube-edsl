{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
module Parser where

import Control.Applicative
import Data.ByteString.Char8 (unpack,pack)
import Data.Char
import Data.Monoid
import Text.Parser.Expression
import Text.Parser.Token.Style
import Text.PrettyPrint.ANSI.Leijen (pretty)
import Text.Trifecta
import Text.Trifecta.Delta
import Text.Trifecta.Indentation (IndentationRel (Ge, Gt), localIndentation, localAbsoluteIndentation, mkIndentationState, infIndentation, IndentationParserT, evalIndentationParserT)
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashSet as HashSet

import Text.Show.Pretty

import Control.Monad
import Text.Parser.LookAhead

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
        , (\(r, p, e) -> [ValueDef (p, e)]) <$> valueDef_
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
    optional (operator "!") <* void_ typeExp

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
ty = do
    p1 <- position
    t <- typeAtom
    f p1 t
  where
    f p1 t = do
        a <- typeAtom
        p2 <- position
        f p1 $ Ty' (p1, p2) $ TApp_ t a
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
    <|> addPos Ty' (TTuple_ <$> (parens $ sepBy ty comma))
    <|> addPos (\p -> Ty' p . TApp_ (Ty' p $ TCon_ "[]")) (brackets ty)
-- <|> (do typeAtom; operator "->"; typeAtom)

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

--valuePatterns :: P (Pat Range)
--valuePatterns = 

valuePattern :: P (Pat Range)
valuePattern
    =   appP <$> some valuePatternAtom

appP :: [Pat Range] -> Pat Range
appP (PCon r n xs: ps) = PCon r n $ xs ++ ps

valuePatternAtom :: P (Pat Range)
valuePatternAtom
    =   undef "_" (operator "_")
    <|> (try $ undef "@" $ var *> operator "@" *> valuePatternAtom)
    <|> (\p1 v p2 -> PVar (p1,p2) v) <$> position <*> var <*> position
    <|> ((\c -> PCon mempty c []) <$> try dataConstructor)
    <|> tuplePattern
    <|> recordPat
    <|> pat
 where
  tuplePattern :: P (Pat Range)
  tuplePattern = try' "tuple" $ (\p1 (v, vs) p2 -> PTuple (p1,p2) (v:vs)) <$> position <*> parens ((,) <$> valuePattern <*> some (comma *> valuePattern)) <*> position

  recordPat :: P (Pat Range)
  recordPat = (\p1 v p2 -> PRecord (p1,p2) v) <$> position <*> braces (sepBy ((,) <$> var <* colon <*> valuePattern) comma) <*> position

  pat = parens ((try $ undef "data Const 2" $ dataConstructor *> some valuePatternAtom) <|> (\p1 v p2 -> PVar (p1,p2) v) <$> position <*> var <*> position)

valueDef :: P (Exp Range -> Exp Range)
valueDef = (\(r, p, e) -> ELet r p e) <$> valueDef_

valueDef_ :: P (Range, Pat Range, Exp Range)
valueDef_ = do
  p1 <- position
  (n, f) <- 
    try' "definition" (do
      n <- varId
      p1' <- position
      localIndentation Gt $ do
        a <- many valuePatternAtom
        operator "="
        let args r p e = ELam r p e
        return (PVar (p1,p1') n, \d p2 -> foldr (args (p1,p2)) d a)
    )
   <|>
    try' "node definition" (do
      n <- valuePattern
      p1' <- position
      localIndentation Gt $ do
        operator "="
        return (n, \d _ -> d)
    )
  localIndentation Gt $ do
    d <- expression
    optional $ do
      keyword "where"
      localIndentation Ge $ localAbsoluteIndentation $ some (valueDef <|> undef "ts" typeSignature)
    p2 <- position
    return ((p1,p2), n, f d p2)

application :: [Exp Range] -> Exp Range
application [e] = e
application es = EApp mempty (application $ init es) (last es)

expression :: P (Exp Range)
expression = do
  e <-
      ifthenelse <|>
      caseof <|>
      letin <|>
      lambda <|>
      (\e -> application [EVar mempty "negate", e]) <$> (operator "-" *> expressionOpAtom) <|> -- TODO
      expressionOpAtom
  optional (operator "::" *> void_ typeExp)  -- TODO
  return e
 where
  lambda :: P (Exp Range)
  lambda = (\p1 (p: _ {-TODO-}) e p2 -> ELam (p1,p2) p e) <$> position <* operator "\\" <*> many valuePatternAtom <* operator "->" <*> expression <*> position

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
    f e op e' = application [EVar mempty op, e, e']

    op = ident lcOps
        <|> runUnspaced (Unspaced (operator "`") *> Unspaced var <* Unspaced (operator "`"))

expressionAtom :: P (Exp Range)
expressionAtom =
  listExp <|>
  ((\p1 l p2 -> ELit (p1,p2) l) <$> position <*> literalExp <*> position) <|>
  recordExp <|>
  recordExp' <|>
  recordFieldProjection <|>
  (\p1 v p2 -> EVar (p1,p2) v) <$> position <*> qVar <*> position <|>
  (\p1 v p2 -> EVar (p1,p2) v) <$> position <*> dataConstructor <*> position <|> -- TODO: distinct data constructors
  unit <|>
  try tuple <|>
  parens expression
 where
  tuple :: P (Exp Range)
  tuple = (\p1 (v, vs) p2 -> if null vs then v else ETuple (p1,p2) (v:vs)) <$> position <*> parens ((,) <$> expression <*> some (comma *> expression)) <*> position

  unit :: P (Exp Range)
  unit = try $ (\p1 p2 -> ETuple (p1,p2) []) <$> position <* parens (pure ()) <*> position

  recordExp :: P (Exp Range)
  recordExp = (\p1 v p2 -> ERecord (p1,p2) v) <$> position <*> braces (sepBy ((,) <$> var <* colon <*> expression) comma) <*> position

  recordExp' :: P (Exp Range)
  recordExp' = try $ dataConstructor *> ((\p1 v p2 -> ERecord (p1,p2) v) <$> position <*> braces (sepBy ((,) <$> var <* keyword "=" <*> expression) comma) <*> position)

  recordFieldProjection :: P (Exp Range)
  recordFieldProjection = try $ (\p1 r p f p2 -> EApp (p1,p2) (EFieldProj (p,p2) f) (EVar (p1,p) r)) <$> position <*> var <*> position <* dot <*> var <*> position

  literalExp :: P Lit
  literalExp =
      LFloat <$> try double <|>
      LInt <$> try integer <|>
      LChar <$> charLiteral <|>
      LString <$> stringLiteral <|>
      (LNat . fromIntegral <$ operator "#" <*> integer) <?> "type level nat"

  listExp :: P (Exp Range)
  listExp = foldr cons nil <$> (brackets $ commaSep expression)
    where
      nil = EVar mempty "[]"
      cons a b = EApp mempty (EApp mempty (EVar mempty ":") a) b

indentState = mkIndentationState 0 infIndentation True Ge
indentState' = mkIndentationState 0 infIndentation False Ge

test = parseLC "syntax02.lc"

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

