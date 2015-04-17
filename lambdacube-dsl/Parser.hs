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

varId :: P String
varId = var <|> parens (ident lcOps :: P String)

moduleName :: P ()
moduleName = void_ $ do
  l <- sepBy1 (ident lcIdents) dot
  when (any (isLower . head) l) $ fail "module name must start with capital letter"

moduleDef :: P Module
moduleDef = do
  optional $ do
    keyword "module"
    moduleName
    optional $ parens (commaSep varId)
    keyword "where"
  defs <- localAbsoluteIndentation $ do
    many importDef
    -- TODO: unordered definitions
    many $ choice
        [ DDataDef <$> dataDef
        , TypeSig <$> try' "type signature" typeSignature
        , undef "typeSyn" $ typeSynonym
        , undef "class" $ typeClassDef
        , (\(r, (r', n), e) -> ValueDef (n, e)) <$> try' "definition" valueDef_
        , undef "fixity" fixityDef
        , undef "instance" typeClassInstanceDef
        ]
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
  moduleName
  optional $ parens (commaSep varId)

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
  varId
  localIndentation Gt $ do
    operator "::"
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

dataDef :: P DataDef
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

tApp :: TyR -> TyR -> TyR
tApp a b = Ty' mempty $ TApp_ a b

ty :: P TyR
ty = chainr1 (foldl1 tApp <$> some typeAtom) (do operator "->"; return $ \a b -> Ty' mempty $ TArr_ a b)

typeAtom :: P TyR
typeAtom = typeRecord
    <|> Ty' mempty . TVar_ C <$> (try typeVar)
    <|> try (parens $ pure $ Ty' mempty $ TCon_ C "()")
    <|> Ty' mempty . TCon_ C <$> typeConstructor
    <|> Ty' mempty . TTuple_ C <$> (parens $ sepBy ty comma)
    <|> Ty' mempty . TApp_ (Ty' mempty (TCon_ C "[]")) <$> (brackets ty)-- <|> (do typeAtom; operator "->"; typeAtom)

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

valuePattern :: P (Exp Range)
valuePattern
    =   undef "_" (operator "_")
    <|> (try $ undef "@" $ var *> operator "@" *> valuePattern)
    <|> (\p1 v p2 -> EVar (p1,p2) v) <$> position <*> var <*> position
    <|> (undef "data Constr" $ try dataConstructor)
    <|> try pat
 where
  pat = parens ((try $ undef "data Const 2" $ dataConstructor *> some valuePattern) <|> (\p1 v p2 -> EVar (p1,p2) v) <$> position <*> var <*> position)

valueDef :: P (Exp Range -> Exp Range)
valueDef = (\(r, (r', n), e) -> ELet r (PVar r' n) e) <$> valueDef_

valueDef_ :: P (Range, (Range, String), Exp Range)
valueDef_ = do
  p1 <- position
  n <- varId
  p1' <- position
  localIndentation Gt $ do
    a <- many valuePattern
    operator "="
    d <- expression
    optional $ do
      keyword "where"
      localAbsoluteIndentation $ some valueDef
    p2 <- position
    return ((p1,p2), ((p1,p1'), n), foldr (args (p1,p2)) d a)
  where
    args r (EVar r' n) e = ELam r (PVar r' n) e

application :: [Exp Range] -> Exp Range
application [e] = e
application es = EApp (mempty,mempty){-undefined-}{-TODO-} (application $ init es) (last es)

expression :: P (Exp Range)
expression = application <$> some (
  exp listExp <|>
  exp ((\p1 l p2 -> ELit (p1,p2) l) <$> position <*> literalExp <*> position) <|>
  exp recordExp <|>
  exp recordExp' <|>
  exp recordFieldProjection <|>
  exp lambda <|>
  exp ifthenelse <|>
  exp caseof <|>
  exp' letin <|>
  (\p1 v p2 -> EVar (p1,p2) v) <$> position <*> var <*> position <|>
  (\p1 v p2 -> EVar (p1,p2) v) <$> position <*> dataConstructor <*> position <|> -- TODO: distinct data constructors
  exp unit <|>
  exp tuple <|>
  parens expression)    -- TODO: tuple aready handles parens
 where
  exp :: P (Exp Range) -> P (Exp Range)
  exp p = try (p <* optional (operator "::" *> void_ typeExp))

  exp' p = p <* optional (operator "::" *> void_ typeExp)

  tuple :: P (Exp Range)
  tuple = (\p1 (v, vs) p2 -> if null vs then v else ETuple (p1,p2) (v:vs)) <$> position <*> parens ((,) <$> expression <*> some (comma *> expression)) <*> position

  unit :: P (Exp Range)
  unit = (\p1 p2 -> ETuple (p1,p2) []) <$> position <* parens (pure ()) <*> position

  lambda :: P (Exp Range)
  lambda = (\p1 (EVar r n: _ {-TODO-}) e p2 -> ELam (p1,p2) (PVar r n) e) <$> position <* operator "\\" <*> many valuePattern <* operator "->" <*> expression <*> position

  ifthenelse :: P (Exp Range)
  ifthenelse = undef "if" $ keyword "if" *> (expression *> keyword "then" *> expression *> keyword "else" *> expression)

  caseof :: P (Exp Range)
  caseof = undef "case" $ do
    keyword "case"
    expression
    keyword "of"
    localAbsoluteIndentation $ some $
        valuePattern *> operator "->" *> localIndentation Gt expression

  letin :: P (Exp Range)
  letin = do
      keyword "let"
      l <- localIndentation Ge $ localAbsoluteIndentation (some valueDef)
      keyword "in"
      a <- expression
      return $ foldr ($) a l

  recordExp :: P (Exp Range)
  recordExp = (\p1 v p2 -> ERecord (p1,p2) v) <$> position <*> braces (sepBy ((,) <$> var <* colon <*> expression) comma) <*> position

  recordExp' :: P (Exp Range)
  recordExp' = dataConstructor *> ((\p1 v p2 -> ERecord (p1,p2) v) <$> position <*> braces (sepBy ((,) <$> var <* keyword "=" <*> expression) comma) <*> position)

  recordFieldProjection :: P (Exp Range)
  recordFieldProjection = try ((\p1 r p f p2 -> EApp (p1,p2) (EFieldProj (p,p2) f) (EVar (p1,p) r)) <$> position <*> var <*> position <* dot <*> var <*> position)

  literalExp :: P Lit
  literalExp =
      LFloat <$> try double <|>
      LInt <$> integer <|>
      LChar <$> charLiteral <|>
      LString <$> stringLiteral <|>
      LNat . fromIntegral <$ operator "#" <*> integer

  listExp :: P (Exp Range)
  listExp = foldr cons nil <$> (brackets $ commaSep expression)
    where
      nil = EVar mempty "[]"
      cons a b = EApp mempty (EApp mempty (EVar mempty ":") a) b

indentState = mkIndentationState 0 infIndentation True Ge
indentState' = mkIndentationState 0 infIndentation False Ge

test = parseLC "syntax02.lc"

parseLC :: String -> IO (Either String Module)
parseLC fname = do
  src <- BS.readFile fname
  case parseByteString (runLCParser $ evalIndentationParserT (whiteSpace *> moduleDef <* eof) indentState) (Directed (pack fname) 0 0 0 0) src of
    Failure m -> do
      print m
      return (Left $ show m)
    Success e -> print "Parsed" >> (return $ Right e)

parseLC_ :: String -> IO (BS.ByteString, Result (Exp Range))
parseLC_ fname = do
  src <- BS.readFile fname
  return (src, parseByteString (runLCParser $ evalIndentationParserT (whiteSpace *> expression <* eof) indentState') (Directed (pack fname) 0 0 0 0) src)
--main = test

-- AST
data Module
  = Module
  { moduleImports :: ()
  , moduleExports :: ()
  , typeAliases   :: ()
  , definitions   :: [ValueDef]
  , dataDefs      :: [DataDef]
  , typeClasses   :: ()
  , instances     :: ()
  }
    deriving (Show)

data Definition
  = ValueDef ValueDef
  | TypeSig ()
  | DDataDef DataDef
    deriving (Show)

type ValueDef = (String, Exp Range)
data DataDef = DataDef String [String] [ConDef]
    deriving (Show)
data ConDef = ConDef EName [TyR]
    deriving (Show)

data Expression -- record, list, tuple, literal, var, application, lambda, ifthenelse, letin, caseof, dataconstructor
  = Expression

data Type
  = Type

data TypeClassDefinition
  = TypeClassDefinition -- name, [base class], [type signature (declaration)]

data TypeClassInstance -- name, type, [definition]
  = TypeClassInstance
