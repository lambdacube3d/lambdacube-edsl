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
import Text.Trifecta.Indentation as I
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

typeConstructor :: P ()
typeConstructor = void_ $ do
  i <- ident lcIdents
  if isUpper $ head i then return i else fail "type name must start with capital letter"

typeVar :: P ()
typeVar = void_ $ do
  i <- ident lcIdents
  if isUpper $ head i then fail "type variable name must start with lower case letter" else return i

dataConstructor :: P String
dataConstructor = try $ do
  i <- ident lcIdents
  if isUpper $ head i then return i else fail "data constructor must start with capital letter"

var :: P String
var = try $ do
  i <- ident lcIdents
  if isUpper $ head i then fail "variable name must start with lower case letter" else return i

varId :: P String
varId = var <|> parens (ident lcOps :: P String)

moduleName :: P ()
moduleName = void_ $ do
  l <- sepBy1 (ident lcIdents) dot
  when (any (isLower . head) l) $ fail "module name must start with capital letter"

moduleDef :: P ()
moduleDef = void_ $ do
  optional $ do
    keyword "module"
    moduleName
    optional $ parens (commaSep varId)
    keyword "where"
  localAbsoluteIndentation $ do
    many importDef
    -- TODO: unordered definitions
    many $ choice [dataDef,try typeSignature,typeSynonym,typeClassDef,void_ $ try valueDef,fixityDef,typeClassInstanceDef]

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
    typeExp

typeSignature :: P ()
typeSignature = void_ $ do
  varId
  localIndentation Gt $ do
    operator "::"
    typeExp

typePattern :: P ()
typePattern = choice [try typeVar, typeConstructor, void_ $ parens ((typeConstructor <* some typePattern) <|> typePattern)]

tcExp :: P ()
tcExp = void_ $ try $ do
  let tyC = void_ $ typeConstraint >> typePattern
  tyC <|> (void_ $ parens (sepBy1 tyC comma))
  operator "=>"

typeExp :: P ()
typeExp = do
  optional (keyword "forall" >> some typeVar >> operator ".")
  optional (tcExp <?> "type context")
  ty

dataDef :: P ()
dataDef = do
  keyword "data"
  localIndentation Gt $ do
  typeConstructor
  many typeVar
  let dataConDef = typeConstructor <* many typeExp
  operator "=" *> dataConDef
  many (operator "|" *> dataConDef)
  return ()

typeRecord :: P ()
typeRecord = void_ $ do
  braces (commaSep1 typeSignature >> optional (operator "|" >> typeVar))

ty = chainl1 (void_ $ some typeAtom) (do operator "->"; return const)

typeAtom :: P ()
typeAtom = typeRecord <|> try typeVar <|> typeConstructor <|> (parens ty) <|> brackets ty-- <|> (do typeAtom; operator "->"; typeAtom)

typeClassDef :: P ()
typeClassDef = void_ $ do
  keyword "class"
  localIndentation Gt $ do
    optional tcExp
    typeConstraint
    typeVar
    optional $ do
      keyword "where"
      localIndentation Gt $ localAbsoluteIndentation $ do
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
      localIndentation Gt $ localAbsoluteIndentation $ do
        many valueDef

fixityDef :: P ()
fixityDef = void_ $ do
  keyword "infix" <|> keyword "infixl" <|> keyword "infixr"
  localIndentation Gt $ do
    natural
    ident lcOps :: P String

undef = (const undefined <$>)

valuePattern :: P (Exp Range)
valuePattern
    =   try (undef $ operator "_")
    <|> (try $ undef $ var *> operator "@" *> valuePattern)
    <|> (\p1 v p2 -> EVar (p1,p2) v) <$> position <*> var <*> position
    <|> (undef $ try dataConstructor)
    <|> try pat
 where
  pat = parens ((try $ undef $ dataConstructor *> some valuePattern) <|> (\p1 v p2 -> EVar (p1,p2) v) <$> position <*> var <*> position)

valueDef :: P (Exp Range -> Exp Range)
valueDef = do
  p1 <- position
  n <- varId
  p1' <- position
  (a, d) <- localIndentation Gt $ do
    a <- many valuePattern
    operator "="
    d <- expression
    return (a, d)
  optional $ localIndentation Gt $ do
    keyword "where"
    localIndentation Gt $ localAbsoluteIndentation $ some valueDef
  p2 <- position
  return $ \e -> ELet (p1,p2) (PVar (p1,p1') n) (foldr (args (p1,p2)) d a) e
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
  exp recordFieldProjection <|>
  exp lambda <|>
  exp ifthenelse <|>
  exp caseof <|>
  exp letin <|>
  (\p1 v p2 -> EVar (p1,p2) v) <$> position <*> var <*> position <|>
  (\p1 v p2 -> EVar (p1,p2) v) <$> position <*> dataConstructor <*> position <|> -- TODO: distinct data constructors
  exp unit <|>
  exp tuple <|>
  parens expression)    -- TODO: tuple aready handles parens
 where
  gt = localIndentation Gt

  exp :: P (Exp Range) -> P (Exp Range)
  exp p = try (p <* optional (operator "::" *> typeExp))

  tuple :: P (Exp Range)
  tuple = (\p1 (v, vs) p2 -> if null vs then v else ETuple (p1,p2) (v:vs)) <$> position <*> parens ((,) <$> expression <*> some (comma *> expression)) <*> position

  unit :: P (Exp Range)
  unit = (\p1 p2 -> ETuple (p1,p2) []) <$> position <* parens (pure ()) <*> position

  lambda :: P (Exp Range)
  lambda = (\p1 (EVar r n: _ {-TODO-}) e p2 -> ELam (p1,p2) (PVar r n) e) <$> position <* operator "\\" <*> many valuePattern <* operator "->" <*> expression <*> position

  ifthenelse :: P (Exp Range)
  ifthenelse = undef $ keyword "if" *> (gt $ expression *> keyword "then" *> gt expression *> keyword "else" *> gt expression)

  caseof :: P (Exp Range)
  caseof = undef $ do
    let casePat = valuePattern *> operator "->" *> (localIndentation Gt $ expression)
    localIndentation Ge $ do
      l <- keyword "case" *> expression *> keyword "of" <* (localIndentation Gt $ localAbsoluteIndentation $ some casePat) -- WORKS
      return ()

  letin :: P (Exp Range)
  letin = do
    localIndentation Ge $ do
      l <- keyword "let" *> (localIndentation Gt $ localAbsoluteIndentation $ some valueDef) -- WORKS
      a <- keyword "in" *> (localIndentation Gt expression)
      return $ foldr ($) a l

  recordExp :: P (Exp Range)
  recordExp = (\p1 v p2 -> ERecord (p1,p2) v) <$> position <*> braces (sepBy ((,) <$> var <* colon <*> expression) comma) <*> position

  recordFieldProjection :: P (Exp Range)
  recordFieldProjection = try ((\p1 r p f p2 -> EApp (p1,p2) (EFieldProj (p,p2) f) (EVar (p1,p) r)) <$> position <*> var <*> position <* dot <*> var <*> position)

  literalExp :: P Lit
  literalExp =
      LFloat <$> {-try-} double <|>
      LInt <$> integer <|>
      LChar <$> charLiteral <|>
      LString <$> stringLiteral <|>
      LNat . fromIntegral <$ operator "#" <*> integer

  listExp :: P (Exp Range)
  listExp = undef $ brackets $ commaSep expression

indentState = mkIndentationState 0 infIndentation True Ge

test = parseLC "syntax02.lc"

parseLC :: String -> IO (Either String ())
parseLC fname = do
  src <- BS.readFile fname
  case parseByteString (runLCParser $ evalIndentationParserT (whiteSpace *> moduleDef <* eof) indentState) (Directed (pack fname) 0 0 0 0) src of
    Failure m -> do
      print m
      return (Left $ show m)
    Success e -> print "Parsed" >> (return $ Right ())

parseLC_ :: String -> IO (BS.ByteString, Result (Exp Range))
parseLC_ fname = do
  src <- BS.readFile fname
  return (src, parseByteString (runLCParser $ evalIndentationParserT (whiteSpace *> expression <* eof) indentState) (Directed (pack fname) 0 0 0 0) src)
main = test

-- AST
data Module
  = Module
  { moduleImports :: ()
  , moduleExports :: ()
  , typeAliases   :: ()
  , definitions   :: ()
  , typeClasses   :: ()
  , instances     :: ()
  }

data Definition
  = Definition -- name, maybe type, maybe fixity, expression

data Expression -- record, list, tuple, literal, var, application, lambda, ifthenelse, letin, caseof, dataconstructor
  = Expression

data Type
  = Type

data TypeClassDefinition
  = TypeClassDefinition -- name, [base class], [type signature (declaration)]

data TypeClassInstance -- name, type, [definition]
  = TypeClassInstance
