{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--module Parser where

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
keyword w = reserve lcIdents w

operator :: String -> P ()
operator w = reserve lcOps w

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

dataConstructor :: P ()
dataConstructor = void_ $ do
  i <- ident lcIdents
  if isUpper $ head i then return i else fail "data constructor must start with capital letter"

var :: P ()
var = void_ $ do
  i <- ident lcIdents
  if isUpper $ head i then fail "variable name must start with lower case letter" else return i

varId :: P ()
varId = void_ var <|> void_ (parens $ (ident lcOps :: P String))

moduleName :: P ()
moduleName = void_ $ do
  l <- sepBy1 (ident lcIdents) dot
  when (any (isLower . head) l) $ fail "module name must start with capital letter"

moduleDef :: P ()
moduleDef = void_ $ optional $ do
  keyword "module"
  moduleName
  optional $ parens (commaSep varId)
  keyword "where"
  localAbsoluteIndentation $ do
    many importDef
    -- TODO: unordered definitions
    many $ choice [dataDef,try typeSignature,typeSynonym,typeClassDef,try valueDef,fixityDef,typeClassInstanceDef]

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

valuePattern :: P ()
valuePattern = try (operator "_") <|> (try $ void_ $ var *> operator "@" *> valuePattern) <|> try var <|> try dataConstructor <|> try pat
 where
  pat = parens ((try $ void_ $ dataConstructor *> some valuePattern) <|> var)

valueDef :: P ()
valueDef = try $ do
  varId
  localIndentation Gt $ do
    many valuePattern
    operator "="
    expression
  optional $ localIndentation Gt $ do
    keyword "where"
    localIndentation Gt $ localAbsoluteIndentation $ some valueDef
  return ()

recordExp :: P ()
recordExp = void_ $ braces (sepBy (varId <* colon <* expression) comma)

literalExp :: P ()
literalExp = choice [void_ (stringLiteral :: P String),void_ double,void_ integer]

listExp :: P ()
listExp = void_ $ brackets $ commaSep expression

expression :: P ()
expression = void_ $ some (try listExp <|> try literalExp <|> try recordExp <|> try ifthenelse <|> try caseof <|> try letin <|> try var <|> try dataConstructor <|> (try $ parens expression) <|> (try $ parens (pure ())) <|> (parens (expression <* some (comma *> expression))))
 where
  gt = localIndentation Gt
  ifthenelse = keyword "if" *> (gt $ expression *> keyword "then" *> gt expression *> keyword "else" *> gt expression)
  caseof = do
    let casePat = valuePattern *> operator "->" *> (localIndentation Gt $ expression)
    localIndentation Ge $ do
      l <- keyword "case" *> expression *> keyword "of" <* (localIndentation Gt $ localAbsoluteIndentation $ some casePat) -- WORKS
      return ()
  letin = do
    localIndentation Ge $ do
      l <- keyword "let" *> (localIndentation Gt $ localAbsoluteIndentation $ some valueDef) -- WORKS
      a <- keyword "in" *> (localIndentation Gt expression)
      return ()

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

main = test