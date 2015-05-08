module ParserUtil
    ( module ParserUtil
    , ParseError
    ) where

import Control.Monad.Reader
import qualified Text.Parsec.Indentation.Char as I
import qualified Text.Parsec.Indentation.Token as I
import qualified Text.Parsec.Token as P
import Text.Parsec.Indentation as I
import Text.Parsec.Language (haskellDef)
import Text.Parsec hiding (optional)

type P_ st = Parsec (I.IndentStream (I.CharIndentStream String)) st

lexer = I.makeTokenParser $ I.makeIndentLanguageDef haskellDef

position :: P_ st SourcePos
position = getPosition

optional :: P_ st a -> P_ st (Maybe a)
optional = optionMaybe

keyword :: String -> P_ st ()
keyword = P.reserved lexer

operator :: String -> P_ st ()
operator = P.reservedOp lexer

lcIdents = P.identifier lexer
lcOps = P.operator lexer

ident = id
--ident _   = P.identifier lexer
--identOp   = P.operator lexer
parens    = P.parens lexer
braces    = P.braces lexer
brackets  = P.brackets lexer
commaSep  = P.commaSep lexer
commaSep1 = P.commaSep1 lexer
dot       = P.dot lexer
comma     = P.comma lexer
colon     = P.colon lexer
natural   = P.natural lexer
integer   = P.integer lexer
double    = P.float lexer
charLiteral   = P.charLiteral lexer
stringLiteral = P.stringLiteral lexer
whiteSpace    = P.whiteSpace lexer
