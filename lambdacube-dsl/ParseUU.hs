{-# LANGUAGE FlexibleContexts, RankNTypes #-}
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances
--import Text.ParserCombinators.UU.Utils
import qualified Data.ListLike as LL
import Data.Char
import Text.Printf

--import Indentation
{-
import Text.Parser.Indentation.Implementation (IndentationState(..), IndentationRel(..), LocalState)
import qualified Text.Parser.Indentation.Implementation as I

-- indentation parsing combinators
localTokenMode :: (IndentationRel -> IndentationRel) -> ParserTrafo a a
localTokenMode = I.localTokenMode localState

localIndentation :: IndentationRel -> ParserTrafo a a
localIndentation = I.localIndentation localStateUnlessAbsMode

absoluteIndentation :: ParserTrafo a a
absoluteIndentation = I.absoluteIndentation localState

ignoreAbsoluteIndentation :: ParserTrafo a a
ignoreAbsoluteIndentation = I.ignoreAbsoluteIndentation localState

localAbsoluteIndentation :: ParserTrafo a a
localAbsoluteIndentation = I.localAbsoluteIndentation localState
-}
-- helper
checkIndentation :: ParserTrafo a a
checkIndentation m = m
{-
{-
  do
    is <- get
    p <- position
  -- put the following to the advance function:
    let ok is' = do x <- m; put is'; return x
        err msg = fail msg
    I.updateIndentation is (fromIntegral $ column p + 1) ok err
-}

--localState :: LocalState (Parser a)
localState pre post m = m
{-
 do
  is <- get
  put (pre is)
  x <- m
  is' <- get
  put (post is is')
  return x
-}
localStateUnlessAbsMode pre post m = m
{-
  IndentationParserT $ do
    a <- gets I.indentationStateAbsMode
    unIndentationParserT $ if a then m else localState pre post m
-}

{-
type Indentation = Int
data IndentationRel = Eq | Any | Const Indentation | Ge | Gt deriving (Show, Eq)

class IndentationParsing m where
  localTokenMode :: (IndentationRel -> IndentationRel) -> m a -> m a
  localIndentation :: IndentationRel -> m a -> m a
  absoluteIndentation :: m a -> m a
  ignoreAbsoluteIndentation :: m a -> m a
  localAbsoluteIndentation :: m a -> m a

instance (Monad m) => IndentationParsing (IndentationParserT t m) where
  {-# INLINE localTokenMode #-}
  localTokenMode = I.localTokenMode localState

  {-# INLINE localIndentation #-}
  localIndentation = I.localIndentation localStateUnlessAbsMode

  {-# INLINE absoluteIndentation #-}
  absoluteIndentation = I.absoluteIndentation localState

  {-# INLINE ignoreAbsoluteIndentation #-}
  ignoreAbsoluteIndentation = I.ignoreAbsoluteIndentation localState

  {-# INLINE localAbsoluteIndentation #-}
  localAbsoluteIndentation = I.localAbsoluteIndentation localState

instance (DeltaParsing m) => TokenParsing (IndentationParserT Token m) where
  token p = checkIndentation (token (unIndentationParserT p))

---------------------
-- Private Helpers --
---------------------

{-# INLINE localState #-}
localState :: (Monad m) => LocalState (IndentationParserT t m a)
localState pre post m = IndentationParserT $ do
  is <- get
  put (pre is)
  x <- unIndentationParserT m
  is' <- get
  put (post is is')
  return x

{-# INLINE localStateUnlessAbsMode #-}
localStateUnlessAbsMode :: (Monad m) => LocalState (IndentationParserT t m a)
localStateUnlessAbsMode pre post m = IndentationParserT $ do
  a <- gets I.indentationStateAbsMode
  unIndentationParserT $ if a then m else localState pre post m

{-# INLINE checkIndentation #-}
checkIndentation :: (DeltaParsing m) => LazyState.StateT IndentationState m a -> IndentationParserT t m a
checkIndentation m = IndentationParserT $ do
    is <- get
    p <- position
    let ok is' = do x <- m; put is'; return x
        err msg = fail msg
    I.updateIndentation is (fromIntegral $ column p + 1) ok err
-}
-}

-- utility
pCR :: Parser Char
pCR = pSym '\r'

pLF :: Parser Char
pLF = pSym '\n'

pUpper :: Parser Char
pUpper = pRange ('A','Z')

pLower :: Parser Char
pLower = pRange ('a','z')

pLetter :: Parser Char
pLetter = pUpper <|> pLower

pDigit :: Parser Char
pDigit = pRange ('0','9')

pAscii :: Parser Char
pAscii = pRange ('\000', '\254')

pParens :: ParserTrafo a a
pParens p = lexeme $ pLParen *> p <* pRParen

pBrackets :: ParserTrafo a a
pBrackets p = lexeme $ pLBracket *> p <* pRBracket

pBraces :: ParserTrafo a a
pBraces p = lexeme $ pLBrace *> p <* pRBrace

pComment :: Parser ()
pComment = void_ $ pCommentLine <|> pCommentBlock

pCommentBlock :: Parser ()
pCommentBlock = void_ $ pToken "{-" *> inComment
 where
  inComment = pMany (pAny pSym notStartEnd) *> ((void_ $ pToken "-}") <<|> (pAny pSym startEnd *> inComment))
  notStartEnd = [c | c <- ['\000'..'\254'], notElem c startEnd]
  startEnd = "-"

pCommentLine :: Parser ()
pCommentLine = void_ $ pToken "--" *> pMunch (/= '\n')

pSpacesOrComment :: Parser ()
pSpacesOrComment = void_ $ pSpaces <* pMany (pComment *> pSpaces)

pSpaces :: Parser String
pSpaces = pMunch (`elem` " \r\n\t") <?> "Whitespace"

lexeme :: ParserTrafo a a
lexeme p = checkIndentation (p <* pSpacesOrComment)

pDot, pComma, pDQuote, pLParen, pRParen, pLBracket, pRBracket, pLBrace, pRBrace :: Parser Char
pDot      = lexeme $ pSym '.'
pComma    = lexeme $ pSym ','
pDQuote   = lexeme $ pSym '"'
pLParen   = lexeme $ pSym '('
pRParen   = lexeme $ pSym ')'
pLBracket = lexeme $ pSym '['
pRBracket = lexeme $ pSym ']'
pLBrace   = lexeme $ pSym '{'
pRBrace   = lexeme $ pSym '}'

pNaturalRaw :: (Num a) => Parser a
pNaturalRaw = foldl (\a b -> a * 10 + b) 0 <$> pList1 pDigitAsNum <?> "Natural"

pNatural :: Num a => Parser a
pNatural = lexeme pNaturalRaw

pDigitAsNum ::  Num a => Parser a
pDigitAsNum =
  digit2Int <$> pDigit
  where
  digit2Int a = fromInteger $ toInteger $ ord a - ord '0'

pSymbol :: (IsLocationUpdatedBy loc Char, LL.ListLike state Char) => String -> P (Str Char state loc)  String
pSymbol   = lexeme . pToken

execParser :: Parser a -> String -> (a, [Error LineColPos])
execParser p = parse_h ((,) <$> p <*> pEnd) . createStr (LineColPos 0 0 0)

runParser :: String -> Parser a -> String -> a
runParser inputName p s | (a,b) <- execParser p s =
    if null b
    then a
    else error (printf "Failed parsing '%s' :\n%s\n" inputName (pruneError s b))
         -- We do 'pruneError' above because otherwise you can end
         -- up reporting huge correction streams, and that's
         -- generally not helpful... but the pruning does discard info...
    where -- | Produce a single simple, user-friendly error message
          pruneError :: String -> [Error LineColPos] -> String
          pruneError _ [] = ""
          pruneError _ (DeletedAtEnd x     : _) = printf "Unexpected '%s' at end." x
          pruneError s (Inserted _ pos exp : _) = prettyError s exp pos
          pruneError s (Deleted  _ pos exp : _) = prettyError s exp pos
          prettyError :: String -> [String] -> LineColPos -> String
          prettyError s exp p@(LineColPos line c abs) = printf "Expected %s at %s :\n%s\n%s\n%s\n"
                                                           (show_expecting p exp)
                                                           (show p)
                                                           aboveString
                                                           inputFrag
                                                           belowString
                             where
                                s' = map (\c -> if c=='\n' || c=='\r' || c=='\t' then ' ' else c) s
                                aboveString = replicate 30 ' ' ++ "v"
                                belowString = replicate 30 ' ' ++ "^"
                                inputFrag   = replicate (30 - abs) ' ' ++ (take 71 $ drop (abs - 30) s')

--void_ :: Parser a -> Parser ()
void_ a = a *> pure ()

sepBy s t = pure () <|> t <* many (s *> t)
sepBy1 s t = t <* many (s *> t)

pAlfaNum :: Parser ()
pAlfaNum = void_ $ pLetter <|> pDigit <|> pSym '_' <|> pSym '\''

pOperator :: Parser ()
pOperator = void_ $ lexeme $ pSome (pAny pSym ":!#$%&*+./<=>?@\\^|-~")

upperIdent :: Parser ()
upperIdent = void_ $ lexeme $ pUpper *> pMany pAlfaNum

lowerIdent :: Parser ()
lowerIdent = void_ $ lexeme $ (pLower <|> pSym '_') *> pMany pAlfaNum

moduleName :: Parser ()
moduleName = void_ $ lexeme $ sepBy1 (pSym '.') upperIdent

dataConstructor :: Parser ()
dataConstructor = upperIdent

typeConstraint :: Parser ()
typeConstraint = void_ $ upperIdent

typeConstructor :: Parser ()
typeConstructor = void_ $ upperIdent

typeVar :: Parser ()
typeVar = void_ $ lowerIdent

var :: Parser ()
var = void_ $ lowerIdent

varId :: Parser ()
varId = void_ var <|> void_ (pParens pOperator)

moduleDef :: Parser ()
moduleDef = void_ $ do
  opt (do
    pSymbol "module"
    moduleName
    opt (void_ $ pParens (sepBy pComma varId)) ()
    pSymbol "where") ""
  pMany importDef
  pMany (typeSynonym <<|> fixityDef <<|> typeClassDef <<|> typeClassInstanceDef <<|> typeSignature <|> valueDef)
  {-
  optional $ parens (commaSep varId)
  kw "where"
  localAbsoluteIndentation $ do
    many importDef
  -}

importDef :: Parser ()
importDef = do
  pSymbol "import"
  moduleName
  opt (void_ $ pParens (sepBy pComma (varId <|> dataConstructor))) ()

typeSynonym :: Parser ()
typeSynonym = void_ $ pSymbol "type" *> typeConstructor *> pMany typeVar *> pSymbol "=" *> typeExp  {-TEMP-} <* pSymbol ";"
{-
  localIndentation Gt $ do
    typeConstructor
    many typeVar
    op "="
    typeExp
-}

fixityDef :: Parser ()
fixityDef = void_ $ (pSymbol "infix" <|> pSymbol "infixl" <|> pSymbol "infixr") *> pNatural *> pOperator
{-
  localIndentation Gt $ do
    natural
    ident lcOps :: P String
-}

typeSignature :: Parser ()
typeSignature = varId *> pSymbol "::" *> typeExp  {-TEMP-} <* pSymbol ";"
---
typeExp :: Parser ()
typeExp = void_ $ opt (pSymbol "forall" *> pSome typeVar *> pSymbol ".") "" *>
  ((tcExp *> ty) <|> ty)


typeRecord :: Parser ()
typeRecord = void_ $ pBraces (sepBy1 pComma typeSignature *> opt (pSymbol "|" *> typeVar) ())


typePattern :: Parser ()
typePattern = typeVar-- <<|> typeConstructor <<|> (void_ $ pParens ((void_ $ typeConstructor *> pSome typePattern) <<|> typePattern))

tcExp :: Parser ()
tcExp = void_ $ (tyC <<|> (void_ $ pParens (sepBy1 pComma tyC))) *> pSymbol "=>"
 where
  tyC = void_ $ typeConstraint *> pMany typePattern

ty :: Parser ()
ty = void_ $ pList1Sep (pSymbol "->") (pSome typeAtom) -- chainl1 (void_ $ some typeAtom) (do op "->"; return const)

typeAtom :: Parser ()
typeAtom = typeRecord <|> typeVar <<|> typeConstructor <<|> (pParens ty) <<|> pBrackets ty-- <|> (do typeAtom; op "->"; typeAtom)
---

typeClassDef :: Parser ()
typeClassDef = void_ $ -- localIndentation Gt $ do
  pSymbol "class" *> (tcExp <|> pure ()) *> typeConstraint *> typeVar *> ((pSymbol "where" *> pMany typeSignature *> pure ()) <|> pure ()) *> pSymbol ";;"
{-
  kw "class"
  localIndentation Gt $ do
    optional tcExp
    typeConstraint
    many typeVar
    optional $ do
      kw "where"
      localIndentation Gt $ localAbsoluteIndentation $ do
        many typeSignature
-}

typeClassInstanceDef :: Parser ()
typeClassInstanceDef = void_ $ pSymbol "instance" *> (tcExp <|> pure ()) *> typeConstraint *> typePattern *> ((pSymbol "where" *> pMany valueDef *> pure ()) <|> pure ()) *> pSymbol ";;"

{-
  kw "instance"
  localIndentation Gt $ do
    optional tcExp
    typeConstraint
    many typePattern
    optional $ do
      kw "where"
      localIndentation Gt $ localAbsoluteIndentation $ do
        many valueDef
-}

valuePattern :: Parser ()
valuePattern = var <|> dataConstructor <|> (void_ $ opt (void_ $ var *> pSymbol "@") () *> pParens ((void_ $ dataConstructor *> pSome valuePattern) <|> valuePattern))

valueDef :: Parser ()
valueDef = varId *> pMany valuePattern *> pSymbol "=" *> expression *> pSymbol "#" *> pure ()
{-
  varId
  localIndentation Gt $ do
    many valuePattern
    op "="
    expression
-}
expression :: Parser ()
expression = void_ $ pMany (var <|> dataConstructor <|> pParens expression)

test :: IO ()
test = do
  src <- readFile "syntax03.lc"
  print $ execParser (pSpacesOrComment *> moduleDef) src
  print $ runParser "" (pSpacesOrComment *> moduleDef) src

main = test