{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ParseTrifectaLC where

import Data.ByteString.Char8 (unpack,pack)
import qualified Data.ByteString.Char8 as BS
import Text.PrettyPrint.ANSI.Leijen (pretty)
import Data.Monoid
import Control.Applicative
import Text.Trifecta
import Text.Trifecta.Indentation as I
import Text.Trifecta.Delta
import Text.Parser.Expression
import Text.Parser.Token.Style
import qualified Data.HashSet as HashSet

import Text.Show.Pretty

import Control.Monad
import Text.Parser.LookAhead

import CompositionalLC hiding (test)
import Type

{-
type Indentation = Int
data IndentationRel = Eq | Any | Const Indentation | Ge | Gt deriving (Show, Eq)

class IndentationParsing m where
  localTokenMode :: (IndentationRel -> IndentationRel) -> m a -> m a
  localIndentation :: IndentationRel -> m a -> m a
  absoluteIndentation :: m a -> m a
  ignoreAbsoluteIndentation :: m a -> m a
  localAbsoluteIndentation :: m a -> m a
-}

type P a = IndentationParserT Char (LCParser Parser) a

newtype LCParser p a = LCParser { runLCParser :: p a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, Parsing, CharParsing, LookAheadParsing, DeltaParsing)

instance TokenParsing p => TokenParsing (LCParser p) where
  someSpace = LCParser $ buildSomeSpaceParser someSpace lcCommentStyle
  nesting = LCParser . nesting . runLCParser
  highlight h = LCParser . highlight h . runLCParser
  semi = token $ char ';' <?> ";"
  token p = p <* whiteSpace

lcCommentStyle = haskellCommentStyle

lcOps = emptyOps
  { _styleReserved = HashSet.fromList ["=","\\","*.","#"]
  }

lcIdents = haskell98Idents { _styleReserved = HashSet.fromList reservedIdents }
  where
    reservedIdents =
      [ "let"
      , "in"
      ]

kw w = reserve lcIdents w

op w = reserve lcOps w

var :: P String
var = ident lcIdents

lit :: P Lit
lit = LFloat <$> {-try-} double <|>
      LInt <$> integer <|>
      LChar <$> charLiteral <|>
      LString <$> stringLiteral <|>
      LNat . fromIntegral <$ op "#" <*> integer

letin :: P (Exp Range)
letin = do
  localIndentation Ge $ do
    l <- kw "let" *> (localIndentation Gt $ localAbsoluteIndentation $ some def) -- WORKS
    a <- kw "in" *> (localIndentation Gt expr)
    return $ foldr ($) a l

def :: P (Exp Range -> Exp Range)
def = (\p1 n a d p2 e -> ELet (p1,p2) n (foldr (args (p1,p2)) d a) e) <$> position <*> var <*> many var <* kw "=" <*> localIndentation Gt expr <*> position
  where
    args r n e = ELam r n e

expr :: P (Exp Range)
expr = buildExpressionParser table expr'

table :: OperatorTable (IndentationParserT Char (LCParser Parser)) (Exp Range)
table =
  [ [binary "*." (\a b -> let r = mergeRange a b in EApp r mempty (EApp r mempty (EVar r mempty "PrimMulMatVec") a) b) AssocLeft]
   --[Prefix $ do op "*."; return id]
  ]
 where
  mergeRange a b = let (a1,_) = getTag a
                       (_,b2) = getTag b
                   in (a1,b2)
  binary  name fun assoc = Infix (do op name; return (fun)) assoc

expr' :: P (Exp Range)
expr' = lam <|> letin <|> formula

formula = (\p1 l p2 -> foldl1 (EApp (p1,p2) mempty) l) <$> position <*> some atom <*> position

atom =
  (\p1 l p2 -> ELit (p1,p2) l) <$> position <*> lit <*> position <|>
  (\p1 v p2 -> EVar (p1,p2) mempty v) <$> position <*> var <*> position <|>
  (\p1 v p2 -> if length v == 1 then head v else ETuple (p1,p2) v) <$> position <*> parens (commaSep expr) <*> position <|>
  parens expr

lam :: P (Exp Range)
lam = (\p1 n e p2 -> ELam (p1,p2) n e) <$> position <* op "\\" <*> var <* op "->" <*> expr <*> position

indentState = mkIndentationState 0 infIndentation True Ge

parseLC :: String -> IO (Either String (Exp Typing))
parseLC fname = do
  src <- BS.readFile fname
  case parseByteString (runLCParser $ evalIndentationParserT (whiteSpace *> expr <* eof) indentState) (Directed (pack fname) 0 0 0 0) src of
    Failure m -> do
      print m
      return (Left $ show m)
    Success e -> do
      --let r = render s
      --print $ pretty $ delta r
      --print $ pretty r
      --putStrLn $ ppShow e
      case inference src e of
        Right t   -> do
          --putStrLn $ ppShow t
          return (Right t)
        Left m    -> do
          putStrLn $ "error: " ++ m
          return (Left m)
