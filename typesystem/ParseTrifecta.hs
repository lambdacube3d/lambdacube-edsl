import Data.ByteString.Char8 (unpack,pack)
import qualified Data.ByteString.Char8 as BS
import Text.PrettyPrint.ANSI.Leijen (pretty)
import Data.Monoid
import Control.Applicative
import Text.Trifecta
import Text.Trifecta.Indentation as I
import Text.Trifecta.Delta
import Text.Parser.Token.Style
import qualified Data.HashSet as HashSet

import Compositional hiding (test)

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
type P a = IndentationParserT Char Parser a

--buildSomeSpaceParser :: CharParsing m => m () -> CommentStyle -> m ()
--haskellIdents :: TokenParsing m => IdentifierStyle m

lcIdents = emptyIdents { _styleReserved = HashSet.fromList reservedIdents }
  where
    reservedIdents =
      [ "let"
      , "upper"
      , "in"
      ]

kw w = reserve lcIdents w

op w = reserve haskellOps w

var :: P String
var = ident lcIdents

lit :: P Lit
lit = LFloat <$ try double <|> LInt <$ integer <|> LChar <$ charLiteral

letin :: P Exp
letin = do
  spaces
  localIndentation Ge $ do
    l <- kw "let" *> (localIndentation Gt $ some $ localAbsoluteIndentation $ def) -- WORKS
    a <- kw "in" *> (localIndentation Gt expr)
    return $ foldr ($) a l

def :: P (Exp -> Exp)
def = (\p1 n d p2 e -> ELet (p1,p2) n d e) <$> position <*> var <* kw "=" <*> expr <*> position

expr :: P Exp
expr = letin <|> lam <|> formula

formula = (\p1 l p2 -> foldl1 (EApp (p1,p2)) l) <$> position <*> some atom <*> position

atom =
  (\p1 f p2 -> EPrimFun (p1,p2) f) <$> position <*> primFun <*> position <|> 
  (\p1 l p2 -> ELit (p1,p2) l) <$> position <*> lit <*> position <|>
  (\p1 v p2 -> EVar (p1,p2) v) <$> position <*> var <*> position <|>
  parens expr

primFun = PUpper <$ kw "upper" <|>
          PAddI <$ kw "add"

lam :: P Exp
lam = (\p1 n e p2 -> ELam (p1,p2) n e) <$> position <* op "\\" <*> var <* op "->" <*> expr <*> position

indentState = mkIndentationState 1 infIndentation True Gt

test :: IO ()
test = do
  let fname = "example01.lc"
  src <- BS.readFile fname
  case parseByteString (evalIndentationParserT (expr <* eof) indentState) (Directed (pack fname) 0 0 0 0) src of
    Failure m -> print m
    Success e -> do
      --let r = render s
      --print $ pretty $ delta r
      --print $ pretty r
      case inference src e of
        Right t   -> putStrLn $ show e ++ " :: " ++ show t
        Left m    -> putStrLn $ "error: " ++ m
