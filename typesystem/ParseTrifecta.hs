import Data.ByteString.Char8 (unpack,pack)
import Text.PrettyPrint.ANSI.Leijen (pretty)
import Data.Monoid
import Control.Applicative
import Text.Trifecta
import Text.Trifecta.Indentation as I
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

--block :: P Exp
block = do
  spaces
  localIndentation Ge $ do
    l <- kw "let" *> (localIndentation Ge $ do
      (localIndentation Gt $ some $ localAbsoluteIndentation $ assign)) -- WORKS
    r <- rend
    p <- position
    c <- careting
    (a :~ s,t) <- kw "in" *> (slicedWith (,) $ spanned $ localIndentation Gt expr)
    --liftM $ raiseErr $ Err Nothing [explain r mempty] mempty
    return $ (unpack t,p,{-r <> render c <> -}render s,foldr ($) a l)
    --return $ (p,r <> render s,foldr ($) a l))

assign :: P (Exp -> Exp)
assign = ELet <$> var <* kw "=" <*> expr

expr :: P Exp
expr =  lam <|> formula

formula = (foldl1 EApp) `fmap` (some atom)

atom = EPrimFun <$> primFun <|> ELit <$> lit <|> EVar <$> var <|> parens expr

primFun = PUpper <$ kw "upper"

lam :: P Exp
lam = ELam <$ op "\\" <*> var <* op "->" <*> expr

indentState = mkIndentationState 1 infIndentation True Gt

test :: IO ()
test = do
  r <- parseFromFileEx (evalIndentationParserT (block <* eof) indentState) "example01.lc"
  case r of
    Failure m -> print m
    Success (a,d,r,e) -> do
      --putStrLn a
      print $ pretty d
      print $ pretty r
      case inference e of
        Right t   -> putStrLn $ show e ++ " :: " ++ show t
        Left m    -> putStrLn $ "error: " ++ m
