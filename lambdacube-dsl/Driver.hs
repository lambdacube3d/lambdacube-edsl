module Driver where

import Data.List
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Text.Show.Pretty
import System.Directory
import System.FilePath

import qualified Type as AST
import Type hiding (ELet, EApp, ELam, EVar, ELit, ETuple, ECase, Exp, Pat, PVar, PLit, PTuple, PCon, Wildcard)
import Core
import CoreToIR
import Parser
import Typecheck hiding (Exp(..))
import Typing (primFunMap)

testCompile n = test'' n (compilePipeline . reduce mempty mempty)
run' n = testCompile n >>= putStrLn
run = run' "gfx03"

test'' n f = test_ n $ ppShow . f
test_ n f = either error f <$> runMM "./tests/accept" (parseAndToCoreMain n)

reducedMain :: FilePath -> MName -> IO (Either String Exp)
reducedMain path fname =
    runMM path $ reduce mempty mempty <$> parseAndToCoreMain fname

runMM path = runExceptT . flip evalStateT mempty . flip runReaderT path

parseAndToCoreMain :: MName -> MM Exp
parseAndToCoreMain m = toCore mempty <$> getDef m "main"

type Modules = [(MName, Module (Subst, Typing))]

type MM = ReaderT FilePath (StateT Modules (ExceptT String IO))

typeCheckLC :: MName -> MM (Module (Subst, Typing))
typeCheckLC mname = do
 c <- gets $ lookup mname
 case c of
    Just m -> return m
    _ -> do
     fname <- asks $ flip lcModuleFile mname
     b <- liftIO $ doesFileExist fname
     if not b then throwError $ "can't find module " ++ fname
     else do
      res <- liftIO $ parseLC fname
      case res of
        Left m -> throwError m
        Right (src, e) -> do
          ms <- mapM (typeCheckLC . qData) $ moduleImports e
          case joinPolyEnvs $ PolyEnv primFunMap: map exportEnv ms of
            Left m -> throwError m
            Right env -> case inference_ env e of
                Left m    -> throwError $ m src
                Right x   -> do
                    modify ((mname, x):)
                    return x

lcModuleFile path n = path </> (n ++ ".lc")

getDef :: MName -> EName -> MM (AST.Exp (Subst, Typing))
getDef m d = do
    maybe (throwError $ d ++ " is not defined in " ++ m) return =<< getDef_ m d Nothing

getDef_ :: MName -> EName -> Maybe Typing -> MM (Maybe (AST.Exp (Subst, Typing)))
getDef_ m d mt = do
    typeCheckLC m
    ms <- get
    case [ buildLet (concatMap (definitions . snd) (reverse dss) ++ reverse ps) e
         | ((m', defs): dss) <- tails ms, m' == m
         , ((AST.PVar (_, t) d', e):ps) <- tails $ reverse $ definitions defs, d' == d, maybe True (== t) mt
         ] of
        [e] -> return $ Just e
        [] -> return Nothing

