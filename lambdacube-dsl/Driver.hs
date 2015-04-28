module Driver where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Arrow
import Text.Show.Pretty
import System.Directory
import System.FilePath
import Debug.Trace

import qualified Type as AST
import Type hiding (ELet, EApp, ELam, EVar, ELit, ETuple, ECase, Exp, Pat, PVar, PLit, PTuple, PCon, Wildcard)
import Core
import qualified IR as IR
import qualified CoreToIR as IR
import Parser
import Typecheck hiding (Exp(..))

compileMain :: FilePath -> MName -> IO (Either String IR.Pipeline)
compileMain path fname = fmap IR.compilePipeline <$> reducedMain path fname

reducedMain :: FilePath -> MName -> IO (Either String Exp)
reducedMain path fname =
    runMM [path] $ mkReduce <$> parseAndToCoreMain fname

runMM :: [FilePath] -> MM a -> IO (Either String a) 
runMM paths = flip evalStateT mempty . runExceptT . flip runReaderT paths

catchMM :: MM a -> MM (Either String a)
catchMM = mapReaderT $ \m -> lift $ runExceptT m

parseAndToCoreMain :: MName -> MM Exp
parseAndToCoreMain m = toCore mempty <$> getDef m "main"

type Modules = ([FilePath], Map FilePath ModuleT)

type MM = ReaderT [FilePath] (ExceptT String (StateT Modules IO))

clearImports = modify (const [] *** id)

loadModule :: MName -> MM (FilePath, ModuleT)
loadModule mname = do
  fnames <- asks $ map $ flip lcModuleFile mname
  let
    find [] = throwError $ "can't find module " ++ intercalate "; " fnames
    find (fname: fs) = do
     b <- liftIO $ doesFileExist fname
     if not b then find fs
     else do
       c <- gets $ Map.lookup fname . snd
       case c of
         Just m -> do
            modify $ (\x -> if fname `elem` x then x else fname: x) *** id
            return (fname, m)
         _ -> do
          res <- liftIO $ parseLC fname
          case res of
            Left m -> throwError m
            Right (src, e) -> do
              ms <- mapM (loadModule . qData) $ moduleImports e
              case joinPolyEnvs $ map (exportEnv .snd) ms of
                Left m -> throwError m
                Right env -> case inference_ env e of
                    Left m    -> throwError $ m src
                    Right x   -> do
                        modify $ (fname:) *** Map.insert fname x
                        return (fname, x)

  find fnames

lcModuleFile path n = path </> (n ++ ".lc")

getDef :: MName -> EName -> MM (AST.Exp (Subst, Typing))
getDef m d = do
    either (\s -> throwError $ m ++ "." ++ d ++ ": " ++ s) return =<< getDef_ m d

getDef_ :: MName -> EName -> MM (Either String (AST.Exp (Subst, Typing)))
getDef_ m d = do
    clearImports
    (fm, _) <- loadModule m
    (ms_, mods) <- get
    let ms = zip ms_ $ map (mods Map.!) ms_
    return $ case
        [ buildLet ((\ds -> [d | ValueDef d <- ds]) (concatMap (definitions . snd) (reverse dss) ++ reverse ps)) e
         | ((m', defs): dss) <- tails ms, m' == fm
         , (ValueDef (AST.PVar (_, t) d', e):ps) <- tails $ reverse $ definitions defs, d' == d
         ] of
        [e] -> Right e
        [] -> Left "not found"

