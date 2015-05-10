{-# LANGUAGE OverloadedStrings #-}
module Driver where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity
import Control.Arrow hiding ((<+>))
import System.Directory
import System.FilePath
import Debug.Trace

import Pretty hiding ((</>))
import Type
import Core
import qualified IR as IR
import qualified CoreToIR as IR
import Parser
import Typecheck hiding (Exp(..))

type Modules = ([FilePath], Map FilePath PolyEnv)

type MM = ReaderT [FilePath] (ErrorT (StateT Modules (VarMT IO)))

compileMain :: FilePath -> MName -> IO (Either String IR.Pipeline)
compileMain path fname = fmap IR.compilePipeline <$> reducedMain path fname

reducedMain :: FilePath -> MName -> IO (Either String Exp)
reducedMain path fname =
    runMM [path] $ mkReduce <$> parseAndToCoreMain fname

runMM :: [FilePath] -> MM a -> IO (Either String a) 
runMM paths
    = flip evalStateT ['t': show i | i <- [0..]]
    . flip evalStateT mempty
    . fmap (either (Left . show) Right)
    . runExceptT
    . flip runReaderT paths

catchMM :: MM a -> MM (Either String a)
catchMM = mapReaderT $ \m -> lift $ either (Left . show) Right <$> runExceptT m

parseAndToCoreMain :: MName -> MM Exp
parseAndToCoreMain m = fst <$> getDef m (ExpN "main")

clearImports = modify (const [] *** id)

loadModule :: MName -> MM (FilePath, PolyEnv)
loadModule mname = do
  fnames <- asks $ map $ flip lcModuleFile mname
  let
    find :: [FilePath] -> MM (FilePath, PolyEnv)
    find [] = throwErrorTCM $ "can't find module" <+> hsep (map text fnames)
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
            (src, e) <- lift $ mapExceptT (lift . lift) $ parseLC fname
            ms <- mapM loadModule $ moduleImports e
            mapError (InFile src) $ do
                env <- joinPolyEnvs $ map snd ms
                x <- lift $ mapExceptT (lift . mapStateT liftIdentity) $ inference_ env e
                modify $ (fname:) *** Map.insert fname x
                return (fname, x)

  find fnames

lcModuleFile path n = path </> (showN n ++ ".lc")

getDef :: MName -> EName -> MM ExpT
getDef = getDef_
--    either (\s -> throwErrorTCM $ pShow m <> "." <> pShow d <> ":" <+> s) return =<< getDef_ m d

getDef_ :: MName -> EName -> MM ExpT
getDef_ m d = do
    clearImports
    (fm, _) <- loadModule m
    (ms_, mods) <- get
    let ms = zip ms_ $ map (mods Map.!) ms_
    throwErrorTCM "not found"
{- TODO
    return $ case
        [ buildLet ((\ds -> [d | DValueDef d <- ds]) (concatMap (definitions . snd) (reverse dss) ++ reverse ps)) e
         | ((m', defs): dss) <- tails ms, m' == fm
         , (DValueDef (ValueDef (PVar (VarE d' _)) e):ps) <- tails $ reverse $ definitions defs, d' == d
         ] of
        [e] -> Right e
        [] -> Left "not found"
-}
