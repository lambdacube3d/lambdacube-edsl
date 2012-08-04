{-# LANGUAGE RecordWildCards #-}
module Typing.MonoEnv
       ( MonoEnv(..)
       , monoVar
       , monoVars
       , removeMonoVars
       , restrictMonoVars
       , mapMono
       ) where

import Typing.Repr

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import Data.Set.Unicode
import Data.Monoid hiding ((<>))
import Text.PrettyPrint.Leijen

import Prelude hiding (mapM)
import Data.Traversable (mapM)

data MonoEnv = MonoEnv{ monoVarMap :: Map Var Ty }
             deriving Show

instance Monoid MonoEnv where
    mempty = MonoEnv{ monoVarMap = mempty }
    MonoEnv{ monoVarMap = mvs } `mappend` MonoEnv{ monoVarMap = mvs' } = MonoEnv{ monoVarMap = mvs `mappend` mvs' }

instance Pretty MonoEnv where
    pretty MonoEnv{..} = encloseSep lbrace rbrace (comma <> space) . map (\(x, τ) -> text x <+> text "∷" <+> pretty τ) $ Map.toList monoVarMap

monoVar :: Var -> Ty -> MonoEnv
monoVar x τ = MonoEnv $ Map.singleton x τ

monoVars :: MonoEnv -> Set Var
monoVars = Map.keysSet . monoVarMap

removeMonoVars :: Set Var -> MonoEnv -> MonoEnv
removeMonoVars vars m@MonoEnv{..} = m{ monoVarMap = removeFrom monoVarMap }
  where
    removeFrom = Map.filterWithKey (\ var _ -> var ∉ vars)

restrictMonoVars :: Set Var -> MonoEnv -> MonoEnv
restrictMonoVars vars m@MonoEnv{..} = m{ monoVarMap = restrict monoVarMap }
  where
    restrict = Map.filterWithKey (\ var _ -> var ∈ vars)

mapMono :: (Monad m) => (Ty -> m Ty) -> MonoEnv -> m MonoEnv
mapMono f m@MonoEnv{..} = do
    mvm' <- mapM f monoVarMap
    return m{ monoVarMap = mvm' }
