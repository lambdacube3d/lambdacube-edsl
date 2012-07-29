{-# LANGUAGE RecordWildCards #-}
module Typing.MonoEnv
       ( MonoEnv(..)
       , monoVar
       , monoVars
       , removeMonoVars
       ) where

import Typing.Repr

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import Data.Set.Unicode
import Data.Monoid

data MonoEnv = MonoEnv{ monoVarMap :: Map Var Ty }
             deriving Show

instance Monoid MonoEnv where
    mempty = MonoEnv{ monoVarMap = mempty }
    MonoEnv{ monoVarMap = mvs } `mappend` MonoEnv{ monoVarMap = mvs' } = MonoEnv{ monoVarMap = mvs <> mvs' }

monoVar :: Var -> Ty -> MonoEnv
monoVar x τ = MonoEnv $ Map.singleton x τ

monoVars :: MonoEnv -> Set Var
monoVars = Map.keysSet . monoVarMap

removeMonoVars :: Set Var -> MonoEnv -> MonoEnv
removeMonoVars vars m@MonoEnv{..} = m{ monoVarMap = removeFrom monoVarMap }
  where
    removeFrom = Map.filterWithKey (\var _ -> var ∉ vars)
