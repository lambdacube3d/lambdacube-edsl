{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Typing.Unify
       ( MonoEnv
       , monoVar
       , monoVars
       , removeMonoVars
       , Unify
       , runUnify
       , unify
       ) where

import Typing.Repr
import Typing.Fresh
import Typing.MonoEnv
import Typing.Subst

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.Set as Set

import Control.Applicative
import Control.Monad hiding (mapM)
import Data.Monoid
import Data.Maybe (mapMaybe)
import Prelude hiding (mapM)
import Data.Traversable (mapM)

import Control.Monad.Trans
import Control.Monad.State hiding (mapM)

data TyEq = Ty :~: Ty
          deriving Show

data Unification = Skip
                 | Substitute Tv Ty
                 | Recurse [TyEq]
                 | Flip
                 | Incongruent

unify1 :: TyEq -> Unification
unify1 tyEq = case tyEq of
    TyVar x     :~: t -> Substitute x t
    t           :~: TyVar x -> Flip
    TyFun       :~: TyFun -> Skip
    TyApp t u   :~: TyApp t' u' -> Recurse [t :~: t', u :~: u']
    TyCon c     :~: TyCon c' | c == c' -> Skip
    _ -> Incongruent

type Unify = SubstT (Fresh Tv)

runUnify :: Unify a -> Fresh Tv a
runUnify = runSubstT

instance MonadFresh Tv Unify where
    fresh = lift fresh

unifyEqs :: [TyEq] -> Unify ()
unifyEqs tyEqs = case tyEqs of
    [] -> return ()
    (e:es) -> case unify1 e of
        Skip -> unifyEqs es
        Substitute x t -> do
            addSubst x t
            es' <- mapM (\(t :~: t') -> (:~:) <$> subst t <*> subst t') es
            unifyEqs es'
        Recurse es' -> unifyEqs $ es' ++ es
        Flip -> unifyEqs $ (flip e):es
        Incongruent -> fail $ unwords ["cannot unify", show e]
  where
    flip (t :~: t') = t' :~: t

substMonoEnv :: MonoEnv -> Unify MonoEnv
substMonoEnv m = do
    monoVarMap' <- mapM subst $ monoVarMap m
    return m{ monoVarMap = monoVarMap' }

unify :: [MonoEnv] -> [Ty] -> Unify (MonoEnv, Ty)
unify ms τs = do
    α <- fresh
    let eqs = map (TyVar α :~:) τs
        eqs' = concatMap toEqs . Set.toList $ vars
    unifyEqs $ eqs ++ eqs'
    ms' <- mapM substMonoEnv ms
    τ <- substTv α
    return (mconcat ms', τ)
  where
    vars = Set.unions $ map monoVars ms
    toEqs v = case mapMaybe (Map.lookup v . monoVarMap) ms of
        [] -> []
        t:ts -> map (t :~:) ts
