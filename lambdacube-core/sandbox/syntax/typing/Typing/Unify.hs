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

occurs :: Tv -> Ty -> Bool
occurs x (TyVar y) = x == y
occurs x (TyApp t u) = occurs x t || occurs x u
occurs x _ = False

newtype Unify a = Unify{ unUnify :: StateT Subst (Fresh Tv) a }
                deriving (Functor, Applicative, Monad)

runUnify :: Unify a -> Fresh Tv a
runUnify u = evalStateT (unUnify u) mempty

instance MonadFresh Tv Unify where
    fresh = Unify $ lift fresh

type Subst = Map Tv Ty

substTv :: Tv -> Unify Ty
substTv x = do
    mt <- Unify $ gets $ Map.lookup x
    case mt of
        Nothing -> return $ TyVar x
        Just t -> subst t

subst :: Ty -> Unify Ty
subst t = case t of
    TyVar x -> substTv x
    TyApp t u -> TyApp <$> subst t <*> subst u
    _ -> return t

addSubst :: Tv -> Ty -> Unify ()
addSubst x (TyVar y) | x == y = return ()
addSubst x t | occurs x t = fail $ unwords ["infinite type:", show x, "=", show t]
             | otherwise = Unify $ modify $ Map.insert x t

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
