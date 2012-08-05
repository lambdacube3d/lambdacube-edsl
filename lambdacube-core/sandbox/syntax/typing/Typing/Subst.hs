{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Typing.Subst
       ( Subst, runSubst
       , addSubst
       , substTv
       , subst
       ) where

import Typing.Repr
import Typing.TC
import Typing.Fresh

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Applicative
import Data.Monoid
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Identity

newtype Subst a = Subst{ unSubst :: StateT (Map Tv Ty) TC a }
                deriving (Functor, Applicative, Monad)

instance MonadFresh Tv Subst where
    fresh = Subst . lift $ fresh

runSubst :: Subst a -> TC a
runSubst m = evalStateT (unSubst m) mempty

substTv :: Tv -> Subst Ty
substTv x = do
    mt <- Subst $ gets $ Map.lookup x
    case mt of
        Nothing -> return $ TyVar x
        Just t -> subst t

subst :: Ty -> Subst Ty
subst t = case t of
    TyVar x -> substTv x
    TyApp t u -> liftM2 TyApp (subst t) (subst u)
    _ -> return t

addSubst :: Tv -> Ty -> Subst ()
addSubst x (TyVar y) | x == y = return ()
addSubst x t | occurs x t = fail $ unwords ["infinite type:", show x, "=", show t]
             | otherwise = Subst $ modify $ Map.insert x t

