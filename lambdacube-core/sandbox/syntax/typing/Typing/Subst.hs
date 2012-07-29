{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Typing.Subst
       ( SubstT, runSubstT
       , Subst, runSubst
       , addSubst
       , substTv
       , subst
       ) where

import Typing.Repr

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Applicative
import Data.Monoid
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Identity

newtype SubstT m a = SubstT{ unSubstT :: StateT (Map Tv Ty) m a }
                   deriving (Functor, Applicative, Monad, MonadTrans)

type Subst = SubstT Identity

runSubstT :: (Monad m) => SubstT m a -> m a
runSubstT m = evalStateT (unSubstT m) mempty

runSubst :: Subst a -> a
runSubst m = evalState (unSubstT m) mempty

substTv :: (Monad m) => Tv -> SubstT m Ty
substTv x = do
    mt <- SubstT $ gets $ Map.lookup x
    case mt of
        Nothing -> return $ TyVar x
        Just t -> subst t

subst :: (Monad m) => Ty -> SubstT m Ty
subst t = case t of
    TyVar x -> substTv x
    TyApp t u -> liftM2 TyApp (subst t) (subst u)
    _ -> return t

addSubst :: (Monad m) => Tv -> Ty -> SubstT m ()
addSubst x (TyVar y) | x == y = return ()
addSubst x t | occurs x t = fail $ unwords ["infinite type:", show x, "=", show t]
             | otherwise = SubstT $ modify $ Map.insert x t

