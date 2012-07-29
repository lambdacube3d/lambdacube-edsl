{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Typing.Fresh where

import Control.Applicative

import Data.Stream (Stream(..))

import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Identity

class (Monad m) => MonadFresh a m | m -> a where
    fresh :: m a

newtype FreshT v m a = Fresh{ unFresh :: StateT (Stream v) m a }
                deriving (Functor, Applicative, Monad, MonadTrans)

type Fresh v = FreshT v Identity

instance (Monad m) => MonadFresh v (FreshT v m) where
    fresh = do
        Cons x xs <- Fresh get
        Fresh $ put xs
        return x

runFresh :: Fresh v a -> Stream v -> a
runFresh = evalState . unFresh
