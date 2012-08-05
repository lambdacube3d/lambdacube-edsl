{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Typing.TC (TC, runTC) where

import Typing.Repr
import Typing.Fresh

import Data.Stream (Stream(..))
import qualified Data.Stream as Stream

import Control.Monad.State
import Control.Applicative

newtype TC a = TC{ unTC :: State (Stream Tv) a }
             deriving (Functor, Applicative, Monad)

instance MonadFresh Tv TC where
    fresh = TC $ do
        Cons x xs <- get
        put xs
        return x

runTC :: TC a -> a
runTC m = evalState (unTC m) tvs
  where
    tvs = Stream.map (('v':) . show) nats
    nats = Stream.iterate succ 0
