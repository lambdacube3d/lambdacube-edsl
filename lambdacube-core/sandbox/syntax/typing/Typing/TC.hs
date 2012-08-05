{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Typing.TC
       ( TC, runTC
       , throwTC
       , TypeError(..), TypeErrorCtx(..)
       , ScopeError(..)
       , TCError(..)
       , TyEq(..)
       ) where

import Typing.Repr
import Typing.Fresh
import Typing.MonoEnv

import Data.Stream (Stream(..))
import qualified Data.Stream as Stream

import Control.Monad.State
import Control.Applicative

data TyEq = Ty :~: Ty
          deriving Show

data TypeError = TEOccurs Tv Ty
               | TEIncongruent TyEq
               deriving Show

data TypeErrorCtx = TECtx { teMonoEnvs :: [MonoEnv]
                          , teTys :: [Ty]
                          , teEquation :: Maybe TyEq
                          }
                  deriving Show

data ScopeError = SEUnresolvedCon Con
                | SEPatternConflict Var
                deriving Show

data TCError = TypeError TypeErrorCtx TypeError
             | ScopeError ScopeError
             deriving Show

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

throwTC :: TCError -> TC a
throwTC = fail . show -- TODO
