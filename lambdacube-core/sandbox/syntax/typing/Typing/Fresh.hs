{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Typing.Fresh where

class (Monad m) => MonadFresh a m | m -> a where
    fresh :: m a
