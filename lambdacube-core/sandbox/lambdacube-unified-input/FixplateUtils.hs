module FixplateUtils where

import Data.Generics.Fixplate

import Data.Traversable as T

synthRewriteM :: (Traversable f, Monad m) => (f a -> m a) -> (Attr f a -> m (Maybe (f (Attr f a)))) -> Attr f a -> m (Attr f a)
synthRewriteM calc = synthRewriteM' (calc . fmap attribute)

synthRewriteM' :: (Traversable f, Monad m) => (f (Attr f a) -> m a) -> (Attr f a -> m (Maybe (f (Attr f a)))) -> Attr f a -> m (Attr f a)
synthRewriteM' calc h0 = rewrite where
  rewrite x = go False x >>= return . snd
  synth x = do
    a <- calc x
    return $ Fix $ Ann a x
  hsynth x = do
    m <- h0 =<< synth x
    case m of 
        Nothing -> return Nothing
        Just y  -> synth y >>= return . Just
  go changed0 old@(Fix (Ann _ x)) = do
    (changed1,y) <- mapAccumLM go changed0 x
    v <- hsynth y
    case v of
      Nothing -> do
        w <- if changed1
          then synth y
          else return old
        return (changed1,w)
      Just z -> do
        r <- rewrite z
        return (True, r)

newtype StateL m s a = StateL { runStateL :: s -> m (s, a) }
instance Monad m => Monad (StateL m s) where
    return x = StateL (\ s -> return (s, x))
    StateL kf >>= mf = StateL $ \ s -> do
        (s', f) <- kf s
        let StateL kv = mf f
        kv s'
mapAccumLM :: (Traversable t, Monad m) => (a -> b -> m (a, c)) -> a -> t b -> m (a, t c)
mapAccumLM f s t = runStateL (T.mapM (StateL . flip f) t) s
