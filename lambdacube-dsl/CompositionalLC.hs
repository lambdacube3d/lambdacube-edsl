{-# LANGUAGE LambdaCase #-}
module CompositionalLC
    ( inference
    , compose
    , applyTy
    ) where

import qualified Debug.Trace as D
import Data.Maybe
import Data.ByteString.Char8 (ByteString)
import Data.Foldable (foldMap)
import Data.Functor.Identity
import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Monoid
import Data.Functor
import qualified Data.Traversable as T
import qualified Data.Foldable as F

import Type
import Typing

trace__ = D.trace

trace_ _ = id
trace = trace_

{-
  ->>
  perdicate resolution operator

  O
  instance environment

  +
  e.g. O + O'
  substitution-resolution-combinator

  typing = type + mono env + instance env
-}

withRanges :: [Range] -> Unique a -> Unique a
withRanges rl a = do
  (x,y,rl0) <- get
  put (x,y,rl)
  res <- a
  (z,q,_) <- get
  put (z,q,rl0)
  return res

-- substitution
applyTy :: Subst -> Ty -> Ty
applyTy st tv@(TVar _ a) = case Map.lookup a st of
  Nothing -> tv
  Just t  -> t
applyTy st (TFun f) = tFun $ fmap (applyTy st) f
applyTy st (TTuple f l) = TTuple f (map (applyTy st) l)
applyTy st (TArr a b) = TArr (applyTy st a) (applyTy st b)
applyTy st (TImage f a b) = TImage f (applyTy st a) (applyTy st b)
applyTy st (TVertexStream f a b) = TVertexStream f (applyTy st a) (applyTy st b)
applyTy st (TFragmentStream f a b) = TFragmentStream f (applyTy st a) (applyTy st b)
applyTy st (TFrameBuffer f a b) = TFrameBuffer f (applyTy st a) (applyTy st b)
applyTy st (TPrimitiveStream f a b f' c) = TPrimitiveStream f (applyTy st a) (applyTy st b) f' (applyTy st c)
applyTy st (TBlending f a) = TBlending f (applyTy st a)
applyTy st (TFragmentOperation f a) = TFragmentOperation f (applyTy st a)
applyTy st (TInterpolated f a) = TInterpolated f (applyTy st a)
applyTy st (TFetchPrimitive f a) = TFetchPrimitive f (applyTy st a)
applyTy st (TVertexOut f a) = TVertexOut f (applyTy st a)
applyTy st (TInput f a) = TInput f (applyTy st a)
applyTy st (TRasterContext f a) = TRasterContext f (applyTy st a)
applyTy st (TAccumulationContext f a) = TAccumulationContext f (applyTy st a)
applyTy st (TFragmentFilter f a) = TFragmentFilter f (applyTy st a)
applyTy st (TFragmentOut f a) = TFragmentOut f (applyTy st a)
applyTy st (Color a) = Color (applyTy st a)
applyTy st (Depth a) = Depth (applyTy st a)
applyTy st (Stencil a) = Stencil (applyTy st a)
{-
applyTy st (TBlendEquation C) = TBlendEquation C
applyTy st (TBlendingFactor C) = TBlendingFactor C
applyTy st (TV4F C) = TV4F C
applyTy st (TFloat C) = TFloat C
applyTy st (TNat n) = TNat n
applyTy st (TPolygonMode C) = TPolygonMode C
applyTy st (TPolygonOffset C) = TPolygonOffset C
applyTy st (TPointSize C) = TPointSize C
applyTy st (TCullMode C) = TCullMode C
applyTy st (TFrontFace C) = TFrontFace C
-}
applyTy _ t = t --error $ "applyTy: " ++ show t

applyMonoEnv :: Subst -> MonoEnv -> MonoEnv
applyMonoEnv s e = fmap (applyTy s) e

applyInstEnv :: Subst -> InstEnv -> Unique InstEnv
applyInstEnv s e = concat <$> mapM tyInst ((trace_ (show (s,e,"->",e'))) e')
 where
  e' = flip fmap e $ \case
            CClass c t -> CClass c $ applyTy s t
            CEq a b -> CEq (applyTy s a) (applyTy s b)
  tyInst (CClass CNum (TFun (TFMatVecElem (TVar C _)))) = return []     -- hack
  tyInst x@(CClass c TVar{}) = return [x]
  tyInst (CClass c t) = if isInstance c t then return [] else err
   where err = throwErrorUnique $ "no " ++ show c ++ " instance for " ++ show t

--  tyInst (TEq v (TFun (TFFTRepr' (TInterpolated C (TV4F C))))) = return []

  tyInst x = return [x] -- TODO: reduction of type families

joinInstEnv :: [InstEnv] -> InstEnv
joinInstEnv e = Set.toList . Set.unions . map Set.fromList $ e
    -- TODO: constraint solving

freeVarsTy :: Ty -> Set TName
freeVarsTy (TVar _ a) = Set.singleton a
freeVarsTy (TArr a b) = freeVarsTy a `mappend` freeVarsTy b
freeVarsTy (TFun f) = foldMap freeVarsTy f
freeVarsTy (TVertexStream _ a b) = freeVarsTy a `mappend` freeVarsTy b
freeVarsTy (TFragmentStream _ a b) = freeVarsTy a `mappend` freeVarsTy b
freeVarsTy (TPrimitiveStream _ a b _ c) = freeVarsTy a `mappend` freeVarsTy b `mappend` freeVarsTy c
freeVarsTy (TImage _ a b) = freeVarsTy a `mappend` freeVarsTy b
freeVarsTy (TTuple _ a) = foldl mappend mempty $ map freeVarsTy a
freeVarsTy (TArray _ a) = freeVarsTy a
freeVarsTy (TBlending _ a) = freeVarsTy a
freeVarsTy (TFragmentOperation _ a) = freeVarsTy a
freeVarsTy (TInterpolated _ a) = freeVarsTy a
freeVarsTy (TFetchPrimitive _ a) = freeVarsTy a
freeVarsTy (TVertexOut _ a) = freeVarsTy a
freeVarsTy (TRasterContext _ a) = freeVarsTy a
freeVarsTy (TAccumulationContext _ a) = freeVarsTy a
freeVarsTy (TFragmentFilter _ a) = freeVarsTy a
freeVarsTy (TFragmentOut _ a) = freeVarsTy a
freeVarsTy (Color a) = freeVarsTy a
freeVarsTy _ = mempty

freeVarsMonoEnv :: MonoEnv -> Set TName
freeVarsMonoEnv m = F.foldMap freeVarsTy m

freeVarsInstEnv :: InstEnv -> Set TName
freeVarsInstEnv i = F.foldMap (freeVarsTy . (\(CClass _ ty) -> ty)) i

-- union mono envs matching on intersection
joinMonoEnv :: MonoEnv -> MonoEnv -> Unique MonoEnv
joinMonoEnv a b = do
  let merge k ml mr = do
        l <- ml
        r <- mr
        if l == r then ml else throwErrorUnique $ k ++ " mismatch " ++ show l ++ " with " ++ show r
  T.sequence $ Map.unionWithKey merge (fmap return a) (fmap return b)

instTyping :: Typing -> Unique (Typing,Subst)
instTyping (m,i,t) = do
  let fv = freeVarsTy t `mappend` freeVarsMonoEnv m -- `mappend` freeVarsInstEnv i
  newVars <- replicateM (Set.size fv) (newVar C)
  let s = Map.fromList $ zip (Set.toList fv) newVars
  i' <- applyInstEnv s i
  return ((applyMonoEnv s m,i',applyTy s t),s)

bindVar :: TName -> Ty -> Unique Subst
bindVar n t
  | tvarEq t = return mempty
  | n `Set.member` freeVarsTy t = throwErrorUnique $ "Infinite type, type variable " ++ n ++ " occurs in " ++ show t
  | otherwise = return $ Map.singleton n t
 where
  tvarEq (TVar _ m) = m == n
  tvarEq _ = False

compose :: Subst -> Subst -> Subst
compose b a = mappend a $ applyTy a <$> b

unifyTy :: Ty -> Ty -> Unique Subst
unifyTy (TVar _ u) t = bindVar u t
unifyTy t (TVar _ u) = bindVar u t
unifyTy a@(TTuple f1 t1) b@(TTuple f2 t2) = do
  let go s [] [] = return s
      go s (a1:xs1) (a2:xs2) = do
        s1 <- unifyTy a1 a2
        return $ s `compose` s1
      go _ _ _ = throwErrorUnique $ "can not unify " ++ show a ++ " with " ++ show b
  go mempty t1 t2
unifyTy (TArr a1 b1) (TArr a2 b2) = do
  s1 <- unifyTy a1 a2
  s2 <- unifyTy (applyTy s1 b1) (applyTy s1 b2)
  return $ s1 `compose` s2
unifyTy (TImage f1 a1 b1) (TImage f2 a2 b2) = do
  s1 <- unifyTy a1 a2
  s2 <- unifyTy (applyTy s1 b1) (applyTy s1 b2)
  return $ s1 `compose` s2
unifyTy (TFrameBuffer f1 a1 b1) (TFrameBuffer f2 a2 b2) = do
  s1 <- unifyTy a1 a2
  s2 <- unifyTy (applyTy s1 b1) (applyTy s1 b2)
  return $ s1 `compose` s2
unifyTy (TVertexStream f1 a1 b1) (TVertexStream f2 a2 b2) = do
  s1 <- unifyTy a1 a2
  s2 <- unifyTy (applyTy s1 b1) (applyTy s1 b2)
  return $ s1 `compose` s2
unifyTy (TFragmentStream f1 a1 b1) (TFragmentStream f2 a2 b2) = do
  s1 <- unifyTy a1 a2
  s2 <- unifyTy (applyTy s1 b1) (applyTy s1 b2)
  return $ s1 `compose` s2
unifyTy (TPrimitiveStream f1 a1 b1 g1 c1) (TPrimitiveStream f2 a2 b2 g2 c2) = do
  s1 <- unifyTy a1 a2
  s2 <- unifyTy (applyTy s1 b1) (applyTy s1 b2)
  s3 <- unifyTy (applyTy s2 $ applyTy s1 c1) (applyTy s2 $ applyTy s1 c2)
  return $ s1 `compose` s2 `compose` s3
unifyTy (TInput _ a1) (TInput _ a2) = unifyTy a1 a2
unifyTy (TBlending _ a1) (TBlending _ a2) = unifyTy a1 a2
unifyTy (TInterpolated _ a1) (TInterpolated _ a2) = unifyTy a1 a2
unifyTy (TVertexOut _ a1) (TVertexOut _ a2) = unifyTy a1 a2
unifyTy (TFetchPrimitive _ a1) (TFetchPrimitive _ a2) = unifyTy a1 a2
unifyTy (TRasterContext _ a1) (TRasterContext _ a2) = unifyTy a1 a2
unifyTy (TFragmentOperation _ a1) (TFragmentOperation _ a2) = unifyTy a1 a2
unifyTy (TAccumulationContext _ a1) (TAccumulationContext _ a2) = unifyTy a1 a2
unifyTy (TFragmentFilter _ a1) (TFragmentFilter _ a2) = unifyTy a1 a2
unifyTy (TFragmentOut _ a1) (TFragmentOut _ a2) = unifyTy a1 a2
unifyTy (Color a1) (Color a2) = unifyTy a1 a2
unifyTy a b
  | a == b = return mempty
  | otherwise = throwErrorUnique $ "can not unify " ++ show a ++ " with " ++ show b

unifyEqs :: [(Ty,Ty)] -> Unique Subst
unifyEqs eqs = do
  let uniTy s (a,b) = do
        s' <- unifyTy (applyTy s a) (applyTy s b)
        return $ s `compose` s'
  foldM uniTy mempty eqs

unify :: [MonoEnv] -> [Ty] -> Unique Subst
unify ml tl = do
  a <- newVar C
  let toEqs :: EName -> [(Ty,Ty)]
      toEqs v = case mapMaybe (Map.lookup v) ml of
        [] -> []
        x:xs -> map ((,) x) xs

      vars :: Set EName
      vars = mconcat . map Map.keysSet $ ml

      varEqs :: [(Ty,Ty)]
      varEqs = concatMap toEqs . Set.toList $ vars

      tyEqs :: [(Ty,Ty)]
      tyEqs = map ((,) a) tl

  unifyEqs $ tyEqs ++ varEqs

prune :: Typing -> Typing
prune (m,i,t) = (m,i,t) --(m,i',t)
 where
  v = Set.map (TVar C) $ freeVarsTy t `mappend` freeVarsMonoEnv m
  i' = flip filter i $ \case
        CClass _ a -> Set.member a v
        _ -> True -- ???

unamb :: PolyEnv -> Typing -> Unique ()
unamb env (m,i,t) = do
  let v = Set.map (TVar C) $ freeVarsTy t `mappend` freeVarsMonoEnv m
  return ()
  forM_ i $ \case
        --CClass _ a -> if Set.member a v then return () else throwErrorUnique $ unlines ["ambiguous type: " ++ show (i,t),"env: " ++ show m, "free vars: " ++ show v, "poly env: " ++ show env]
        _ -> return ()

infer :: PolyEnv -> Exp Range -> Unique (Exp Typing)
infer penv (ETuple r t) = withRanges [r] $ do
  te <- mapM (infer penv) t
  let (ml,il,tl) = unzip3 $ map getTag te
  s <- unify ml []
  m <- foldM (\a b -> joinMonoEnv (applyMonoEnv s a) (applyMonoEnv s b)) mempty ml
  i <- joinInstEnv <$> mapM (applyInstEnv s) il
  let ty = (m,i,TTuple C $ map (applyTy s) tl)
  return (ETuple ty te)
infer penv (ELit r l) = withRanges [r] $ ELit <$> inferLit l <*> pure l
infer penv (EVar r _ n)
  | isPrimFun n = withRanges [r] $ EVar <$> inferPrimFun n <*> pure mempty <*> pure n
  | otherwise = withRanges [r] $ case Map.lookup n penv of
      Nothing -> do
        t <- trace "mono var" <$> newVar C
        return $ EVar (Map.singleton n t,mempty,t) mempty n
      Just t -> trace "poly var" <$> do
        (t',s) <- instTyping t
        return $ EVar t' s n 
infer penv (ELam r n f) = withRanges [r] $ do
  tf <- infer penv f
  let (m,i,t) = getTag tf
  case Map.lookup n m of
    Nothing -> do
      a <- newVar C
      return $ ELam (m,i,a ~> t) n tf
    Just a -> return $ ELam (Map.delete n m,i,a ~> t) n tf
infer penv (EApp r _ f a) = withRanges [r] $ do
  tf <- infer penv f
  ta <- infer penv a
  let (m1,i1,t1) = getTag tf
      (m2,i2,t2) = getTag ta
  a <- newVar C
  s <- unify [m1,m2] [t1,t2 ~> a]
  m3 <- joinMonoEnv (applyMonoEnv s m1) (applyMonoEnv s m2)
  i3 <- joinInstEnv <$> mapM (applyInstEnv s) [i1, i2]
  unamb penv (m3,i3,applyTy s a)
  let tyBind = Map.filterWithKey (\k _ -> Set.member k tyFree) s 
      tyFree = freeVarsTy tyF
      (_,_,tyF) = getTag tf
  return $ trace__ ("app subst:\n    " ++ show tyF ++ "\n    " ++ show tyBind) $ EApp (m3,i3,applyTy s a) s tf ta
infer penv (ELet r n x e) = withRanges [r] $ do
  tx <- infer penv x
  let d1@(m1,i1,t1) = getTag tx
  s0 <- unify [m1] [t1]
  let m0 = Map.delete n $ applyMonoEnv s0 m1
      t0 = applyTy s0 t1
  i0 <- trace_ "1" <$> applyInstEnv s0 i1
  trace (show ("m1",m1,"let1",d1,"let2",(m0,i0,t0))) $ unamb penv (m0,i0,t0)
  let penv' = Map.insert n (m0,i0,t0) penv
  te <- infer penv' e
  let (m',i',t') = getTag te
  s_ <- unify [m0,m'] []
  let s = s0 `compose` s_
  m <- joinMonoEnv (applyMonoEnv s m') (applyMonoEnv s m0)
  a1 <- trace_ "2" <$> applyInstEnv s i'
  a2 <- trace_ "3" <$> applyInstEnv s i0
  let i = joinInstEnv [a1,a2]
      ty = prune $ trace (show ("s",s,"penv",penv',"in",(m',i',t'))) $ trace_ (show $ ("s0",s0,m1,"s",s,m0,m')) $ (m,i,applyTy s t')
  return $ ELet ty n tx te

inference :: ByteString -> Exp Range -> Either String (Exp Typing)
inference src e = case scopeChk src e of
  Left m  -> Left m
  Right () -> runIdentity $ runExceptT $ (flip evalStateT) (0,src,[]) act
   where
    act = do
      a <- infer mempty e
      unamb mempty $ getTag a
      return a

-- scope checking
scopeCheck :: ByteString -> Set EName -> Exp Range -> Either String ()
scopeCheck src vars (EVar r _ n) = if Set.member n vars then return () else throwErrorSrc src [r] $ "Variable " ++ n ++ " is not in scope."
scopeCheck src vars (EApp r _ f a) = scopeCheck src vars f >> scopeCheck src vars a
scopeCheck src vars (ELam r n f) = if Set.notMember n vars then scopeCheck src (Set.insert n vars) f else throwErrorSrc src [r] $ "Variable name clash: " ++ n
scopeCheck src vars (ELet r n x e) = do
  let vars' = Set.insert n vars
  if Set.notMember n vars then scopeCheck src vars' x >> scopeCheck src vars' e else throwErrorSrc src [r] $ "Variable name clash: " ++ n
scopeCheck src vars _ = return ()

scopeChk :: ByteString -> Exp Range -> Either String ()
scopeChk src e = scopeCheck src primFunSet e
