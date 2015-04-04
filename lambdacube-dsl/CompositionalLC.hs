{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
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
import Control.Monad.Tardis

import Type
import Typing

trace__ = D.trace

trace_ _ = id
trace = trace_

-------------------------------------------------------------------------------- utility

withRanges :: [Range] -> Unique a -> Unique a
withRanges rl a = do
  (x,y,rl0) <- get
  put (x,y,rl)
  res <- a
  (z,q,_) <- get
  put (z,q,rl0)
  return res

-- union mono envs matching on intersection
joinMonoEnv :: MonoEnv -> MonoEnv -> Unique MonoEnv
joinMonoEnv a b = T.sequence $ Map.unionWithKey merge (fmap return a) (fmap return b)
  where
    merge k ml mr = do
        l <- ml
        r <- mr
        if l == r then ml else throwErrorUnique $ k ++ " mismatch " ++ show l ++ " with " ++ show r

joinMonoEnvs :: [MonoEnv] -> Unique MonoEnv
joinMonoEnvs = foldM joinMonoEnv mempty

-------------------------------------------------------------------------------- scope checking

scopeChk :: ByteString -> Exp Range -> Either String ()
scopeChk src e = scopeCheck src primFunSet e

scopeCheck :: ByteString -> Set EName -> Exp Range -> Either String ()
scopeCheck src vars (EVar r _ n) = if Set.member n vars then return () else throwErrorSrc src [r] $ "Variable " ++ n ++ " is not in scope."
scopeCheck src vars (EApp r _ f a) = scopeCheck src vars f >> scopeCheck src vars a
scopeCheck src vars (ELam r n f) = if Set.notMember n vars then scopeCheck src (Set.insert n vars) f else throwErrorSrc src [r] $ "Variable name clash: " ++ n
scopeCheck src vars (ELet r n x e) = do
  let vars' = Set.insert n vars
  if Set.notMember n vars then scopeCheck src vars' x >> scopeCheck src vars' e else throwErrorSrc src [r] $ "Variable name clash: " ++ n
scopeCheck src vars _ = return ()

-------------------------------------------------------------------------------- free vars

freeVarsTy :: Ty -> Set TName
freeVarsTy (TVar _ a) = Set.singleton a
freeVarsTy (Ty x) = foldMap freeVarsTy x

freeVarsMonoEnv :: MonoEnv -> Set TName
freeVarsMonoEnv = foldMap freeVarsTy

freeVarsInstEnv :: InstEnv -> Set TName
freeVarsInstEnv = foldMap $ \case
    CClass _ ty -> freeVarsTy ty
    CEq ty f -> freeVarsTy ty `mappend` foldMap freeVarsTy f

-------------------------------------------------------------------------------- substitution

applyTy :: Subst -> Ty -> Ty
applyTy st ty | Map.null st = ty -- optimization
applyTy st tv@(TVar _ a) = case Map.lookup a st of
  Nothing -> tv
  Just t  -> t
applyTy st (Ty t) = Ty $ fmap (applyTy st) t

applyMonoEnv :: Subst -> MonoEnv -> MonoEnv
applyMonoEnv s e = fmap (applyTy s) e

-- basic substitution (renaming of variables)
applyInstEnv :: Subst -> InstEnv -> InstEnv
applyInstEnv = map . applyConstraint

applyConstraint s = \case
    CClass c t -> CClass c $ applyTy s t
    CEq t f -> CEq (applyTy s t) $ fmap (applyTy s) f

-- replace free type variables with fresh type variables
instTyping :: Typing -> Unique (Typing,Subst)
instTyping (m,i,t) = do
  let fv = freeVarsTy t `mappend` freeVarsMonoEnv m `mappend` freeVarsInstEnv i
  newVars <- replicateM (Set.size fv) (newVar C)
  let s = Map.fromList $ zip (Set.toList fv) newVars
  return ((applyMonoEnv s m, applyInstEnv s i, applyTy s t), s)

-------------------------------------------------------------------------------- unification

uniTy :: Subst -> Ty -> Ty -> Unique Subst
uniTy s a b = (s `compose`) <$> unifyTy (applyTy s a) (applyTy s b)

-- make single tvar substitution; check infinite types
bindVar :: TName -> Ty -> Unique Subst
bindVar n t
  | tvarEq t = return mempty
  | n `Set.member` freeVarsTy t = throwErrorUnique $ "Infinite type, type variable " ++ n ++ " occurs in " ++ show t
  | otherwise = return $ Map.singleton n t
 where
  tvarEq (TVar _ m) = m == n
  tvarEq _ = False

-- compose substitutions
-- Note: domain of substitutions is disjunct
compose :: Subst -> Subst -> Subst
s1 `compose` s2 = mappend s2 $ applyTy s2 <$> s1

-- unify frequencies?
unifyTy :: Ty -> Ty -> Unique Subst
unifyTy (TVar _ u) t = bindVar u t
unifyTy t (TVar _ u) = bindVar u t
unifyTy (TTuple f1 t1) (TTuple f2 t2) = unifyEqs $ zip t1 t2
unifyTy (TArr a1 b1) (TArr a2 b2) = unifyEqs [(a1,a2),(b1,b2)]
unifyTy (TImage f1 a1 b1) (TImage f2 a2 b2) = unifyEqs [(a1,a2),(b1,b2)]
unifyTy (TFrameBuffer f1 a1 b1) (TFrameBuffer f2 a2 b2) = unifyEqs [(a1,a2),(b1,b2)]
unifyTy (TVertexStream f1 a1 b1) (TVertexStream f2 a2 b2) = unifyEqs [(a1,a2),(b1,b2)]
unifyTy (TFragmentStream f1 a1 b1) (TFragmentStream f2 a2 b2) = unifyEqs [(a1,a2),(b1,b2)]
unifyTy (TPrimitiveStream f1 a1 b1 g1 c1) (TPrimitiveStream f2 a2 b2 g2 c2) = unifyEqs [(a1,a2),(b1,b2),(c1,c2)]
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

-- compositional (not linear) unification?
unifyEqs :: [(Ty,Ty)] -> Unique Subst
unifyEqs = foldM (uncurry . uniTy) mempty

unifyEqs' :: [[Ty]] -> Unique Subst
unifyEqs' = unifyEqs . concatMap pairs
  where
    pairs [] = []
    pairs (x:xs) = map ((,) x) xs

-- unify the types of each distinct variable in the monoenvs and the types in the [Ty] list
unify :: [MonoEnv] -> [Ty] -> Unique Subst
unify ml tl = unifyEqs' $ tl: Map.elems (Map.unionsWith (++) $ map (Map.map (:[])) ml)

--------------------------------------------------------------------------------

type SubstAction = TardisT Subst Subst Unique

untilNoUnif :: (a -> SubstAction a) -> Subst -> a -> Unique (Subst, a)
untilNoUnif act = f where
    f acc x = do
        (x', (_, fw)) <- runTardisT (act x) (mempty, mempty)
        if Map.null fw then return (acc, x) else f (acc `compose` fw) x'

applyPast f x = do
    s <- getPast
    return $ f s x

applyFuture f x = do
    s <- getFuture
    return $ f s x

addUnif_ s = do
    modifyForwards (`compose` s)
    modifyBackwards (s `compose`)

addUnif t1 t2 = lift (unifyTy t1 t2) >>= addUnif_
addUnif' tss = lift (unifyEqs' tss) >>= addUnif_

simplifyInstEnv :: InstEnv -> SubstAction InstEnv
simplifyInstEnv = fmap concat . mapM (applyPast applyConstraint >=> simplifyInst)

rightReduce :: InstEnv -> SubstAction InstEnv
rightReduce ie = do
    addUnif' $ map snd xs
    return $ [x | x@CClass{} <- ie] ++ [CEq ty f | (f, ty: _) <- xs]
  where
    xs = Map.toList $ Map.unionsWith (++) [Map.singleton f [ty] | CEq ty f <- ie]

simplifyInst c@(CClass _ TVar{}) = (:[]) <$> applyFuture applyConstraint c
simplifyInst (CClass c t) = if isInstance c t then return [] else lift $ throwErrorUnique $ "no " ++ show c ++ " instance for " ++ show t
simplifyInst c@(CEq ty f) = reduceTF (\t -> addUnif ty t >> return []) error{-TODO: proper error handling-} ((:[]) <$> applyFuture applyConstraint c) f

joinInstEnv :: Subst -> [InstEnv] -> Unique (Subst, InstEnv)
joinInstEnv s es = do
    (s, es) <- untilNoUnif (mapM simplifyInstEnv) s $ map (applyInstEnv s) es
    untilNoUnif (rightReduce >=> simplifyInstEnv) s $ applyInstEnv s $ concat es
    -- TODO: simplify class constraints:  (Ord a, Eq a) --> Ord a
    -- TODO: type family injectivity reductions:  (v ~ F a, v ~ F b) --> a ~ b   if F injective in its parameter

simplifyTyping (me, ie, t) = do
    (s, ie) <- joinInstEnv mempty [ie]
    return (applyMonoEnv s me, ie, applyTy s t)

-- TODO
prune :: Typing -> Typing
prune (m,i,t) = (m,i,t) --(m,i',t)
 where
  v = Set.map (TVar C) $ freeVarsTy t `mappend` freeVarsMonoEnv m
  i' = flip filter i $ \case
        CClass _ a -> Set.member a v
        _ -> True -- ???

-- TODO
unamb :: PolyEnv -> Typing -> Unique ()
unamb env (m,i,t) = do
  let v = Set.map (TVar C) $ freeVarsTy t `mappend` freeVarsMonoEnv m
  return ()
  forM_ i $ \case
        --CClass _ a -> if Set.member a v then return () else throwErrorUnique $ unlines ["ambiguous type: " ++ show (i,t),"env: " ++ show m, "free vars: " ++ show v, "poly env: " ++ show env]
        _ -> return ()

monoVar n = do
    t <- trace "mono var" <$> newVar C
    return ((Map.singleton n t, mempty, t), mempty)

infer :: PolyEnv -> Exp Range -> Unique (Exp Typing)
infer penv (ETuple r t) = withRanges [r] $ do
  te@(unzip3 . map getTag -> (ml, il, tl)) <- mapM (infer penv) t
  ETuple <$> (snd <$> unif ml il (TTuple C tl) []) <*> pure te
infer penv (ELit r l) = withRanges [r] $ ELit <$> inferLit l <*> pure l
infer penv (EVar r _ n) = withRanges [r] $ do
  (t, s) <- if isPrimFun n
    then (,) <$> (inferPrimFun n >>= simplifyTyping) <*> pure mempty
    else maybe (monoVar n) (fmap (trace "poly var") . instTyping) $ Map.lookup n penv
  return $ EVar t s n 
infer penv (ELam r n f) = withRanges [r] $ do
  tf@(getTag -> (m, i, t)) <- infer penv f
  a <- maybe (newVar C) return $ Map.lookup n m
  return $ ELam (Map.delete n m, i, a ~> t) n tf
infer penv (EApp r _ f a) = withRanges [r] $ do
  tf@(getTag -> (m1, i1, t1)) <- infer penv f
  ta@(getTag -> (m2, i2, t2)) <- infer penv a
  v <- newVar C
  (s, ty) <- unif [m1, m2] [i1, i2] v [t1, t2 ~> v]
  unamb penv ty     -- move into unif?
  let tyBind = Map.filterWithKey (\k _ -> Set.member k tyFree) s 
      tyFree = freeVarsTy t1
  return $ trace__ ("app subst:\n    " ++ show t1 ++ "\n    " ++ show tyBind) $ EApp ty s tf ta
infer penv (ELet r n x e) = withRanges [r] $ do
  tx@(getTag -> d1@(m1, i1, t1)) <- infer penv x
  ty_@(m0, i0, t0) <- snd <$> unif [m1] [i1] t1 [t1]    -- this has no effect
  trace (show ("m1",m1,"let1",d1,"let2",(m0,i0,t0))) $ unamb penv ty_    -- move into unif?
  te@(getTag -> (m', i', t')) <- infer (Map.insert n (m0, i0, t0) penv) e
  ELet <$> (snd <$> unif [m0, m'] [i', i0] t' []) <*> pure n <*> pure tx <*> pure te

unif ms is t b = do
    s <- unify ms b
    (s, i) <- joinInstEnv s is
    m <- joinMonoEnvs $ map (applyMonoEnv s) ms
    return (s, (m, i, applyTy s t))

-------------------------------------------------------------------------------- main inference function

inference :: ByteString -> Exp Range -> Either String (Exp Typing)
inference src e = case scopeChk src e of
  Left m  -> Left m
  Right () -> runIdentity $ runExceptT $ (flip evalStateT) (0,src,[]) act
   where
    act = do
      a <- infer mempty e
      unamb mempty $ getTag a
      return a

