{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module CompositionalLC
    ( inference
    , compose
    , applyTy
    ) where

import qualified Debug.Trace as D
import Data.Function
import Data.List
import Data.Maybe
import Data.ByteString.Char8 (ByteString)
import Data.Foldable (foldMap)
import Data.Functor.Identity
import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Control.Arrow
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
scopeCheck src vars (EVar r n) = if Set.member n vars then return () else throwErrorSrc src [r] $ "Variable " ++ n ++ " is not in scope."
scopeCheck src vars (EApp r f a) = scopeCheck src vars f >> scopeCheck src vars a
scopeCheck src vars (ELam r n f) = if Set.notMember n vars then scopeCheck src (Set.insert n vars) f else throwErrorSrc src [r] $ "Variable name clash: " ++ n
scopeCheck src vars (ELet r n x e) = do
  let vars' = Set.insert n vars
  if Set.notMember n vars then scopeCheck src vars' x >> scopeCheck src vars' e else throwErrorSrc src [r] $ "Variable name clash: " ++ n
scopeCheck src vars _ = return ()

-------------------------------------------------------------------------------- free vars

class FreeVars a where freeVars :: a -> Set TName

instance FreeVars Ty where
    freeVars (TVar _ a) = Set.singleton a
    freeVars (Ty x) = foldMap freeVars x

instance FreeVars a => FreeVars (Map EName a) where
    freeVars = foldMap freeVars

instance FreeVars a => FreeVars [a] where
    freeVars = foldMap freeVars

instance FreeVars a => FreeVars (ClassConstraint a) where
    freeVars = foldMap freeVars

instance FreeVars a => FreeVars (EqConstraint a) where
    freeVars = foldMap freeVars

instance FreeVars a => FreeVars (TypeFun a) where
    freeVars = foldMap freeVars

instance (FreeVars a, FreeVars b) => FreeVars (a, b) where
    freeVars (cs, es) = freeVars cs `mappend` freeVars es

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
applyInstEnv s (cs, es) = (map (applyClassConstraint s) cs, applyEqInstEnv s es)

applyEqInstEnv =  map . applyEqConstraint

applyClassConstraint = fmap . applyTy
applyEqConstraint = fmap . applyTy

-- replace free type variables with fresh type variables
instTyping :: Typing -> Unique (Typing,Subst)
instTyping (m,i,t) = do
  let fv = freeVars t
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
  | n `Set.member` freeVars t = throwErrorUnique $ "Infinite type, type variable " ++ n ++ " occurs in " ++ show t
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

type SubstAction = TardisT Subst (Subst, Subst) Unique

untilNoUnif :: (a -> SubstAction a) -> Subst -> a -> Unique (Subst, a)
untilNoUnif act acc x = do
    (x', (_, (fw, _))) <- runTardisT (act x) (mempty, mempty)
    if Map.null fw then return (acc, x) else untilNoUnif act (acc `compose` fw) x'

applyPast f x = do
    (_, s) <- getPast
    return $ f s x

applyFuture f x = do
    s <- getFuture
    return $ f s x

addUnif_ s = do
    modifyForwards ((`compose` s) *** (`compose` s))
    modifyBackwards (s `compose`)

barrier :: x -> SubstAction x
barrier x = do
    modifyForwards $ \(all, _partial) -> (all, mempty)
    return x

addUnif t1 t2 = lift (unifyTy t1 t2) >>= addUnif_
addUnif' tss = lift (unifyEqs' tss) >>= addUnif_

simplifyInstEnv :: EqInstEnv -> SubstAction EqInstEnv
simplifyInstEnv = fmap concat . mapM (applyPast applyEqConstraint >=> simplifyInst)

isSplit a b c = not (b `hasCommon` c) && a == (b `mappend` c)

simplifyInst x@(Split (TRecord a) (TRecord b) (TRecord c))
    | isSplit (Map.keysSet a) (Map.keysSet b) (Map.keysSet c) = addUnif' [[t, x Map.! f] | (f, t) <- Map.toList a] >> return []
    | otherwise = error "not split" -- TODO: better error handling
  where x = b `mappend` c
simplifyInst x@(Split (TRecord a) (TRecord b) c@(TVar C _))
    | Map.keysSet b `Set.isSubsetOf` Map.keysSet a = do
        addUnif' $ [c, TRecord $ a Map.\\ b]: [[t, a Map.! f] | (f, t) <- Map.toList b]
        return []
    | otherwise = error "not split" -- TODO: better error handling
simplifyInst x@(Split (TRecord a) c@(TVar C _) (TRecord b))
    | Map.keysSet b `Set.isSubsetOf` Map.keysSet a = do
        addUnif' $ [c, TRecord $ a Map.\\ b]: [[t, a Map.! f] | (f, t) <- Map.toList b]
        return []
    | otherwise = error "not split" -- TODO: better error handling
simplifyInst x@(Split c@(TVar C _) (TRecord a) (TRecord b))
    | not $ Map.keysSet b `hasCommon` Map.keysSet a = do
        addUnif c $ TRecord $ a `Map.union` b
        return []
    | otherwise = error "not split" -- TODO: better error handling
simplifyInst x@(Split a@TVar{} b@TVar{} c) = (:[]) <$> applyFuture applyEqConstraint x
simplifyInst x@(Split a@TVar{} b c@TVar{}) = (:[]) <$> applyFuture applyEqConstraint x
simplifyInst x@(Split a b@TVar{} c@TVar{}) = (:[]) <$> applyFuture applyEqConstraint x
simplifyInst x@(Split a@TVar{} b@TVar{} c@TVar{}) = (:[]) <$> applyFuture applyEqConstraint x
simplifyInst x@(Split a b c) = error "bad split" -- TODO: better error handling
simplifyInst c@(CEq ty f) = reduceTF
    (\t -> addUnif ty t >> return [])
    (lift . throwErrorUnique . (("error during reduction of " ++ show f ++ "  ") ++))
    ((:[]) <$> applyFuture applyEqConstraint c ) f

simplifyInstEnv' :: EqInstEnv -> SubstAction EqInstEnv
simplifyInstEnv' is = mapM_ simplifyInst' is >> return is

simplifyInst' c@(CEq ty f) = mapM_ (uncurry addUnif) $ backPropTF ty f
simplifyInst' _ = return ()

rightReduce :: EqInstEnv -> SubstAction EqInstEnv
rightReduce ie = do
    addUnif' $ map snd xs
    return $ [CEq ty f | (f, ty: _) <- xs] ++ [s | s@Split{} <- ie]
  where
    xs = Map.toList $ Map.unionsWith (++) [Map.singleton f [ty] | CEq ty f <- ie]

injectivityTest :: EqInstEnv -> SubstAction EqInstEnv
injectivityTest ie = do
    addUnif' $ concatMap (concatMap testInj . groupBy ((==) `on` injType) . sortBy (compare `on` injType) . snd) xs
    return $ [CEq ty f | (ty, fs) <- xs, f <- nub fs] ++ [s | s@Split{} <- ie]
  where
    xs = Map.toList $ Map.unionsWith (++) [Map.singleton ty [f] | CEq ty f <- ie]

simplifyClassInst cl@(CClass c t) = isInstance (\c t -> return [CClass c t]) throwErrorUnique (return [cl]) (return []) c t

joinInstEnv :: Subst -> [InstEnv] -> Unique (Subst, InstEnv)
joinInstEnv s (unzip -> (concat -> cs, concat -> es)) = do
    (s, es) <- untilNoUnif (rightReduce >=> applyPast applyEqInstEnv >=> barrier >=>
                            injectivityTest >=> applyPast applyEqInstEnv >=> barrier >=>
                            simplifyInstEnv' >=>
                            simplifyInstEnv) s
                    $ applyEqInstEnv s es
    cs <- concat <$> mapM (simplifyClassInst . applyClassConstraint s) cs
    -- if needed, simplify class constraints here:  (Ord a, Eq a) --> Ord a
    return (s, (cs, es))

simplifyTyping (me, ie, t) = do
    (s, ie) <- joinInstEnv mempty [ie]
    return (applyMonoEnv s me, ie, applyTy s t)

{- fail on ambiguous types
Ambiguous:
  (Show a) => Int
  (Int ~ F a) => Int
  (a ~ F a) => Int
  (a ~ F b, b ~ F a) => Int
Not ambiguous:
  (Show a, a ~ F b) => b
  (Show a, b ~ F a) => b
-}
unamb :: PolyEnv -> Typing -> Unique ()
unamb env ty@(m,pe@(is,es),t)
    | used `Set.isSubsetOf` defined = return ()
    | otherwise = throwErrorUnique $ unlines ["ambiguous type: " ++ show ty, "env: " ++ show m, "defined vars: " ++ show defined, "used vars: " ++ show used, "poly env: " ++ show env]
  where
    used = freeVars pe
    defined = untilFix (growDefinedVars es) $ freeVars (m, t)

growDefinedVars es s = s `mappend` mconcat
        (  [freeVars f | CEq ty f <- es, freeVars ty `hasCommon` s]
        ++ [freeVars ty | CEq ty f <- es, freeVars f `hasCommon` s]
        ++ [freeVars (b, c) | Split a b c <- es, freeVars a `hasCommon` s]
        ++ [freeVars a | Split a b c <- es, freeVars (b, c) `hasCommon` s]
        )

hasCommon a b = not $ Set.null $ a `Set.intersection` b

untilFix f s
    | s == s' = s
    | otherwise = untilFix f s'
  where
    s' = f s

monoVar n = do
    t <- trace "mono var" <$> newVar C
    return $ EVar (Map.singleton n t, mempty, t) n

unif penv ms is t b = do
    s <- unify ms b
    (s, i) <- joinInstEnv s is
    m <- joinMonoEnvs $ map (applyMonoEnv s) ms
    let ty = (m, i, applyTy s t)
    unamb penv ty
    return (s, ty)

infer :: PolyEnv -> Exp Range -> Unique (Exp Typing)
-- _.f :: Split r (Record [f :: a]) r' => r -> a
infer penv (EFieldProj r e fn) = withRanges [r] $ do
    te@(getTag -> (m, i, r)) <- infer penv e
    a <- newVar C
    r' <- newVar C
    ty <- simplifyTyping (m, (mempty, [Split r (TRecord $ Map.singleton fn a) r']) `mappend` i, a)
    return $ EFieldProj ty te fn
infer penv (ERecord r (unzip -> (fs, es))) = withRanges [r] $ do
    trs@(unzip3 . map getTag -> (ml, il, tl)) <- mapM (infer penv) es
    ty <- snd <$> unif penv ml il (TRecord $ Map.fromList {-TODO: check-} $ zip fs tl) []
    return $ ERecord ty $ zip fs trs
infer penv (ETuple r t) = withRanges [r] $ do
    te@(unzip3 . map getTag -> (ml, il, tl)) <- mapM (infer penv) t
    ETuple <$> (snd <$> unif penv ml il (TTuple C tl) []) <*> pure te
infer penv (ELit r l) = withRanges [r] $ ELit <$> inferLit l <*> pure l
infer penv (EVar r n) = withRanges [r] $ do
    inferPrimFun
        (\x -> EVar <$> simplifyTyping x <*> pure n)
        (maybe (monoVar n) (fmap ((\(t, s) -> ESubst t s $ EVar t n) . trace "poly var") . instTyping) $ Map.lookup n penv)
        n
infer penv (ELam r n f) = withRanges [r] $ do
    tf@(getTag -> (m, i, t)) <- infer penv f
    a <- maybe (newVar C) return $ Map.lookup n m
    return $ ELam (Map.delete n m, i, a ~> t) n tf
infer penv (EApp r f a) = withRanges [r] $ do
    tf@(getTag -> (m1, i1, t1)) <- infer penv f
    ta@(getTag -> (m2, i2, t2)) <- infer penv a
    v <- newVar C
    (s, ty) <- unif penv [m1, m2] [i1, i2] v [t1, t2 ~> v]
    let tyBind = Map.filterWithKey (\k _ -> Set.member k tyFree) s 
        tyFree = freeVars t1
    return $ trace ("app subst:\n    " ++ show t1 ++ "\n    " ++ show tyBind) $ ESubst ty s $ EApp ty tf ta
infer penv (ELet r n x e) = withRanges [r] $ do
    tx@(getTag -> ty) <- infer penv x
    te@(getTag -> ty') <- infer (Map.insert n ty penv) e
    return $ ELet ty' n tx te

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

