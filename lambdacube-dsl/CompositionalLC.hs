{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module CompositionalLC
    ( inference
    , compose
    , subst
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
import Control.Monad.Writer
import Control.Arrow
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

hasCommon :: Ord a => Set a -> Set a -> Bool
hasCommon a b = not $ Set.null $ a `Set.intersection` b

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

-- basic substitution (renaming of variables)
class Substitute a where subst :: Subst -> a -> a

instance Substitute Ty where
    subst st ty | Map.null st = ty -- optimization
    subst st tv@(TVar _ a) = case Map.lookup a st of
      Nothing -> tv
      Just t  -> t
    subst st (Ty t) = Ty $ fmap (subst st) t

instance Substitute MonoEnv where
    subst s e = fmap (subst s) e

instance (Substitute a, Substitute b) => Substitute (a, b) where
    subst s (cs, es) = (subst s cs, subst s es)

instance (Substitute a, Substitute b, Substitute c) => Substitute (a, b, c) where
    subst s (a, b, c) = (subst s a, subst s b, subst s c)

instance Substitute a => Substitute [a] where
    subst = map . subst

instance Substitute a => Substitute (ClassConstraint a) where
    subst = fmap . subst

instance Substitute a => Substitute (EqConstraint a) where
    subst = fmap . subst

-- replace free type variables with fresh type variables
instTyping :: Typing -> Unique (Typing,Subst)
instTyping ty@(_, _, freeVars -> fv) = do
  newVars <- replicateM (Set.size fv) (newVar C)
  let s = Map.fromList $ zip (Set.toList fv) newVars
  return (subst s ty, s)

-------------------------------------------------------------------------------- unification

-- make single tvar substitution; check infinite types
bindVar :: TName -> Ty -> Unique Subst
bindVar n (TVar _ m) | m == n = return mempty
bindVar n t
  | n `Set.member` freeVars t = throwErrorUnique $ "Infinite type, type variable " ++ n ++ " occurs in " ++ show t
  | otherwise = return $ Map.singleton n t

-- compose substitutions
-- Note: domain of substitutions is disjunct
compose :: Subst -> Subst -> Subst
s1 `compose` s2 = mappend s2 $ subst s2 <$> s1

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
unifyEqs = foldM uniTy mempty
  where
    uniTy s (a, b) = (s `compose`) <$> unifyTy (subst s a) (subst s b)

unifyEqs' :: [[Ty]] -> Unique Subst
unifyEqs' = unifyEqs . concatMap pairs
  where
    pairs [] = []
    pairs (x:xs) = map ((,) x) xs

--------------------------------------------------------------------------------

joinInstEnv :: Subst -> [InstEnv] -> Unique (Subst, InstEnv)
joinInstEnv s (unzip -> (concat -> cs, concat -> es)) = do
    (s, es) <- untilNoUnif s $ nub $ subst s es
    cs <- concat <$> mapM (simplifyClassInst . subst s) cs
    -- if needed, simplify class constraints here:  (Ord a, Eq a) --> Ord a
    return (s, (cs, es))
  where
    untilNoUnif acc es = do
        (es, w) <- runWriterT $ do
            -- right reduce
            tell $ Map.elems $ Map.unionsWith (++) [Map.singleton f [ty] | CEq ty f <- es]
            -- injectivity test
            tell $ concatMap (concatMap testInj . groupBy ((==) `on` injType) . sortBy (compare `on` injType))
                 $ Map.elems $ Map.unionsWith (++) [Map.singleton ty [f] | CEq ty f <- es]
            filterM simplifyInst es
        s <- unifyEqs' w
        if Map.null s then return (acc, es) else untilNoUnif (acc `compose` s) $ nub $ subst s es

    simplifyClassInst cl@(CClass c t) = isInstance (\c t -> return [CClass c t]) throwErrorUnique (return [cl]) (return []) c t

    discard x = tell x >> return False
    keep = return True
    fail = lift . throwErrorUnique

    isSplit a b c = not (b `hasCommon` c) && a == (b `mappend` c)

    diff a b c
        | Map.keysSet b `Set.isSubsetOf` Map.keysSet a =
            discard $ [c, TRecord $ a Map.\\ b]: [[t, a Map.! f] | (f, t) <- Map.toList b]
        | otherwise = fail "not split" -- TODO: better error handling

    simplifyInst = \case
        Split (TRecord a) (TRecord b) (TRecord c)
            | isSplit (Map.keysSet a) (Map.keysSet b) (Map.keysSet c) -> discard [[t, x Map.! f] | (f, t) <- Map.toList a]
            | otherwise -> fail "not split" -- TODO: better error handling
          where x = b `mappend` c
        Split (TRecord a) (TRecord b) c@(TVar C _) -> diff a b c
        Split (TRecord a) c@(TVar C _) (TRecord b) -> diff a b c
        Split c@(TVar C _) (TRecord a) (TRecord b)
            | not $ Map.keysSet b `hasCommon` Map.keysSet a -> discard [[c, TRecord $ a `Map.union` b]]
            | otherwise -> fail "not split" -- TODO: better error handling
        Split TVar{} TVar{} _ -> keep
        Split TVar{} _ TVar{} -> keep
        Split a TVar{} TVar{} -> keep
        Split TVar{} TVar{} TVar{} -> keep
        Split a b c -> fail "bad split" -- TODO: better error handling
        CEq ty f -> do
            forM_ (backPropTF ty f) $ \(t1, t2) -> tell [[t1, t2]]
            reduceTF
                (\t -> discard [[ty, t]])
                (fail . (("error during reduction of " ++ show f ++ "  ") ++))
                keep f

simplifyTyping :: Typing -> Unique Typing   
simplifyTyping (me, ie, t) = do
    (s, ie) <- joinInstEnv mempty [ie]
    return (subst s me, ie, subst s t)

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
    defined = growDefinedVars mempty $ freeVars (m, t)

    growDefinedVars acc s
        | Set.null s = acc
        | otherwise = growDefinedVars (acc `mappend` s) (grow s Set.\\ acc)

    grow = foldMap g es
      where
        a --> b = \s -> if a `hasCommon` s then b else mempty
        a <-> b = (a --> b) `mappend` (b --> a)
        g (CEq ty f) = freeVars ty <-> freeVars f
        g (Split a b c) = freeVars a <-> freeVars (b, c)

monoVar n = do
    t <- trace "mono var" <$> newVar C
    return $ EVar (Map.singleton n t, mempty, t) n

unif penv ms is t b = do
    s <- unifyEqs' $ b: Map.elems (Map.unionsWith (++) $ map (Map.map (:[])) ms)
    (s, i) <- joinInstEnv s is
    m <- joinMonoEnvs $ subst s ms
    let ty = (m, i, subst s t)
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

