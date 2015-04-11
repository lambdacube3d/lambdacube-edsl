{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
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
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Control.Arrow

import Type
import Typing

-------------------------------------------------------------------------------- utility

trace__ = D.trace

trace_ _ = id
trace = trace_

withRanges :: [Range] -> Unique a -> Unique a
withRanges rl a = do
  (d,x,y,rl0) <- get
  put (d,x,y,rl)
  res <- a
  (d,z,q,_) <- get
  put (d,z,q,rl0)
  return res

hasCommon :: Ord a => Set a -> Set a -> Bool
hasCommon a b = not $ Set.null $ a `Set.intersection` b

groupBy' :: Ord a => [(a, b)] -> [[b]]
groupBy' = unifyMaps . map (uncurry Map.singleton)

unifyMaps :: Ord a => [Map a b] -> [[b]]
unifyMaps = Map.elems . Map.unionsWith (++) . map ((:[]) <$>)

-------------------------------------------------------------------------------- free vars

class FreeVars a where freeVars :: a -> Set TName

instance FreeVars Ty where
    freeVars (TVar _ a) = Set.singleton a
    freeVars (Ty x) = foldMap freeVars x

instance (FreeVars a, FreeVars b) => FreeVars (a, b) where
    freeVars (cs, es) = freeVars cs <> freeVars es

instance FreeVars a => FreeVars (Map EName a)       where freeVars = foldMap freeVars
instance FreeVars a => FreeVars [a]                 where freeVars = foldMap freeVars
instance FreeVars a => FreeVars (ClassConstraint a) where freeVars = foldMap freeVars
instance FreeVars a => FreeVars (EqConstraint a)    where freeVars = foldMap freeVars
instance FreeVars a => FreeVars (TypeFun a)         where freeVars = foldMap freeVars

-------------------------------------------------------------------------------- substitution

-- basic substitution (renaming of variables)
class Substitute a where subst :: Subst -> a -> a

instance Substitute Ty where
    subst st ty | Map.null st = ty -- optimization
    subst st tv@(TVar _ a) = fromMaybe tv $ Map.lookup a st
    subst st (Ty t) = Ty $ subst st <$> t

instance (Substitute a, Substitute b) => Substitute (a, b) where
    subst s = subst s *** subst s
instance (Substitute a, Substitute b, Substitute c) => Substitute (a, b, c) where
    subst s (a, b, c) = (subst s a, subst s b, subst s c)

instance Substitute MonoEnv                             where subst = fmap . subst
instance Substitute a => Substitute [a]                 where subst = fmap . subst
instance Substitute a => Substitute (ClassConstraint a) where subst = fmap . subst
instance Substitute a => Substitute (EqConstraint a)    where subst = fmap . subst

-------------------------------------------------------------------------------- type unification

-- compose substitutions
-- Note: domain of substitutions is disjunct
compose :: Subst -> Subst -> Subst
s1 `compose` s2 = s2 <> (subst s2 <$> s1)

-- compositional (not linear) unification?
unifyTypes :: [[Ty]] -> Unique Subst
unifyTypes xss = flip execStateT mempty $ forM_ xss $ \xs -> sequence_ $ zipWith uni xs $ tail xs
  where
    uni :: Ty -> Ty -> StateT Subst Unique ()
    uni a b = gets subst1 >>= \f -> unifyTy (f a) (f b)
      where
        subst1 s tv@(TVar _ a) = fromMaybe tv $ Map.lookup a s
        subst1 _ t = t

        singSubst n t (TVar _ a) | a == n = t
        singSubst n t (Ty ty) = Ty $ singSubst n t <$> ty

        -- make single tvar substitution; check infinite types
        bindVar n t = do
            s <- get
            let t' = subst s t
            if n `Set.member` freeVars t
                then lift $ throwErrorUnique $ "Infinite type, type variable " ++ n ++ " occurs in " ++ show t
                else put $ Map.insert n t' $ singSubst n t' <$> s

        unifyTy :: Ty -> Ty -> StateT Subst Unique ()
        unifyTy (TVar _ u) (TVar _ v) | u == v = return ()
        unifyTy (TVar _ u) _ = bindVar u b
        unifyTy _ (TVar _ u) = bindVar u a
        unifyTy (TTuple f1 t1) (TTuple f2 t2) = sequence_ $ zipWith uni t1 t2
        unifyTy (TArr a1 b1) (TArr a2 b2) = uni a1 a2 >> uni b1 b2
        unifyTy (TImage f1 a1 b1) (TImage f2 a2 b2) = uni a1 a2 >> uni b1 b2
        unifyTy (TFrameBuffer f1 a1 b1) (TFrameBuffer f2 a2 b2) = uni a1 a2 >> uni b1 b2
        unifyTy (TVertexStream f1 a1 b1) (TVertexStream f2 a2 b2) = uni a1 a2 >> uni b1 b2
        unifyTy (TFragmentStream f1 a1 b1) (TFragmentStream f2 a2 b2) = uni a1 a2 >> uni b1 b2
        unifyTy (TPrimitiveStream f1 a1 b1 g1 c1) (TPrimitiveStream f2 a2 b2 g2 c2) = uni a1 a2 >> uni b1 b2 >> uni c1 c2
        unifyTy (TInput _ a1) (TInput _ a2) = uni a1 a2
        unifyTy (TBlending _ a1) (TBlending _ a2) = uni a1 a2
        unifyTy (TInterpolated _ a1) (TInterpolated _ a2) = uni a1 a2
        unifyTy (TVertexOut _ a1) (TVertexOut _ a2) = uni a1 a2
        unifyTy (TFetchPrimitive _ a1) (TFetchPrimitive _ a2) = uni a1 a2
        unifyTy (TRasterContext _ a1) (TRasterContext _ a2) = uni a1 a2
        unifyTy (TFragmentOperation _ a1) (TFragmentOperation _ a2) = uni a1 a2
        unifyTy (TAccumulationContext _ a1) (TAccumulationContext _ a2) = uni a1 a2
        unifyTy (TFragmentFilter _ a1) (TFragmentFilter _ a2) = uni a1 a2
        unifyTy (TFragmentOut _ a1) (TFragmentOut _ a2) = uni a1 a2
        unifyTy (Color a1) (Color a2) = uni a1 a2
        unifyTy a b
          | a == b = return ()
          | otherwise = lift $ throwErrorUnique $ "can not unify " ++ show a ++ " with " ++ show b

-------------------------------------------------------------------------------- typing unification

joinInstEnv :: Subst -> [InstEnv] -> Unique (Subst, InstEnv)
joinInstEnv s (unzip -> (concat -> cs, concat -> es)) = do
    (s, es) <- untilNoUnif s $ nub $ subst s es
    cs <- concat <$> mapM simplifyClassInst (subst s cs)
    -- if needed, simplify class constraints here:  (Ord a, Eq a) --> Ord a
    return (s, (cs, es))
  where
    untilNoUnif acc es = do
        (es, w) <- runWriterT $ do
            -- right reduce
            tell $ groupBy' [(f, ty) | CEq ty f <- es]
            -- injectivity test
            tell $ concatMap (concatMap testInj . groupBy') $ groupBy' [(ty, (it, f)) | CEq ty f <- es, Just it <- [injType f]]
            filterM simplifyInst es
        s <- unifyTypes w
        if Map.null s then return (acc, es) else untilNoUnif (acc `compose` s) $ nub $ subst s es

    simplifyClassInst cl@(CClass c t) = isInstance (\c t -> return [CClass c t]) throwErrorUnique (return [cl]) (return []) c t

    discard x = tell x >> return False
    keep = return True
    fail = lift . throwErrorUnique

    diff a b c = case Map.keys $ b Map.\\ a of
        [] -> discard $ [c, TRecord $ a Map.\\ b]: unifyMaps [a, b]
        ks -> fail $ "extra keys: " ++ show ks

    simplifyInst = \case
        Split (TRecord a) (TRecord b) (TRecord c) ->
          case (Map.keys $ Map.intersection b c, Map.keys $ a Map.\\ (b <> c), Map.keys $ (b <> c) Map.\\ a) of
            ([], [], []) -> discard $ unifyMaps [a, b, c]
            ks -> fail $ "extra keys: " ++ show ks
        Split (TRecord a) (TRecord b) c@(TVar C _) -> diff a b c
        Split (TRecord a) c@(TVar C _) (TRecord b) -> diff a b c
        Split c@(TVar C _) (TRecord a) (TRecord b) -> case Map.keys $ Map.intersection a b of
            [] -> discard [[c, TRecord $ a <> b]]
            ks -> fail $ "extra keys: " ++ show ks
        Split TVar{} TVar{} _ -> keep
        Split TVar{} _ TVar{} -> keep
        Split a TVar{} TVar{} -> keep
        Split TVar{} TVar{} TVar{} -> keep
        Split a b c -> fail $ "bad split: " ++ show (a, b, c)
        CEq ty f -> do
            forM_ (backPropTF ty f) $ \(t1, t2) -> tell [[t1, t2]]
            reduceTF
                (\t -> discard [[ty, t]])
                (fail . (("error during reduction of " ++ show f ++ "  ") ++))
                keep f

simplifyTyping :: PolyEnv -> Typing -> Unique Typing   
simplifyTyping penv (me, ie, t) = do
    (s, ie) <- joinInstEnv mempty [ie]
    let ty = (subst s me, ie, subst s t)
    unamb penv ty
    return ty

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
unamb penv ty@(m,pe@(is,es),t)
    = addUnambCheck $ if used `Set.isSubsetOf` defined then Nothing else Just $ unlines ["ambiguous type: " ++ show ty, "env: " ++ show m, "defined vars: " ++ show defined, "used vars: " ++ show used, "poly env: " ++ show penv]
  where
    used = freeVars pe
    defined = growDefinedVars mempty $ freeVars (m, t)

    growDefinedVars acc s
        | Set.null s = acc
        | otherwise = growDefinedVars (acc <> s) (grow s Set.\\ acc)

    grow = foldMap g es
      where
        a --> b = \s -> if a `hasCommon` s then b else mempty
        a <-> b = (a --> b) <> (b --> a)
        g (CEq ty f) = freeVars ty <-> freeVars f
        g (Split a b c) = freeVars a <-> freeVars (b, c)

addUnambCheck c = modify $ \(cs, x, y, z) -> (c: cs, x, y, z)

unif :: PolyEnv -> [Ty] -> [MonoEnv] -> [InstEnv] -> Ty -> Unique (Subst, Typing)
unif penv b ms is t = do
    s <- unifyTypes $ b: unifyMaps ms
    (s, i) <- joinInstEnv s is
    let ty = (Map.unions $ subst s ms, i, subst s t)
    unamb penv ty
    return (s, ty)

-------------------------------------------------------------------------------- type inference & scope checking

inference :: ByteString -> Exp Range -> Either String (Exp Typing)
inference src = runExcept . flip evalStateT (mempty, 0, src, []) . chk . infer mempty
  where
    chk m = do
        e <- m
        (cs, _, _, _) <- get
        check cs
        return e
    check cs@(Just _: _) = throwErrorUnique $ head $ catMaybes $ reverse cs
    check _ = return ()

type PolyEnv = Map EName Typing

infer :: PolyEnv -> Exp Range -> Unique (Exp Typing)
infer penv exp = withRanges [getTag exp] $ case exp of
    -- _.f :: Split r (Record [f :: a]) r' => r -> a
    EFieldProj r e fn -> newV $ \a r' -> do
        te@(getTag -> (m, i, r)) <- infer penv e
        ty <- simplifyTyping penv (m, (mempty, [Split r (TRecord $ Map.singleton fn a) r']) <> i, a)
        return $ EFieldProj ty te fn
    ERecord r (unzip -> (fs, es)) -> do
        trs@(unzip3 . map getTag -> (ml, il, tl)) <- mapM (infer penv) es
        ty <- snd <$> unif penv [] ml il (TRecord $ Map.fromList {-TODO: check-} $ zip fs tl)
        return $ ERecord ty $ zip fs trs
    ETuple r t -> do
        te@(unzip3 . map getTag -> (ml, il, tl)) <- mapM (infer penv) t
        ETuple <$> (snd <$> unif penv [] ml il (TTuple C tl)) <*> pure te
    ELit r l -> ELit <$> inferLit l <*> pure l
    EVar r n ->
        inferPrimFun
            (\x -> EVar <$> simplifyTyping penv x <*> pure ("prim:" ++ n))
            (maybe (throwErrorUnique $ "Variable " ++ n ++ " is not in scope.") instTyping $ Map.lookup n penv)
            n
      where
        instTyping ty@(_, _, freeVars -> fv) = do
            newVars <- replicateM (Set.size fv) (newVar C)
            let s = Map.fromList $ zip (Set.toList fv) newVars
            return $ ESubst s $ EVar (subst s ty) n
    ELam r n f
        | Map.member n penv -> throwErrorUnique $ "Variable name clash: " ++ n
        | otherwise -> newV $ \t -> do
            tf@(getTag -> (m, i, t)) <- infer (Map.insert n (Map.singleton n t, mempty, t) penv) f
            a <- maybe (newVar C) return $ Map.lookup n m
            let ty = (Map.delete n m, i, a ~> t)
            unamb penv ty
            return $ ELam ty n tf
    EApp r f a -> newV $ \v -> do
        tf@(getTag -> (m1, i1, t1)) <- infer penv f
        ta@(getTag -> (m2, i2, t2)) <- infer penv a
        (s, ty) <- unif penv [t1, t2 ~> v] [m1, m2] [i1, i2] v
        return $ ESubst s $ EApp ty tf ta
    ELet r n x e
        | Map.member n penv -> throwErrorUnique $ "Variable name clash: " ++ n
        | otherwise -> do
            tx@(getTag -> ty) <- infer penv x
            te@(getTag -> ty') <- infer (Map.insert n ty penv) e
            return $ ELet ty' n tx te

