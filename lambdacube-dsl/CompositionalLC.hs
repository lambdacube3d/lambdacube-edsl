{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module CompositionalLC
    ( inference, inference_
    , composeSubst
    , subst
    , freeVars
    , polyVars
    , instantiateTyping
    , typing
    ) where

import Data.Function
import Data.List
import Data.Maybe
import Data.Foldable (Foldable, foldMap, toList)
import qualified Data.Traversable as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.Writer
import Control.Arrow

import Type
import Typing


class Substitute a where subst :: Subst -> a -> a

instance Substitute Ty where
    subst st ty | Map.null st = ty -- optimization
    subst st (Ty_ k tv@(TVar_ a)) = fromMaybe (Ty_ (subst st k) tv) $ Map.lookup a st
    subst st (Ty_ k t) = Ty_ (subst st k) $ subst st <$> t
    subst _ (StarToStar f n) = StarToStar f n

instance Substitute a => Substitute [a]                 where subst = fmap . subst
instance Substitute a => Substitute (Typing_ a)         where subst = fmap . subst
instance Substitute a => Substitute (MonoEnv a)         where subst = fmap . subst
instance Substitute a => Substitute (Constraint a)      where subst = fmap . subst


-- Note: domain of substitutions is disjunct
composeSubst :: Subst -> Subst -> Subst
s1 `composeSubst` s2 = s2 <> (subst s2 <$> s1)

-- unify each types in the sublists
unifyTypes :: Bool -> [[Ty]] -> TCM Subst
unifyTypes bidirectional xss = flip execStateT mempty $ forM_ xss $ \xs -> sequence_ $ zipWith uni xs $ tail xs
  where
    uni :: Ty -> Ty -> StateT Subst TCM ()
    uni a b = gets subst1 >>= \f -> unifyTy (f a) (f b)
      where
        subst1 s tv@(TVar a) = fromMaybe tv $ Map.lookup a s
        subst1 _ t = t

        singSubst n t (TVar a) | a == n = t
        singSubst n t (Ty_ k ty) = Ty_ (singSubst n t k) $ singSubst n t <$> ty
        singSubst _ _ (StarToStar f n) = StarToStar f n

        -- make single tvar substitution; check infinite types
        bindVar n t = do
            s <- get
            let t' = subst s t
            if n `Set.member` freeVars t
                then lift $ throwErrorTCM $ "Infinite type, type variable " ++ n ++ " occurs in " ++ show t
                else put $ Map.insert n t' $ singSubst n t' <$> s

        unifyTy :: Ty -> Ty -> StateT Subst TCM ()
        unifyTy Star Star = return ()
        unifyTy (TVar u) (TVar v) | u == v = return ()
        unifyTy (TVar u) _ = bindVar u b
        unifyTy _ (TVar u) | bidirectional = bindVar u a
        unifyTy (TCon0 u) (TCon0 v) | u == v = return ()
        unifyTy (TTuple t1) (TTuple t2) = sequence_ $ zipWith uni t1 t2
        unifyTy (TApp k1 a1 b1) (TApp k2 a2 b2) = uni a1 a2 >> uni b1 b2
        unifyTy (TArr a1 b1) (TArr a2 b2) = uni a1 a2 >> uni b1 b2
        unifyTy (TVec a1 b1) (TVec a2 b2) | a1 == a2 = uni b1 b2
        unifyTy (TMat a1 b1 c1) (TMat a2 b2 c2) | a1 == a2 && b1 == b2 = uni c1 c2
        unifyTy a b
          | a == b = return ()
          | otherwise = lift $ throwErrorTCM $ "can not unify " ++ show a ++ " with " ++ show b

unifyTypings = unifyTypings_ True

unifyTypings_
    :: NewVar a
    => Bool         -- bidirectional unification
    -> [[Typing]]   -- unify each group
    -> ([Ty] -> a)  -- main typing types for each unified group -> result typing
    -> TCM (Subst, Typing)
unifyTypings_ bidirectional ts f = do
    (s', t) <- newV $ f $ map (typingType . head) ts
    let ms = map monoEnv $ t: concat ts
    s <- unifyTypes bidirectional $ (map . map) typingType ts ++ unifyMaps ms
    (s, i) <- untilNoUnif s $ nub $ subst s $ concatMap constraints $ t: concat ts
    let ty = typing (Map.unions $ subst s ms) i (subst s $ typingType t)
    ambiguityCheck ty
    return (s <> s', ty)
  where
    groupByFst :: Ord a => [(a, b)] -> [[b]]
    groupByFst = unifyMaps . map (uncurry Map.singleton)

    untilNoUnif acc es = do
        (es, w) <- runWriterT $ do
            -- unify left hand sides where the right hand side is equal:  (t1 ~ F a, t2 ~ F a)  -->  t1 ~ t2
            tell $ groupByFst [(f, ty) | CEq ty f <- es]
            -- injectivity test:  (t ~ Vec a1 b1, t ~ Vec a2 b2)  -->  a1 ~ a2, b1 ~ b2
            tell $ concatMap (concatMap transpose . groupByFst) $ groupByFst [(ty, (it, is)) | CEq ty (injType -> Just (it, is)) <- es]
            concat <$> mapM reduceConstraint es
        s <- unifyTypes True w
        if Map.null s then return (acc, es) else untilNoUnif (acc `composeSubst` s) $ nub $ subst s es

-- Ambiguous: (Int ~ F a) => Int
-- Not ambiguous: (Show a, a ~ F b) => b
ambiguityCheck :: Typing -> TCM ()
ambiguityCheck ty = do
    e <- errorTCM
    let c = if used `Set.isSubsetOf` defined then Nothing else Just $ e <> \_ -> unlines
            ["ambiguous type: " ++ show ty, "defined vars: " ++ show defined, "used vars: " ++ show used]
    modify $ (c:) *** id
  where
    used = freeVars $ constraints ty
    defined = dependentVars (constraints ty) $ freeVars (monoEnv ty) <> freeVars (typingType ty)

-- complex example:
--      forall b y {-monomorph vars-} . (b ~ F y) => b ->      -- monoenv & monomorph part of instenv
--      forall a x {-polymorph vars-} . (Num a, a ~ F x) => a  -- type & polymorph part of instenv
instantiateTyping :: Typing -> TCM (Subst, Typing)
instantiateTyping ty = do
    let fv = polyVars ty
    newVars <- replicateM (Set.size fv) (newVar C)
    let s = Map.fromDistinctAscList $ zip (Set.toList fv) newVars
    return (s, subst s ty)

--calcPolyVars :: Typing -> Set TName
typing me cs ty = Typing me cs ty $
    dependentVars cs (freeVars ty) Set.\\ freeVars me  -- TODO: make it more precise if necessary

-- compute dependent type vars in constraints
-- Example:  dependentVars [(a, b) ~ F b c, d ~ F e] [c] == [a,b,c]
dependentVars :: [Constraint Ty] -> Set TName -> Set TName
dependentVars ie s = cycle mempty s
  where
    cycle acc s
        | Set.null s = acc
        | otherwise = cycle (acc <> s) (grow s Set.\\ acc)

    grow = flip foldMap ie $ \case
        CEq ty f -> freeVars ty <-> freeVars f
        Split a b c -> freeVars a <-> (freeVars b <> freeVars c)
        CUnify{} -> error "dependentVars: impossible" 
        CClass{} -> mempty
      where
        a --> b = \s -> if Set.null $ a `Set.intersection` s then mempty else b
        a <-> b = (a --> b) <> (b --> a)


inference :: Module Range -> Either ErrorMsg (Module (Subst, Typing))
inference = inference_ $ PolyEnv primFunMap

inference_ :: PolyEnv -> Module Range -> Either ErrorMsg (Module (Subst, Typing))
inference_ primFunMap m = runExcept $ fst <$>
    evalRWST (inferModule m) (primFunMap, mempty) (mempty, ['t':show i | i <- [0..]])
  where
    inferModule Module{..} = do
        dataDefs <- return [] -- TODO mapM inferDataDef dataDefs
        definitions <- mapM inferDef definitions
        return Module{..}

    inferDef (n, e) = do
        e <- inferTyping e <* checkUnambError
        return (n, e)

removeMonoVars vs (Typing me cs t pvs) = typing (foldr Map.delete me $ Set.toList vs) cs t

inferKind :: Ty' Range -> TCM (Ty' (Subst, Typing))
inferKind (Ty' r ty) = local (id *** const [r]) $ case ty of
    Forall_ n t -> do
        let n' = '\'':n
        tf <- withTyping (Map.singleton n' star) $ inferKind t
        ty <- unifyTypings [[star], getTag' tf] $ \[a, t] -> a ~> t
        return $ Ty' (id *** removeMonoVars (Set.singleton n) $ ty) $ Forall_ n tf
    _ -> do
        ty' <- T.mapM inferKind ty
        (\t -> Ty' t ty') <$> case ty' of
            TNat_ _ -> return (mempty, [] ==> NatKind)
            Star_ C -> return (mempty, [] ==> Star)
            TVec_ _ b -> unifyTypings [star: getTag' b] $ \_ -> Star
            TMat_ _ _ b -> unifyTypings [star: getTag' b] $ \_ -> Star
            TArr_ a b -> unifyTypings [star: getTag' a, star: getTag' b] $ \_ -> Star
            TApp_ tf ta -> unifyTypings [getTag' tf, getTag' ta] $ \[tf, ta] v -> [tf ~~~ ta ~> v] ==> v
            TVar_ n -> asks (getPolyEnv . fst) >>= fromMaybe (throwErrorTCM $ "Variable " ++ n ++ " is not in scope.") . Map.lookup ('\'':n)
  where
    getTag' = (:[]) . snd . getTagT
    star = [] ==> Star

inferTyping :: Exp Range -> TCM (Exp (Subst, Typing))
inferTyping (Exp r e) = local (id *** const [r]) $ case e of
    ELam_ p f -> do
        p_@(p, tr) <- inferPatTyping False p
        tf <- withTyping tr $ inferTyping f
        ty <- unifyTypings [getTagP' p_, getTag' tf] $ \[a, t] -> a ~> t
        return $ ELam (id *** removeMonoVars (Map.keysSet tr) $ ty) p tf
    ELet_ (PVar _ n) x e -> do
        tx <- inferTyping x
        te <- withTyping (Map.singleton n $ head $ getTag' tx) $ inferTyping e
        return $ ELet (mempty, head $ getTag' te) (PVar (getTag tx) n) tx te
    ELet_ p x e -> do          -- monomorph let; TODO?
        tx <- inferTyping x
        p_@(p, tr) <- inferPatTyping False p
        te <- withTyping tr $ inferTyping e
        ty <- unifyTypings [getTagP' p_ ++ getTag' tx, getTag' te] $ \[_, te] -> te
        return $ ELet ty p tx te
    ECase_ e cs -> do
        te <- inferTyping e
        cs <- forM cs $ \(p, exp) -> do
            (p, tr) <- inferPatTyping False p
            exp <- withTyping tr $ inferTyping exp
            return (p, exp)
        ty <- unifyTypings [getTag' te ++ concatMap getTagP' cs, concatMap (getTag' . snd) cs] $ \[_, x] -> x
        return $ ECase ty te cs
    _ -> do
        e' <- T.mapM inferTyping e
        (\t -> Exp t $ setTag undefined e') <$> case e' of
            EApp_ tf ta -> unifyTypings [getTag' tf, getTag' ta] $ \[tf, ta] v -> [tf ~~~ ta ~> v] ==> v
            EFieldProj_ fn -> fieldProjType fn
            ERecord_ (unzip -> (fs, es)) -> unifyTypings (map getTag' es) $ TRecord . Map.fromList . zip fs
            ETuple_ te -> unifyTypings (map (getTag') te) TTuple
            ELit_ l -> noSubst $ inferLit l
            EVar_ n -> asks (getPolyEnv . fst) >>= fromMaybe (throwErrorTCM $ "Variable " ++ n ++ " is not in scope.") . Map.lookup n
            ETyping_ e ty -> unifyTypings_ False [getTag' e ++ [ty]] $ \[ty] -> ty
  where
    inferPatTyping :: Bool -> Pat Range -> TCM (Pat (Subst, Typing), Map EName Typing)
    inferPatTyping polymorph p_@(Pat pt p) = local (id *** const [pt]) $ do
        p' <- T.mapM (inferPatTyping polymorph) p
        (t, tr) <- case p' of
            PLit_ n -> noTr $ noSubst $ inferLit n
            Wildcard_ -> noTr $ newV $ \t -> t :: Ty
            PVar_ n -> addTr (\t -> Map.singleton n (snd t)) $ newV $ \t ->
                if polymorph then [] ==> t else Typing (Map.singleton n t) mempty t mempty :: Typing
            PTuple_ ps -> noTr $ unifyTypings (map getTagP' ps) TTuple
            PCon_ n ps -> noTr $ do
                (_, tn) <- asks (getPolyEnv . fst) >>= fromMaybe (throwErrorTCM $ "Constructor " ++ n ++ " is not in scope.") . Map.lookup n
                unifyTypings ([tn]: map getTagP' ps) (\(tn: tl) v -> [tn ~~~ tl ~~> v] ==> v)
            PRecord_ (unzip -> (fs, ps)) -> noTr $ unifyTypings (map getTagP' ps)
                (\tl v v' -> [Split v v' $ TRecord $ Map.fromList $ zip fs tl] ==> v)
        let trs = Map.unionsWith (++) . map ((:[]) <$>) $ tr: map snd (toList p')
        tr <- case filter ((>1) . length . snd) $ Map.toList trs of
            [] -> return $ Map.map head trs
            ns -> throwErrorTCM $ "conflicting definitions for " ++ show (map fst ns)
        return (Pat t $ fst <$> p', tr)

    getTag' = (:[]) . snd . getTag
    getTagP' = (:[]) . snd . getTagP . fst
    noSubst = fmap ((,) mempty)
    noTr = addTr $ const mempty
    addTr tr m = (\x -> (x, tr x)) <$> m

withTyping :: Map EName Typing -> TCM a -> TCM a
withTyping ts m = do
    penv <- asks $ getPolyEnv . fst
    case toList $ Map.keysSet ts `Set.intersection` Map.keysSet penv of
        [] -> local ((<> PolyEnv (instantiateTyping <$> ts)) *** id) m
        ks -> throwErrorTCM $ "Variable name clash: " ++ show ks

