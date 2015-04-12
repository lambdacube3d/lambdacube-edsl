{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module CompositionalLC
    ( inference
    , composeSubst
    , subst
    ) where

import Data.Function
import Data.List
import Data.Maybe
import Data.Foldable (foldMap)
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


class FreeVars a where freeVars :: a -> Set TName

instance FreeVars Ty where
    freeVars (TVar _ a) = Set.singleton a
    freeVars (Ty x) = foldMap freeVars x

instance FreeVars a => FreeVars [a]                 where freeVars = foldMap freeVars
instance FreeVars a => FreeVars (Typing_ a)         where freeVars = foldMap freeVars
instance FreeVars a => FreeVars (TypeFun a)         where freeVars = foldMap freeVars
instance FreeVars a => FreeVars (MonoEnv a)         where freeVars = foldMap freeVars
instance FreeVars a => FreeVars (Constraint a)      where freeVars = foldMap freeVars


class Substitute a where subst :: Subst -> a -> a

instance Substitute Ty where
    subst st ty | Map.null st = ty -- optimization
    subst st tv@(TVar _ a) = fromMaybe tv $ Map.lookup a st
    subst st (Ty t) = Ty $ subst st <$> t

instance Substitute a => Substitute [a]                 where subst = fmap . subst
instance Substitute a => Substitute (Typing_ a)         where subst = fmap . subst
instance Substitute a => Substitute (MonoEnv a)         where subst = fmap . subst
instance Substitute a => Substitute (Constraint a)      where subst = fmap . subst


-- composeSubst substitutions
-- Note: domain of substitutions is disjunct
composeSubst :: Subst -> Subst -> Subst
s1 `composeSubst` s2 = s2 <> (subst s2 <$> s1)

-- unify each types in the sublists
unifyTypes :: [[Ty]] -> TCM Subst
unifyTypes xss = flip execStateT mempty $ forM_ xss $ \xs -> sequence_ $ zipWith uni xs $ tail xs
  where
    uni :: Ty -> Ty -> StateT Subst TCM ()
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
                then lift $ throwErrorTCM $ "Infinite type, type variable " ++ n ++ " occurs in " ++ show t
                else put $ Map.insert n t' $ singSubst n t' <$> s

        unifyTy :: Ty -> Ty -> StateT Subst TCM ()
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
          | otherwise = lift $ throwErrorTCM $ "can not unify " ++ show a ++ " with " ++ show b


unifyTypings
    :: [Typing]
    -> ([Ty] -> ([Ty], Ty))   -- main typing types -> (extra unification, result typing type)
    -> TCM (Subst, Typing)
unifyTypings ts f = do
    let (b, t) = f $ map typingType ts
        ms = map monoEnv ts
    s <- unifyTypes $ b: unifyMaps ms
    (s, i) <- untilNoUnif s $ nub $ subst s $ concatMap constraints ts
    let ty = Typing (Map.unions $ subst s ms) i (subst s t)
    ambiguityCheck ty
    return (s, ty)
  where
    groupByFst :: Ord a => [(a, b)] -> [[b]]
    groupByFst = unifyMaps . map (uncurry Map.singleton)

    unifyMaps :: Ord a => [Map a b] -> [[b]]
    unifyMaps = Map.elems . Map.unionsWith (++) . map ((:[]) <$>)

    untilNoUnif acc es = do
        (es, w) <- runWriterT $ do
            -- unify left hand sides where the right hand side is equal:  (t1 ~ F a, t2 ~ F a)  -->  t1 ~ t2
            tell $ groupByFst [(f, ty) | CEq ty f <- es]
            -- injectivity test:  (t ~ Vec a1 b1, t ~ Vec a2 b2)  -->  a1 ~ a2, b1 ~ b2
            tell $ concatMap (concatMap transpose . groupByFst) $ groupByFst [(ty, (it, is)) | CEq ty (injType -> Just (it, is)) <- es]
            concat <$> mapM reduceConstraint es
        s <- unifyTypes w
        if Map.null s then return (acc, es) else untilNoUnif (acc `composeSubst` s) $ nub $ subst s es

    reduceConstraint x = case x of
        Split (TRecord a) (TRecord b) (TRecord c) ->
          case (Map.keys $ Map.intersection b c, Map.keys $ a Map.\\ (b <> c), Map.keys $ (b <> c) Map.\\ a) of
            ([], [], []) -> discard $ unifyMaps [a, b, c]
            ks -> fail $ "extra keys: " ++ show ks
        Split (TRecord a) (TRecord b) c@TVar{} -> diff a b c
        Split (TRecord a) c@TVar{} (TRecord b) -> diff a b c
        Split c@TVar{} (TRecord a) (TRecord b) -> case Map.keys $ Map.intersection a b of
            [] -> discard [[c, TRecord $ a <> b]]
            ks -> fail $ "extra keys: " ++ show ks
        Split a b c
            | isRec a && isRec b && isRec c -> keep
            | otherwise -> fail $ "bad split: " ++ show x
        CClass c t -> isInstance (\c t -> return [CClass c t]) fail keep (discard []) c t
        CEq ty f -> reduceTF
                (\t -> discard [[ty, t]])
                (fail . (("error during reduction of " ++ show f ++ "  ") ++))
                (tell [[t1, t2] | (t1, t2) <- backPropTF ty f] >> keep)
                f
      where
        isRec TVar{}    = True
        isRec TRecord{} = True
        isRec _ = False

        diff a b c = case Map.keys $ b Map.\\ a of
            [] -> discard $ [c, TRecord $ a Map.\\ b]: unifyMaps [a, b]
            ks -> fail $ "extra keys: " ++ show ks

        discard xs = tell xs >> return []
        keep = return [x]
        fail = lift . throwErrorTCM

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
        _ -> mempty
      where
        a --> b = \s -> if Set.null $ a `Set.intersection` s then mempty else b
        a <-> b = (a --> b) <> (b --> a)


inference :: Exp Range -> Either ErrorMsg (Exp Typing)
inference e = runExcept $ fst <$> evalRWST (inferTyping e <* checkUnambError) mempty (mempty, 0)

inferTyping :: Exp Range -> TCM (Exp Typing)
inferTyping exp = local (id *** const [getTag exp]) $ addSubst <$> case exp of
    ELam _ n f -> do
        tv <- newV $ \t -> return $ Typing (Map.singleton n t) mempty t :: TCM Typing
        tf <- insert' n tv $ inferTyping f
        (s, ty) <- unifyTypings [tv, getTag tf] $ \[a, t] -> ([], a ~> t)
        return (s, ELam ty n tf)
    ELet _ n x e -> do
        tx <- inferTyping x
        te <- insert' n (getTag tx) $ inferTyping e
        return (mempty, ELet (getTag te) n tx te)
    Exp e -> do
        e' <- T.mapM inferTyping e
        (s, t) <- case e' of
            EApp_ _ tf ta -> newV $ \v -> unifyTypings [getTag tf, getTag ta] (\[tf, ta] -> ([tf, ta ~> v], v))
            EFieldProj_ _ fn -> (,) mempty <$> fieldProjType fn
            ERecord_ _ trs -> unifyTypings (map (getTag . snd) trs) $ \tl -> ([], TRecord $ Map.fromList $ zip (map fst trs) tl)
            ETuple_ _ te -> unifyTypings (map getTag te) (\tl -> ([], TTuple C tl))
            ELit_ _ l -> (,) mempty <$> inferLit l
            EVar_ _ n -> inferPrimFun
                (\x -> unifyTypings [x] (\[x] -> ([], x)))
                (asks fst >>= maybe (throwErrorTCM $ "Variable " ++ n ++ " is not in scope.") instTyping . Map.lookup n)
                n
        return (s, Exp $ setTag t e')
  where
    -- complex example:
    --      forall b y {-monomorph vars-} . (b ~ F y) => b ->      -- monoenv & monomorph part of instenv
    --      forall a x {-polymorph vars-} . (Num a, a ~ F x) => a  -- type & polymorph part of instenv
    instTyping ty = do
        let fv = dependentVars (constraints ty) $ freeVars (typingType ty)  -- TODO: make it more precise if necessary
        newVars <- replicateM (Set.size fv) (newVar C)
        let s = Map.fromDistinctAscList $ zip (Set.toList fv) newVars
        return (s, subst s ty)

    fieldProjType fn = newV $ \a r r' -> return $ [Split r r' (TRecord $ Map.singleton fn a)] ==> r ~> a :: TCM Typing

    insert' n t m = do
        penv <- asks fst
        if Map.member n penv || Set.member n primFunSet
            then throwErrorTCM $ "Variable name clash: " ++ n
            else local (Map.insert n t *** id) m

    addSubst (s, t@EApp{}) = ESubst (getTag t) s t
    addSubst (s, t@EVar{}) = ESubst (getTag t) s t
    addSubst (_, t) = t
