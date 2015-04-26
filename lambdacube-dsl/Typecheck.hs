{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-} -- for ghc-7.10.1
module Typecheck where

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
import Text.Show.Pretty

import Type
import Typing
import Parser (ModuleR)

class Substitute a where subst :: Subst -> a -> a

instance Substitute Ty where
    subst st ty | Map.null st = ty -- optimization
    subst st (Ty_ k tv@(TVar_ a)) = fromMaybe (Ty_ (subst st k) tv) $ Map.lookup a st
    subst st (Ty_ k t) = Ty_ (subst st k) $ subst st <$> t
    subst _ (StarToStar n) = StarToStar n

instance Substitute a => Substitute [a]                 where subst = fmap . subst
instance Substitute a => Substitute (Typing_ a)         where subst = fmap . subst
instance Substitute a => Substitute (MonoEnv a)         where subst = fmap . subst
instance Substitute a => Substitute (Constraint a)      where subst = fmap . subst


-- Note: domain of substitutions is disjunct
composeSubst :: Subst -> Subst -> Subst
s1 `composeSubst` s2 = s2 <> (subst s2 <$> s1)

subst1 :: Subst -> Ty -> Ty
subst1 s tv@(TVar a) = fromMaybe tv $ Map.lookup a s
subst1 _ t = t

-- unify each types in the sublists
unifyTypes :: Bool -> [[Ty]] -> TCM Subst
unifyTypes = unifyTypes_ throwErrorTCM

unifyTypes_ :: (Monad m, Foldable t) => (String -> m ()) -> Bool -> t [Ty] -> m Subst
unifyTypes_ fail bidirectional xss = flip execStateT mempty $ forM_ xss $ \xs -> sequence_ $ zipWith uni xs $ tail xs
  where
--    uni :: Ty -> Ty -> StateT Subst TCM ()
    uni a b = gets subst1 >>= \f -> unifyTy (f a) (f b)
      where
        singSubst n t (TVar a) | a == n = t
        singSubst n t (Ty_ k ty) = Ty_ (singSubst n t k) $ singSubst n t <$> ty
        singSubst _ _ (StarToStar n) = StarToStar n

        -- make single tvar substitution; check infinite types
        bindVar n t = do
            s <- get
            let t' = subst s t
            if n `Set.member` freeVars t
                then lift $ fail $ "Infinite type, type variable " ++ n ++ " occurs in " ++ show t
                else put $ Map.insert n t' $ singSubst n t' <$> s

--        unifyTy :: Ty -> Ty -> StateT Subst TCM ()

        -- TODO: generalize this or normalize kinds
        unifyTy (StarToStar 0) Star = return ()
        unifyTy (StarToStar 1) (TArr a b) = uni Star a >> uni Star b
        unifyTy (TArr Star Star) StarStar = return ()

        unifyTy Star Star = return ()
        unifyTy (TVar u) (TVar v) | u == v = return ()
        unifyTy (TVar u) _ = bindVar u b
        unifyTy _ (TVar u) | bidirectional = bindVar u a
        unifyTy (TCon k u) (TCon k' v) | u == v = uni k k' --return ()
        unifyTy (TTuple t1) (TTuple t2) = sequence_ $ zipWith uni t1 t2
        unifyTy (TApp k1 a1 b1) (TApp k2 a2 b2) = uni a1 a2 >> uni b1 b2
        unifyTy (TArr a1 b1) (TArr a2 b2) = uni a1 a2 >> uni b1 b2
        unifyTy (TVec a1 b1) (TVec a2 b2) | a1 == a2 = uni b1 b2
        unifyTy (TMat a1 b1 c1) (TMat a2 b2 c2) | a1 == a2 && b1 == b2 = uni c1 c2
        unifyTy a b
          | a == b = return ()
          | otherwise = lift $ fail $ "cannot unify " ++ ppShow a ++ "\n with " ++ ppShow b

unifyTypings = unifyTypings_ True

unifyTypings_
    :: NewVar a
    => Bool         -- bidirectional unification
    -> [[Typing]]   -- unify each group
    -> ([Ty] -> a)  -- main typing types for each unified group -> result typing
    -> TCM STyping
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
    let c = if all ok $ constraints ty then Nothing else Just $ e <> \_ -> unlines
            ["ambiguous type: " ++ show ty, "defined vars: " ++ show defined] --, "used vars: " ++ show used]
    modify $ (c:) *** id
  where
    ok c = not (Set.null used) && used `Set.isSubsetOf` defined
      where
        used = freeVars c
    defined = dependentVars (constraints ty) $ freeVars (monoEnv ty) <> freeVars (typingType ty)

-- complex example:
--      forall b y {-monomorph vars-} . (b ~ F y) => b ->      -- monoenv & monomorph part of instenv
--      forall a x {-polymorph vars-} . (Num a, a ~ F x) => a  -- type & polymorph part of instenv
instantiateTyping :: Typing -> TCM STyping
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

type ModuleT = Module Typing STyping

inference :: ModuleR -> Either ErrorMsg ModuleT
inference = inference_ $ PolyEnv primFunMap

primed = ('\'':)

inference_ :: PolyEnv -> ModuleR -> Either ErrorMsg ModuleT
inference_ primFunMap m = runExcept $ fst <$>
    evalRWST (inferModule m) (primFunMap, mempty) (mempty, ['t':show i | i <- [0..]])
  where
    inferModule Module{..} = withTyping (Map.fromList $ tyConKinds dataDefs) $ do
        let d = dataDefs
        axioms <- forM axioms $ \(n, a) -> do
            a' <- inferKind a
            return (n, a')
        dataDefs <- mapM inferDataDef dataDefs
        definitions <- withTyping (Map.fromList $ tyConTypes dataDefs) $ inferDefs $ selectorDefs d ++ definitions
        return Module{..}

    inferDefs [] = return []
    inferDefs (d:ds) = do
        (d, f) <- inferDef d
        ds <- f $ inferDefs ds
        return (d: ds)

    inferDataDef (DataDef con vars cdefs) = do
        cdefs <- withTyping (Map.fromList [(primed v, [] ==> Star) | v <- vars]) $ mapM inferConDef cdefs
        return $ DataDef con vars cdefs

    inferConDef (ConDef n tys) = do
        tys <- mapM inferFieldKind tys
        return $ ConDef n tys

    inferFieldKind (FieldTy mn t) = do
        t <- inferKind' t
        return $ FieldTy mn t

    inferDef (p@(PVar _ n), e) = do
        (p, tr) <- inferPatTyping False p
        (Exp (s'', te) exp) <- withTyping tr $ inferTyping e
        (s, t) <- unifyTypings [[snd $ getTag p, te]] $ \[t] -> t
        let e = Exp (s <> s'', removeMonoVars (Set.singleton n) t) exp
        let f = withTyping $ Map.singleton n $ snd . getTag $ e
        return ((PVar (getTag e) n, replCallType n (getTag e) e), f)

-- TODO
replCallType n nt = \case
    EVar _ n' | n == n' -> EVar nt n
    Exp t e -> Exp t $ f <$> e
--    x -> error $ "replCallType: " ++ ppShow x
  where f = replCallType n nt

modTag f (Exp t x) = Exp (f t) x

selectorDefs :: [DataDef (Ty' Range)] -> [ValueDef Range]
selectorDefs dataDefs =
    [ ( PVar mempty sel
      , ELam mempty
            (PCon mempty cn
                [ if sel == sel' then PVar mempty "x" else Wildcard mempty
                | FieldTy (Just sel') _ <- tys]
            )
            (EVar mempty "x")
      )
    | d@(DataDef n vs cs) <- dataDefs
    , ConDef cn tys <- cs
    , FieldTy (Just sel) t <- tys
    ]

selectorTypes :: [DataDef Typing] -> [(EName, Typing)]
selectorTypes dataDefs =
    [ (sel, tyConResTy d .~> t)
    | d@(DataDef n vs cs) <- dataDefs
    , ConDef cn tys <- cs
    , FieldTy (Just sel) t <- tys
    ]

tyConResTy :: DataDef a -> Typing
tyConResTy (DataDef n vs _)
    = [] ==> foldl app (Ty_ k $ TCon_ n) (zip [i-1,i-2..] $ map (Ty_ Star . TVar_) vs)
  where
    app x (i, y) = TApp (StarToStar i) x y
    k = StarToStar i
    i = length vs

tyConTypes :: [DataDef Typing] -> [(EName, Typing)]
tyConTypes dataDefs =
    [ (cn, foldr (.~>) (tyConResTy d) $ map fieldType tys)
    | d@(DataDef n vs cs) <- dataDefs
    , ConDef cn tys <- cs
    ]

tyConKinds dataDefs = [(primed n, [] ==> foldr (~>) Star (replicate (length vs) Star)) | DataDef n vs _ <- dataDefs]

exportEnv :: ModuleT -> PolyEnv
exportEnv Module{..}
    = PolyEnv $ fmap instantiateTyping $ Map.fromList $
            [(n, snd $ getTag e) | (PVar _ n, e) <- definitions]
        ++  tyConKinds dataDefs
        ++  tyConTypes dataDefs
        ++  selectorTypes dataDefs
        ++  map (id *** convTy) axioms

joinPolyEnvs ps = case filter (not . isSing . snd) $ Map.toList ms of
    [] -> Right $ PolyEnv $ head <$> ms
    xss -> Left $ "Definition clash: " ++ show (map fst xss)
  where
    isSing [_] = True
    isSing _ = False
    ms = Map.unionsWith (++) [(:[]) <$> e | PolyEnv e <- ps]

removeMonoVars vs (Typing me cs t pvs) = typing (foldr Map.delete me $ Set.toList vs) cs t

-- TODO: unification
(.~>) :: Typing -> Typing -> Typing
t .~> s = typing (monoEnv t <> monoEnv s) (constraints t ++ constraints s) (typingType t ~> typingType s)

convTy :: Ty' STyping -> Typing
convTy = f mempty where
    f sub (Ty' (s, k) (TConstraintArg_ c t)) = typing (monoEnv t') ((typingType {-TODO-} . convTy <$> c): constraints t') $ typingType t'
      where
        sub' = s `composeSubst` sub
        t' = f sub' t
    f sub (Ty' (s, k) t) = typing me cs $ subst1 sub{-TODO: move innerwards-} $ ty_ (subst sub $ typingType k) (typingType <$> t')
      where
        sub' = s `composeSubst` sub
        t' = f sub' <$> t
        me = subst sub (monoEnv k) <> foldMap monoEnv t'
        cs = subst sub (constraints k) <> foldMap constraints t'

inferKind' :: Ty' Range -> TCM Typing
inferKind' t = convTy <$> inferKind t

inferKind :: Ty' Range -> TCM (Ty' STyping)
inferKind (Ty' r ty) = local (id *** const [r]) $ case ty of
    Forall_ n t -> do
        let n' = '\'':n
        tf <- withTyping (Map.singleton n' star) $ inferKind t
        ty <- unifyTypings [[star], getTag' tf] $ \[a, t] -> a ~> t
        return $ Ty' (id *** removeMonoVars (Set.singleton n) $ ty) $ Forall_ n tf
    _ -> do
        ty' <- T.mapM inferKind ty
        (\t -> Ty' t ty') <$> case ty' of
            TConstraintArg_ c t -> return (mempty, error "tcarg")
            TNat_ _ -> return (mempty, [] ==> NatKind)
            Star_ C -> return (mempty, [] ==> Star)
            TTuple_ ts -> unifyTypings (map ((star:) . getTag') ts) $ \_ -> Star
--            TMat_ _ _ b -> unifyTypings [star: getTag' b] $ \_ -> Star
            TArr_ a b -> unifyTypings [star: getTag' a, star: getTag' b] $ \_ -> Star
            TApp_ tf ta -> unifyTypings [getTag' tf, getTag' ta] $ \[tf, ta] v -> [tf ~~~ ta ~> v] ==> v
            TVar_ n -> asks (getPolyEnv . fst) >>= fromMaybe (addTypeVar ('\'':n)) . Map.lookup ('\'':n)
            TCon_ n -> asks (getPolyEnv . fst) >>= fromMaybe (throwErrorTCM $ "Type constructor " ++ n ++ " is not in scope.") . Map.lookup ('\'':n)
            x -> error $ " inferKind: " ++ show x
  where
    getTag' = (:[]) . snd . getTag
    star = [] ==> Star

    addTypeVar n = newV $ \t -> Typing (Map.singleton n t) mempty t mempty :: Typing

inferTyping :: Exp Range -> TCM (Exp STyping)
-- hack
inferTyping (ENamedRecord r n (unzip -> (fs, es)))
    = inferTyping $ foldl (EApp mempty) (EVar mempty n) es
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
    ETyping_ e ty -> do
        te@(Exp _ e') <- inferTyping e
        ty <- inferKind' ty
        t <- unifyTypings_ False [getTag' te ++ [ty]] $ \[ty] -> ty
        return $ Exp t e'
    ECase_ e cs -> do
        te <- inferTyping e
        cs <- forM cs $ \(p, exp) -> do
            (Pat pt p, tr) <- inferPatTyping False p
            Exp t exp <- withTyping tr $ inferTyping exp
            let del = id *** removeMonoVars (Map.keysSet tr)
            return (Pat (del pt) p, Exp (del t) exp)
        ty <- unifyTypings [getTag' te ++ concatMap getTagP' cs, concatMap (getTag' . snd) cs] $ \[_, x] -> x
        return $ ECase ty te cs
    _ -> do
        e' <- T.mapM inferTyping e
        (\t -> Exp t $ setTag (error "e1") (error "e2") e') <$> case e' of
            EApp_ tf ta -> unifyTypings [getTag' tf, getTag' ta] $ \[tf, ta] v -> [tf ~~~ ta ~> v] ==> v
            EFieldProj_ fn -> fieldProjType fn
            ERecord_ Nothing (unzip -> (fs, es)) -> unifyTypings (map getTag' es) $ TRecord . Map.fromList . zip fs
{-
            ERecord_ (Just n) (unzip -> (fs, es)) -> do -- TODO: handle field names
                (s', nt) <- asks (getPolyEnv . fst) >>= fromMaybe (throwErrorTCM $ "Variable " ++ n ++ " is not in scope.") . Map.lookup n
                (s, t) <- unifyTypings ([nt]: map getTag' es) $ \(tf: ts) v -> [tf ~~~ foldr (~>) v ts] ==> v
                return (s <> s', t)
-}
            ETuple_ te -> unifyTypings (map (getTag') te) TTuple
            ELit_ l -> noSubst $ inferLit l
            EVar_ n -> asks (getPolyEnv . fst) >>= fromMaybe (throwErrorTCM $ "Variable " ++ n ++ " is not in scope.") . Map.lookup n
            EAlts_ _ xs -> unifyTypings [concatMap getTag' xs] $ \[x] -> x
            ENext_ -> newV $ \t -> t :: Ty          -- TODO
            x -> error $ "inferTyping: " ++ ppShow x
  where
    getTag' = (:[]) . snd . getTag
    getTagP' = (:[]) . snd . getTag . fst
    noSubst = fmap ((,) mempty)

inferPatTyping :: Bool -> Pat Range -> TCM (Pat STyping, Map EName Typing)
inferPatTyping polymorph p_@(Pat pt p) = local (id *** const [pt]) $ do
    p' <- T.mapM (inferPatTyping polymorph) p
    (t, tr) <- case p' of
        PLit_ n -> noTr $ noSubst $ inferLit n
        Wildcard_ -> noTr $ newV $ \t -> t :: Ty
        PVar_ n -> addTr (\t -> Map.singleton n (snd t)) $ newV $ \t ->
            if polymorph then [] ==> t else Typing (Map.singleton n t) mempty t mempty :: Typing
        PAt_ n p -> addTr (\t -> Map.singleton n (snd t)) $ newV $ snd . getTag . fst $ p
        PTuple_ ps -> noTr $ unifyTypings (map getTagP' ps) TTuple
        PCon_ n ps -> noTr $ do
            (_, tn) <- asks (getPolyEnv . fst) >>= fromMaybe (throwErrorTCM $ "Constructor " ++ n ++ " is not in scope.") . Map.lookup n
            unifyTypings ([tn]: map getTagP' ps) (\(tn: tl) v -> [tn ~~~ tl ~~> v] ==> v)
        PRecord_ (unzip -> (fs, ps)) -> noTr $ unifyTypings (map getTagP' ps)
            (\tl v v' -> [Split v v' $ TRecord $ Map.fromList $ zip fs tl] ==> v)
--            x -> error $ "inferPatTyping: " ++ ppShow x
    let trs = Map.unionsWith (++) . map ((:[]) <$>) $ tr: map snd (toList p')
    tr <- case filter ((>1) . length . snd) $ Map.toList trs of
        [] -> return $ Map.map head trs
        ns -> throwErrorTCM $ "conflicting definitions for " ++ show (map fst ns)
    return (Pat t $ fst <$> p', tr)
  where
    getTagP' = (:[]) . snd . getTag . fst
    noSubst = fmap ((,) mempty)
    noTr = addTr $ const mempty
    addTr tr m = (\x -> (x, tr x)) <$> m

withTyping :: Map EName Typing -> TCM a -> TCM a
withTyping ts m = do
    penv <- asks $ getPolyEnv . fst
    case toList $ Map.keysSet ts `Set.intersection` Map.keysSet penv of
        [] -> local ((<> PolyEnv (instantiateTyping <$> ts)) *** id) m
        ks -> throwErrorTCM $ "Variable name clash: " ++ show ks

