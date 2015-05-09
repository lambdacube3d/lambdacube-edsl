{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-} -- for ghc-7.10.1
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Typecheck where

import Data.Function
import Data.List
import Data.Maybe
import Data.Foldable (Foldable, foldMap, toList, foldrM)
import qualified Data.Traversable as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Arrow hiding ((<+>))
import Debug.Trace

import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Traversable as T
import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.Writer
import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Text.Parsec.Pos

import Pretty
import Type


matches TVar{} _ = True
matches x ts = x `elem'` ts

elem' a b = b a

unifyMaps :: Ord a => [Map a b] -> [[b]]
unifyMaps = Map.elems . Map.unionsWith (++) . map ((:[]) <$>)

isRec TRecord{} = True
isRec t = isVar t

isVar TVar{} = True
isVar _ = False

iff x y b = if b then x else y

{- TODO
  type family NoStencilRepr a :: *
    type instance NoStencilRepr ZZ = ZZ
    type instance NoStencilRepr (Stencil a :+: b) = NoStencilRepr b
    type instance NoStencilRepr (Color a :+: b) = Color a :+: NoStencilRepr b
    type instance NoStencilRepr (Depth a :+: b) = Depth a :+: NoStencilRepr b
-}

{- currently not used
  [injective] type family PrimitiveVertices (primitive :: PrimitiveType) a
    type instance PrimitiveVertices Point a             = a
    type instance PrimitiveVertices Line a              = (a,a)
    type instance PrimitiveVertices LineAdjacency a     = (a,a,a,a)
    type instance PrimitiveVertices Triangle a          = (a,a,a)
    type instance PrimitiveVertices TriangleAdjacency a = (a,a,a,a,a,a)
-}
{- currently not used
  - texturing -
  [semiinjective] type family TexDataRepr arity (t :: TextureSemantics *)
    type instance TexDataRepr Red  (v a) = a
    type instance TexDataRepr RG   (v a) = V2 a
    type instance TexDataRepr RGB  (v a) = V3 a
    type instance TexDataRepr RGBA (v a) = V4 a

  [injective if (= SigleTex)] type family TexArrRepr (a :: Nat) :: TextureArray
    --type instance TexArrRepr 1 = SingleTex
    --type instance TexArrRepr ((2 <= t) => t) = ArrayTex
    -- FIXME: implement properly
    type instance TexArrRepr 1 = SingleTex
    type instance TexArrRepr 2 = ArrayTex
    type instance TexArrRepr 3 = ArrayTex
    type instance TexArrRepr 4 = ArrayTex
    type instance TexArrRepr 5 = ArrayTex
    type instance TexArrRepr 6 = ArrayTex
    type instance TexArrRepr 7 = ArrayTex
    type instance TexArrRepr 8 = ArrayTex
    type instance TexArrRepr 9 = ArrayTex

  [semiinjective] type family TexSizeRepr (a :: TextureShape)
    type instance TexSizeRepr (Tex1D)   = Word32
    type instance TexSizeRepr (Tex2D)   = V2U
    type instance TexSizeRepr (TexRect) = V2U
    type instance TexSizeRepr (Tex3D)   = V3U

  [injective in 4th param, semiinjective in 3rd param] type family TexelRepr sampler
    type instance TexelRepr (Sampler dim arr (v t) Red)     = t
    type instance TexelRepr (Sampler dim arr (v t) RG)      = V2 t
    type instance TexelRepr (Sampler dim arr (v t) RGB)     = V3 t
    type instance TexelRepr (Sampler dim arr (v t) RGBA)    = V4 t
-}


-------------------------------------------------------------------------------- constraints reduction

type ConstraintSolvRes = TCM (Subst, [[Ty]])

reduceConstraint :: IdN -> ConstraintT -> ConstraintSolvRes
reduceConstraint cvar x = do
  builtinInstances <- asks $ instanceDefs
  case x of
    Split (TRecord a) (TRecord b) (TRecord c) ->
      case (Map.keys $ Map.intersection b c, Map.keys $ a Map.\\ (b <> c), Map.keys $ (b <> c) Map.\\ a) of
        ([], [], []) -> discard Refl $ unifyMaps [a, b, c]
--        ks -> failure $ "extra keys:" <+> pShow ks
    Split (TRecord a) (TRecord b) c@TVar{} -> diff a b c
    Split (TRecord a) c@TVar{} (TRecord b) -> diff a b c
    Split c@TVar{} (TRecord a) (TRecord b) -> case Map.keys $ Map.intersection a b of
        [] -> discard Refl [[c, TRecord $ a <> b]]
--        ks -> failure $ "extra keys:" <+> pShow ks
    Split a b c
        | isRec a && isRec b && isRec c -> nothing
--        | otherwise -> failure $ "bad split:" <+> pShow x

    CClass _ TVar{} -> nothing
    CClass c t -> case c of

        IsTypeLevelNatural -> case t of
            TNat{} -> discard Refl []
            _ -> noInstance

        IsValidOutput -> discard Refl [] -- TODO

        IsValidFrameBuffer -> case t of
            TTuple ts
                | any isVar ts -> nothing
                | sum [1 | Depth{} <- ts] <= 1 && sum [1 | Stencil{} <- ts] <= 1 -> discard Refl []
                | otherwise -> noInstance
            _ -> discard Refl []

        IsInputTuple -> case t of
            TTuple ts
                | any isVar ts -> nothing
                | length [() | TInput{} <- ts] == length ts -> discard Refl []
                | otherwise -> noInstance
            _ -> discard Refl []

        _ -> maybe noInstance (\s -> iff (discard Refl []) (noInstance' s) . Set.member t $ s) $ Map.lookup c builtinInstances

      where
        msg' = "no" <+> pShow c <+> "instance for" <+> pShow t
        noInstance = failure msg'
        noInstance' s = failure $ msg' </> "possible instances:" </> pShow s

    CUnify a b -> discard Refl [[a, b]]

    CEq res f -> case f of

        TFMat (TVec n t1) (TVec m t2) | t1 `elem` [TFloat] && t1 == t2 -> reduced $ TMat n m t1
        TFMat a b -> check (a `matches` floatVectors && b `matches` floatVectors) $ observe res $ \case
            TMat n m t -> keep [[a, TVec n t], [b, TVec m t]]
            _ -> fail "no instance"

        TFVec (TENat n) ty | n `elem` [2,3,4] && ty `elem'` floatIntWordBool -> reduced $ TVec n ty
        TFVec a b -> check (a `matches` nat234 && b `matches` floatIntWordBool {- -- FIXME -}) $ observe res $ \case
            TVec n t -> keep [[a, TENat n], [b, t]]
            _ -> fail "no instance tfvec"

        TFVecScalar a b -> case a of
            TENat 1 -> case b of
                TVar{} | res `matches` floatIntWordBool -> keep [[b, res]]
                b -> check (b `elem'` floatIntWordBool) $ reduced b
            TVar{} -> check (b `matches` floatIntWordBool) $ observe res $ \case
                t | t `elem'` floatIntWordBool -> keep [[a, TENat 1], [b, t]]
                _ -> like $ TFVec a b
            _ -> like $ TFVec a b

        TFMatVecElem t -> observe t $ \case
            TVec n t -> reduced t
            TMat _ _ t -> reduced t
            _ -> fail "no instance"

        TFMatVecScalarElem t -> observe t $ \case
            t | t `elem'` floatIntWordBool -> reduced t
            t -> like $ TFMatVecElem t

        TFColorRepr ty -> observe ty $ \case
            TTuple ts -> reduced . TTuple $ map Color ts
            ty -> reduced $ Color ty

        TFFTRepr' ty -> caseTuple "expected Input/Interpolated/Depth/Color" ty (reduced . tTuple) $ \case
            TInterpolated a -> reduce' a
            TInput a        -> reduce' a
            Depth a         -> reduce' a
            Color a         -> reduce' a
            _ -> fail'

        TFFragOps ty -> caseTuple "expected FragmentOperation" ty (reduced . tTuple) $ \case
            TFragmentOperation a -> reduce' a
            _ -> fail'

        TFFrameBuffer ty -> caseTuple "expected (Image Nat)" ty end $ \case
            TImage a b -> observe' a $ \case
                TENat n -> reduce' (n, b)
                _ -> fail'
            _ -> fail'
          where
            end (unzip -> (n: ns, tys))
                | all (==n) ns = reduced $ TFrameBuffer (TENat n) $ tTuple tys
                | otherwise = fail "frambuffer number of layers differ"

        TFJoinTupleType (TTuple []) x -> reduced x
        TFJoinTupleType x (TTuple []) -> reduced x
        TFJoinTupleType TVar{} _ -> nothing  -- TODO: observe res?
        TFJoinTupleType _ TVar{} -> nothing  -- TODO: observe res?
        TFJoinTupleType (TTuple l) (TTuple r) -> reduced $ TTuple (l ++ r)
        TFJoinTupleType l (TTuple r) -> reduced $ TTuple (l : r)
        TFJoinTupleType (TTuple l) r -> reduced $ TTuple (l ++ [r])
        TFJoinTupleType l r -> reduced $ TTuple [l,r]

      where
        like f = reduceConstraint cvar (CEq res f)
        reduced t = discard Refl [[res, t]]
        check b m = if b then m else fail "no instance (1)"
        fail :: Doc -> ConstraintSolvRes
        fail = failure . (("error during reduction of" </> pShow res <+> "~" <+> pShow f) </>)

        reduce' = Just . Just
        nothing' = Just Nothing
        fail' = Nothing
        observe' TVar{} _ = nothing'
        observe' x f = f x

        caseTuple :: Doc -> Ty -> ([a] -> ConstraintSolvRes) -> (Ty -> Maybe (Maybe a)) -> ConstraintSolvRes
        caseTuple msg ty end f = observe ty $ \case
            TTuple ts -> maybe (fail $ msg <+> "inside tuple") (maybe nothing end . sequence) $ mapM f' ts
            _ -> maybe (fail msg) (maybe nothing (end . (:[]))) $ f' ty
          where f' x = observe' x f

        tTuple [x] = x
        tTuple xs = TTuple xs

  where
    diff a b c = case Map.keys $ b Map.\\ a of
        [] -> discard Refl $ [c, TRecord $ a Map.\\ b]: unifyMaps [a, b]
--        ks -> failure $ "extra keys:" <+> pShow ks
    discard w xs = return (Map.singleton cvar $ Ty_ (ConstraintKind x) $ Witness w, xs)
    keep xs = return (mempty, xs)
    failure :: Doc -> ConstraintSolvRes
    failure = throwErrorTCM

    nothing = return mempty
    observe TVar{} _ = nothing
    observe x f = f x

nat234 (TENat i) = i `elem` [2..4]
nat234 _ = False

floatIntWordBool = \case
    TFloat -> True
    TInt -> True
    TWord -> True
    TBool -> True
    _ -> False
floatVectors (TVec i TFloat) = i `elem` [2..4]
floatVectors _ = False

data InjType
    = ITMat | ITVec | ITVecScalar
    deriving (Eq, Ord, Show)

injType :: TypeFunT -> Maybe (InjType, [Ty])
injType = \case
    TFMat a b -> Just (ITMat, [a, b])
    TFVec a b -> Just (ITVec, [a, b])
    TFVecScalar a b -> Just (ITVecScalar, [a, b])
    _ -> Nothing

--------------------------------------------------------------------------------

pairsWith f xs = zipWith f xs $ drop 1 xs

-- unify each types in the sublists
unifyTypes :: Bool -> [[Ty]] -> TCM Subst
unifyTypes bidirectional tys = flip execStateT mempty $ forM_ tys $ sequence_ . pairsWith uni
  where
--    uni :: Ty -> Ty -> StateT Subst TCM ()
    uni a b = gets subst1 >>= \f -> unifyTy (f a) (f b)
      where
        singSubst n t = \case
            StarToStar n -> StarToStar n
            Ty_ k ty -> case ty of
                Forall_ (Just n') a b | n /= n' -> Ty_ k' $ Forall_ (Just n') (singSubst n t a) b
                TVar_ a | a == n -> t
                ty -> Ty_ k' $ singSubst n t <$> ty
              where k' = singSubst n t k

        -- make single tvar substitution; check infinite types
        bindVar n k t = do
            uni k (kindOf t)
            s <- get
            let t' = subst s t
            if n `Set.member` freeVars t
                then lift $ throwErrorTCM $ "Infinite type, type variable" <+> pShow n <+> "occurs in" <+> pShow t
                else put $ Map.insert n t' $ singSubst n t' <$> s

        unifyTy (TArr a b) (StarToStar i) | i > 0 = uni Star a >> uni (StarToStar $ i-1) b
        unifyTy (StarToStar i) (TArr a b) | i > 0 = uni Star a >> uni (StarToStar $ i-1) b
        unifyTy Star Star = return ()

        unifyTy (Forall a k t) (Forall a' k' t') = uni k k' >> bindVar a k (TVar k' a') >> uni t t'  -- TODO! protect a in t
        unifyTy (TArr a1 b1) (TArr a2 b2) = uni a1 a2 >> uni b1 b2
        unifyTy (TVar k u) (TVar k' v) | u == v = uni k k'
        unifyTy (TVar k u) _ = bindVar u k b
        unifyTy _ (TVar k u) | bidirectional = bindVar u k a
        unifyTy (TLit k l) (TLit k' l') | l == l' = uni k k'
        unifyTy (TCon k u) (TCon k' v) | u == v = uni k k'
        unifyTy (TTuple t1) (TTuple t2) = sequence_ $ zipWith uni t1 t2
        unifyTy (TApp k1 a1 b1) (TApp k2 a2 b2) = uni a1 a2 >> uni b1 b2
        unifyTy (TVec a1 b1) (TVec a2 b2) | a1 == a2 = uni b1 b2
        unifyTy (TMat a1 b1 c1) (TMat a2 b2 c2) | a1 == a2 && b1 == b2 = uni c1 c2
        unifyTy a b
          | a == b = return ()      -- TODO: eliminate this
          | otherwise = lift $ throwErrorTCM $ "cannot unify" <+> pShow a </> "with" <+> pShow b </> "----------- equation" </> pShow tys

subst1 :: Subst -> Ty -> Ty
subst1 s tv@(TVar _ a) = fromMaybe tv $ Map.lookup a s
subst1 _ t = t

joinSubsts :: [Subst] -> TCM (Subst, Subst)
joinSubsts ss = do
    s <- unifyTypes True $ unifyMaps ss
    return (s, foldMap (subst s <$>) ss <> s)

appSubst :: Subst -> SubstEnv -> SubstEnv
appSubst s e = (Left <$> s) <> subst s e

groupByFst :: Ord a => [(a, b)] -> [[b]]
groupByFst = unifyMaps . map (uncurry Map.singleton)

joinSE :: [SubstEnv] -> TCM SubstEnv
joinSE ss = do
    s <- unifyTypes True $ concatMap (pairsWith ff) $ unifyMaps ss
    untilNoUnif $ appSubst s $ Map.unionsWith gg ss
  where
    gg (Left s) _ = Left s
    gg Right{} b = b

    ff (Left s) (Left s') = [s, s']
    ff (Left s) (Right s') = [kindOf s, s']
    ff (Right s) (Left s') = [s, kindOf s']
    ff (Right s) (Right s') = [s, s']

    untilNoUnif :: SubstEnv -> TCM SubstEnv
    untilNoUnif es = do
        (unzip -> (ss, concat -> eqs)) <- sequence [reduceConstraint n c | (n, Right (ConstraintKind c)) <- Map.toList es]
        s0 <- unifyTypes True
            -- unify left hand sides where the right hand side is equal:  (t1 ~ F a, t2 ~ F a)  -->  t1 ~ t2
             $ groupByFst [(f, ty) | Right (ConstraintKind (CEq ty f)) <- toList es]
            -- injectivity test:  (t ~ Vec a1 b1, t ~ Vec a2 b2)  -->  a1 ~ a2, b1 ~ b2
            ++ concatMap (concatMap transpose . groupByFst) (groupByFst
                    [(ty, (it, is)) | Right (ConstraintKind (CEq ty (injType -> Just (it, is)))) <- toList es])
            ++ eqs

        (_, s) <- joinSubsts $ s0: ss
            -- TODO nub constraints?
        if Map.null s then return es else untilNoUnif $ appSubst s es


--------------------------------------------------------------------------------

instance Applicative TCMS where
    pure a = TypingT $ pure (mempty, a)
    a <*> b = a >>= \f -> f <$> b

instance Monad TCMS where
    TypingT m >>= f = TypingT $ m >>= \(e1, a) -> case f a of
        TypingT m' -> do
            (e2, b) <- m'
            e <- joinSE [e1, e2]
            return (e, b)

instance (m ~ TCM, MonadReader r m) => MonadReader r (TypingT m) where
    ask = lift ask
    local f (TypingT m) = TypingT $ local f m

instance (m ~ TCM, MonadError r m) => MonadError r (TypingT m) where
    catchError (TypingT m) f = TypingT $ catchError m $ (\(TypingT m) -> m) <$> f
    throwError e = TypingT $ throwError e

addConstraint c = TypingT $ lift newName >>= \n -> pure (Map.singleton n $ Right $ ConstraintKind c, ())

addUnif :: Ty -> Ty -> TCMS ()
addUnif t1 t2 = addConstraint $ t1 ~~~ t2

checkStarKind t = addUnif Star (kindOf t)

star = return Star

newVar :: Ty -> TCM Ty
newVar k = TVar k <$> lift newName

newVar'' = newVar =<< newVar Star

newStarV = lift (newVar Star)

addTypeVar n = newStarV >>= \t -> TypingT $ pure (Map.singleton n $ Right t, t)

joinPolyEnvs ps = case filter (not . isSing . snd) $ Map.toList ms of
    [] -> return $ PolyEnv (foldMap instanceDefs ps) (head <$> ms) mempty{-TODO-} mempty{-TODO-}
    xss -> throwErrorTCM $ "Definition clash:" <+> pShow (map fst xss)
  where
    isSing [_] = True
    isSing _ = False
    ms = Map.unionsWith (++) [(:[]) <$> e | PolyEnv _ e _ _ <- ps]
--      , instances     = Map.unionsWith (<>) [Map.singleton c $ Set.singleton t | InstanceDef c t <- defs] -- TODO: check clash
--      , precedences   = Map.fromList [(n, p) | DFixity n p <- defs]     -- TODO: check multiple definitions

--withTyping :: Env InstType -> TCM a -> TCM a
withTyping ts m = do
    env <- ask
    env <- joinPolyEnvs [env, PolyEnv mempty ts mempty mempty]
    local (const env) m

--withTyping' :: Env InstType -> TCMS a -> TCMS a
withTyping' = withTyping

----------------------------

-- TODO!!: introduce new vars into the env??
instantiateTyping_ :: [TName] -> Ty -> InstType
instantiateTyping_ fv ty = lift $ do
    newVars <- replicateM (length fv) newName
    let s = Map.fromList $ zip fv newVars
    return $ repl s ty

instantiateTyping' :: SubstEnv -> Ty -> TCM InstType
instantiateTyping' se ty = do
    pe <- asks $ getPolyEnv
    return $ instantiateTyping_ (filter (`Map.notMember` pe) [n | (n, Right _) <- Map.toList se]) ty


instantiateTyping :: TCMS Ty -> TCM InstType
instantiateTyping ty = do
    (se, ty) <- runTypingT ty
    instantiateTyping' se ty


--------------------------------------------------------------------------------

inferLit :: Lit -> Ty
inferLit a = case a of
    LInt _    -> TInt
    LChar _   -> TChar
    LFloat _  -> TFloat
    LString _ -> TString
    LNat _    -> TNat

--------------------------------------------------------------------------------

{-
-- Ambiguous: (Int ~ F a) => Int
-- Not ambiguous: (Show a, a ~ F b) => b
ambiguityCheck :: Doc -> Typing -> TCM ()
ambiguityCheck msg ty = do
    e <- errorTCM
    let c = if all ok $ constraints ty then Nothing else Just $ \f -> e f </>
            "during" <+> msg </> "ambiguous type:" <+> pShow ty </> "defined vars:" <+> pShow (toList defined)
    modify $ (c:) *** id
  where
    ok c = not (Set.null used) && used `Set.isSubsetOf` defined
      where
        used = freeVars c
    defined = dependentVars (constraints ty) $ freeVars (monoEnv ty) <> freeVars (typingType ty)

typing me ty = TypingConstr me ty
--    dependentVars cs (freeVars ty) Set.\\ freeVars me  -- TODO: make it more precise if necessary

-- compute dependent type vars in constraints
-- Example:  dependentVars [(a, b) ~ F b c, d ~ F e] [c] == [a,b,c]
dependentVars :: [ConstraintT] -> Set TName -> Set TName
dependentVars ie s = cycle mempty s
  where
    cycle acc s
        | Set.null s = acc
        | otherwise = cycle (acc <> s) (grow s Set.\\ acc)

    grow = flip foldMap ie $ \case
        CEq ty f -> freeVars ty <-> freeVars f
        Split a b c -> freeVars a <-> (freeVars b <> freeVars c)
        CUnify{} -> mempty --error "dependentVars: impossible" 
        CClass{} -> mempty
      where
        a --> b = \s -> if Set.null $ a `Set.intersection` s then mempty else b
        a <-> b = (a --> b) <> (b --> a)
-}

--lookEnv :: IdN -> T
lookEnv n m = asks (fmap toTCMS . Map.lookup n . getPolyEnv) >>= fromMaybe m

-- TODO: ambiguity check
inferKind :: TyR -> TCMS Ty
inferKind (Ty' r ty) = addRange r $ case ty of
    Forall_ (Just n) k t -> do
        k <- inferKind k
        t <- withTyping' (Map.singleton n $ pure k) $ inferKind t
        return $ Ty Star $ Forall_ (Just n) k t --  TODO: review
    _ -> do
        ty <- traverse inferKind ty
        k <- case ty of
            TLit_ l -> return $ inferLit l
            StarC -> star
            ConstraintKind_ c -> star
            TTuple_ ts -> mapM_ checkStarKind ts >> star
            Forall_ Nothing a b -> checkStarKind a >> checkStarKind b >> star
            TApp_ tf ta -> newStarV >>= \v -> addUnif tf (ta ~> v) >> return v
            TVar_ n -> lookEnv n $ addTypeVar n
            TCon_ n -> lookEnv n $ lookEnv (toExpN n) $ lift $ throwErrorTCM $ "Type constructor" <+> pShow n <+> "is not in scope."
--            x -> error $ " inferKind: " ++ ppShow x
        return $ Ty k ty


{-
generalizeTypeVars :: Typing -> Typing
generalizeTypeVars t = t -- TODO!   removeMonoVars (Set.fromList $ filter isTypeVar $ Map.keys $ monoEnv t) t

removeMonoVars vs (TypingConstr me t) = --Typing me cs t $ pvs <> vs --
    typing (foldr Map.delete me $ Set.toList vs) t

simpArr (TArr a b) = TArr a b   -- patt syn trick
simpArr x = x
-}


inferTyping :: ExpR -> TCMS (Exp, Ty)
{-
-- hack
inferTyping (ENamedRecord r n (unzip -> (fs, es)))
    = inferTyping $ foldl (EApp mempty) (EVar mempty n) es
-}
inferTyping (Exp r e) = addRange r $ case e of

    ELam_ p f -> do
        ((p, tp), (ns, tr)) <- inferPatTyping False p
        (f, t) <- withTyping' tr $ inferTyping f
        return (ELam p f, tp ~> t)     -- TODO: removeMonoVars

    ELet_ (PVar' _ n) x e -> do
        (se, (x, tx)) <- lift $ runTypingT $ inferTyping x
        it <- lift $ instantiateTyping' se tx
        (e, t) <- withTyping' (Map.singleton n it) $ inferTyping e
        return (ELet (PVar $ VarE n tx) x e, t)
{-
    ELet_ p x e -> do          -- monomorph let; TODO?
        tx <- inferTyping x
        p_@(p, (_, tr)) <- inferPatTyping False p
        te <- withTyping tr $ inferTyping e
        ty <- unifyTypings "let" [getTagP' p_ ++ getTag' tx, getTag' te] $ \[_, te] -> toTyping te
        return $ ELet ty p tx te
    ETypeSig_ e ty -> do
        te@(Exp' _ e') <- inferTyping e
        ty <- generalizeTypeVars <$> inferKind' ty
        t <- unifyTypings_ False "typesig" [getTag' te ++ [ty]] $ \[ty] -> toTyping ty
        return $ Exp' t e'
-}
    EType_ ta -> do
        t <- inferKind ta
        return (EType t, kindOf t)
    _ -> do
        e <- traverse inferTyping e
        t <- case e of
            EApp_ (_, tf) (EType t, ta)
                -> newStarV >>= \v -> lift newVar'' >>= \u@(TVar _ x) -> addUnif u t >> addUnif tf (Forall x ta v) >> return v
            EApp_ (_, tf) (_, ta) -> newStarV >>= \v -> addUnif tf (ta ~> v) >> return v
--            EFieldProj_ fn -> noSubst $ fieldProjType fn
--            ERecord_ (unzip -> (fs, es)) -> unifyTypings "record" (map getTag' es) $ toTyping . TRecord . Map.fromList . zip fs
{-
            ERecord_ (Just n) (unzip -> (fs, es)) -> do -- TODO: handle field names
                (s', nt) <- asks (getPolyEnv . fst) >>= fromMaybe (throwErrorTCM $ "Variable " ++ n ++ " is not in scope.") . Map.lookup n
                (s, t) <- unifyTypings ([nt]: map getTag' es) $ \(tf: ts) v -> [tf ~~~ foldr (~>) v ts] ==> v
                return (s <> s', t)
-}
            ETuple_ te -> return $ TTuple $ map snd te
            ELit_ l -> return $ inferLit l
            EVar_ n -> lookEnv n $ lift $ throwErrorTCM $ "Variable" <+> pShow n <+> "is not in scope."
            EAlts_ _ xs -> newStarV >>= \v -> mapM_ (addUnif v . snd) xs >> return v
            ENext_ -> newStarV          -- TODO: review
            x -> error $ "inferTyping: " ++ ppShow x
        return (Exp'' $ mapExp (error "e1") (error "e2") (error "e3") $ fst <$> e, t)

{-
fieldProjType :: FName -> TCM Typing
fieldProjType fn = newV $ \a r r' -> return $ [Split r r' (TRecord $ Map.singleton fn a)] ==> r ~> a :: TCM ([ConstraintT], Ty)
-}

inferPatTyping :: Bool -> PatR -> TCMS ((Pat, Ty), (Set EName, Env InstType))
inferPatTyping polymorph p_@(Pat pt p) = addRange pt $ do
    p <- traverse (inferPatTyping polymorph) p
    (t, (n, tr)) <- case p of
        PLit_ n -> noTr $ pure $ inferLit n
        Wildcard_ -> noTr $ newStarV >>= \t -> return t
        PVar_ n -> addTr (Set.singleton n) (\t -> Map.singleton n $ pure t) $ newStarV >>= \t ->
            if polymorph then return t else TypingT $ return (Map.singleton n $ Right t, t)     -- TODO: instantiate?
{-
        PAt_ n p -> addTr (Set.singleton n) (\t -> Map.singleton n $ instantiateTyping_ [] $ snd t) $ noSubst $ newV $ snd . getTag . fst $ p
-}
        PTuple_ ps -> noTr $ pure $ TTuple $ map (snd . fst) ps
        PCon_ n ps -> noTr $ do
            tn <- lookEnv n $ lift $ throwErrorTCM $ "Constructor" <+> pShow n <+> "is not in scope."
            newStarV >>= \v -> addUnif tn (map (snd . fst) ps ~~> v) >> return v
{-
        PRecord_ (unzip -> (fs, ps)) -> noTr $ unifyTypings "record pat" (map getTagP' ps)
            (\tl v v' -> [Split v v' $ TRecord $ Map.fromList $ zip fs tl] ==> v)
--            x -> error $ "inferPatTyping: " ++ ppShow x
-}
    let trs = Map.unionsWith (++) . map ((:[]) <$>) $ tr: map (snd . snd) (toList p)
        ns = Set.unions $ n: map (fst . snd) (toList p)
    tr <- case filter (not . null . drop 1 . snd) $ Map.toList trs of
        [] -> return $ Map.map head trs
        ns -> lift $ throwErrorTCM $ "conflicting definitions for" <+> pShow (map fst ns)
    return ((Pat'' $ mapPat (error "p1") (error "p2") $ fst . fst <$> p, t), (ns, tr))
  where
--    getTagP' = (:[]) . snd . getTag . fst
    noTr = addTr mempty $ const mempty
    addTr n tr m = (\x -> (x, (n, tr x))) <$> m


--------------------------------------------------------------------------------

{-
instance MonadWriter (Env b) (Typing_ b) where
    writer (t, m) = TypingConstr m t
    listen (TypingConstr m t) = TypingConstr m (t, m)
    pass (TypingConstr m (t, f)) = TypingConstr (f m) t

type Typing = Typing_ Ty Ty

-- TODO: review applications of this
typingToTy :: Typing -> Ty
typingToTy ty = foldr forall_ (typingType ty) $ orderEnv $ monoEnv ty
  where
    forall_ (n, k) t = Forall n k t

    -- TODO: make more efficient
    orderEnv :: Env Ty -> [(IdN, Ty)]
    orderEnv env = f mempty . Map.toList $ env
      where
        f :: Set IdN -> [(IdN, Ty)] -> [(IdN, Ty)]
        f s [] = []
        f s ts = case [x | x@((n, t), ts') <- getOne ts, freeVars t `Set.isSubsetOf` s] of
            (((n, t), ts):_) -> (n, t): f (Set.insert n s) ts
            _ -> error $ show $ "orderEnv:" <+> pShow ty
        getOne xs = [(b, a ++ c) | (a, b: c) <- zip (inits xs) (tails xs)]
-}

--------------------------------------------------------------------------------

{-
mangleAx (n, t) = (if kinded $ res $ typingType t then toTypeN n else n, t)
  where
    res (TArr a b) = res b
    res t = t
    kinded = \case
        StarToStar n -> True
        Ty_ _ StarC -> True
        _ -> False
-}
inference_ :: PolyEnv -> ModuleR -> ErrorT Identity PolyEnv
inference_ penv m = flip evalStateT ['t': show i | i <- [0..]] $ flip runReaderT penv $ inferModule m
  where
    inferModule Module{..} = do
        inferDefs definitions

trace' s = trace (show s) s

tyConKind :: [TyR] -> TCM InstType
tyConKind vs = instantiateTyping $ foldr (liftA2 (~>)) star $ map (fmap kindOf . inferKind) vs

inferConDef :: Name -> [(Name, TyR)] -> WithRange ConDef -> TCM (Env InstType)
inferConDef con (unzip -> (vn, vt)) (r, ConDef n tys) = addRange r $ do
    ty <- instantiateTyping $ do
        ks <- mapM inferKind vt
        withTyping' (Map.fromList $ zip vn $ map pure ks) $ do
--        tys <- mapM inferFieldKind tys
            let
                tyConResTy :: TCMS Ty
                tyConResTy
                    = inferKind $ foldl app (Ty' mempty $ TCon_ con) $ map (Ty' mempty . TVar_) vn
                  where
                    app a b = Ty' mempty $ TApp_ a b

            foldr (liftA2 (~>)) tyConResTy $ map inferFieldKind tys
    return $ Map.singleton n ty
  where
    inferFieldKind (FieldTy mn t) = inferKind t

{-
    tyConTypes :: DataDef (TCMS Ty) -> [(EName, TCMS Ty)]
    tyConTypes d@(DataDef n _ cs) =
        [ (cn, instantiateTyping $ generalizeTypeVars $ foldr (.~>) (tyConResTy d) $ map fieldType tys)
        | ConDef cn tys <- cs
        ]
-}
{- TODO
    selectorDefs :: DataDef (Ty' Range) -> [DefinitionR]
    selectorDefs d@(DataDef n _ cs) =
        [ ValueDef
          ( PVar' mempty sel
          , ELam mempty
                (PCon mempty cn
                    [ if sel == sel' then PVar' mempty (ExpN "x") else Wildcard mempty
                    | FieldTy (Just sel') _ <- tys]
                )
                (EVar mempty $ ExpN "x")
          )
        | ConDef cn tys <- cs
        , FieldTy (Just sel) _ <- tys
        ]

    selectorTypes :: DataDef (TCMS Ty) -> [(EName, TCMS Ty)]
    selectorTypes d@(DataDef n _ cs) =
        [ (sel, instantiateTyping $ generalizeTypeVars $ tyConResTy d .~> t)
        | ConDef cn tys <- cs
        , FieldTy (Just sel) t <- tys
        ]
-}
inferDef :: ValueDefR -> TCM (TCM a -> TCM a)
inferDef (ValueDef p@(PVar' _ n) e) = do
    (se, (exp, te)) <- runTypingT $ do
        ((p, tp), (ns, tr)) <- inferPatTyping False p
        (exp, te) <- withTyping' tr $ inferTyping e
        addUnif tp te
        return (exp, te) 
    -- TODO: removeMonoVars?
    -- TODO: subst down
    f <- instantiateTyping' se te
    return ({-ValueDef (PVar $ VarE n te) (exp, te), -}withTyping $ Map.singleton n f) -- TODO: replCallType n (getTag e) e

{-
-- TODO: revise
replCallType n nt = \case
    EVar _ n' | n == n' -> EVar nt n
    Exp' t e -> Exp' t $ f <$> e
--    x -> error $ "replCallType: " ++ ppShow x
  where f = replCallType n nt

modTag f (Exp' t x) = Exp' (f t) x

-}


inferDefs :: [DefinitionR] -> TCM PolyEnv
inferDefs [] = ask
inferDefs ((r, d): ds) = do
    f <- addRange r $ case d of
        DValueDef d -> do
            inferDef d
        DDataDef con vars cdefs -> do
          tk <- tyConKind $ map snd vars
          withTyping (Map.singleton con tk) $ do
            ev <- mapM (inferConDef con vars) cdefs
            return $ withTyping (mconcat ev)
        GADT con vars cdefs -> error "ddef gadt" -- do
{-
  vars <- forM vars $ \(n, k) -> do
    k <- inferKind' k
    return (n, k)
  withTyping (uncurry Map.singleton $ tyConKind con vars) $ do
    cdefs <- forM cdefs $ \(c, t) -> do
        t <- inferKind' t
        return (c, generalizeTypeVars t)
    let d' = GADT con vars cdefs
    ds <- withTyping (instantiateTyping <$> Map.fromList cdefs) $ inferDefs ds
    return (d': ds)
-}
        ClassDef con vars cdefs -> error "ddef class" -- do
{-
  vars <- forM vars $ \(n, k) -> do
    k <- inferKind' k
    return (n, k)
  do --withTyping (uncurry Map.singleton $ tyConKind con vars) $ do
    cdefs <- inferDefs cdefs
    let c = CClass con $ head{-TODO-} $ map g vars
        g (n, k) = TVar (typingToTy k) n
    cdefs <- mapM (\(TypeSig (n, t)) -> TypeSig . (,) n <$> addConstr [c] t) cdefs
    let d' = ClassDef con vars cdefs
    ds <- withTyping (Map.fromList $ classTypings d') $ inferDefs ds
    return (d': ds)
-}
        DAxiom (TypeSig n a) -> error "ddef axiom" -- do
{-
    a' <- inferKind' a
    let (n', a'') = mangleAx . (id *** generalizeTypeVars) $ (n, a')
    ds <- withTyping (Map.singleton n' $ instantiateTyping a'') $ inferDefs ds
    return (TypeSig (n', a''): ds)
-}
        InstanceDef c t xs -> error "ddef instance" -- do
{-
    t <- inferKind' t
    let tt = typingToTy t
    local ((\pe -> pe {instanceDefs = Map.alter (Just . maybe (Set.singleton tt) (Set.insert tt)) c $ instanceDefs pe}) *** id) $ do
        xs <- inferDefs xs      -- TODO: check types
        ds <- withTyping (Map.fromList $ concatMap axs xs) $ inferDefs ds
        return (InstanceDef c t xs: ds)
-}
    f $ inferDefs ds

{-
addConstr :: [ConstraintT] -> Typing -> TCM Typing
addConstr cs ty = foldrM f ty cs  where
    f c ty = do
        n <- newName
        return $ typing (Map.insert n (ConstraintKind c) $ monoEnv ty) $ typingType ty
-}

