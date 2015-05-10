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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
module Typecheck where

import Data.Function
import Data.List
import Data.Maybe
import Data.Either
import Data.Monoid
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

import Text.Parsec.Pos

import Pretty
import Type

--------------------------------------------------------------------------------

pairsWith f xs = zipWith f xs $ drop 1 xs

unifyMaps_ :: (Ord a) => (a -> Doc) -> [Map a b] -> [WithExplanation [b]]
unifyMaps_ f = map (f *** id) . Map.toList . Map.unionsWith (++) . map ((:[]) <$>)

unifyMaps :: (Ord a, PShow a) => [Map a b] -> [WithExplanation [b]]
unifyMaps = map (pShow *** id) . Map.toList . Map.unionsWith (++) . map ((:[]) <$>)

groupByFst :: (Ord a, PShow a) => [(a, b)] -> [WithExplanation [b]]
groupByFst = unifyMaps . map (uncurry Map.singleton)

matches TVar{} _ = True
matches x ts = x `elem'` ts

elem' a b = b a

isRec TRecord{} = True
isRec t = isVar t

isVar TVar{} = True
isVar _ = False

nat234 (TENat i) = i `elem` [2..4]
nat234 _ = False

floatIntWordBool = \case
    TFloat -> True
    TInt -> True
    TWord -> True
    TBool -> True
    _ -> False

data InjType
    = ITMat | ITVec | ITVecScalar
    deriving (Show, Eq, Ord)

instance PShow InjType where
    pShowPrec p = text . show

injType :: TypeFunT -> Maybe (InjType, [Ty])
injType = \case
    TFMat a b -> Just (ITMat, [a, b])
    TFVec a b -> Just (ITVec, [a, b])
    TFVecScalar a b -> Just (ITVecScalar, [a, b])
    _ -> Nothing


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

type ConstraintSolvRes = (Subst, [WithExplanation [Ty]])

reduceConstraint :: forall m . (MonadReader PolyEnv m, MonadError ErrorMsg m) => IdN -> ConstraintT -> m ConstraintSolvRes
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
        [] -> discard Refl [WithExplanation "???" [c, TRecord $ a <> b]]
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

        _ -> maybe noInstance (\s -> if Set.member t $ s then discard Refl [] else noInstance' s) $ Map.lookup c builtinInstances

      where
        msg' = "no" <+> pShow c <+> "instance for" <+> pShow t
        noInstance = failure msg'
        noInstance' s = failure $ msg' </> "possible instances:" </> pShow s

    CUnify a b -> discard Refl [WithExplanation "~~~" [a, b]]

    CEq res f -> case f of

        TFMat (TVec n t1) (TVec m t2) | t1 `elem` [TFloat] && t1 == t2 -> reduced $ TMat n m t1
        TFMat a b -> observe res $ \case
            TMat n m t -> keep [WithExplanation "Mat res 1" [a, TVec n t], WithExplanation "Mat res 2" [b, TVec m t]]
            _ -> fail "no instance"

        TFVec (TENat n) ty | n `elem` [2,3,4] && ty `elem'` floatIntWordBool -> reduced $ TVec n ty
        TFVec a b -> check (a `matches` nat234 && b `matches` floatIntWordBool {- -- FIXME -}) $ observe res $ \case
            TVec n t -> keep [WithExplanation "Vec res 1" [a, TENat n], WithExplanation "Vec res 2" [b, t]]
            _ -> fail "no instance tfvec"

        TFVecScalar a b -> case a of
            TENat 1 -> case b of
                TVar{} | res `matches` floatIntWordBool -> keep [WithExplanation "VecScalar dim 1" [b, res]]
                b -> check (b `elem'` floatIntWordBool) $ reduced b
            TVar{} -> check (b `matches` floatIntWordBool) $ observe res $ \case
                t | t `elem'` floatIntWordBool -> keep [WithExplanation "VecScalar res 1" [a, TENat 1], WithExplanation "VecScalar res 2" [b, t]]
                _ -> like $ TFVec a b
            _ -> like $ TFVec a b

        TFMatVecElem t -> observe t $ \case
            TVec n t -> reduced t
            TMat _ _ t -> reduced t
            _ -> nothing

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
        reduced t = discard Refl [WithExplanation "type family reduction" [res, t]]
        check b m = if b then m else fail "no instance (1)"
        fail :: Doc -> m ConstraintSolvRes
        fail = failure . (("error during reduction of" </> pShow res <+> "~" <+> pShow f) </>)

        reduce' = Just . Just
        nothing' = Just Nothing
        fail' = Nothing
        observe' TVar{} _ = nothing'
        observe' x f = f x

        caseTuple :: Doc -> Ty -> ([a] -> m ConstraintSolvRes) -> (Ty -> Maybe (Maybe a)) -> m ConstraintSolvRes
        caseTuple msg ty end f = observe ty $ \case
            TTuple ts -> maybe (fail $ msg <+> "inside tuple") (maybe nothing end . sequence) $ mapM f' ts
            _ -> maybe (fail msg) (maybe nothing (end . (:[]))) $ f' ty
          where f' x = observe' x f

        tTuple [x] = x
        tTuple xs = TTuple xs

  where
    diff a b c = case Map.keys $ b Map.\\ a of
        [] -> discard Refl $ WithExplanation "???" [c, TRecord $ a Map.\\ b]: unifyMaps [a, b]
--        ks -> failure $ "extra keys:" <+> pShow ks
    discard w xs = return (Map.singleton cvar $ Ty_ (ConstraintKind x) $ Witness w, xs)
    keep xs = return (mempty, xs)
    failure :: Doc -> m ConstraintSolvRes
    failure = throwErrorTCM

    nothing = return mempty
    observe TVar{} _ = nothing
    observe x f = f x

--------------------------------------------------------------------------------

-- unify each types in the sublists
unifyTypes :: forall m . (MonadError ErrorMsg m) => Bool -> [WithExplanation [Ty]] -> m Subst
unifyTypes bidirectional tys = flip execStateT mempty $ forM_ tys $ sequence_ . pairsWith uni . snd
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
        bindVar n t = do
            s <- get
            let t' = subst s t
            if n `Set.member` freeVars t
                then throwErrorTCM $ "Infinite type, type variable" <+> pShow n <+> "occurs in" <+> pShow t
                else put $ Map.insert n t' $ singSubst n t' <$> s

        unifyTy a@(StarToStar i) b@(StarToStar j)
            | i == j = return ()
            | otherwise = throwError $ UnificationError a b $ filter (not . null . drop 1 . snd) tys
        unifyTy a_@(Ty__ k t) b_@(Ty__ k' t') = uni k k' >> unifyTy' t t'
          where
            unifyTy' (Forall_ (Just a) k t) (Forall_ (Just a') k' t') = uni k k' >> bindVar a (TVar k' a') >> uni t t'  -- TODO! protect a in t
            unifyTy' (Forall_ Nothing a1 b1) (Forall_ Nothing a2 b2) = uni a1 a2 >> uni b1 b2
            unifyTy' (TVar_ u) (TVar_ v) | u == v = return ()
            unifyTy' (TVar_ u) _ = bindVar u b
            unifyTy' _ (TVar_ u) | bidirectional = bindVar u a
            unifyTy' (TLit_ l) (TLit_ l') | l == l' = return ()
            unifyTy' (TCon_ u) (TCon_ v) | u == v = return ()
            unifyTy' (TTuple_ t1) (TTuple_ t2) = sequence_ $ zipWith uni t1 t2
            unifyTy' (TApp_ a1 b1) (TApp_ a2 b2) = uni a1 a2 >> uni b1 b2
            unifyTy' Star_ Star_ = return ()
            unifyTy' a b
              | otherwise = throwError $ UnificationError a_ b_ $ filter (not . null . drop 1 . snd) tys

subst1 :: Subst -> Ty -> Ty
subst1 s tv@(TVar _ a) = fromMaybe tv $ Map.lookup a s
subst1 _ t = t

joinSubsts :: forall m . (MonadError ErrorMsg m) => [Subst] -> m Subst
joinSubsts ss = do
    s <- addCtx "joinSubsts" $ unifyTypes True $ unifyMaps ss
    return $ foldMap (subst s <$>) ss <> s

appSubst :: Subst -> SubstEnv -> SubstEnv
appSubst s e = (Left <$> s) <> subst s e

showVar (N _ _ n (NameInfo _ i)) = text n <> "{" <> i <> "}"

substEnvSubst = Map.map (\(Left x) -> x) . Map.filter isLeft

appSES :: (Substitute x, Monad m) => TypingT m x -> TypingT m x
appSES (WriterT' m) = WriterT' $ do
    (se, x) <- m
    return (se, subst (substEnvSubst se) x)

joinSE :: forall m . (MonadReader PolyEnv m, MonadError ErrorMsg m) => [SubstEnv] -> m SubstEnv
joinSE = \case
    [a, b]
        | Map.null a -> untilNoUnif b     -- optimization
        | Map.null b -> untilNoUnif a     -- optimization
    [a, b] -> do
        s <- addCtx "joinSE" $ unifyTypes True $ concatMap ff $ unifyMaps_ showVar [a, b]
        let sa = appSubst s a
            sb = appSubst s b
            f x y = appSubst (substEnvSubst x) y
        untilNoUnif $ Map.unionWith gg (f sb sa) (f sa sb)
  where
    gg (Left s) _ = Left s
    gg Right{} b = b

    ff (expl, ss) = case (WithExplanation (expl <+> "subst") [s | Left s <- ss], WithExplanation (expl <+> "typesig") [s | Right s <- ss]) of 
        (WithExplanation _ [], ss) -> [ss]
        (ss, WithExplanation _ []) -> [ss]
        (subs@(WithExplanation i (s:_)), sigs@(WithExplanation i' (s':_))) -> [subs, sigs, WithExplanation ("subskind" <+> i <+> i') [kindOf s, s']]

    untilNoUnif :: SubstEnv -> m SubstEnv
    untilNoUnif es = do
        (unzip -> (ss, concat -> eqs)) <- sequence [reduceConstraint n c | (n, Right (ConstraintKind c)) <- Map.toList es]
        s0 <- addCtx "untilNoUnif" $ unifyTypes True
            -- unify left hand sides where the right hand side is equal:  (t1 ~ F a, t2 ~ F a)  -->  t1 ~ t2
             $ groupByFst [(f, ty) | Right (ConstraintKind (CEq ty f)) <- toList es]
            -- injectivity test:  (t ~ Vec a1 b1, t ~ Vec a2 b2)  -->  a1 ~ a2, b1 ~ b2
            ++ concatMap (\(s, l) -> map ((,) s) $ transpose l)
                    (groupByFst
                    [((ty, it), is) | Right (ConstraintKind (CEq ty (injType -> Just (it, is)))) <- toList es])
            ++ eqs

        s <- joinSubsts $ s0: ss
            -- TODO nub constraints?
        if Map.null s then return es else untilNoUnif $ appSubst s es

instance Monoid' SubstEnv where
    type MonoidConstraint SubstEnv m = (MonadReader PolyEnv m, MonadError ErrorMsg m)
    mempty' = mempty
    mappend' a b = joinSE [a, b]

--------------------------------------------------------------------------------

--newVar :: Ty -> TCM Ty
newVar i k = TVar k <$> newName i

newStarVar i = newVar i Star

addConstraints m = WriterT' $ pure (m, ())
addConstraint c = newName "constraint" >>= \n -> addConstraints $ Map.singleton n $ Right $ ConstraintKind c

addUnif :: Ty -> Ty -> TCMS ()
addUnif t1 t2 = addConstraint $ t1 ~~~ t2

checkStarKind t = addUnif Star (kindOf t)

star = return Star

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

----------------------------

instantiateTyping' :: SubstEnv -> Ty -> TCM InstType
instantiateTyping' se ty = do
    pe <- asks $ getPolyEnv
    let p n (Right _) = n `Map.notMember` pe
        p _ _ = False
        se' = Map.filterWithKey p se
        fv = Map.keys se'
    return $ WriterT' $ do
        newVars <- replicateM (length fv) $ newName "instvar"
        let s = Map.fromList $ zip fv newVars
        return (repl s se', repl s ty)

instantiateTyping = fmap fst . instantiateTyping''

instantiateTyping'' :: TCMS Ty -> TCM (InstType, Ty)
instantiateTyping'' ty = do
    (se, ty) <- runWriterT' $ appSES ty
    ambiguityCheck ".." se ty
    x <- instantiateTyping' se ty
    return (x, ty)

--lookEnv :: IdN -> T
lookEnv n m = asks (fmap toTCMS . Map.lookup n . getPolyEnv) >>= fromMaybe m


-- Ambiguous: (Int ~ F a) => Int
-- Not ambiguous: (Show a, a ~ F b) => b
--ambiguityCheck :: Doc -> TCMS Ty -> TCMS Ty
ambiguityCheck msg se ty = do
    pe <- asks getPolyEnv
    let
        cs = [c | (n, Right (ConstraintKind c)) <- Map.toList se]
        defined = dependentVars cs $ Map.keysSet pe <> freeVars ty
        def n = n `Set.member` defined || n `Map.member` pe
        ok c = any def $ freeVars c
    if all ok cs then return () else throwError . ErrorMsg $
            "during" <+> msg </> "ambiguous type:" <+> pShow cs <+> "=>" <+> pShow ty </> "defined vars:" <+> pShow (toList defined)

--typing me ty = TypingConstr me ty
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


{-
generalizeTypeVars :: Typing -> Typing
generalizeTypeVars t = t -- TODO!   removeMonoVars (Set.fromList $ filter isTypeVar $ Map.keys $ monoEnv t) t
-}
removeMonoVars = mapWriterT' $ fmap $ \(se, (s, x)) -> (foldr Map.delete se $ Set.toList s, x)
{-
simpArr (TArr a b) = TArr a b   -- patt syn trick
simpArr x = x
-}


--------------------------------------------------------------------------------

inferLit :: Lit -> Ty
inferLit a = case a of
    LInt _    -> TInt
    LChar _   -> TChar
    LFloat _  -> TFloat
    LString _ -> TString
    LNat _    -> TNat

inferKind_ = {-appSES .-} inferKind

-- TODO: ambiguity check
inferKind :: TyR -> TCMS Ty
inferKind ty_@(Ty' r ty) = addRange r $ addCtx ("kind inference of" <+> pShow ty) $ appSES $ case ty of
    Forall_ (Just n) k t -> do
        k <- inferKind k
        t <- withTyping (Map.singleton n $ pureInstType k) $ inferKind t
        return $ Ty Star $ Forall_ (Just n) k t --  TODO: review
    _ -> do
        ty <- traverse inferKind ty
        k <- case kindOf <$> ty of
            TLit_ l -> return $ inferLit l
            Star_ -> star
            ConstraintKind_ c -> star
            TTuple_ ts -> mapM_ checkStarKind ts >> star
            Forall_ Nothing a b -> checkStarKind a >> checkStarKind b >> star
            TApp_ tf ta -> newStarVar "tapp" >>= \v -> addUnif tf (ta ~> v) >> return v
            TVar_ n -> lookEnv n $ newStarVar ("tvar" <+> pShow r) >>= \t -> addConstraints (Map.singleton n $ Right t) >> return t
            TCon_ n -> lookEnv n $ lookEnv (toExpN n) $ lift $ throwErrorTCM $ "Type constructor" <+> pShow n <+> "is not in scope."
--            x -> error $ " inferKind: " ++ ppShow x
        case ty of
            Forall_ Nothing (ConstraintKind c) b -> do
                addConstraint c
                return b
            _ -> return $ Ty k ty

inferPatTyping :: Bool -> PatR -> TCMS ((Pat, Ty), (Set EName, Env' InstType))
inferPatTyping polymorph p_@(Pat pt p) = addRange pt $ addCtx ("type inference of pattern" <+> pShow p_) $ do
    p <- traverse (inferPatTyping polymorph) p
    (t, (n, tr)) <- case p of
        PLit_ n -> noTr $ pure $ inferLit n
        Wildcard_ -> noTr $ newStarVar "_" >>= \t -> return t
        PVar_ n -> addTr (Set.singleton n) (\t -> Map.singleton n $ pureInstType t) $ newStarVar "pvar" >>= \t@(TVar k tn) -> do
            addConstraints $ Map.singleton tn $ Right k
            when (not polymorph) $ addConstraints $ Map.singleton n $ Right t 
            return t

        PAt_ n p -> addTr (Set.singleton n) (\t -> Map.singleton n $ pureInstType t) $ pure $ snd . fst $ p

        PTuple_ ps -> noTr $ pure $ TTuple $ map (snd . fst) ps
        PCon_ n ps -> noTr $ do
            tn <- lookEnv n $ lift $ throwErrorTCM $ "Constructor" <+> pShow n <+> "is not in scope."
            newStarVar "pcon" >>= \v -> addUnif tn (map (snd . fst) ps ~~> v) >> return v

        PRecord_ (unzip -> (fs, ps)) -> noTr $ do
            v <- newStarVar "pfp2"
            v' <- newStarVar "pfp3"
            addConstraint $ Split v v' $ TRecord $ Map.fromList $ zip fs $ map (snd . fst) ps
            return v

    let trs = Map.unionsWith (++) . map ((:[]) <$>) $ tr: map (snd . snd) (toList p)
        ns = Set.unions $ n: map (fst . snd) (toList p)
    tr <- case filter (not . null . drop 1 . snd) $ Map.toList trs of
        [] -> return $ Map.map head trs
        ns -> lift $ throwErrorTCM $ "conflicting definitions for" <+> pShow (map fst ns)
    return ((Pat'' $ mapPat (error "p1") (error "p2") $ fst . fst <$> p, t), (ns, tr))
  where
    noTr = addTr mempty $ const mempty
    addTr n tr m = (\x -> (x, (n, tr x))) <$> m

inferTyping :: ExpR -> TCMS (Exp, Ty)
inferTyping e_@(Exp r e) = addRange r $ addCtx ("type inference of" <+> pShow e_) $ case e of

    -- hack
    ENamedRecord_ n (unzip -> (fs, es)) ->
        inferTyping $ foldl (EApp' mempty) (EVar' mempty n) es

    ELam_ p f -> removeMonoVars $ do
        ((p, tp), (ns, tr)) <- inferPatTyping False p
        (f, t) <- addCtx "?" $ withTyping tr $ inferTyping f
        return $ (,) ns (ELam p f, tp ~> t)

    ELet_ (PVar' _ n) x e -> do
        (se, (x, tx)) <- lift $ runWriterT' $ inferTyping x
        it <- lift $ instantiateTyping' se tx
        (e, t) <- withTyping (Map.singleton n it) $ inferTyping e
        return (ELet (PVar $ VarE (IdN n) tx) x e, t)
    ELet_ p x e -> do          -- monomorph let; TODO?
        (se, (x, tx)) <- lift $ runWriterT' $ inferTyping x
        ((p, tp), (_, tr)) <- inferPatTyping False p
        (e, t) <- withTyping tr $ inferTyping e
        addUnif tx tp
        return (ELet p x e, t)
    ETypeSig_ e ty -> do
        (e, te) <- inferTyping e
        ty <- inferKind ty
        addUnif te ty  -- TODO: one directional
        return (e, te)
    EType_ ta -> do
        t <- inferKind ta
        return (EType t, kindOf t)
    EVar_ n -> do
        t <- lookEnv n $ lift $ throwErrorTCM $ "Variable" <+> pShow n <+> "is not in scope."
        return (EVar $ VarE (IdN n) t, t)
    _ -> do
        e <- traverse inferTyping e
        t <- case e of
            EApp_ (_, tf) (EType t, ta)
                -> newName "apptype" >>= \x -> addUnif t (TVar ta x) >> newStarVar "etyapp" >>= \v -> addUnif tf (Forall x ta v) >> return v
            EApp_ (_, tf) (_, ta) -> newStarVar "eapp" >>= \v -> addUnif tf (ta ~> v) >> return v
            EFieldProj_ fn -> do
                a <- newStarVar "fp1"
                r <- newStarVar "fp2"
                r' <- newStarVar "fp3"
                addConstraint $ Split r r' $ TRecord $ Map.singleton (IdN fn) a
                return $ r ~> a
            ERecord_ (unzip -> (fs, es)) -> return $ TRecord $ Map.fromList $ zip (map IdN fs) $ map snd es
            ENamedRecord_ n (unzip -> (fs, es)) -> do -- TODO: handle field names
                t <- lookEnv n $ throwErrorTCM $ "Variable" <+> pShow n <+> "is not in scope."
                v <- newStarVar "namedrecord"
                addUnif t $ foldr (~>) v $ map snd es
                return v
            ETuple_ te -> return $ TTuple $ map snd te
            ELit_ l -> return $ inferLit l
            EAlts_ _ xs -> newStarVar "ealts" >>= \v -> mapM_ (addUnif v . snd) xs >> return v
            ENext_ -> newStarVar "enext"          -- TODO: review
            x -> error $ "inferTyping: " ++ ppShow x
        return (Exp'' $ mapExp (error "e1") (error "e2") (error "e3") $ fst <$> e, t)

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

--trace' s = trace (show s) s

tyConKind :: [TyR] -> TCM InstType
tyConKind vs = instantiateTyping $ foldr (liftA2 (~>)) star $ map inferKind vs

inferConDef :: Name -> [(Name, TyR)] -> WithRange ConDef -> TCM (Env' InstType)
inferConDef con (unzip -> (vn, vt)) (r, ConDef n tys) = addRange r $ do
    ty <- instantiateTyping $ do
        ks <- mapM inferKind vt
        withTyping (Map.fromList $ zip vn $ map pureInstType ks) $ do
            let tyConResTy :: TCMS Ty
                tyConResTy
                    = inferKind $ foldl app (Ty' mempty $ TCon_ con) $ map (Ty' mempty . TVar_) vn
                  where
                    app a b = Ty' mempty $ TApp_ a b

            foldr (liftA2 (~>)) tyConResTy $ map inferFieldKind tys
    return $ Map.singleton n ty
  where
    inferFieldKind (FieldTy mn t) = inferKind t

selectorDefs :: DefinitionR -> [DefinitionR]
selectorDefs (r, DDataDef n _ cs) =
    [ (r, DValueDef $ ValueDef
      ( PVar' mempty sel)
      ( ELam' mempty
            (PCon' mempty cn
                [ if sel == sel' then PVar' mempty (ExpN "x") else Pat mempty Wildcard_
                | FieldTy (Just sel') _ <- tys]
            )
            (EVar' mempty $ ExpN "x")
      ))
    | (rc, ConDef cn tys) <- cs
    , FieldTy (Just sel) _ <- tys
    ]

inferDef :: ValueDefR -> TCM (TCM a -> TCM a)
inferDef (ValueDef p@(PVar' _ n) e) = do
    (se, (exp, te)) <- runWriterT' $ do
        ((p, tp), (ns, tr)) <- inferPatTyping False p
        (exp, te) <- withTyping tr $ inferTyping e
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
inferDefs (dr@(r, d): ds@(inferDefs -> cont)) = addRange r $ case d of
    DValueDef d -> do
        inferDef d >>= ($ cont)
    DDataDef con vars cdefs -> do
        tk <- tyConKind $ map snd vars
        withTyping (Map.singleton con tk) $ do
            ev <- mapM (inferConDef con vars) cdefs
            withTyping (mconcat ev) $ do
                inferDefs $ selectorDefs dr ++ ds
    GADT con vars cdefs -> do
        tk <- tyConKind $ map snd vars
        withTyping (Map.singleton con tk) $ do
            cdefs <- forM cdefs $ \(c, t) -> do
                ty <- instantiateTyping $ inferKind t
                return $ Map.singleton c ty
            withTyping (mconcat cdefs) cont
    ClassDef con [(vn, vark)] cdefs -> do
        cdefs <- forM cdefs $ \(TypeSig n t) -> do
            t <- instantiateTyping $ do
                vark <- inferKind vark
                addConstraint $ CClass (IdN con) $ TVar vark $ IdN vn
                inferKind t
            return $ Map.singleton n t
        withTyping (mconcat cdefs) cont
    DAxiom (TypeSig n t) -> do
        (t, t') <- instantiateTyping'' $ inferKind t
        let res (TArr a b) = res b
            res t = t
            n' = (if isStar $ res t' then toTypeN else id) n
        withTyping (Map.singleton n' t) cont
    InstanceDef c t xs -> do  -- TODO: check types
        (ce, t) <- runWriterT' $ inferKind_ t     -- TODO: ce
        let ce' = Map.filter (either (const False) (const True)) ce
        when (not $ Map.null ce') $ throwErrorTCM $ "not null ce" <+> pShow ce'
        local (\pe -> pe {instanceDefs = Map.alter (Just . maybe (Set.singleton t) (Set.insert t)) c $ instanceDefs pe}) $ do
            cont
--            xs <- forM xs $ \d -> inferDef d
--            foldr ($) cont xs

inference_ :: PolyEnv -> ModuleR -> ErrorT (VarMT Identity) PolyEnv
inference_ penv Module{..} = flip runReaderT penv $ diffEnv <$> inferDefs definitions
  where
    diffEnv p = p {getPolyEnv = getPolyEnv p Map.\\ getPolyEnv penv}

