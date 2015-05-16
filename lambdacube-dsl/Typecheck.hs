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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
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
import GHC.Exts (Constraint)

import Text.Parsec.Pos

import Pretty
import Type

--------------------------------------------------------------------------------

pairsWith f xs = zipWith f xs $ drop 1 xs

unifyMaps_ :: (Ord a) => (a -> Doc) -> [Map a b] -> [WithExplanation [b]]
unifyMaps_ f = map (f *** id) . filter (not . null . drop 1 . snd) . Map.toList . Map.unionsWith (++) . map ((:[]) <$>)

unifyMaps :: (Ord a, PShow a) => [Map a b] -> [WithExplanation [b]]
unifyMaps = unifyMaps_ pShow

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

injType :: TypeFunT -> Maybe (InjType, [Exp])
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

type ConstraintSolvRes = (Subst, [WithExplanation [Exp]])

reduceConstraint :: forall m . (MonadReader PolyEnv m, MonadError ErrorMsg m) => IdN -> ConstraintT -> m ConstraintSolvRes
reduceConstraint a b = reduceConstraint_ a b b

reduceConstraint_ :: forall m . (MonadReader PolyEnv m, MonadError ErrorMsg m) => IdN -> ConstraintT -> ConstraintT -> m ConstraintSolvRes
reduceConstraint_ cvar orig x = do
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

        _ -> maybe noInstance (\s -> maybe (noInstance' s) (\w -> discard w []) $ Map.lookup t s) $ Map.lookup c builtinInstances

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

        TFVec (TENat n) ty | n `elem` [2,3,4] {- && ty `elem'` floatIntWordBool -} -> reduced $ TVec n ty
        TFVec a b -> check (a `matches` nat234 && b `matches` floatIntWordBool {- -- FIXME -}) $ observe res $ \case
            TVec n t -> keep [WithExplanation "Vec res 1" [a, TENat n], WithExplanation "Vec res 2" [b, t]]
            _ -> fail "no instance tfvec"

        TFVecScalar a b -> case a of
            TENat 1 -> case b of
                TVar{} | res `matches` floatIntWordBool -> keep [WithExplanation "VecScalar dim 1" [b, res]]
                b -> check (b `elem'` floatIntWordBool) $ reduced b
            TVar{} -> check (b `matches` floatIntWordBool) $ observe res $ \case
                t | t `elem'` floatIntWordBool -> keep [WithExplanation "VecScalar res 1" [a, TENat 1], WithExplanation "VecScalar res 2" [b, t]]
                TVec n t -> keep [WithExplanation "VecScalar res 1" [a, TENat n], WithExplanation "VecScalar res 2" [b, t]]
                _ -> nothing --like $ TFVec a b
            _ -> like $ TFVec a b

        TFMatVecElem t -> observe t $ \case
            TVec _ t -> reduced t
            TMat _ _ t -> reduced t
            _ -> fail $ "no instance matvecelem" <+> pShow t

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
        like f = reduceConstraint_ cvar x (CEq res f)
        reduced t = discard Refl [WithExplanation "type family reduction" [res, t]]
        check b m = if b then m else fail "no instance (1)"
        fail :: Doc -> m ConstraintSolvRes
        fail = failure . (("error during reduction of" </> pShow res <+> "~" <+> pShow f) </>)

        reduce' = Just . Just
        nothing' = Just Nothing
        fail' = Nothing
        observe' TVar{} _ = nothing'
        observe' x f = f x

        caseTuple :: Doc -> Exp -> ([a] -> m ConstraintSolvRes) -> (Exp -> Maybe (Maybe a)) -> m ConstraintSolvRes
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
    discard w xs = return (singSubst' cvar $ Witness (ConstraintKind orig) w, xs)
    keep xs = return (mempty, xs)
    failure :: Doc -> m ConstraintSolvRes
    failure = throwErrorTCM

    nothing = return mempty
    observe TVar{} _ = nothing
    observe x f = f x

--------------------------------------------------------------------------------

-- unify each types in the sublists
unifyTypes :: forall m . (MonadError ErrorMsg m) => Bool -> [WithExplanation [Exp]] -> m Subst
unifyTypes bidirectional tys = flip execStateT mempty $ forM_ tys $ sequence_ . pairsWith uni . snd
  where
--    uni :: Exp -> Exp -> StateT TEnv TCM ()
    uni a b = gets subst1 >>= \f -> unifyTy (f a) (f b)

    -- make single tvar substitution; check infinite types
    bindVar n t = do
        s <- get
        let t' = subst_ s t
        if n `Set.member` freeVars t'
            then throwErrorTCM $ "Infinite type, type variable" <+> pShow n <+> "occurs in" <+> pShow t'
            else put $ singSubst' n t' <> s

    bindVars (TVar tu u) (TVar tv v)
        | u == v = return ()
        | u < v = bindVar u (TVar tv v)
        | otherwise = bindVar v (TVar tu u)

    unifyTy :: Exp -> Exp -> StateT Subst m ()
    unifyTy (Exp t) (Exp t') = unifyTy' t t'
      where
        unifyTy' (Forall_ (Just a) k t) (Forall_ (Just a') k' t') = uni k k' >>
            -- TODO! protect a in t
            -- uni t (repl (Map.singleton a' a) t')
            bindVars (TVar k a) (TVar k' a') >> uni t t'
        unifyTy' (Forall_ Nothing a1 b1) (Forall_ Nothing a2 b2) = uni a1 a2 >> uni b1 b2
        unifyTy' (EVar_ k u) (EVar_ k' v) = uni k k' >> bindVars (Exp t) (Exp t')
        unifyTy' (EVar_ k u) _ = bindVar u (Exp t')
        unifyTy' _ (EVar_ k v) | bidirectional = bindVar v (Exp t)
        unifyTy' (ELit_ l) (ELit_ l') | l == l' = return ()
        unifyTy' (TCon_ k u) (TCon_ k' v) | u == v = uni k k' >> return ()
        unifyTy' (TTuple_ t1) (TTuple_ t2) = sequence_ $ zipWith uni t1 t2
        unifyTy' (EApp_ k a1 b1) (EApp_ k' a2 b2) = uni k k' >> uni a1 a2 >> uni b1 b2
        unifyTy' Star_ Star_ = return ()
        unifyTy' (TRecord_ xs) (TRecord_ xs') | Map.keys xs == Map.keys xs' = sequence_ $ zipWith uni (Map.elems xs) (Map.elems xs')
        unifyTy' (ConstraintKind_ (CUnify a b)) (ConstraintKind_ (CUnify a' b')) = uni a a' >> uni b b'   -- ???
        unifyTy' (ConstraintKind_ (CClass a b)) (ConstraintKind_ (CClass a' b')) | a == a' = uni b b'   -- ???
        unifyTy' (ConstraintKind_ (CEq a (TypeFun n b))) (ConstraintKind_ (CEq a' (TypeFun n' b'))) | n == n' = uni a a' >> sequence_ (zipWith uni b b')   -- ???
        unifyTy' (ConstraintKind_ (Split a b c)) (ConstraintKind_ (Split a' b' c')) = uni a a' >> uni b b' >> uni c c'   -- ???
        unifyTy' _ _
          | otherwise = throwError $ UnificationError (Exp t) (Exp t') $ filter (not . null . drop 1 . snd) tys

    subst1 = subst_  -- TODO

-- TODO: revise applications
appSES :: (Substitute x, PShow x, Monad m) => TypingT m x -> TypingT m x
appSES (WriterT' m) = WriterT' $ do
    (se, x) <- m
--    trace (ppShow (se,x)) $ 
    return $ (se, subst se x)

removeMonoVars = mapWriterT' $ fmap $ \(en@(TEnv se), (s, x)) -> (TEnv $ foldr Map.delete se $ Set.toList s, subst en x)

runWriterT'' = runWriterT' . appSES

closeSubst (TEnv m) = s where s = TEnv $ subst s <$> m

--  { x = (a, b),         z = x,   z = (b, a)
joinSubsts :: forall m . (MonadError ErrorMsg m) => [TEnv] -> m TEnv
joinSubsts (map getTEnv -> ss) = do
    s <- addCtx "joinSubsts" $ unifyTypes True $ concatMap ff $ unifyMaps ss
    if nullSubst s
        then return $ closeSubst $ TEnv $ Map.unionsWith gg ss
        else joinSubsts [toTEnv s, subst_ s $ TEnv $ Map.unionsWith gg ss]
  where
    gg (ISubst s) _ = ISubst s
    gg _ b = b

    ff (expl, ss) = case ( WithExplanation (expl <+> "subst") [s | ISubst s <- ss]
                         , WithExplanation (expl <+> "typesig") [s | ISig s <- ss]) of 
        (WithExplanation _ [], ss) -> [ss]
        (ss, WithExplanation _ []) -> [ss]
        (subs@(WithExplanation i (s:_)), sigs@(WithExplanation i' (s':_))) -> [subs, sigs, WithExplanation ("subskind" <+> i <+> i') [tyOf s, s']]

joinSE :: forall m . (MonadReader PolyEnv m, MonadError ErrorMsg m) => [TEnv] -> m TEnv
joinSE = \case
    [a, b]
        | Map.null $ getTEnv a -> untilNoUnif b     -- optimization
        | Map.null $ getTEnv b -> untilNoUnif a     -- optimization
    ab -> joinSubsts ab >>= untilNoUnif

untilNoUnif :: forall m . (MonadReader PolyEnv m, MonadError ErrorMsg m) => TEnv -> m TEnv
untilNoUnif es = do
    let cs = [(n, c) | (n, ISig (ConstraintKind c)) <- Map.toList $ getTEnv es]
    (unzip -> (ss, concat -> eqs)) <- sequence $ map (uncurry reduceConstraint) $ cs
    s0 <- addCtx "untilNoUnif" $ unifyTypes True
        -- unify left hand sides where the right hand side is equal:  (t1 ~ F a, t2 ~ F a)  -->  t1 ~ t2
         $ groupByFst [(f, ty) | CEq ty f <- map snd cs]
        -- injectivity test:  (t ~ Vec a1 b1, t ~ Vec a2 b2)  -->  a1 ~ a2, b1 ~ b2
        ++ concatMap (\(s, l) -> map ((,) s) $ transpose l)
                (groupByFst
                [((ty, it), is) | CEq ty (injType -> Just (it, is)) <- map snd cs])
        ++ eqs

        -- TODO nub constraints?
    if all nullSubst (s0: ss) then return es else do
        joinSubsts (toTEnv s0: es: map toTEnv ss) >>= untilNoUnif

instance Monoid' TEnv where
    type MonoidConstraint TEnv m = (MonadReader PolyEnv m, MonadError ErrorMsg m)
    mempty' = mempty
    mappend' a b = joinSE [a, b]

--------------------------------------------------------------------------------

newStarVar :: Doc -> TCMS Exp
newStarVar i = do
    n <- newName i
    let v = TVar Star n
    addConstraints $ singSubstTy n Star
    return v

addConstraints m = WriterT' $ pure (m, ())
addConstraint c = newName "constraint" >>= \n -> addConstraints $ singSubstTy n $ ConstraintKind c

addUnif :: Exp -> Exp -> TCMS ()
addUnif t1 t2 = addConstraint $ t1 ~~~ t2

checkStarKind t = addUnif Star t

star = return Star

----------------------------

instantiateTyping_' :: Bool -> Doc -> TEnv -> Exp -> TCM ([(IdN, Exp)], InstType')
instantiateTyping_' typ info se ty = do
    ambiguityCheck ("ambcheck" <+> info) se ty
--    pe <- asks $ getPolyEnv
    let p n (ISig _) = True --n `Map.notMember` pe
        p _ _ = False
        se' = Map.filterWithKey p $ getTEnv se
        fv = Map.keys se'
    return $ (,) (if typ then [(n, t) | (n, ISig t) <- Map.toList se'] else []) $ \info' -> WriterT' $ do
        newVars <- forM fv $ \case
            TypeN' n i -> newName $ "instvar" <+> info' <+> info <+> text n <+> i
            v -> error $ "instT: " ++ ppShow v
        let s = Map.fromList $ zip fv newVars
        return (TEnv $ repl s se', (if typ then zipWith TVar [repl s x | ISig x <- Map.elems se'] newVars else [], repl s ty))

instantiateTyping' = instantiateTyping_' False

instantiateTyping'' :: Bool -> Doc -> TCMS Exp -> TCM (([(IdN, Exp)], InstType'), Exp)
instantiateTyping'' typ i ty = do
    (se, ty) <- runWriterT'' ty
    x <- instantiateTyping_' typ i se ty
    return (x, ty)

instantiateTyping i = fmap (snd . fst) . instantiateTyping'' False i

--lookEnv :: IdN -> T
lookEnv :: Name -> TCMS ([Exp], Exp) -> TCMS ([Exp], Exp)
lookEnv n m = asks (Map.lookup n . getPolyEnv) >>= maybe m (toTCMS . ($ pShow n))

lookEnv' n m = asks (Map.lookup n . typeFamilies) >>= maybe m (toTCMS . ($ pShow n))

lookEnv'' n = asks (Map.lookup n . classDefs) -- >>= maybe (undefined <$> throwErrorTCM n)

-- Ambiguous: (Int ~ F a) => Int
-- Not ambiguous: (Show a, a ~ F b) => b
--ambiguityCheck :: Doc -> TCMS Exp -> TCMS Exp
ambiguityCheck msg se ty = do
    pe <- asks getPolyEnv
    let
        cs = [(n, c) | (n, ISig c) <- Map.toList $ getTEnv se]
        defined = dependentVars cs $ Map.keysSet pe <> freeVars ty
        def n = n `Set.member` defined || n `Map.member` pe
        ok (n, Star) = True
        ok (n, c) = any def $ Set.insert n $ freeVars c
    case filter (not . ok) cs of
        [] -> return ()
        err -> throwError . ErrorMsg $
            "during" <+> msg </> "ambiguous type:" <$$> pShow se <$$> pShow cs </> "=>" </> pShow ty <$$> "problematic vars:" <+> pShow err

-- compute dependent type vars in constraints
-- Example:  dependentVars [(a, b) ~ F b c, d ~ F e] [c] == [a,b,c]
dependentVars :: [(IdN, Exp)] -> Set TName -> Set TName
dependentVars ie s = cycle mempty s
  where
    cycle acc s
        | Set.null s = acc
        | otherwise = cycle (acc <> s) (grow s Set.\\ acc)

    grow = flip foldMap ie $ \(n, t) -> (Set.singleton n <-> freeVars t) <> case t of
        ConstraintKind c -> case c of
            CEq ty f -> freeVars ty <-> freeVars f
            Split a b c -> freeVars a <-> (freeVars b <> freeVars c)
            CUnify{} -> mempty --error "dependentVars: impossible" 
            CClass{} -> mempty
        _ -> mempty
      where
        a --> b = \s -> if Set.null $ a `Set.intersection` s then mempty else b
        a <-> b = (a --> b) <> (b --> a)


--------------------------------------------------------------------------------

inferKind_ = {-appSES .-} inferKind

-- TODO: ambiguity check
inferKind :: TyR -> TCMS Exp
inferKind ty_@(ExpR r ty) = addRange r $ addCtx ("kind inference of" <+> pShow ty) $ appSES $ case ty of
    Forall_ (Just n) k t -> removeMonoVars $ do
        k <- inferKind k
        addConstraints $ singSubstTy n k
        t <- withTyping (Map.singleton n $ monoInstType n k) $ inferKind t
        return $ (,) (Set.fromList [n]) $ Exp $ Forall_ (Just n) k t
    _ -> do
        ty <- traverse inferKind ty
        k <- case tyOf <$> ty of
            ELit_ l -> return $ inferLit l
            Star_ -> star
            ConstraintKind_ c -> case c of
                CEq t (TypeFun f ts) -> do
                    (_, tf) <- lookEnv' f $ throwErrorTCM $ "Type family" <+> pShow f <+> "is not in scope."
                    addUnif tf $ ts ~~> t
                    star
                CClass _ t -> checkStarKind t >> star           -- TODO
                _ -> star
            TTuple_ ts -> mapM_ checkStarKind ts >> star
            Forall_ Nothing a b -> checkStarKind a >> checkStarKind b >> star
            EApp_ _ tf ta -> appTy tf ta
            EVar_ _ n -> fmap snd . lookEnv n $ newStarVar ("tvar" <+> pShow r) >>= \t -> addConstraints (singSubstTy n t) >> return ([], t)
            TCon_ _ n -> fmap snd . lookEnv n $ lookEnv (toExpN n) $ throwErrorTCM $ "Type constructor" <+> pShow n <+> "is not in scope."
--            x -> error $ " inferKind: " ++ ppShow x
        case ty of
            Forall_ Nothing (ConstraintKind c) b -> do
                addConstraint c
                return b
            _ -> return $ Exp $ mapExp_ (const k) id (error "x1") (error "x2") ty

appTy (TArr ta v) ta' = addUnif ta ta' >> return v      -- optimalization
appTy tf ta = newStarVar ("tapp" <+> pShow tf <+> "|" <+> pShow ta) >>= \v -> addUnif tf (ta ~> v) >> return v

inferPatTyping :: Bool -> PatR -> TCMS (Pat, InstEnv)
inferPatTyping polymorph p_@(PatR pt p) = addRange pt $ addCtx ("type inference of pattern" <+> pShow p_) $ case p of

  PVar_ () n -> do
        t <- newStarVar ("pvar" <+> pShow n)
        addConstraints $ singSubstTy n t
        let tr = Map.singleton n $ monoInstType n t
        return (PVar t n, tr)
  _ -> do
    p <- traverse (inferPatTyping polymorph) p
    (res, tr) <- case p of
      PCon_ () n ps -> do
            (_, tn) <- lookEnv n $ lift $ throwErrorTCM $ "Constructor" <+> pShow n <+> "is not in scope."
            v <- newStarVar "pcon"
            addUnif tn $ map (tyOfPat . fst) ps ~~> v
            return (PCon v n $ fst <$> ps, mempty)
      _ -> do
       (t, tr) <- case p of
        PLit_ n -> noTr $ pure $ inferLit n
        Wildcard_ () -> noTr $ newStarVar "_" >>= pure

        PAt_ n p -> addTr (\t -> Map.singleton n $ monoInstType n t) $ pure $ tyOfPat . fst $ p

        PTuple_ ps -> noTr $ pure $ TTuple $ map (tyOfPat . fst) ps

        PRecord_ (unzip -> (fs, ps)) -> noTr $ do
            v <- newStarVar "pfp2"
            v' <- newStarVar "pfp3"
            addConstraint $ Split v v' $ TRecord $ Map.fromList $ zip fs $ map (tyOfPat . fst) ps
            return v
       return (Pat $ mapPat (const t) id id $ fst <$> p, tr)

    let trs = Map.unionsWith (++) . map ((:[]) <$>) $ tr: map snd (toList p)
    tr <- case filter (not . null . drop 1 . snd) $ Map.toList trs of
        [] -> return $ Map.map head trs
        ns -> lift $ throwErrorTCM $ "conflicting definitions for" <+> pShow (map fst ns)
    return (res, tr)
  where
    noTr = addTr $ const mempty
    addTr tr m = (\x -> (x, tr x)) <$> m

eLam (n, t) e = ELam (PVar t n) e

inferTyping :: ExpR -> TCMS Exp
inferTyping e_@(ExpR r e) = addRange r $ addCtx ("type inference of" <+> pShow e_) $ appSES $ case e of

    -- hack
    ENamedRecord_ n (unzip -> (fs, es)) ->
        inferTyping $ foldl (EAppR' mempty) (EVarR' mempty n) es

    ELam_ p f -> removeMonoVars $ do
        (p, tr) <- inferPatTyping False p
        f <- addCtx "?" $ withTyping tr $ inferTyping f
        return $ (,) (Map.keysSet tr) $ ELam p f

    ELet_ (PVar' _ n) x_ e -> do
        ((fs, it), x, se) <- lift $ do
            (se, x) <- runWriterT'' $ inferTyping x_
            it <- addRange (getTag x_) $ addCtx "let" $ instantiateTyping_' True (pShow n) se $ tyOf x
            return (it, x, se)
        addConstraints $ TEnv $ Map.filter (eitherItem (const True) (const False)) $ getTEnv se
        e <- withTyping (Map.singleton n it) $ inferTyping e
        return $ ELet (PVar (tyOf x) n) (foldr eLam x fs) e
    ELet_ p x e -> removeMonoVars $ do          -- monomorph let; TODO?
        x <- inferTyping x
        (p, tr) <- inferPatTyping False p
        addUnif (tyOf x) (tyOfPat p)
        e <- withTyping tr $ inferTyping e
        return $ (,) (Map.keysSet tr) $ ELet p x e
    ETypeSig_ e ty -> do
        e <- inferTyping e
        ty <- inferKind ty
        addUnif (tyOf e) ty  -- TODO: one directional
        return e
    EType_ ta -> do
        t <- inferKind ta
        return $ EType t
    EVar_ _ n -> do
        (ty, t) <- lookEnv n $ lift $ throwErrorTCM $ "Variable" <+> pShow n <+> "is not in scope."
        return $ buildApp (`TVar` n) t $ map EType ty
    _ -> do
        e <- traverse inferTyping e
        t <- case e of
            EApp_ _ tf (EType t) -> do
                x <- newName "apptype"
                addUnif t (TVar (tyOf t) x)
                v <- newStarVar "etyapp"
                addUnif (tyOf tf) (Forall x (tyOf t) v)
                return v
            EApp_ _ tf ta -> appTy (tyOf tf) (tyOf ta)
            EFieldProj_ () fn -> do
                a <- newStarVar "fp1"
                r <- newStarVar "fp2"
                r' <- newStarVar "fp3"
                addConstraint $ Split r r' $ TRecord $ Map.singleton (IdN fn) a
                return $ r ~> a
            ERecord_ (unzip -> (fs, es)) -> return $ TRecord $ Map.fromList $ zip fs $ map tyOf es
            ENamedRecord_ n (unzip -> (fs, es)) -> do -- TODO: handle field names
                (_, t) <- lookEnv n $ throwErrorTCM $ "Variable" <+> pShow n <+> "is not in scope."
                v <- newStarVar "namedrecord"
                addUnif t $ foldr (~>) v $ map tyOf es
                return v
            ETuple_ te -> return $ TTuple $ map tyOf te
            ELit_ l -> return $ inferLit l
            EAlts_ _ xs -> newStarVar "ealts" >>= \v -> mapM_ (addUnif v . tyOf) xs >> return v
            ENext_ () -> newStarVar "enext"          -- TODO: review
            x -> error $ "inferTyping: " ++ ppShow x
        return $ Exp $ mapExp_ (const t) (error "e1") (error "e2") (error "e3") e

--------------------------------------------------------------------------------

-- TODO: review applications of this
typingToTy :: TEnv -> Exp -> Exp
typingToTy env ty = foldr forall_ ty $ orderEnv env
  where
    forall_ (n, k) t = Forall n k t

    -- TODO: make more efficient
    orderEnv :: TEnv -> [(IdN, Exp)]
    orderEnv env = f mempty [(n, t) | (n, ISig t) <- Map.toList $ getTEnv env]
      where
        f :: Set IdN -> [(IdN, Exp)] -> [(IdN, Exp)]
        f s [] = []
        f s ts = case [x | x@((n, t), ts') <- getOne ts, freeVars t `Set.isSubsetOf` s] of
            (((n, t), ts):_) -> (n, t): f (Set.insert n s) ts
            _ -> error $ show $ "orderEnv:" <+> pShow ty
        getOne xs = [(b, a ++ c) | (a, b: c) <- zip (inits xs) (tails xs)]


--------------------------------------------------------------------------------

tyConKind :: [TyR] -> TCM InstType'
tyConKind = tyConKind_ $ ExpR mempty Star_

tyConKind_ :: TyR -> [TyR] -> TCM InstType'
tyConKind_ res vs = instantiateTyping "tyconkind" $ foldr (liftA2 (~>)) (inferKind res) $ map inferKind vs

mkInstType v k = \d -> WriterT' $ pure (singSubstTy v k, ([], k))
monoInstType v k = \d -> WriterT' $ pure (mempty, ([], k))

inferConDef :: Name -> [(Name, TyR)] -> WithRange ConDef -> TCM InstEnv
inferConDef con (unzip -> (vn, vt)) (r, ConDef n tys) = addRange r $ do
    ty <- instantiateTyping (pShow con) $ do
        ks <- mapM inferKind vt
        withTyping (Map.fromList $ zip vn $ zipWith mkInstType vn ks) $ do
            let tyConResTy :: TCMS Exp
                tyConResTy
                    = inferKind $ foldl app (ExpR mempty $ TCon_ () con) $ map (ExpR mempty . EVar_ ()) vn
                  where
                    app a b = ExpR mempty $ EApp_ () a b

            foldr (liftA2 (~>)) tyConResTy $ map inferFieldKind tys
    return $ Map.singleton n ty
  where
    inferFieldKind (FieldTy mn t) = inferKind t

selectorDefs :: DefinitionR -> [DefinitionR]
selectorDefs (r, DDataDef n _ cs) =
    [ (r, DValueDef $ ValueDef
      ( PVar' mempty sel)
      ( ELamR' mempty
            (PCon' mempty cn
                [ if sel == sel' then PVar' mempty (ExpN "x") else PatR mempty (Wildcard_ ())
                | FieldTy (Just sel') _ <- tys]
            )
            (EVarR' mempty $ ExpN "x")
      ))
    | (rc, ConDef cn tys) <- cs
    , FieldTy (Just sel) _ <- tys
    ]

inferDef :: ValueDefR -> TCM (TEnv, TCM a -> TCM a)
inferDef (ValueDef p@(PVar' _ n) e) = do
    (se, exp) <- runWriterT'' $ removeMonoVars $ do
        tn@(TVar _ tv) <- newStarVar $ pShow n
        addConstraints $ singSubstTy n tn
        exp <- withTyping (Map.singleton n $ monoInstType n tn) $ inferTyping e
        addUnif tn $ tyOf exp
        return $ (,) (Set.fromList [n, tv]) exp
    (fs, f) <- addCtx ("inst" <+> pShow n) $ instantiateTyping_' True (pShow n) se $ tyOf exp
    the <- asks thunkEnv
    let th = th' where
         th' = subst the
           $ subst
                ( singSubst n $ foldl (TApp (error "et")) th' $ map (\(n, t) -> EType $ TVar t n) fs
                )
           $ flip (foldr eLam) fs exp
    return (singSubst n th, withTyping $ Map.singleton n f)

-- non recursive
inferDef' :: InstType -> ValueDefR -> TCM (Env' Exp, TCM a -> TCM a)
inferDef' ty (ValueDef p@(PVar' _ n) e) = do
    (se, (exp, (te, fs))) <- runWriterT'' $ removeMonoVars $ do
        (fn, t) <- toTCMS ty
        tn@(TVar _ tv) <- newStarVar $ pShow n
        addConstraints $ singSubstTy n tn
        exp <- inferTyping e
        addUnif t $ tyOf exp
        return $ (,) (Set.fromList [n, tv]) (exp, (tyOf exp, fn))
    (_fs, f) <- addCtx ("inst" <+> pShow n) $ instantiateTyping_' True (pShow n) se te
    -- TODO: unify fs - _fs
    the <- asks thunkEnv
    let th = recEnv (PVar undefined n) $ subst the        -- recEnv not needed?
            $ flip (foldr eLam) [(n, v) | v@(TVar _ n) <- fs] exp
    return (Map.singleton n th, withTyping $ Map.singleton n f)

inferDefs :: [DefinitionR] -> TCM PolyEnv
inferDefs [] = ask
inferDefs (dr@(r, d): ds@(inferDefs -> cont)) = case d of
    DValueDef d -> do
        (e, f) <- addRange r (inferDef d)
        addPolyEnv (emptyPolyEnv {thunkEnv = e}) $ f cont
    TypeFamilyDef con vars res -> do
        tk <- tyConKind_ res $ map snd vars
        addPolyEnv (emptyPolyEnv {typeFamilies = Map.singleton con tk}) cont
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
                ty <- instantiateTyping ("GADT" <+> pShow c) $ inferKind t
                return $ Map.singleton c ty
            withTyping (mconcat cdefs) cont
    ClassDef con [(vn, vark)] cdefs -> do
        (unzip -> (ths, cdefs)) <- forM cdefs $ \(TypeSig n t_) -> do
            ((fs, t), _) <- instantiateTyping'' True (pShow n) $ do
                vark <- inferKind vark
                addConstraint $ CClass (IdN con) $ TVar vark $ IdN vn
                inferKind t_
            let find = head $ [i | (i, ConstraintKind (CClass n _)) <- zip [0..] $ map snd fs, n == con] ++ error (show $ "classDef:" <+> showVar con <$$> pShow fs <$$> pShow t_)
--            let t' = (mapWriterT' ((id *** ((ConstraintKind cstr:) *** id)) <$>)) <$> t
--                rearrange cs = head [b: as ++ bs | (as, b@(_, ConstraintKind _): bs) <- zip (inits cs) (tails cs)]
            return (singSubst n $ Exp $ ExtractInstance [] find n, Map.singleton n t)
        addPolyEnv (emptyPolyEnv {thunkEnv = mconcat ths, classDefs = Map.singleton con $ ClassD $ mconcat cdefs}) $ withTyping (mconcat cdefs) cont
    DAxiom (TypeSig n t) -> do
        ((_, t), t') <- instantiateTyping'' False (pShow n) $ inferKind t
        let res (TArr a b) = res b
            res t = t
            n' = (if isStar $ res t' then toTypeN else id) n
            isPrim (ExpN s) = take 4 s == "prim"
            arity = f t' where
                f (TArr _ x) = 1 + f x
                f _ = 0
            f | isPrim n = addPolyEnv (emptyPolyEnv {thunkEnv = singSubst n $ Exp $ PrimFun n [] arity})
              | otherwise = id
        f $ withTyping (Map.singleton n' t) cont
    InstanceDef c t xs -> do  -- TODO: check types
        (ClassD cs) <- lookEnv'' c >>= maybe (throwErrorTCM "can't find class") return
        (ce, t) <- runWriterT'' $ inferKind_ t     -- TODO: ce
--        let ce' = Map.filter (either (const False) (const True)) ce
--        when (not $ Map.null ce') $ throwErrorTCM $ "not null ce" <+> pShow ce'
        xs <- local (\pe -> pe {instanceDefs = Map.alter (Just . maybe (Map.singleton t Refl) (Map.insert t Refl)) c $ instanceDefs pe}) -- fake
                $ forM xs $ \x@(ValueDef (PVar' _ n)  _) -> do
            inferDef' (cs Map.! n $ "instance") x
        let w = WInstance $ mconcat $ map fst xs
        local (\pe -> pe {instanceDefs = Map.alter (Just . maybe (Map.singleton t w) (Map.insert t w)) c $ instanceDefs pe}) $ do
--            foldr ($) cont xs
            cont

inference_ :: PolyEnv -> ModuleR -> ErrorT (VarMT Identity) PolyEnv
inference_ penv@PolyEnv{..} Module{..} = flip runReaderT penv $ diffEnv <$> inferDefs definitions
  where
    diffEnv (PolyEnv i c g p (TEnv th) tf) = PolyEnv
        (Map.differenceWith (\a b -> Just $ a Map.\\ b) i instanceDefs)
        (c Map.\\ classDefs)
        (g Map.\\ getPolyEnv)
        (p Map.\\ precedences)
        (TEnv $ th Map.\\ getTEnv thunkEnv)
        (tf Map.\\ typeFamilies)

{-
class Num' a where
    fromInt' :: (Read a, Show x) => Int -> [x] -> a

data Dict (c :: Constraint) = c => D

fromInt'' :: forall a x . (Num' a, Read a, Show x) => Int -> [x] -> a
fromInt'' = fromInt_ (D :: Dict (Num' a))

fromInt_ :: Dict (Num' a) -> forall a x . (Num' a, Read a, Show x) => Int -> [x] -> a
fromInt_ D = fromInt'

primIntToFloat :: (Show c) => Int -> c -> Float
primIntToFloat = undefined

instance Num' Float where
    fromInt' = primIntToFloat
-----------------------------------------------------
given:  a :: *, cn :: Num a, cc :: Read a, x :: *, cx :: Show x  |  Int -> [x] -> a

needed:            c :: *,  cd :: Show c    | Int -> c -> Float

result:
    c  :=  [x]
    cd :=  dictShowList cx

-}

