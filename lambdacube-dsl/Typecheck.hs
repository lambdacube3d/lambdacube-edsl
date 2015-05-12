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

    -- make single tvar substitution; check infinite types
    bindVar n t = do
        s <- get
        let t' = subst' s (Set.singleton n) t
        if n `Set.member` freeVars t'
            then throwErrorTCM $ "Infinite type, type variable" <+> pShow n <+> "occurs in" <+> pShow t'
            else put $ Map.insert n t'{-t-} $ singSubst n t' <$> s
      where
        subst' st acc = recsubst r1 r2 where
            r2 n = subst' (Map.delete n st) acc
            r1 def a
--                    | Set.member a acc = error "cycle"
                | Just t <- Map.lookup a st = t --subst' st (Set.insert a acc) t
                | otherwise = def

        singSubst n t = recsubst r1 r2 where
            r2 n' | n /= n' = singSubst n t
                  | otherwise = id
            r1 def a
                | a == n = t
                | otherwise = def

    unifyTy :: Ty -> Ty -> StateT Subst m ()
    unifyTy a@(StarToStar i) b@(StarToStar j)
        | i == j = return ()
        | otherwise = throwError $ UnificationError a b $ filter (not . null . drop 1 . snd) tys
    unifyTy a@(Ty__ k t) b@(Ty__ k' t') = uni k k' >> unifyTy' t t'
      where
        bindVars a@(TVar _ u) b@(TVar _ v)
            | u == v = return ()
            | u < v = bindVar u b
            | otherwise = bindVar v a

        unifyTy' (Forall_ (Just a) k t) (Forall_ (Just a') k' t') = uni k k' >>
            -- TODO! protect a in t
            -- uni t (repl (Map.singleton a' a) t')
            bindVars (TVar k a) (TVar k' a') >> uni t t'
        unifyTy' (Forall_ Nothing a1 b1) (Forall_ Nothing a2 b2) = uni a1 a2 >> uni b1 b2
        unifyTy' (TVar_ u) (TVar_ v) = bindVars a b
        unifyTy' (TVar_ u) _ = bindVar u b
        unifyTy' _ (TVar_ v) | bidirectional = bindVar v a
        unifyTy' (TLit_ l) (TLit_ l') | l == l' = return ()
        unifyTy' (TCon_ u) (TCon_ v) | u == v = return ()
        unifyTy' (TTuple_ t1) (TTuple_ t2) = sequence_ $ zipWith uni t1 t2
        unifyTy' (TApp_ a1 b1) (TApp_ a2 b2) = uni a1 a2 >> uni b1 b2
        unifyTy' Star_ Star_ = return ()
        unifyTy' _ _
          | otherwise = throwError $ UnificationError a b $ filter (not . null . drop 1 . snd) tys

    subst1 :: Subst -> Ty -> Ty
    subst1 s tv@(TVar _ a) = fromMaybe tv $ Map.lookup a s
    subst1 _ t = t

-- TODO: revise applications
appSES :: (Substitute x, Monad m) => TypingT m x -> TypingT m x
appSES (WriterT' m) = WriterT' $ do
    (se, x) <- m
    return (either (Left . substEnvSubst se) (Right . substEnvSubst se) <$> se, substEnvSubst se x)

removeMonoVars = mapWriterT' $ fmap $ \(se, (s, x)) -> (foldr Map.delete se $ Set.toList s, substEnvSubst se x)

runWriterT'' = runWriterT' . appSES

--  {x |-> Float,  z |-> x}  {z |-> y}
--  {x |-> y}

joinSE :: forall m . (MonadReader PolyEnv m, MonadError ErrorMsg m) => [SubstEnv] -> m SubstEnv
joinSE = \case
    [a, b]
        | Map.null a -> untilNoUnif b     -- optimization
        | Map.null b -> untilNoUnif a     -- optimization
    ab -> do
        s <- addCtx "joinSE" $ unifyTypes True $ concatMap ff $ unifyMaps_ showVar ab
        untilNoUnif $ appSubst s $ Map.unionsWith gg ab
  where
    joinSubsts :: forall m . (MonadError ErrorMsg m) => [Subst] -> m Subst
    joinSubsts ss = do
        s <- addCtx "joinSubsts" $ unifyTypes True $ unifyMaps ss
        return $ mconcat $ ((subst s <$>) <$> ss)

    appSubst :: Subst -> SubstEnv -> SubstEnv
    appSubst s e = either (Left . substEnvSubst e') (Right . substEnvSubst e') <$> e' where e' = (Left <$> s) <> e

    gg (Left s) _ = Left s
    gg Right{} b = b

    ff (expl, ss) = case ( WithExplanation (expl <+> "subst") [s | Left s <- ss]
                         , WithExplanation (expl <+> "typesig") [s | Right s <- ss]) of 
        (WithExplanation _ [], ss) -> [ss]
        (ss, WithExplanation _ []) -> [ss]
        (subs@(WithExplanation i (s:_)), sigs@(WithExplanation i' (s':_))) -> [subs, sigs, WithExplanation ("subskind" <+> i <+> i') [kindOf s, s']]

    untilNoUnif :: SubstEnv -> m SubstEnv
    untilNoUnif es = do
        let cs = [(n, c) | (n, Right (ConstraintKind c)) <- Map.toList es]
        (unzip -> (ss, concat -> eqs)) <- sequence $ map (uncurry reduceConstraint) $ cs
        s0 <- addCtx "untilNoUnif" $ unifyTypes True
            -- unify left hand sides where the right hand side is equal:  (t1 ~ F a, t2 ~ F a)  -->  t1 ~ t2
             $ groupByFst [(f, ty) | CEq ty f <- map snd cs]
            -- injectivity test:  (t ~ Vec a1 b1, t ~ Vec a2 b2)  -->  a1 ~ a2, b1 ~ b2
            ++ concatMap (\(s, l) -> map ((,) s) $ transpose l)
                    (groupByFst
                    [((ty, it), is) | CEq ty (injType -> Just (it, is)) <- map snd cs])
            ++ eqs

        s <- joinSubsts $ s0: ss
            -- TODO nub constraints?
        if Map.null s then return es else untilNoUnif $ appSubst s es

instance Monoid' SubstEnv where
    type MonoidConstraint SubstEnv m = (MonadReader PolyEnv m, MonadError ErrorMsg m)
    mempty' = mempty
    mappend' a b = joinSE [a, b]

--------------------------------------------------------------------------------

newStarVar :: Doc -> TCMS Ty
newStarVar i = do
    n <- newName i
    let v = TVar Star n
    addConstraints $ Map.singleton n $ Right Star
    return v

addConstraints m = WriterT' $ pure (m, ())
addConstraint c = newName "constraint" >>= \n -> addConstraints $ Map.singleton n $ Right $ ConstraintKind c

addUnif :: Ty -> Ty -> TCMS ()
addUnif t1 t2 = addConstraint $ t1 ~~~ t2

checkStarKind t = addUnif Star t

star = return Star

----------------------------

instantiateTyping_' :: Bool -> Doc -> SubstEnv -> Ty -> TCM ([(IdN, Ty)], InstType')
instantiateTyping_' typ info se ty = do
    ambiguityCheck ("ambcheck" <+> info) se ty
--    pe <- asks $ getPolyEnv
    let p n (Right _) = True --n `Map.notMember` pe
        p _ _ = False
        se' = Map.filterWithKey p se
        fv = Map.keys se'
    return $ (,) (if typ then [(n, t) | (n, Right t) <- Map.toList se'] else []) $ \info' -> WriterT' $ do
        newVars <- forM fv $ \case
            TypeN' n i -> newName $ "instvar" <+> info' <+> info <+> text n <+> i
            v -> error $ "instT: " ++ ppShow v
        let s = Map.fromList $ zip fv newVars
        return (repl s se', (if typ then zipWith TVar [repl s x | Right x <- Map.elems se'] newVars else [], repl s ty))

instantiateTyping' = instantiateTyping_' False

instantiateTyping'' :: Bool -> Doc -> TCMS Ty -> TCM (([(IdN, Ty)], InstType'), Ty)
instantiateTyping'' typ i ty = do
    (se, ty) <- runWriterT'' ty
    x <- instantiateTyping_' typ i se ty
    return (x, ty)

instantiateTyping i = fmap (snd . fst) . instantiateTyping'' False i

--lookEnv :: IdN -> T
lookEnv :: Name -> TCMS ([Ty], Ty) -> TCMS ([Ty], Ty)
lookEnv n m = asks (Map.lookup n . getPolyEnv) >>= maybe m (toTCMS . ($ pShow n))

lookEnv' n m = asks (Map.lookup n . typeFamilies) >>= maybe m (toTCMS . ($ pShow n))

lookEnv'' n = asks (Map.lookup n . classDefs) -- >>= maybe (undefined <$> throwErrorTCM n)

-- Ambiguous: (Int ~ F a) => Int
-- Not ambiguous: (Show a, a ~ F b) => b
--ambiguityCheck :: Doc -> TCMS Ty -> TCMS Ty
ambiguityCheck msg se ty = do
    pe <- asks getPolyEnv
    let
        cs = [(n, c) | (n, Right c) <- Map.toList se]
        defined = dependentVars cs $ Map.keysSet pe <> freeVars ty
        def n = n `Set.member` defined || n `Map.member` pe
        ok (n, c) = any def $ Set.insert n $ freeVars c
    case filter (not . ok) cs of
        [] -> return ()
        err -> throwError . ErrorMsg $
            "during" <+> msg </> "ambiguous type:" <$$> pShow se <$$> pShow cs </> "=>" </> pShow ty <$$> "problematic vars:" <+> pShow err

-- compute dependent type vars in constraints
-- Example:  dependentVars [(a, b) ~ F b c, d ~ F e] [c] == [a,b,c]
dependentVars :: [(IdN, Ty)] -> Set TName -> Set TName
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
    Forall_ (Just n) k t -> removeMonoVars $ do
        k <- inferKind k
        addConstraints $ Map.singleton n $ Right k
        t <- withTyping (Map.singleton n $ monoInstType n k) $ inferKind t
        return $ (,) (Set.fromList [n]) $ Ty Star $ Forall_ (Just n) k t
    _ -> do
        ty <- traverse inferKind ty
        k <- case kindOf <$> ty of
            TLit_ l -> return $ inferLit l
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
            TApp_ tf ta -> appTy tf ta
            TVar_ n -> fmap snd . lookEnv n $ newStarVar ("tvar" <+> pShow r) >>= \t -> addConstraints (Map.singleton n $ Right t) >> return ([], t)
            TCon_ n -> fmap snd . lookEnv n $ lookEnv (toExpN n) $ throwErrorTCM $ "Type constructor" <+> pShow n <+> "is not in scope."
--            x -> error $ " inferKind: " ++ ppShow x
        case ty of
            Forall_ Nothing (ConstraintKind c) b -> do
                addConstraint c
                return b
            _ -> return $ Ty k ty

appTy (TArr ta v) ta' = addUnif ta ta' >> return v      -- optimalization
appTy tf ta = newStarVar "tapp" >>= \v -> addUnif tf (ta ~> v) >> return v

inferPatTyping :: Bool -> PatR -> TCMS ((Pat, Ty), InstEnv)
inferPatTyping polymorph p_@(Pat pt p) = addRange pt $ addCtx ("type inference of pattern" <+> pShow p_) $ case p of

  PVar_ n -> do
        t <- newStarVar "pvar"
        addConstraints $ Map.singleton n $ Right t
        let tr = Map.singleton n $ monoInstType n t
        return ((Pat'' $ PVar_ $ VarE n {- $ trace'-} t, t), tr)
  _ -> do
    p <- traverse (inferPatTyping polymorph) p
    (res, t, tr) <- case p of
      PCon_ n ps -> do
            (_, tn) <- lookEnv n $ lift $ throwErrorTCM $ "Constructor" <+> pShow n <+> "is not in scope."
            v <- newStarVar "pcon"
            addUnif tn (map (snd . fst) ps ~~> v)
            return (Pat'' $ PCon_ (n, tn) $ fst . fst <$> ps, v, mempty)
      _ -> do
       (t, tr) <- case p of
        PLit_ n -> noTr $ pure $ inferLit n
        Wildcard_ -> noTr $ newStarVar "_" >>= \t -> return t

        PAt_ n p -> addTr (\t -> Map.singleton n $ monoInstType n t) $ pure $ snd . fst $ p

        PTuple_ ps -> noTr $ pure $ TTuple $ map (snd . fst) ps

        PRecord_ (unzip -> (fs, ps)) -> noTr $ do
            v <- newStarVar "pfp2"
            v' <- newStarVar "pfp3"
            addConstraint $ Split v v' $ TRecord $ Map.fromList $ zip fs $ map (snd . fst) ps
            return v
       return (Pat'' $ mapPat (error "p1") (`VarE` error "p2") $ fst . fst <$> p, t, tr)

    let trs = Map.unionsWith (++) . map ((:[]) <$>) $ tr: map snd (toList p)
    tr <- case filter (not . null . drop 1 . snd) $ Map.toList trs of
        [] -> return $ Map.map head trs
        ns -> lift $ throwErrorTCM $ "conflicting definitions for" <+> pShow (map fst ns)
    return ((res, t), tr)
  where
    noTr = addTr $ const mempty
    addTr tr m = (\x -> (x, tr x)) <$> m

eLam (n, t) e = ELam' mempty (PVar $ VarE n t) e

inferTyping :: ExpR -> TCMS (Thunk, Ty)
inferTyping e_@(Exp r e) = addRange r $ addCtx ("type inference of" <+> pShow e_) $ appSES $ case e of

    -- hack
    ENamedRecord_ n (unzip -> (fs, es)) ->
        inferTyping $ foldl (EApp' mempty) (EVar' mempty n) es

    ELam_ p f -> removeMonoVars $ do
        ((p, tp), tr) <- inferPatTyping False p
        (f, t) <- addCtx "?" $ withTyping tr $ inferTyping f
        return $ (,) (Map.keysSet tr) (ELam' mempty p f, tp ~> t)

    ELet_ (PVar' _ n) x_ e -> do
        ((fs, it), x, tx, se) <- lift $ do
            (se, (x, tx)) <- runWriterT'' $ inferTyping x_
            it <- addRange (getTag x_) $ addCtx "let" $ instantiateTyping_' True (pShow n) se tx
            return (it, x, tx, se)
        addConstraints $ Map.filter isLeft se
        (e, t) <- withTyping (Map.singleton n it) $ inferTyping e
        return (ELet' mempty (PVar $ VarE (IdN n) tx) (foldr eLam x fs) e, t)
    ELet_ p x e -> removeMonoVars $ do          -- monomorph let; TODO?
        (x, tx) <- inferTyping x
        ((p, tp), tr) <- inferPatTyping False p
        addUnif tx tp
        (e, t) <- withTyping tr $ inferTyping e
        return $ (,) (Map.keysSet tr) (ELet' mempty p x e, t)
    ETypeSig_ e ty -> do
        (e, te) <- inferTyping e
        ty <- inferKind ty
        addUnif te ty  -- TODO: one directional
        return (e, te)
    EType_ ta -> do
        t <- inferKind ta
        return (EType' mempty t, kindOf t)
    EVar_ n -> do
        (ty, t) <- lookEnv n $ lift $ throwErrorTCM $ "Variable" <+> pShow n <+> "is not in scope."
        return (foldl (EApp' mempty) (EVar' mempty $ VarE (IdN n) $ foldr TArr t ty) $ map (EType' mempty) ty, t)
    _ -> do
        e <- traverse inferTyping e
        t <- case e of
            EApp_ (_, tf) (EType' _ t, ta) -> do
                x <- newName "apptype"
                addUnif t (TVar ta x)
                v <- newStarVar "etyapp"
                addUnif tf (Forall x ta v)
                return v
            EApp_ (_, tf) (_, ta) -> appTy tf ta
            EFieldProj_ fn -> do
                a <- newStarVar "fp1"
                r <- newStarVar "fp2"
                r' <- newStarVar "fp3"
                addConstraint $ Split r r' $ TRecord $ Map.singleton (IdN fn) a
                return $ r ~> a
            ERecord_ (unzip -> (fs, es)) -> return $ TRecord $ Map.fromList $ zip (map IdN fs) $ map snd es
            ENamedRecord_ n (unzip -> (fs, es)) -> do -- TODO: handle field names
                (_, t) <- lookEnv n $ throwErrorTCM $ "Variable" <+> pShow n <+> "is not in scope."
                v <- newStarVar "namedrecord"
                addUnif t $ foldr (~>) v $ map snd es
                return v
            ETuple_ te -> return $ TTuple $ map snd te
            ELit_ l -> return $ inferLit l
            EAlts_ _ xs -> newStarVar "ealts" >>= \v -> mapM_ (addUnif v . snd) xs >> return v
            ENext_ -> newStarVar "enext"          -- TODO: review
            x -> error $ "inferTyping: " ++ ppShow x
        return (Exp mempty $ mapExp (error "e1") (error "e2") (error "e3") $ fst <$> e, t)

--------------------------------------------------------------------------------

-- TODO: review applications of this
typingToTy :: SubstEnv -> Ty -> Ty
typingToTy env ty = foldr forall_ ty $ orderEnv env
  where
    forall_ (n, k) t = Forall n k t

    -- TODO: make more efficient
    orderEnv :: SubstEnv -> [(IdN, Ty)]
    orderEnv env = f mempty [(n, t) | (n, Right t) <- Map.toList env]
      where
        f :: Set IdN -> [(IdN, Ty)] -> [(IdN, Ty)]
        f s [] = []
        f s ts = case [x | x@((n, t), ts') <- getOne ts, freeVars t `Set.isSubsetOf` s] of
            (((n, t), ts):_) -> (n, t): f (Set.insert n s) ts
            _ -> error $ show $ "orderEnv:" <+> pShow ty
        getOne xs = [(b, a ++ c) | (a, b: c) <- zip (inits xs) (tails xs)]


--------------------------------------------------------------------------------

tyConKind :: [TyR] -> TCM InstType'
tyConKind = tyConKind_ $ Ty' mempty Star_

tyConKind_ :: TyR -> [TyR] -> TCM InstType'
tyConKind_ res vs = instantiateTyping "tyconkind" $ foldr (liftA2 (~>)) (inferKind res) $ map inferKind vs

mkInstType v k = \d -> WriterT' $ pure (Map.singleton v $ Right k, ([], k))
monoInstType v k = \d -> WriterT' $ pure (mempty, ([], k))

inferConDef :: Name -> [(Name, TyR)] -> WithRange ConDef -> TCM InstEnv
inferConDef con (unzip -> (vn, vt)) (r, ConDef n tys) = addRange r $ do
    ty <- instantiateTyping (pShow con) $ do
        ks <- mapM inferKind vt
        withTyping (Map.fromList $ zip vn $ zipWith mkInstType vn ks) $ do
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

inferDef :: ValueDefR -> TCM (EnvMap, TCM a -> TCM a)
inferDef (ValueDef p@(PVar' _ n) e) = do
    (se, (exp, te)) <- runWriterT'' $ removeMonoVars $ do
        tn@(TVar _ tv) <- newStarVar $ pShow n
        addConstraints $ Map.singleton n $ Right tn
        (exp, te) <- withTyping (Map.singleton n $ monoInstType n tn) $ inferTyping e
        return $ (,) (Set.fromList [n, tv]) (exp, te) 
    (fs, f) <- addCtx ("inst" <+> pShow n) $ instantiateTyping_' True (pShow n) se te
    the <- asks thunkEnv
    let th = recEnv (PVar $ VarE n undefined) $ applyEnvBefore (TEnv mempty the)
            $ flip (foldr eLam) fs
            $ applyEnvBefore
                ( TEnv mempty $ Map.singleton n $ Just
                $ foldl (EApp' mempty) (EVar' mempty $ VarE n $ error "ev") $ map (\(n, t) -> EType' mempty $ TVar t n) fs
                ) exp
    return (Map.singleton n $ Just th, withTyping $ Map.singleton n f)

-- non recursive
inferDef' :: InstType -> ValueDefR -> TCM (EnvMap, TCM a -> TCM a)
inferDef' ty (ValueDef p@(PVar' _ n) e) = do
    (se, (exp, (te, fs))) <- runWriterT'' $ removeMonoVars $ do
        (fn, t) <- toTCMS ty
        tn@(TVar _ tv) <- newStarVar $ pShow n
        addConstraints $ Map.singleton n $ Right tn
        (exp, te) <- inferTyping e
        addUnif t te
        return $ (,) (Set.fromList [n, tv]) (exp, (te, fn))
    (_fs, f) <- addCtx ("inst" <+> pShow n) $ instantiateTyping_' True (pShow n) se te
    -- TODO: unify fs - _fs
    the <- asks thunkEnv
    let th = recEnv (PVar $ VarE n undefined) $ applyEnvBefore (TEnv mempty the)        -- recEnv not needed?
            $ flip (foldr eLam) [(n, v) | v@(TVar    _ n) <- fs] exp
    return (Map.singleton n $ Just th, withTyping $ Map.singleton n f)

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
        (unzip -> (ths, cdefs)) <- forM cdefs $ \(TypeSig n t) -> do
            ((fs, t), _) <- instantiateTyping'' False (pShow n) $ do
                vark <- inferKind vark
                addConstraint $ CClass (IdN con) $ TVar vark $ IdN vn
                inferKind t
            let find = head [i | (i, ConstraintKind (CClass n _)) <- zip [0..] $ map snd fs, n == con]
--            let t' = (mapWriterT' ((id *** ((ConstraintKind cstr:) *** id)) <$>)) <$> t
--                rearrange cs = head [b: as ++ bs | (as, b@(_, ConstraintKind _): bs) <- zip (inits cs) (tails cs)]
            return (Map.singleton n $ Just $ Exp mempty $ ExtractInstance (length fs) find n, Map.singleton n t)
        addPolyEnv (emptyPolyEnv {thunkEnv = mconcat ths, classDefs = Map.singleton con $ ClassD $ mconcat cdefs}) $ withTyping (mconcat cdefs) cont
    DAxiom (TypeSig n t) -> do
        ((_, t), t') <- instantiateTyping'' False (pShow n) $ inferKind t
        let res (TArr a b) = res b
            res t = t
            n' = (if isStar $ res t' then toTypeN else id) n
        withTyping (Map.singleton n' t) cont
    InstanceDef c t xs -> do  -- TODO: check types
        (ClassD cs) <- lookEnv'' c >>= maybe (throwErrorTCM "can't find class") return
        (ce, t) <- runWriterT'' $ inferKind_ t     -- TODO: ce
        let ce' = Map.filter (either (const False) (const True)) ce
        when (not $ Map.null ce') $ throwErrorTCM $ "not null ce" <+> pShow ce'
        xs <- local (\pe -> pe {instanceDefs = Map.alter (Just . maybe (Map.singleton t Refl) (Map.insert t Refl)) c $ instanceDefs pe}) -- fake
                $ forM xs $ \x@(ValueDef (PVar' _ n)  _) -> do
            inferDef' (cs Map.! n $ "instance") x
        let w = WInstance $ fmap (fromMaybe (error "impossible")) $ mconcat $ map fst xs
        local (\pe -> pe {instanceDefs = Map.alter (Just . maybe (Map.singleton t w) (Map.insert t w)) c $ instanceDefs pe}) $ do
--            foldr ($) cont xs
            cont

inference_ :: PolyEnv -> ModuleR -> ErrorT (VarMT Identity) PolyEnv
inference_ penv@PolyEnv{..} Module{..} = flip runReaderT penv $ diffEnv <$> inferDefs definitions
  where
    diffEnv (PolyEnv i c g p th tf) = PolyEnv
        (Map.differenceWith (\a b -> Just $ a Map.\\ b) i instanceDefs)
        (c Map.\\ classDefs)
        (g Map.\\ getPolyEnv)
        (p Map.\\ precedences)
        (th Map.\\ thunkEnv)
        (tf Map.\\ typeFamilies)


