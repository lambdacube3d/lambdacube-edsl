{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-} -- for ghc-7.10.1
module Typing where

import Data.List
import Data.Maybe
import Data.Monoid
import Text.PrettyPrint.ANSI.Leijen (pretty)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)
import qualified Data.Traversable as T
import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.Writer
import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Trifecta.Delta
import Text.Trifecta hiding (err)
import Text.Show.Pretty

import Type

matches TVar{} _ = True
matches x ts = x `elem` ts

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

type TCMC = WriterT [[Ty]] TCM

reduceConstraint :: Constraint Ty -> TCMC [Constraint Ty]
reduceConstraint x = do
  builtinInstances <- asks $ instanceDefs . fst
  case x of

    Split (TRecord a) (TRecord b) (TRecord c) ->
      case (Map.keys $ Map.intersection b c, Map.keys $ a Map.\\ (b <> c), Map.keys $ (b <> c) Map.\\ a) of
        ([], [], []) -> discard $ unifyMaps [a, b, c]
        ks -> failure $ "extra keys: " ++ show ks
    Split (TRecord a) (TRecord b) c@TVar{} -> diff a b c
    Split (TRecord a) c@TVar{} (TRecord b) -> diff a b c
    Split c@TVar{} (TRecord a) (TRecord b) -> case Map.keys $ Map.intersection a b of
        [] -> discard [[c, TRecord $ a <> b]]
        ks -> failure $ "extra keys: " ++ show ks
    Split a b c
        | isRec a && isRec b && isRec c -> nothing
        | otherwise -> failure $ "bad split: " ++ show x

    CClass _ TVar{} -> nothing
    CClass c t -> case c of

        IsTypeLevelNatural -> case t of
            TNat{} -> discard []
            _ -> noInstance

        IsValidOutput -> discard [] -- TODO

        IsValidFrameBuffer -> case t of
            TTuple ts
                | any isVar ts -> nothing
                | sum [1 | Depth{} <- ts] <= 1 && sum [1 | Stencil{} <- ts] <= 1 -> discard []
                | otherwise -> noInstance
            _ -> discard []

        IsInputTuple -> case t of
            TTuple ts
                | any isVar ts -> nothing
                | length [() | TInput{} <- ts] == length ts -> discard []
                | otherwise -> noInstance
            _ -> discard []

        _ -> maybe noInstance (\s -> iff (discard []) (noInstance' s) . Set.member t $ s) $ Map.lookup c builtinInstances
      where
        noInstance = failure $ "no " ++ c ++ " instance for " ++ ppShow t
        noInstance' s = failure $ "no " ++ c ++ " instance for " ++ ppShow t ++ "\npossible instances:\n" ++ ppShow s

    CUnify a b -> discard [[a, b]]

    CEq res f -> case f of

        TFMat (TVec n t1) (TVec m t2) | t1 `elem` [TFloat] && t1 == t2 -> reduced $ TMat n m t1
        TFMat a b -> check (a `matches` floatVectors && b `matches` floatVectors) $ observe res $ \case
            TMat n m t -> keep [[a, TVec n t], [b, TVec m t]]
            _ -> fail "no instance"

        TFVec (TNat n) ty | n `elem` [2,3,4] && ty `elem` floatIntWordBool -> reduced $ TVec n ty
        TFVec a b -> check (a `matches` nat234 && b `matches` floatIntWordBool {- -- FIXME -}) $ observe res $ \case
            TVec n t -> keep [[a, TNat n], [b, t]]
            _ -> fail "no instance tfvec"

        TFVecScalar a b -> case a of
            TNat 1 -> case b of
                TVar{} | res `matches` floatIntWordBool -> keep [[b, res]]
                b -> check (b `elem` floatIntWordBool) $ reduced b
            TVar{} -> check (b `matches` floatIntWordBool) $ observe res $ \case
                t | t `elem` floatIntWordBool -> keep [[a, TNat 1], [b, t]]
                _ -> like $ TFVec a b
            _ -> like $ TFVec a b

        TFMatVecElem t -> observe t $ \case
            TVec n t -> reduced t
            TMat _ _ t -> reduced t
            _ -> fail "no instance"

        TFMatVecScalarElem t -> observe t $ \case
            t | t `elem` floatIntWordBool -> reduced t
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
                TNat n -> reduce' (n, b)
                _ -> fail'
            _ -> fail'
          where
            end (unzip -> (n: ns, tys))
                | all (==n) ns = reduced $ TFrameBuffer (TNat n) $ tTuple tys
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
        like f = reduceConstraint (CEq res f)
        reduced t = discard [[res, t]]
        check b m = if b then m else fail "no instance (1)"
        fail = failure . (("error during reduction of " ++ ppShow res ++ "\n ~ " ++ ppShow f ++ "  ") ++)

        reduce' = Just . Just
        nothing' = Just Nothing
        fail' = Nothing
        observe' TVar{} _ = nothing'
        observe' x f = f x

        caseTuple :: String -> Ty -> ([a] -> TCMC [Constraint Ty]) -> (Ty -> Maybe (Maybe a)) -> TCMC [Constraint Ty]
        caseTuple msg ty end f = observe ty $ \case
            TTuple ts -> maybe (fail $ msg ++ " inside tuple") (maybe nothing end . sequence) $ mapM f' ts
            _ -> maybe (fail msg) (maybe nothing (end . (:[]))) $ f' ty
          where f' x = observe' x f

        tTuple [x] = x
        tTuple xs = TTuple xs
  where
    diff a b c = case Map.keys $ b Map.\\ a of
        [] -> discard $ [c, TRecord $ a Map.\\ b]: unifyMaps [a, b]
        ks -> failure $ "extra keys: " ++ show ks

    discard xs = tell xs >> return []
    keep xs = tell xs >> return [x]
    failure = lift . throwErrorTCM

    nothing = keep []
    observe TVar{} _ = nothing
    observe x f = f x

nat234 = [TNat i | i <-[2..4]]
floatIntWord = [TFloat, TInt, TWord]
floatIntWordBool = [TFloat, TInt, TWord, TBool]
vectors t = [TVec i t | i <- [2..4]]
floatVectors = vectors TFloat

data InjType
    = ITMat | ITVec | ITVecScalar
    deriving (Eq, Ord, Show)

injType :: TypeFun Ty -> Maybe (InjType, [Ty])
injType = \case
    TFMat a b -> Just (ITMat, [a, b])
    TFVec a b -> Just (ITVec, [a, b])
    TFVecScalar a b -> Just (ITVecScalar, [a, b])
    _ -> Nothing

vecS = TFVecScalar
floatVecS d = vecS d TFloat

fieldProjType :: FName -> TCM (Subst, Typing)
fieldProjType fn = newV $ \a r r' -> return $ [Split r r' (TRecord $ Map.singleton fn a)] ==> r ~> a :: TCM Typing

inferLit :: Lit -> TCM Typing
inferLit a = case a of
  LInt _ -> do
    --t <- newVar
    --return (mempty,[(CNum,t)],t) -- ????
    ty TInt
  LChar   _ -> ty TChar
  LFloat  _ -> ty TFloat
  LString _ -> ty TString
  LNat i    -> ty $ TNat i
 where
  ty t = return $ [] ==> t

checkUnambError = do
    cs <- gets fst
    case cs of
        (Just _: _) -> throwError $ head $ catMaybes $ reverse cs
        _ -> return ()

throwErrorTCM :: String -> TCM a
throwErrorTCM s = checkUnambError >> errorTCM >>= throwError . fmap (++ s)

errorTCM :: TCM ErrorMsg
errorTCM = do
  rl <- asks snd
  return $ \src -> let
      sl = map mkSpan rl
      fullCode = True
      mkSpan (s,e) = unlines [show $ pretty (s,e), if fullCode then BS.unpack str else show $ pretty r]
        where
          r = render spn
          str = x -- <> BS.takeWhile (\a -> notElem a ['\n','\r']) y
          spn = Span s e str
          (x,y) = BS.splitAt (se - sb) $ BS.drop sb src
          b = rewind s
          sb = fromIntegral $ bytes b
          se = fromIntegral $ bytes e
    in concat sl

class NewVar a where
    newV_ :: Subst -> a -> TCM (Subst, Typing)

newV = newV_ mempty

instance NewVar (TCM Typing)    where newV_ s = fmap $ (,) s
instance NewVar Typing          where newV_ s t = return (s, t)
instance NewVar Ty              where newV_ s t = return (s, [] ==> t)
instance NewVar a => NewVar (Ty -> a) where
    newV_ s f = do
        (s', v) <- newVar''
        newV_ (s' <> s) $ f v

newVar'' = do
    (s', k) <- newVar' Star  -- kind var
    (s, t) <- newVar' k
    return (s <> s', t)

newVar' k = do
    v@(TVar _ n) <- newVar k
    return (Map.singleton n v, v)

-- don't use this, use newV instead
newVar :: Ty -> TCM Ty
newVar k = do
  (d, n: ns) <- get
  put (d, ns)
  return $ Ty_ k (TVar_ n)
