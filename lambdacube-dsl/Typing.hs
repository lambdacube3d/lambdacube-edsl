{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Typing where

import Data.List
import Data.Maybe
import Data.Monoid
import Text.PrettyPrint.ANSI.Leijen (pretty)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)
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
reduceConstraint x = case x of

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
            TTuple C ts
                | any isVar ts -> nothing
                | sum [1 | Depth{} <- ts] <= 1 && sum [1 | Stencil{} <- ts] <= 1 -> discard []
                | otherwise -> noInstance
            _ -> discard []

        IsInputTuple -> case t of
            TTuple C ts
                | any isVar ts -> nothing
                | length [() | TInput{} <- ts] == length ts -> discard []
                | otherwise -> noInstance
            _ -> discard []

        _ -> maybe noInstance (iff (discard []) noInstance . Set.member t) $ Map.lookup c instances
      where
        noInstance = failure $ "no " ++ show c ++ " instance for " ++ show t

    CEq res f -> case f of

        TFMat (TV2F C) (TV2F C) -> reduced $ TM22F C
        TFMat (TV2F C) (TV3F C) -> reduced $ TM23F C
        TFMat (TV2F C) (TV4F C) -> reduced $ TM24F C
        TFMat (TV3F C) (TV2F C) -> reduced $ TM32F C
        TFMat (TV3F C) (TV3F C) -> reduced $ TM33F C
        TFMat (TV3F C) (TV4F C) -> reduced $ TM34F C
        TFMat (TV4F C) (TV2F C) -> reduced $ TM42F C
        TFMat (TV4F C) (TV3F C) -> reduced $ TM43F C
        TFMat (TV4F C) (TV4F C) -> reduced $ TM44F C
        TFMat a b -> check (a `matches` [TV2F C, TV3F C, TV4F C] && b `matches` [TV2F C, TV3F C, TV4F C]) $ observe res $ \case
            TM22F C -> keep [[a, TV2F C], [b, TV2F C]]
            TM23F C -> keep [[a, TV2F C], [b, TV3F C]]
            TM24F C -> keep [[a, TV2F C], [b, TV4F C]]
            TM32F C -> keep [[a, TV3F C], [b, TV2F C]]
            TM33F C -> keep [[a, TV3F C], [b, TV3F C]]
            TM34F C -> keep [[a, TV3F C], [b, TV4F C]]
            TM42F C -> keep [[a, TV4F C], [b, TV2F C]]
            TM43F C -> keep [[a, TV4F C], [b, TV3F C]]
            TM44F C -> keep [[a, TV4F C], [b, TV4F C]]
            _ -> fail "no instance"

        TFVec (TNat 2) (TFloat C) -> reduced $ TV2F C
        TFVec (TNat 3) (TFloat C) -> reduced $ TV3F C
        TFVec (TNat 4) (TFloat C) -> reduced $ TV4F C
        TFVec (TNat 2) (TInt C)   -> reduced $ TV2I C
        TFVec (TNat 3) (TInt C)   -> reduced $ TV3I C
        TFVec (TNat 4) (TInt C)   -> reduced $ TV4I C
        TFVec (TNat 2) (TWord C)  -> reduced $ TV2U C
        TFVec (TNat 3) (TWord C)  -> reduced $ TV3U C
        TFVec (TNat 4) (TWord C)  -> reduced $ TV4U C
        TFVec (TNat 2) (TBool C)  -> reduced $ TV2B C
        TFVec (TNat 3) (TBool C)  -> reduced $ TV3B C
        TFVec (TNat 4) (TBool C)  -> reduced $ TV4B C
        TFVec a b -> check (a `matches` [TNat 2, TNat 3, TNat 4] && b `matches` [TFloat C, TInt C, TWord C, TBool C])
                     $ observe res $ \case
            TV2F C -> keep [[a, TNat 2], [b, TFloat C]]
            TV3F C -> keep [[a, TNat 3], [b, TFloat C]]
            TV4F C -> keep [[a, TNat 4], [b, TFloat C]]
            TV2I C -> keep [[a, TNat 2], [b, TInt C]]
            TV3I C -> keep [[a, TNat 3], [b, TInt C]]
            TV4I C -> keep [[a, TNat 4], [b, TInt C]]
            TV2U C -> keep [[a, TNat 2], [b, TWord C]]
            TV3U C -> keep [[a, TNat 3], [b, TWord C]]
            TV4U C -> keep [[a, TNat 4], [b, TWord C]]
            TV2B C -> keep [[a, TNat 2], [b, TBool C]]
            TV3B C -> keep [[a, TNat 3], [b, TBool C]]
            TV4B C -> keep [[a, TNat 4], [b, TBool C]]
            _ -> fail "no instance"

        TFVecScalar a b -> case a of
            TNat 1 -> case b of
                TVar{} | res `matches` [TFloat C, TInt C, TWord C, TBool C] -> keep [[b, res]]
                b -> check (b `elem` [TFloat C, TInt C, TWord C, TBool C]) $ reduced b
            TVar{} -> check (b `matches` [TFloat C, TInt C, TWord C, TBool C]) $ observe res $ \case
                TFloat C -> keep [[a, TNat 1], [b, TFloat C]]
                TInt C   -> keep [[a, TNat 1], [b, TInt C]]
                TWord C  -> keep [[a, TNat 1], [b, TWord C]]
                TBool C  -> keep [[a, TNat 1], [b, TBool C]]
                _ -> like $ TFVec a b
            _ -> like $ TFVec a b

        TFMatVecElem t -> observe t $ \case
            TV2F C  -> reduced $ TFloat C
            TV3F C  -> reduced $ TFloat C
            TV4F C  -> reduced $ TFloat C
            TV2I C  -> reduced $ TInt C
            TV3I C  -> reduced $ TInt C
            TV4I C  -> reduced $ TInt C
            TV2U C  -> reduced $ TWord C
            TV3U C  -> reduced $ TWord C
            TV4U C  -> reduced $ TWord C
            TV2B C  -> reduced $ TBool C
            TV3B C  -> reduced $ TBool C
            TV4B C  -> reduced $ TBool C
            TM22F C -> reduced $ TFloat C
            TM23F C -> reduced $ TFloat C
            TM24F C -> reduced $ TFloat C
            TM32F C -> reduced $ TFloat C
            TM33F C -> reduced $ TFloat C
            TM34F C -> reduced $ TFloat C
            TM42F C -> reduced $ TFloat C
            TM43F C -> reduced $ TFloat C
            TM44F C -> reduced $ TFloat C
            _ -> fail "no instance"

        TFMatVecScalarElem t -> observe t $ \case
            TFloat C -> reduced $ TFloat C
            TInt C   -> reduced $ TInt C
            TWord C  -> reduced $ TWord C
            TBool C  -> reduced $ TBool C
            t -> like $ TFMatVecElem t

        TFColorRepr ty -> observe ty $ \case
            TTuple C ts -> reduced . TTuple C $ map Color ts
            ty -> reduced $ Color ty

        TFFTRepr' ty -> caseTuple "expected Input/Interpolated/Depth/Color" ty (reduced . tTuple C) $ \case
            TInterpolated C a -> reduce' a
            TInput C a        -> reduce' a
            Depth a           -> reduce' a
            Color a           -> reduce' a
            _ -> fail'

        TFFragOps ty -> caseTuple "expected FragmentOperation" ty (reduced . tTuple C) $ \case
            TFragmentOperation C a -> reduce' a
            _ -> fail'

        TFFrameBuffer ty -> caseTuple "expected (Image Nat)" ty end $ \case
            TImage C a b -> observe' a $ \case
                TNat n -> reduce' (n, b)
                _ -> fail'
            _ -> fail'
          where
            end (unzip -> (n: ns, tys))
                | all (==n) ns = reduced $ TFrameBuffer C (TNat n) $ tTuple C tys
                | otherwise = fail "frambuffer number of layers differ"

        TFJoinTupleType TVar{} _ -> nothing  -- TODO: observe res?
        TFJoinTupleType _ TVar{} -> nothing  -- TODO: observe res?
        TFJoinTupleType (TTuple f l) (TTuple _ r) -> reduced $ TTuple f (l ++ r)
        TFJoinTupleType l (TTuple f r) -> reduced $ TTuple f (l : r)
        TFJoinTupleType (TTuple f l) r -> reduced $ TTuple f (l ++ [r])
        TFJoinTupleType l r -> reduced $ TTuple C [l,r]


        _ -> nothing
      where
        like f = reduceConstraint (CEq res f)
        reduced t = discard [[res, t]]
        check b m = if b then m else fail "no instance"
        fail = failure . (("error during reduction of " ++ show res ++ " ~ " ++ show f ++ "  ") ++)

        reduce' = Just . Just
        nothing' = Just Nothing
        fail' = Nothing
        observe' TVar{} _ = nothing'
        observe' x f = f x

        caseTuple :: String -> Ty -> ([a] -> TCMC [Constraint Ty]) -> (Ty -> Maybe (Maybe a)) -> TCMC [Constraint Ty]
        caseTuple msg ty end f = observe ty $ \case
            TTuple C ts -> maybe (fail $ msg ++ " inside tuple") (maybe nothing end . sequence) $ mapM f' ts
            _ -> maybe (fail msg) (maybe nothing (end . (:[]))) $ f' ty
          where f' x = observe' x f

        tTuple f [x] = x
        tTuple f xs = TTuple f xs
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

instances :: Map Class (Set Ty)
instances = Map.fromList
    [ item CNum         [TInt C, TFloat C]
    , item IsIntegral   [TInt C, TWord C]
    , item IsNumComponent [TFloat C, TInt C, TWord C, TV2F C, TV3F C, TV4F C]
    , item IsSigned     [TFloat C, TInt C]
    , item IsNum        [TFloat C, TInt C, TWord C]
    , item IsFloating   [TFloat C, TV2F C, TV3F C, TV4F C, TM22F C, TM23F C, TM24F C, TM32F C, TM33F C, TM34F C, TM42F C, TM43F C, TM44F C]
    , item IsComponent  [TFloat C, TInt C, TWord C, TBool C, TV2F C, TV3F C, TV4F C]
    ]
  where
    item a b = (a, Set.fromList b)


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

inferPrimFun :: (Typing -> TCM e) -> TCM e -> EName -> TCM e
inferPrimFun ok nothing n = maybe nothing (>>= ok) $ Map.lookup n primFunMap

infix 0 -->
s --> m = tell [(s, newV m)]

isPrimFun n = Map.member n primFunMap
{-
    -- Vec/Mat (de)construction
    PrimTupToV2             :: IsComponent a                            => PrimFun stage ((a,a)     -> V2 a)
    PrimTupToV3             :: IsComponent a                            => PrimFun stage ((a,a,a)   -> V3 a)
    PrimTupToV4             :: IsComponent a                            => PrimFun stage ((a,a,a,a) -> V4 a)
    PrimV2ToTup             :: IsComponent a                            => PrimFun stage (V2 a     -> (a,a))
    PrimV3ToTup             :: IsComponent a                            => PrimFun stage (V3 a   -> (a,a,a))
    PrimV4ToTup             :: IsComponent a                            => PrimFun stage (V4 a -> (a,a,a,a))
-}

primFunMap :: Map EName (TCM Typing)
primFunMap = Map.fromList $ execWriter $ do
  -- temporary const constructor
  "Tup"          --> \a -> [] ==> a ~> a
  "Const"        --> \a -> [] ==> a ~> a
  -- Vector/Matrix
  "True"         --> [] ==> TBool C
  "False"        --> [] ==> TBool C
  "V2B"          --> [] ==> TBool C ~> TBool C ~> TV2B C
  "V3B"          --> [] ==> TBool C ~> TBool C ~> TBool C ~> TV3B C
  "V4B"          --> [] ==> TBool C ~> TBool C ~> TBool C ~> TBool C ~> TV4B C
  "V2U"          --> [] ==> TWord C ~> TWord C ~> TV2U C
  "V3U"          --> [] ==> TWord C ~> TWord C ~> TWord C ~> TV3U C
  "V4U"          --> [] ==> TWord C ~> TWord C ~> TWord C ~> TWord C ~> TV4U C
  "V2I"          --> [] ==> TInt C ~> TInt C ~> TV2I C
  "V3I"          --> [] ==> TInt C ~> TInt C ~> TInt C ~> TV3I C
  "V4I"          --> [] ==> TInt C ~> TInt C ~> TInt C ~> TInt C ~> TV4I C
  "V2F"          --> [] ==> TFloat C ~> TFloat C ~> TV2F C
  "V3F"          --> [] ==> TFloat C ~> TFloat C ~> TFloat C ~> TV3F C
  "V4F"          --> [] ==> TFloat C ~> TFloat C ~> TFloat C ~> TFloat C ~> TV4F C
  "M22F"         --> [] ==> TV2F C ~> TV2F C ~> TM22F C
  "M23F"         --> [] ==> TV2F C ~> TV2F C ~> TV2F C ~> TM23F C
  "M24F"         --> [] ==> TV2F C ~> TV2F C ~> TV2F C ~> TV2F C ~> TM24F C
  "M32F"         --> [] ==> TV3F C ~> TV3F C ~> TM32F C
  "M33F"         --> [] ==> TV3F C ~> TV3F C ~> TV3F C ~> TM33F C
  "M34F"         --> [] ==> TV3F C ~> TV3F C ~> TV3F C ~> TV3F C ~> TM34F C
  "M42F"         --> [] ==> TV4F C ~> TV4F C ~> TM42F C
  "M43F"         --> [] ==> TV4F C ~> TV4F C ~> TV4F C ~> TM43F C
  "M44F"         --> [] ==> TV4F C ~> TV4F C ~> TV4F C ~> TV4F C ~> TM44F C
  -- Input declaration
  "Uni"          --> \t -> [] ==> TInput C t ~> t
  "IBool"        --> [] ==> TString C ~> TInput C (TBool  C)
  "IV2B"         --> [] ==> TString C ~> TInput C (TV2B   C)
  "IV3B"         --> [] ==> TString C ~> TInput C (TV3B   C)
  "IV4B"         --> [] ==> TString C ~> TInput C (TV4B   C)
  "IWord"        --> [] ==> TString C ~> TInput C (TWord  C)
  "IV2U"         --> [] ==> TString C ~> TInput C (TV2U   C)
  "IV3U"         --> [] ==> TString C ~> TInput C (TV3U   C)
  "IV4U"         --> [] ==> TString C ~> TInput C (TV4U   C)
  "IInt"         --> [] ==> TString C ~> TInput C (TInt   C)
  "IV2I"         --> [] ==> TString C ~> TInput C (TV2I   C)
  "IV3I"         --> [] ==> TString C ~> TInput C (TV3I   C)
  "IV4I"         --> [] ==> TString C ~> TInput C (TV4I   C)
  "IFloat"       --> [] ==> TString C ~> TInput C (TFloat C)
  "IV2F"         --> [] ==> TString C ~> TInput C (TV2F   C)
  "IV3F"         --> [] ==> TString C ~> TInput C (TV3F   C)
  "IV4F"         --> [] ==> TString C ~> TInput C (TV4F   C)
  "IM22F"        --> [] ==> TString C ~> TInput C (TM22F  C)
  "IM23F"        --> [] ==> TString C ~> TInput C (TM23F  C)
  "IM24F"        --> [] ==> TString C ~> TInput C (TM24F  C)
  "IM32F"        --> [] ==> TString C ~> TInput C (TM32F  C)
  "IM33F"        --> [] ==> TString C ~> TInput C (TM33F  C)
  "IM34F"        --> [] ==> TString C ~> TInput C (TM34F  C)
  "IM42F"        --> [] ==> TString C ~> TInput C (TM42F  C)
  "IM43F"        --> [] ==> TString C ~> TInput C (TM43F  C)
  "IM44F"        --> [] ==> TString C ~> TInput C (TM44F  C)
  -- BlendingFactor
  "Zero"                   --> [] ==> TBlendingFactor C
  "One"                    --> [] ==> TBlendingFactor C
  "SrcColor"               --> [] ==> TBlendingFactor C
  "OneMinusSrcColor"       --> [] ==> TBlendingFactor C
  "DstColor"               --> [] ==> TBlendingFactor C
  "OneMinusDstColor"       --> [] ==> TBlendingFactor C
  "SrcAlpha"               --> [] ==> TBlendingFactor C
  "OneMinusSrcAlpha"       --> [] ==> TBlendingFactor C
  "DstAlpha"               --> [] ==> TBlendingFactor C
  "OneMinusDstAlpha"       --> [] ==> TBlendingFactor C
  "ConstantColor"          --> [] ==> TBlendingFactor C
  "OneMinusConstantColor"  --> [] ==> TBlendingFactor C
  "ConstantAlpha"          --> [] ==> TBlendingFactor C
  "OneMinusConstantAlpha"  --> [] ==> TBlendingFactor C
  "SrcAlphaSaturate"       --> [] ==> TBlendingFactor C
  -- BlendEquation
  "FuncAdd"                --> [] ==> TBlendEquation C
  "FuncSubtract"           --> [] ==> TBlendEquation C
  "FuncReverseSubtract"    --> [] ==> TBlendEquation C
  "Min"                    --> [] ==> TBlendEquation C
  "Max"                    --> [] ==> TBlendEquation C
  -- LogicOperation
  "Clear"        --> [] ==> TLogicOperation C
  "And"          --> [] ==> TLogicOperation C
  "AndReverse"   --> [] ==> TLogicOperation C
  "Copy"         --> [] ==> TLogicOperation C
  "AndInverted"  --> [] ==> TLogicOperation C
  "Noop"         --> [] ==> TLogicOperation C
  "Xor"          --> [] ==> TLogicOperation C
  "Or"           --> [] ==> TLogicOperation C
  "Nor"          --> [] ==> TLogicOperation C
  "Equiv"        --> [] ==> TLogicOperation C
  "Invert"       --> [] ==> TLogicOperation C
  "OrReverse"    --> [] ==> TLogicOperation C
  "CopyInverted" --> [] ==> TLogicOperation C
  "OrInverted"   --> [] ==> TLogicOperation C
  "Nand"         --> [] ==> TLogicOperation C
  "Set"          --> [] ==> TLogicOperation C
  -- StencilOperation
  "OpZero"       --> [] ==> TStencilOperation C
  "OpKeep"       --> [] ==> TStencilOperation C
  "OpReplace"    --> [] ==> TStencilOperation C
  "OpIncr"       --> [] ==> TStencilOperation C
  "OpIncrWrap"   --> [] ==> TStencilOperation C
  "OpDecr"       --> [] ==> TStencilOperation C
  "OpDecrWrap"   --> [] ==> TStencilOperation C
  "OpInvert"     --> [] ==> TStencilOperation C
  -- ComparisonFunction
  "Never"    --> [] ==> TComparisonFunction C
  "Less"     --> [] ==> TComparisonFunction C
  "Equal"    --> [] ==> TComparisonFunction C
  "Lequal"   --> [] ==> TComparisonFunction C
  "Greater"  --> [] ==> TComparisonFunction C
  "Notequal" --> [] ==> TComparisonFunction C
  "Gequal"   --> [] ==> TComparisonFunction C
  "Always"   --> [] ==> TComparisonFunction C
  -- ProvokingVertex
  "LastVertex"   --> [] ==> TProvokingVertex C
  "FirstVertex"  --> [] ==> TProvokingVertex C
  -- CullMode
  "CullNone"     --> [] ==> TCullMode C
  "CullFront"    --> [] ==> TFrontFace C ~> TCullMode C
  "CullBack"     --> [] ==> TFrontFace C ~> TCullMode C
  -- FrontFace
  "CW"   --> [] ==> TFrontFace C
  "CCW"  --> [] ==> TFrontFace C
  -- PolygonMode
  "PolygonFill"  --> [] ==> TPolygonMode C
  "PolygonPoint" --> [] ==> TPointSize C ~> TPolygonMode C
  "PolygonLine"  --> [] ==> TFloat C ~> TPolygonMode C
  -- PolygonOffset
  "NoOffset" --> [] ==> TPolygonOffset C
  "Offset"   --> [] ==> TFloat C ~> TFloat C ~> TPolygonOffset C
  -- PointSize
  "PointSize"        --> [] ==> TFloat C ~> TPointSize C
  "ProgramPointSize" --> [] ==> TPointSize C
  -- Fragment Out
  {-
    FragmentOut             ::                  FlatExp F a -> FragmentOut (ColorRepr a)
    FragmentOutDepth        :: Exp F Float  ->  FlatExp F a -> FragmentOut (Depth Float :+: ColorRepr a)
    FragmentOutRastDepth    ::                  FlatExp F a -> FragmentOut (Depth Float :+: ColorRepr a)
  -}
  "FragmentOut"           --> \a t -> [a ~~ TFColorRepr t] ==> t ~> TFragmentOut C a
  "FragmentOutDepth"      --> \a b t -> [a ~~ TFColorRepr t, b ~~ TFJoinTupleType (Depth $ TFloat C) a] ==> TFloat C ~> t ~> TFragmentOut C b
  "FragmentOutRastDepth"  --> \a b t -> [a ~~ TFColorRepr t, b ~~ TFJoinTupleType (Depth $ TFloat C) a] ==> t ~> TFragmentOut C b
  -- Vertex Out
  "VertexOut"    --> \a t -> [t ~~ TFFTRepr' a] ==> TV4F C ~> TFloat C ~> TTuple C [] ~> a ~> TVertexOut C t
  -- PointSpriteCoordOrigin
  "LowerLeft"  --> [] ==> TPointSpriteCoordOrigin C
  "UpperLeft"  --> [] ==> TPointSpriteCoordOrigin C
  -- Raster Context
  "TriangleCtx"  --> [] ==> TCullMode C ~> TPolygonMode C ~> TPolygonOffset C ~> TProvokingVertex C ~> TRasterContext C TTriangle
  "PointCtx"     --> [] ==> TPointSize C ~> TFloat C ~> TPointSpriteCoordOrigin C ~> TRasterContext C TPoint
  "LineCtx"      --> [] ==> TFloat C ~> TProvokingVertex C ~> TRasterContext C TLine
  -- Fetch Primitive
  "Points"             --> [] ==> TFetchPrimitive C TPoint
  "Lines"              --> [] ==> TFetchPrimitive C TLine
  "Triangles"          --> [] ==> TFetchPrimitive C TTriangle
  "LinesAdjacency"     --> [] ==> TFetchPrimitive C TLineAdjacency
  "TrianglesAdjacency" --> [] ==> TFetchPrimitive C TTriangleAdjacency
  -- Accumulation Context
  "AccumulationContext"  --> \t' t -> [t' ~~ TFFragOps t] ==> t ~> TAccumulationContext C t'
  -- Image
  "ColorImage" --> \a d color t ->
    [isTypeLevelNatural a, isNum t, color ~~ vecS d t] ==> a ~> color ~> TImage C a (Color color)
  "DepthImage" --> \a ->
    [isTypeLevelNatural a] ==> a ~> TFloat C ~> TImage C a (Depth $ TFloat C)
  "StencilImage" --> \a ->
    [isTypeLevelNatural a] ==> a ~> TInt C ~> TImage C a (Stencil $ TInt C)
  -- Interpolation
  "Flat"           --> \t -> [] ==> t ~> TInterpolated C t
  "Smooth"         --> \t -> [isFloating t] ==> t ~> TInterpolated C t
  "NoPerspective"  --> \t -> [isFloating t] ==> t ~> TInterpolated C t
  -- Fragment Operation
  "ColorOp"    --> \d mask c color ->
    [mask ~~ vecS d (TBool C), color ~~ vecS d c, isNum c] ==> TBlending C c ~> mask ~> TFragmentOperation C (Color color)
  "DepthOp"    --> [] ==> TComparisonFunction C ~> TBool C ~> TFragmentOperation C (Depth $ TFloat C)
    -- "StencilOp       :: StencilTests -> StencilOps -> StencilOps -> FragmentOperation (Stencil Int32)
  -- Blending
  "NoBlending"   --> \t -> [] ==> TBlending C t
  "BlendLogicOp" --> \t -> [isIntegral t] ==> TLogicOperation C ~> TBlending C t
  "Blend"        --> [] ==> TTuple C [TBlendEquation C,TBlendEquation C]
                         ~> TTuple C [TTuple C [TBlendingFactor C,TBlendingFactor C],TTuple C [TBlendingFactor C,TBlendingFactor C]]
                         ~> TV4F C ~> TBlending C (TFloat C)
  -- Fragment Filter
  "PassAll"  --> \t -> [] ==> TFragmentFilter C t
  "Filter"   --> \t -> [] ==> (t ~> TBool C) ~> TFragmentFilter C t
  -- Render Operations
  "Fetch"        --> \a t b -> [IsInputTuple @@ t, b ~~ TFFTRepr' t] ==> TString C ~> TFetchPrimitive C a ~> t ~> TVertexStream C a b
  "Transform"    --> \a b p -> [] ==> (a ~> TVertexOut C b) ~> TVertexStream C p a ~> TPrimitiveStream C p (TNat 1) C b
  "Rasterize"    --> \a b c -> [] ==> TRasterContext C a ~> TPrimitiveStream C a b C c ~> TFragmentStream C b c
  {-
    Accumulate      :: (GPU a, GPU (FTRepr' b), IsValidOutput b)    -- restriction: depth and stencil optional, arbitrary color component
                    => AccumulationContext b
                    -> FragmentFilter a
                    -> (Exp F a -> FragmentOut (NoStencilRepr b))     -- fragment shader
                    -> Exp Obj (FragmentStream layerCount a)
                    -> Exp Obj (FrameBuffer layerCount (FTRepr' b))
                    -> Exp Obj (FrameBuffer layerCount (FTRepr' b))
  -}
  "Accumulate" --> \a b n t ->
    [isValidOutput b, t ~~ TFFTRepr' b] ==>
           TAccumulationContext C b
        ~> TFragmentFilter C a
        ~> (a ~> TFragmentOut C b)
        ~> TFragmentStream C n a
        ~> TFrameBuffer C n t
        ~> TFrameBuffer C n t
  "FrameBuffer"  --> \a t t' n ->
    [t' ~~ TFFTRepr' t, IsValidFrameBuffer @@ t, TFrameBuffer C n t ~~ TFFrameBuffer a] ==> a ~> TFrameBuffer C n t'
  "ScreenOut"    --> \a b -> [] ==> TFrameBuffer C a b ~> TOutput C
  -- * Primitive Functions *
  -- Arithmetic Functions (componentwise)
  "PrimAdd"   --> \a t -> [t ~~ TFMatVecElem a, isNum t] ==> a ~> a ~> a
  "PrimAddS"  --> \a t -> [t ~~ TFMatVecScalarElem a, isNum t] ==> a ~> t ~> a
  "PrimSub"   --> \a t -> [t ~~ TFMatVecElem a, isNum t] ==> a ~> a ~> a
  "PrimSubS"  --> \a t -> [t ~~ TFMatVecScalarElem a, isNum t] ==> a ~> t ~> a
  "PrimMul"   --> \a t -> [t ~~ TFMatVecElem a, isNum t] ==> a ~> a ~> a
  "PrimMulS"  --> \a t -> [t ~~ TFMatVecScalarElem a, isNum t] ==> a ~> t ~> a
  "PrimDiv"   --> \d a t -> [isNum t, a ~~ vecS d t] ==> a ~> a ~> a
  "PrimDivS"  --> \d a t -> [isNum t, a ~~ vecS d t] ==> a ~> t ~> a
  "PrimNeg"   --> \a t -> [t ~~ TFMatVecScalarElem a, isSigned t] ==> a ~> a
  "PrimMod"   --> \d a t -> [isNum t, a ~~ vecS d t] ==> a ~> a ~> a
  "PrimModS"  --> \d a t -> [isNum t, a ~~ vecS d t] ==> a ~> t ~> a
  -- Bit-wise Functions
  "PrimBAnd"      --> \d a t -> [isIntegral t, a ~~ vecS d t] ==> a ~> a ~> a
  "PrimBAndS"     --> \d a t -> [isIntegral t, a ~~ vecS d t] ==> a ~> t ~> a
  "PrimBOr"       --> \d a t -> [isIntegral t, a ~~ vecS d t] ==> a ~> a ~> a
  "PrimBOrS"      --> \d a t -> [isIntegral t, a ~~ vecS d t] ==> a ~> t ~> a
  "PrimBXor"      --> \d a t -> [isIntegral t, a ~~ vecS d t] ==> a ~> a ~> a
  "PrimBXorS"     --> \d a t -> [isIntegral t, a ~~ vecS d t] ==> a ~> t ~> a
  "PrimBNot"      --> \d a t -> [isIntegral t, a ~~ vecS d t] ==> a ~> a
  "PrimBShiftL"   --> \d a b t -> [isIntegral t, a ~~ vecS d t, b ~~ vecS d (TWord C)] ==> a ~> b ~> a
  "PrimBShiftLS"  --> \d a t -> [isIntegral t, a ~~ vecS d t] ==> a ~> TWord C ~> a
  "PrimBShiftR"   --> \d a b t -> [isIntegral t, a ~~ vecS d t, b ~~ vecS d (TWord C)] ==> a ~> b ~> a
  "PrimBShiftRS"  --> \d a t -> [isIntegral t, a ~~ vecS d t] ==> a ~> TWord C ~> a
  -- Logic Functions
  "PrimAnd" --> [] ==> TBool C ~> TBool C ~> TBool C
  "PrimOr"  --> [] ==> TBool C ~> TBool C ~> TBool C
  "PrimXor" --> [] ==> TBool C ~> TBool C ~> TBool C
  "PrimNot" --> \d a -> [a ~~ vecS d (TBool C)] ==> a ~> a
  "PrimAny" --> \d a -> [a ~~ vecS d (TBool C)] ==> a ~> TBool C
  "PrimAll" --> \d a -> [a ~~ vecS d (TBool C)] ==> a ~> TBool C
  -- Angle and Trigonometry Functions
  "PrimACos"    --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimACosH"   --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimASin"    --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimASinH"   --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimATan"    --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimATan2"   --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a ~> a
  "PrimATanH"   --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimCos"     --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimCosH"    --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimDegrees" --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimRadians" --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimSin"     --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimSinH"    --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimTan"     --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimTanH"    --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  -- Exponential Functions
  "PrimPow"     --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a ~> a
  "PrimExp"     --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimLog"     --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimExp2"    --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimLog2"    --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimSqrt"    --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimInvSqrt" --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  -- Common Functions
  "PrimIsNan"       --> \d a b -> [a ~~ vecS d (TFloat C), b ~~ vecS d (TBool C)] ==> a ~> b
  "PrimIsInf"       --> \d a b -> [a ~~ vecS d (TFloat C), b ~~ vecS d (TBool C)] ==> a ~> b
  "PrimAbs"         --> \d a t -> [isSigned t, a ~~ vecS d t] ==> a ~> a
  "PrimSign"        --> \d a t -> [isSigned t, a ~~ vecS d t] ==> a ~> a
  "PrimFloor"       --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimTrunc"       --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimRound"       --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimRoundEven"   --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimCeil"        --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimFract"       --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimModF"        --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> TTuple C [a,a]
  "PrimMin"         --> \d a t -> [isNum t, a ~~ vecS d t] ==> a ~> a ~> a
  "PrimMinS"        --> \d a t -> [isNum t, a ~~ vecS d t] ==> a ~> t ~> a
  "PrimMax"         --> \d a t -> [isNum t, a ~~ vecS d t] ==> a ~> a ~> a
  "PrimMaxS"        --> \d a t -> [isNum t, a ~~ vecS d t] ==> a ~> t ~> a
  "PrimClamp"       --> \d a t -> [isNum t, a ~~ vecS d t] ==> a ~> a ~> a ~> a
  "PrimClampS"      --> \d a t -> [isNum t, a ~~ vecS d t] ==> a ~> t ~> t ~> a
  "PrimMix"         --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a ~> a ~> a
  "PrimMixS"        --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a ~> TFloat C ~> a
  "PrimMixB"        --> \d a b -> [a ~~ vecS d (TFloat C), b ~~ vecS d (TBool C)] ==> a ~> a ~> b ~> a
  "PrimStep"        --> \d a -> [a ~~ TFVec d (TFloat C)] ==> a ~> a ~> a
  "PrimStepS"       --> \d a -> [a ~~ vecS d (TFloat C)] ==> TFloat C ~> a ~> a
  "PrimSmoothStep"  --> \d a -> [a ~~ TFVec d (TFloat C)] ==> a ~> a ~> a ~> a
  "PrimSmoothStepS" --> \d a -> [a ~~ vecS d (TFloat C)] ==> TFloat C ~> TFloat C ~> a ~> a
  -- Integer/Float Conversion Functions
  "PrimFloatBitsToInt"  --> \d fv iv -> [fv ~~ vecS d (TFloat C), iv ~~ vecS d (TInt C)] ==> fv ~> iv
  "PrimFloatBitsToUInt" --> \d fv uv -> [fv ~~ vecS d (TFloat C), uv ~~ vecS d (TWord C)] ==> fv ~> uv
  "PrimIntBitsToFloat"  --> \d fv iv -> [fv ~~ vecS d (TFloat C), iv ~~ vecS d (TInt C)] ==> iv ~> fv
  "PrimUIntBitsToFloat" --> \d fv uv -> [fv ~~ vecS d (TFloat C), uv ~~ vecS d (TWord C)] ==> uv ~> fv
  -- Geometric Functions
  "PrimLength"      --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> TFloat C
  "PrimDistance"    --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a ~> TFloat C
  "PrimDot"         --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a ~> TFloat C
  "PrimCross"       --> \a -> [a ~~ vecS (TNat 3) (TFloat C)] ==> a ~> a ~> a
  "PrimNormalize"   --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimFaceForward" --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a ~> a ~> a
  "PrimReflect"     --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a ~> a
  "PrimRefract"     --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a ~> a ~> a
  -- Matrix Functions
  "PrimTranspose"     --> \a b h w -> [a ~~ TFMat h w, b ~~ TFMat w h] ==> a ~> b
  "PrimDeterminant"   --> \m s -> [m ~~ TFMat s s] ==> m ~> TFloat C
  "PrimInverse"       --> \m s -> [m ~~ TFMat s s] ==> m ~> m
  "PrimOuterProduct"  --> \m h w -> [m ~~ TFMat h w] ==> w ~> h ~> m
  "PrimMulMatVec"     --> \m h w -> [m ~~ TFMat h w] ==> m ~> w ~> h
  "PrimMulVecMat"     --> \m h w -> [m ~~ TFMat h w] ==> h ~> m ~> w
  "PrimMulMatMat"     --> \a b c i j k -> [a ~~ TFMat i j, b ~~ TFMat j k, c ~~ TFMat i k] ==> a ~> b ~> c
  -- Vector and Scalar Relational Functions
  "PrimLessThan"          --> \d a b t -> [isNum t, a ~~ vecS d t, b ~~ vecS d (TBool C)] ==> a ~> a ~> b
  "PrimLessThanEqual"     --> \d a b t -> [isNum t, a ~~ vecS d t, b ~~ vecS d (TBool C)] ==> a ~> a ~> b
  "PrimGreaterThan"       --> \d a b t -> [isNum t, a ~~ vecS d t, b ~~ vecS d (TBool C)] ==> a ~> a ~> b
  "PrimGreaterThanEqual"  --> \d a b t -> [isNum t, a ~~ vecS d t, b ~~ vecS d (TBool C)] ==> a ~> a ~> b
  "PrimEqualV"            --> \d a b t -> [isNum t, a ~~ vecS d t, b ~~ vecS d (TBool C)] ==> a ~> a ~> b
  "PrimEqual"             --> \a t -> [t ~~ TFMatVecScalarElem a] ==> a ~> a ~> TBool C
  "PrimNotEqualV"         --> \d a b t -> [isNum t, a ~~ vecS d t, b ~~ vecS d (TBool C)] ==> a ~> a ~> b
  "PrimNotEqual"          --> \a t -> [t ~~ TFMatVecScalarElem a] ==> a ~> a ~> TBool C
  -- Fragment Processing Functions
  "PrimDFdx"    --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimDFdy"    --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimFWidth"  --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  -- Noise Functions
  "PrimNoise1"  --> \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> TFloat C
  "PrimNoise2"  --> \d a b -> [a ~~ vecS d (TFloat C), b ~~ vecS (TNat 2) (TFloat C)] ==> a ~> b
  "PrimNoise3"  --> \d a b -> [a ~~ vecS d (TFloat C), b ~~ vecS (TNat 3) (TFloat C)] ==> a ~> b
  "PrimNoise4"  --> \d a b -> [a ~~ vecS d (TFloat C), b ~~ vecS (TNat 4) (TFloat C)] ==> a ~> b

--  a -> throwErrorTCM $ "unknown primitive: " ++ show a
--  a -> nothing

fieldProjType :: FName -> TCM Typing
fieldProjType fn = newV $ \a r r' -> return $ [Split r r' (TRecord $ Map.singleton fn a)] ==> r ~> a :: TCM Typing


inferLit :: Lit -> TCM Typing
inferLit a = case a of
  LInt _ -> do
    --t <- newVar C
    --return (mempty,[(CNum,t)],t) -- ????
    ty $ TInt C
  LChar   _ -> ty $ TChar C
  LFloat  _ -> ty $ TFloat C
  LString _ -> ty $ TString C
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
    type NewVarRes a :: *
    newV :: a -> TCM (NewVarRes a)

instance NewVar (TCM a) where
    type NewVarRes (TCM a) = a
    newV = id

instance NewVar Typing where
    type NewVarRes Typing = Typing
    newV = return

instance NewVar a => NewVar (Ty -> a) where
    type NewVarRes (Ty -> a) = NewVarRes a
    newV f = newVar C >>= newV . f

newVar :: Frequency -> TCM Ty
newVar f = do
  (d, n) <- get
  put (d, n+1)
  return $ TVar f $ 't':show n
