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
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Trifecta.Delta
import Text.Trifecta hiding (err)

import Type

isPrimFun n = Set.member n primFunSet
primFunSet = Set.fromList
  [ "Const"
  , "Tup"
  , "Fetch"
  , "Transform"
  , "Rasterize"
  , "Accumulate"
  , "FrameBuffer"
  , "ScreenOut"
  , "Uni"
  , "IBool"
  , "IV2B"
  , "IV3B"
  , "IV4B"
  , "IWord"
  , "IV2U"
  , "IV3U"
  , "IV4U"
  , "IInt"
  , "IV2I"
  , "IV3I"
  , "IV4I"
  , "IFloat"
  , "IV2F"
  , "IV3F"
  , "IV4F"
  , "IM22F"
  , "IM23F"
  , "IM24F"
  , "IM32F"
  , "IM33F"
  , "IM34F"
  , "IM42F"
  , "IM43F"
  , "IM44F"
  , "PassAll"
  , "Filter"
  , "Smooth"
  , "Flat"
  , "NoPerspective"
  , "NoBlending"
  , "BlendLogicOp"
  , "Blend"
  , "DepthOp"
  , "ColorOp"
  , "StencilOp"
  , "DepthImage"
  , "StencilImage"
  , "ColorImage"
  , "TriangleCtx"
  , "PointCtx"
  , "LineCtx"
  , "AccumulationContext"
  , "Points"
  , "Lines"
  , "Triangles"
  , "LinesAdjacency"
  , "TrianglesAdjacency"
  , "VertexOut"
  , "FragmentOut"
  , "FragmentOutDepth"
  , "FragmentOutRastDepth"
  , "True"
  , "False"
  , "V2B"
  , "V3B"
  , "V4B"
  , "V2U"
  , "V3U"
  , "V4U"
  , "V2I"
  , "V3I"
  , "V4I"
  , "V2F"
  , "V3F"
  , "V4F"
  , "M22F"
  , "M23F"
  , "M24F"
  , "M32F"
  , "M33F"
  , "M34F"
  , "M42F"
  , "M43F"
  , "M44F"
  , "CullNone"
  , "CullFront"
  , "CullBack"
  , "CW"
  , "CCW"
  , "PolygonFill"
  , "PolygonPoint"
  , "PolygonLine"
  , "PointSize"
  , "ProgramPointSize"
  , "NoOffset"
  , "Offset"
  , "LastVertex"
  , "FirstVertex"
  , "Never"
  , "Less"
  , "Equal"
  , "Lequal"
  , "Greater"
  , "Notequal"
  , "Gequal"
  , "Always"
  , "OpZero"
  , "OpKeep"
  , "OpReplace"
  , "OpIncr"
  , "OpIncrWrap"
  , "OpDecr"
  , "OpDecrWrap"
  , "OpInvert"
  , "Clear"
  , "And"
  , "AndReverse"
  , "Copy"
  , "AndInverted"
  , "Noop"
  , "Xor"
  , "Or"
  , "Nor"
  , "Equiv"
  , "Invert"
  , "OrReverse"
  , "CopyInverted"
  , "OrInverted"
  , "Nand"
  , "Set"
  , "FuncAdd"
  , "FuncSubtract"
  , "FuncReverseSubtract"
  , "Min"
  , "Max"
  , "Zero"
  , "One"
  , "SrcColor"
  , "OneMinusSrcColor"
  , "DstColor"
  , "OneMinusDstColor"
  , "SrcAlpha"
  , "OneMinusSrcAlpha"
  , "DstAlpha"
  , "OneMinusDstAlpha"
  , "ConstantColor"
  , "OneMinusConstantColor"
  , "ConstantAlpha"
  , "OneMinusConstantAlpha"
  , "SrcAlphaSaturate"
  , "LowerLeft"
  , "UpperLeft"
  -- builtin functions
  -- Vec/Mat (de)construction
  , "PrimTupToV2"
  , "PrimTupToV3"
  , "PrimTupToV4"
  , "PrimV2ToTup"
  , "PrimV3ToTup"
  , "PrimV4ToTup"
  -- Arithmetic Functions (componentwise)
  , "PrimAdd"
  , "PrimAddS"
  , "PrimSub"
  , "PrimSubS"
  , "PrimMul"
  , "PrimMulS"
  , "PrimDiv"
  , "PrimDivS"
  , "PrimNeg"
  , "PrimMod"
  , "PrimModS"
  -- Bit-wise Functions
  , "PrimBAnd"
  , "PrimBAndS"
  , "PrimBOr"
  , "PrimBOrS"
  , "PrimBXor"
  , "PrimBXorS"
  , "PrimBNot"
  , "PrimBShiftL"
  , "PrimBShiftLS"
  , "PrimBShiftR"
  , "PrimBShiftRS"
  -- Logic Functions
  , "PrimAnd"
  , "PrimOr"
  , "PrimXor"
  , "PrimNot"
  , "PrimAny"
  , "PrimAll"
  -- Angle and Trigonometry Functions
  , "PrimACos"
  , "PrimACosH"
  , "PrimASin"
  , "PrimASinH"
  , "PrimATan"
  , "PrimATan2"
  , "PrimATanH"
  , "PrimCos"
  , "PrimCosH"
  , "PrimDegrees"
  , "PrimRadians"
  , "PrimSin"
  , "PrimSinH"
  , "PrimTan"
  , "PrimTanH"
  -- Exponential Functions
  , "PrimPow"
  , "PrimExp"
  , "PrimLog"
  , "PrimExp2"
  , "PrimLog2"
  , "PrimSqrt"
  , "PrimInvSqrt"
  -- Common Functions
  , "PrimIsNan"
  , "PrimIsInf"
  , "PrimAbs"
  , "PrimSign"
  , "PrimFloor"
  , "PrimTrunc"
  , "PrimRound"
  , "PrimRoundEven"
  , "PrimCeil"
  , "PrimFract"
  , "PrimModF"
  , "PrimMin"
  , "PrimMinS"
  , "PrimMax"
  , "PrimMaxS"
  , "PrimClamp"
  , "PrimClampS"
  , "PrimMix"
  , "PrimMixS"
  , "PrimMixB"
  , "PrimStep"
  , "PrimStepS"
  , "PrimSmoothStep"
  , "PrimSmoothStepS"
  -- Integer/Float Conversion Functions
  , "PrimFloatBitsToInt"
  , "PrimFloatBitsToUInt"
  , "PrimIntBitsToFloat"
  , "PrimUIntBitsToFloat"
  -- Geometric Functions
  , "PrimLength"
  , "PrimDistance"
  , "PrimDot"
  , "PrimCross"
  , "PrimNormalize"
  , "PrimFaceForward"
  , "PrimReflect"
  , "PrimRefract"
  -- Matrix Functions
  , "PrimTranspose"
  , "PrimDeterminant"
  , "PrimInverse"
  , "PrimOuterProduct"
  , "PrimMulMatVec"
  , "PrimMulVecMat"
  , "PrimMulMatMat"
  -- Vector and Scalar Relational Functions
  , "PrimLessThan"
  , "PrimLessThanEqual"
  , "PrimGreaterThan"
  , "PrimGreaterThanEqual"
  , "PrimEqualV"
  , "PrimEqual"
  , "PrimNotEqualV"
  , "PrimNotEqual"
  -- Fragment Processing Functions
  , "PrimDFdx"
  , "PrimDFdy"
  , "PrimFWidth"
  -- Noise Functions
  , "PrimNoise1"
  , "PrimNoise2"
  , "PrimNoise3"
  , "PrimNoise4"
  ]

{-
    -- Vec/Mat (de)construction
    PrimTupToV2             :: IsComponent a                            => PrimFun stage ((a,a)     -> V2 a)
    PrimTupToV3             :: IsComponent a                            => PrimFun stage ((a,a,a)   -> V3 a)
    PrimTupToV4             :: IsComponent a                            => PrimFun stage ((a,a,a,a) -> V4 a)
    PrimV2ToTup             :: IsComponent a                            => PrimFun stage (V2 a     -> (a,a))
    PrimV3ToTup             :: IsComponent a                            => PrimFun stage (V3 a   -> (a,a,a))
    PrimV4ToTup             :: IsComponent a                            => PrimFun stage (V4 a -> (a,a,a,a))
-}

backPropTF :: Ty -> TypeFun Ty -> [(Ty, Ty)]
backPropTF (TM22F C) (TFMat a b) = [(a, TV2F C), (b, TV2F C)]
backPropTF (TM23F C) (TFMat a b) = [(a, TV2F C), (b, TV3F C)]
backPropTF (TM24F C) (TFMat a b) = [(a, TV2F C), (b, TV4F C)]
backPropTF (TM32F C) (TFMat a b) = [(a, TV3F C), (b, TV2F C)]
backPropTF (TM33F C) (TFMat a b) = [(a, TV3F C), (b, TV3F C)]
backPropTF (TM34F C) (TFMat a b) = [(a, TV3F C), (b, TV4F C)]
backPropTF (TM42F C) (TFMat a b) = [(a, TV4F C), (b, TV2F C)]
backPropTF (TM43F C) (TFMat a b) = [(a, TV4F C), (b, TV3F C)]
backPropTF (TM44F C) (TFMat a b) = [(a, TV4F C), (b, TV4F C)]
backPropTF (TV2F C) (TFVec a b) = [(a, TNat 2), (b, TFloat C)]
backPropTF (TV3F C) (TFVec a b) = [(a, TNat 3), (b, TFloat C)]
backPropTF (TV4F C) (TFVec a b) = [(a, TNat 4), (b, TFloat C)]
backPropTF (TV2I C) (TFVec a b) = [(a, TNat 2), (b, TInt C)]
backPropTF (TV3I C) (TFVec a b) = [(a, TNat 3), (b, TInt C)]
backPropTF (TV4I C) (TFVec a b) = [(a, TNat 4), (b, TInt C)]
backPropTF (TV2U C) (TFVec a b) = [(a, TNat 2), (b, TWord C)]
backPropTF (TV3U C) (TFVec a b) = [(a, TNat 3), (b, TWord C)]
backPropTF (TV4U C) (TFVec a b) = [(a, TNat 4), (b, TWord C)]
backPropTF (TV2B C) (TFVec a b) = [(a, TNat 2), (b, TBool C)]
backPropTF (TV3B C) (TFVec a b) = [(a, TNat 3), (b, TBool C)]
backPropTF (TV4B C) (TFVec a b) = [(a, TNat 4), (b, TBool C)]
backPropTF b' (TFVecScalar (TNat 1) b) = [(b, b')]
backPropTF (TFloat C) (TFVecScalar a b) = [(a, TNat 1), (b, TFloat C)]
backPropTF (TInt C)   (TFVecScalar a b) = [(a, TNat 1), (b, TInt C)]
backPropTF (TWord C)  (TFVecScalar a b) = [(a, TNat 1), (b, TWord C)]
backPropTF (TBool C)  (TFVecScalar a b) = [(a, TNat 1), (b, TBool C)]
backPropTF x (TFVecScalar a b) = backPropTF x (TFVec a b)
backPropTF _ _ = []

reduceTF :: (Ty -> e) -> (String -> e) -> e -> TypeFun Ty -> e
reduceTF reduced fail nothing = \case
    TFMat (TV2F C) (TV2F C) -> reduced $ TM22F C
    TFMat (TV2F C) (TV3F C) -> reduced $ TM23F C
    TFMat (TV2F C) (TV4F C) -> reduced $ TM24F C
    TFMat (TV3F C) (TV2F C) -> reduced $ TM32F C
    TFMat (TV3F C) (TV3F C) -> reduced $ TM33F C
    TFMat (TV3F C) (TV4F C) -> reduced $ TM34F C
    TFMat (TV4F C) (TV2F C) -> reduced $ TM42F C
    TFMat (TV4F C) (TV3F C) -> reduced $ TM43F C
    TFMat (TV4F C) (TV4F C) -> reduced $ TM44F C
    TFMat TVar{} TVar{} -> nothing
    TFMat TVar{} x | x `elem` [TV2F C, TV3F C, TV4F C] -> nothing
    TFMat x TVar{} | x `elem` [TV2F C, TV3F C, TV4F C] -> nothing
    TFMat _ _ -> fail "no instance"

    TFMatVecElem (TV2F C)  -> reduced $ TFloat C
    TFMatVecElem (TV3F C)  -> reduced $ TFloat C
    TFMatVecElem (TV4F C)  -> reduced $ TFloat C
    TFMatVecElem (TV2I C)  -> reduced $ TInt C
    TFMatVecElem (TV3I C)  -> reduced $ TInt C
    TFMatVecElem (TV4I C)  -> reduced $ TInt C
    TFMatVecElem (TV2U C)  -> reduced $ TWord C
    TFMatVecElem (TV3U C)  -> reduced $ TWord C
    TFMatVecElem (TV4U C)  -> reduced $ TWord C
    TFMatVecElem (TV2B C)  -> reduced $ TBool C
    TFMatVecElem (TV3B C)  -> reduced $ TBool C
    TFMatVecElem (TV4B C)  -> reduced $ TBool C
    TFMatVecElem (TM22F C) -> reduced $ TFloat C
    TFMatVecElem (TM23F C) -> reduced $ TFloat C
    TFMatVecElem (TM24F C) -> reduced $ TFloat C
    TFMatVecElem (TM32F C) -> reduced $ TFloat C
    TFMatVecElem (TM33F C) -> reduced $ TFloat C
    TFMatVecElem (TM34F C) -> reduced $ TFloat C
    TFMatVecElem (TM42F C) -> reduced $ TFloat C
    TFMatVecElem (TM43F C) -> reduced $ TFloat C
    TFMatVecElem (TM44F C) -> reduced $ TFloat C
    TFMatVecElem TVar{} -> nothing
    TFMatVecElem _ -> fail "no instance"

    TFMatVecScalarElem (TFloat C) -> reduced $ TFloat C
    TFMatVecScalarElem (TInt C)   -> reduced $ TInt C
    TFMatVecScalarElem (TWord C)  -> reduced $ TWord C
    TFMatVecScalarElem (TBool C)  -> reduced $ TBool C
    TFMatVecScalarElem t -> reduceTF reduced fail nothing $ TFMatVecElem t

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
    TFVec TVar{} TVar{} -> nothing
    TFVec TVar{} x | x `elem` [TFloat C, TInt C, TWord C, TBool C] -> nothing
    TFVec n TVar{} | n `elem` [TNat 2, TNat 3, TNat 4] -> nothing
    TFVec _ _ -> fail "no instance"

    TFVecScalar (TNat 1) ty -> case ty of
        _ | ty `elem` [TFloat C, TInt C, TWord C, TBool C] -> reduced ty
        TVar{} -> nothing
        _ -> fail "no instace"
    TFVecScalar n ty -> reduceTF reduced fail nothing $ TFVec n ty

    TFFTRepr' ty -> case ty of
        TTuple C ts -> maybe (fail "expected Input/Interpolated/Depth/Color inside tuple") (maybe nothing (reduced . TTuple C) . sequence) $ mapM f ts
        _ -> maybe (fail "expected Input/Interpolated/Depth/Color") (maybe nothing reduced) $ f ty
      where
        f = \case
            TInterpolated C a -> Just $ Just a
            TInput C a        -> Just $ Just a
            Depth a           -> Just $ Just a
            Color a           -> Just $ Just a
            TVar{} -> Just Nothing
            _ -> Nothing

    TFFragOps ty -> case ty of
        TTuple C ts -> maybe (fail "expected FragmentOperation inside tuple") (maybe nothing (reduced . TTuple C) . sequence) $ mapM f ts
        _ -> maybe (fail "expected FragmentOperation") (maybe nothing reduced) $ f ty
      where
        f = \case
            TFragmentOperation C a -> Just $ Just a
            TVar{} -> Just Nothing
            _ -> Nothing

    TFColorRepr ty -> case ty of
        TTuple C ts -> reduced . TTuple C $ map Color ts
        TVar{} -> nothing
        ty -> reduced $ Color ty

    TFFrameBuffer ty -> case ty of
        TTuple C ts -> maybe (fail "expected (Image Nat) inside tuple") (maybe nothing end . sequence) $ mapM f ts
        _ -> maybe (fail "expected (Image Nat)") (maybe nothing (end . (:[]))) $ f ty
      where
        f = \case
            TImage C (TNat n) ty -> Just $ Just (n, ty)
            TImage C TVar{} _ -> Just Nothing
            TVar{} -> Just Nothing
            _ -> Nothing

        end (unzip -> (ns, tys))
            | Just n <- allSame ns = reduced $ TFrameBuffer C (TNat n) $ tTuple C tys
            | otherwise = fail "frambuffer number of layers differ"

        allSame (x:xs) = if all (==x) xs then Just x else Nothing

        tTuple f [x] = x
        tTuple f xs = TTuple f xs

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
    f -> nothing

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

-- reduce: reduce class constraints:  Eq [a] --> Eq a
isInstance :: (Class -> Ty -> e) -> (String -> e) -> e -> e -> Class -> Ty -> e
isInstance reduce fail keep ok = f where

    f _ TVar{} = keep

    f IsTypeLevelNatural (TNat _) = ok

    f IsValidOutput _ = ok -- TODO

    f IsNumComponent (TFloat _) = ok
    f IsNumComponent (TInt _) = ok
    f IsNumComponent (TWord _) = ok
    f IsNumComponent (TV2F _) = ok
    f IsNumComponent (TV3F _) = ok
    f IsNumComponent (TV4F _) = ok

    f IsSigned (TFloat _) = ok
    f IsSigned (TInt _) = ok

    f IsNum (TFloat _) = ok
    f IsNum (TInt _) = ok
    f IsNum (TWord _) = ok

    f IsIntegral (TInt _) = ok
    f IsIntegral (TWord _) = ok

    f IsFloating (TFloat _) = ok
    f IsFloating (TV2F   _) = ok
    f IsFloating (TV3F   _) = ok
    f IsFloating (TV4F   _) = ok
    f IsFloating (TM22F  _) = ok
    f IsFloating (TM23F  _) = ok
    f IsFloating (TM24F  _) = ok
    f IsFloating (TM32F  _) = ok
    f IsFloating (TM33F  _) = ok
    f IsFloating (TM34F  _) = ok
    f IsFloating (TM42F  _) = ok
    f IsFloating (TM43F  _) = ok
    f IsFloating (TM44F  _) = ok

    f IsComponent (TFloat _) = ok
    f IsComponent (TInt _) = ok
    f IsComponent (TWord _) = ok
    f IsComponent (TBool _) = ok
    f IsComponent (TV2F _) = ok
    f IsComponent (TV3F _) = ok
    f IsComponent (TV4F _) = ok

    f IsValidFrameBuffer (TTuple C ts)
        | sum [1 | TVar{} <- ts] > 0 = keep
        | sum [1 | Depth{} <- ts] <= 1 && sum [1 | Stencil{} <- ts] <= 1 = ok
        | otherwise = fail "not valid framebuffer"
    f IsValidFrameBuffer _ = ok

    f IsInputTuple (TTuple C ts)
        | sum [1 | TVar{} <- ts] > 0 = keep
        | length [() | TInput{} <- ts] == length ts = ok
        | otherwise = fail "not valid attribute input definition"
    f IsInputTuple _ = ok

    f c t = case Map.lookup c instances of
        Nothing -> fail $ "no " ++ show c ++ " instance for " ++ show t
        Just ts -> if Set.member t ts then ok else fail $ "no " ++ show c ++ " instance for " ++ show t

instances :: Map Class (Set Ty)
instances = Map.fromList [(CNum,Set.fromList [TInt C,TFloat C])]

joinTupleType :: Ty -> Ty -> Ty
joinTupleType (TTuple f l) (TTuple _ r) = TTuple f (l ++ r)
joinTupleType l (TTuple f r) = TTuple f (l : r)
joinTupleType (TTuple f l) r = TTuple f (l ++ [r])
joinTupleType l r = TTuple C [l,r]

inferPrimFun :: (Typing -> Unique e) -> Unique e -> EName -> Unique e
inferPrimFun ok nothing = f where

 infix 6 ==>
 cs ==> t = ok $ Typing mempty cs t

 f = \case
  -- temporary const constructor
  "Tup"          -> newV $ \a -> [] ==> a ~> a
  "Const"        -> newV $ \a -> [] ==> a ~> a
  -- Vector/Matrix
  "True"         -> [] ==> TBool C
  "False"        -> [] ==> TBool C
  "V2B"          -> [] ==> TBool C ~> TBool C ~> TV2B C
  "V3B"          -> [] ==> TBool C ~> TBool C ~> TBool C ~> TV3B C
  "V4B"          -> [] ==> TBool C ~> TBool C ~> TBool C ~> TBool C ~> TV4B C
  "V2U"          -> [] ==> TWord C ~> TWord C ~> TV2U C
  "V3U"          -> [] ==> TWord C ~> TWord C ~> TWord C ~> TV3U C
  "V4U"          -> [] ==> TWord C ~> TWord C ~> TWord C ~> TWord C ~> TV4U C
  "V2I"          -> [] ==> TInt C ~> TInt C ~> TV2I C
  "V3I"          -> [] ==> TInt C ~> TInt C ~> TInt C ~> TV3I C
  "V4I"          -> [] ==> TInt C ~> TInt C ~> TInt C ~> TInt C ~> TV4I C
  "V2F"          -> [] ==> TFloat C ~> TFloat C ~> TV2F C
  "V3F"          -> [] ==> TFloat C ~> TFloat C ~> TFloat C ~> TV3F C
  "V4F"          -> [] ==> TFloat C ~> TFloat C ~> TFloat C ~> TFloat C ~> TV4F C
  "M22F"         -> [] ==> TV2F C ~> TV2F C ~> TM22F C
  "M23F"         -> [] ==> TV2F C ~> TV2F C ~> TV2F C ~> TM23F C
  "M24F"         -> [] ==> TV2F C ~> TV2F C ~> TV2F C ~> TV2F C ~> TM24F C
  "M32F"         -> [] ==> TV3F C ~> TV3F C ~> TM32F C
  "M33F"         -> [] ==> TV3F C ~> TV3F C ~> TV3F C ~> TM33F C
  "M34F"         -> [] ==> TV3F C ~> TV3F C ~> TV3F C ~> TV3F C ~> TM34F C
  "M42F"         -> [] ==> TV4F C ~> TV4F C ~> TM42F C
  "M43F"         -> [] ==> TV4F C ~> TV4F C ~> TV4F C ~> TM43F C
  "M44F"         -> [] ==> TV4F C ~> TV4F C ~> TV4F C ~> TV4F C ~> TM44F C
  -- Input declaration
  "Uni"          -> newV $ \t -> [] ==> TInput C t ~> t
  "IBool"        -> [] ==> TString C ~> TInput C (TBool  C)
  "IV2B"         -> [] ==> TString C ~> TInput C (TV2B   C)
  "IV3B"         -> [] ==> TString C ~> TInput C (TV3B   C)
  "IV4B"         -> [] ==> TString C ~> TInput C (TV4B   C)
  "IWord"        -> [] ==> TString C ~> TInput C (TWord  C)
  "IV2U"         -> [] ==> TString C ~> TInput C (TV2U   C)
  "IV3U"         -> [] ==> TString C ~> TInput C (TV3U   C)
  "IV4U"         -> [] ==> TString C ~> TInput C (TV4U   C)
  "IInt"         -> [] ==> TString C ~> TInput C (TInt   C)
  "IV2I"         -> [] ==> TString C ~> TInput C (TV2I   C)
  "IV3I"         -> [] ==> TString C ~> TInput C (TV3I   C)
  "IV4I"         -> [] ==> TString C ~> TInput C (TV4I   C)
  "IFloat"       -> [] ==> TString C ~> TInput C (TFloat C)
  "IV2F"         -> [] ==> TString C ~> TInput C (TV2F   C)
  "IV3F"         -> [] ==> TString C ~> TInput C (TV3F   C)
  "IV4F"         -> [] ==> TString C ~> TInput C (TV4F   C)
  "IM22F"        -> [] ==> TString C ~> TInput C (TM22F  C)
  "IM23F"        -> [] ==> TString C ~> TInput C (TM23F  C)
  "IM24F"        -> [] ==> TString C ~> TInput C (TM24F  C)
  "IM32F"        -> [] ==> TString C ~> TInput C (TM32F  C)
  "IM33F"        -> [] ==> TString C ~> TInput C (TM33F  C)
  "IM34F"        -> [] ==> TString C ~> TInput C (TM34F  C)
  "IM42F"        -> [] ==> TString C ~> TInput C (TM42F  C)
  "IM43F"        -> [] ==> TString C ~> TInput C (TM43F  C)
  "IM44F"        -> [] ==> TString C ~> TInput C (TM44F  C)
  -- BlendingFactor
  "Zero"                   -> [] ==> TBlendingFactor C
  "One"                    -> [] ==> TBlendingFactor C
  "SrcColor"               -> [] ==> TBlendingFactor C
  "OneMinusSrcColor"       -> [] ==> TBlendingFactor C
  "DstColor"               -> [] ==> TBlendingFactor C
  "OneMinusDstColor"       -> [] ==> TBlendingFactor C
  "SrcAlpha"               -> [] ==> TBlendingFactor C
  "OneMinusSrcAlpha"       -> [] ==> TBlendingFactor C
  "DstAlpha"               -> [] ==> TBlendingFactor C
  "OneMinusDstAlpha"       -> [] ==> TBlendingFactor C
  "ConstantColor"          -> [] ==> TBlendingFactor C
  "OneMinusConstantColor"  -> [] ==> TBlendingFactor C
  "ConstantAlpha"          -> [] ==> TBlendingFactor C
  "OneMinusConstantAlpha"  -> [] ==> TBlendingFactor C
  "SrcAlphaSaturate"       -> [] ==> TBlendingFactor C
  -- BlendEquation
  "FuncAdd"                -> [] ==> TBlendEquation C
  "FuncSubtract"           -> [] ==> TBlendEquation C
  "FuncReverseSubtract"    -> [] ==> TBlendEquation C
  "Min"                    -> [] ==> TBlendEquation C
  "Max"                    -> [] ==> TBlendEquation C
  -- LogicOperation
  "Clear"        -> [] ==> TLogicOperation C
  "And"          -> [] ==> TLogicOperation C
  "AndReverse"   -> [] ==> TLogicOperation C
  "Copy"         -> [] ==> TLogicOperation C
  "AndInverted"  -> [] ==> TLogicOperation C
  "Noop"         -> [] ==> TLogicOperation C
  "Xor"          -> [] ==> TLogicOperation C
  "Or"           -> [] ==> TLogicOperation C
  "Nor"          -> [] ==> TLogicOperation C
  "Equiv"        -> [] ==> TLogicOperation C
  "Invert"       -> [] ==> TLogicOperation C
  "OrReverse"    -> [] ==> TLogicOperation C
  "CopyInverted" -> [] ==> TLogicOperation C
  "OrInverted"   -> [] ==> TLogicOperation C
  "Nand"         -> [] ==> TLogicOperation C
  "Set"          -> [] ==> TLogicOperation C
  -- StencilOperation
  "OpZero"       -> [] ==> TStencilOperation C
  "OpKeep"       -> [] ==> TStencilOperation C
  "OpReplace"    -> [] ==> TStencilOperation C
  "OpIncr"       -> [] ==> TStencilOperation C
  "OpIncrWrap"   -> [] ==> TStencilOperation C
  "OpDecr"       -> [] ==> TStencilOperation C
  "OpDecrWrap"   -> [] ==> TStencilOperation C
  "OpInvert"     -> [] ==> TStencilOperation C
  -- ComparisonFunction
  "Never"    -> [] ==> TComparisonFunction C
  "Less"     -> [] ==> TComparisonFunction C
  "Equal"    -> [] ==> TComparisonFunction C
  "Lequal"   -> [] ==> TComparisonFunction C
  "Greater"  -> [] ==> TComparisonFunction C
  "Notequal" -> [] ==> TComparisonFunction C
  "Gequal"   -> [] ==> TComparisonFunction C
  "Always"   -> [] ==> TComparisonFunction C
  -- ProvokingVertex
  "LastVertex"   -> [] ==> TProvokingVertex C
  "FirstVertex"  -> [] ==> TProvokingVertex C
  -- CullMode
  "CullNone"     -> [] ==> TCullMode C
  "CullFront"    -> [] ==> TFrontFace C ~> TCullMode C
  "CullBack"     -> [] ==> TFrontFace C ~> TCullMode C
  -- FrontFace
  "CW"   -> [] ==> TFrontFace C
  "CCW"  -> [] ==> TFrontFace C
  -- PolygonMode
  "PolygonFill"  -> [] ==> TPolygonMode C
  "PolygonPoint" -> [] ==> TPointSize C ~> TPolygonMode C
  "PolygonLine"  -> [] ==> TFloat C ~> TPolygonMode C
  -- PolygonOffset
  "NoOffset" -> [] ==> TPolygonOffset C
  "Offset"   -> [] ==> TFloat C ~> TFloat C ~> TPolygonOffset C
  -- PointSize
  "PointSize"        -> [] ==> TFloat C ~> TPointSize C
  "ProgramPointSize" -> [] ==> TPointSize C
  -- Fragment Out
  {-
    FragmentOut             ::                  FlatExp F a -> FragmentOut (ColorRepr a)
    FragmentOutDepth        :: Exp F Float  ->  FlatExp F a -> FragmentOut (Depth Float :+: ColorRepr a)
    FragmentOutRastDepth    ::                  FlatExp F a -> FragmentOut (Depth Float :+: ColorRepr a)
  -}
  "FragmentOut"           -> newV $ \a t -> [a ~~ TFColorRepr t] ==> t ~> TFragmentOut C a
  "FragmentOutDepth"      -> newV $ \a t -> [a ~~ TFColorRepr t] ==> TFloat C ~> t ~> TFragmentOut C (joinTupleType (Depth $ TFloat C) a)
  "FragmentOutRastDepth"  -> newV $ \a t -> [a ~~ TFColorRepr t] ==> t ~> TFragmentOut C (joinTupleType (Depth $ TFloat C) a)
  -- Vertex Out
  "VertexOut"    -> newV $ \a t -> [t ~~ TFFTRepr' a] ==> TV4F C ~> TFloat C ~> TTuple C [] ~> a ~> TVertexOut C t
  -- PointSpriteCoordOrigin
  "LowerLeft"  -> [] ==> TPointSpriteCoordOrigin C
  "UpperLeft"  -> [] ==> TPointSpriteCoordOrigin C
  -- Raster Context
  "TriangleCtx"  -> [] ==> TCullMode C ~> TPolygonMode C ~> TPolygonOffset C ~> TProvokingVertex C ~> TRasterContext C TTriangle
  "PointCtx"     -> [] ==> TPointSize C ~> TFloat C ~> TPointSpriteCoordOrigin C ~> TRasterContext C TPoint
  "LineCtx"      -> [] ==> TFloat C ~> TProvokingVertex C ~> TRasterContext C TLine
  -- Fetch Primitive
  "Points"             -> [] ==> TFetchPrimitive C TPoint
  "Lines"              -> [] ==> TFetchPrimitive C TLine
  "Triangles"          -> [] ==> TFetchPrimitive C TTriangle
  "LinesAdjacency"     -> [] ==> TFetchPrimitive C TLineAdjacency
  "TrianglesAdjacency" -> [] ==> TFetchPrimitive C TTriangleAdjacency
  -- Accumulation Context
  "AccumulationContext"  -> newV $ \t' t -> [t' ~~ TFFragOps t] ==> t ~> TAccumulationContext C t'
  -- Image
  "ColorImage" -> newV $ \a d color t ->
    [isTypeLevelNatural a, isNum t, color ~~ vecS d t] ==> a ~> color ~> TImage C a (Color color)
  "DepthImage" -> newV $ \a ->
    [isTypeLevelNatural a] ==> a ~> TFloat C ~> TImage C a (Depth $ TFloat C)
  "StencilImage" -> newV $ \a ->
    [isTypeLevelNatural a] ==> a ~> TInt C ~> TImage C a (Stencil $ TInt C)
  -- Interpolation
  "Flat"           -> newV $ \t -> [] ==> t ~> TInterpolated C t
  "Smooth"         -> newV $ \t -> [isFloating t] ==> t ~> TInterpolated C t
  "NoPerspective"  -> newV $ \t -> [isFloating t] ==> t ~> TInterpolated C t
  -- Fragment Operation
  "ColorOp"    -> newV $ \d mask c color ->
    [mask ~~ vecS d (TBool C), color ~~ vecS d c, isNum c] ==> TBlending C c ~> mask ~> TFragmentOperation C (Color color)
  "DepthOp"    -> [] ==> TComparisonFunction C ~> TBool C ~> TFragmentOperation C (Depth $ TFloat C)
    -- "StencilOp       :: StencilTests -> StencilOps -> StencilOps -> FragmentOperation (Stencil Int32)
  -- Blending
  "NoBlending"   -> newV $ \t -> [] ==> TBlending C t
  "BlendLogicOp" -> newV $ \t -> [isIntegral t] ==> TLogicOperation C ~> TBlending C t
  "Blend"        -> [] ==> TTuple C [TBlendEquation C,TBlendEquation C]
                         ~> TTuple C [TTuple C [TBlendingFactor C,TBlendingFactor C],TTuple C [TBlendingFactor C,TBlendingFactor C]]
                         ~> TV4F C ~> TBlending C (TFloat C)
  -- Fragment Filter
  "PassAll"  -> newV $ \t -> [] ==> TFragmentFilter C t
  "Filter"   -> newV $ \t -> [] ==> (t ~> TBool C) ~> TFragmentFilter C t
  -- Render Operations
  "Fetch"        -> newV $ \a t b -> [CClass IsInputTuple t, b ~~ TFFTRepr' t] ==> TString C ~> TFetchPrimitive C a ~> t ~> TVertexStream C a b
  "Transform"    -> newV $ \a b p -> [] ==> (a ~> TVertexOut C b) ~> TVertexStream C p a ~> TPrimitiveStream C p (TNat 1) C b
  "Rasterize"    -> newV $ \a b c -> [] ==> TRasterContext C a ~> TPrimitiveStream C a b C c ~> TFragmentStream C b c
  {-
    Accumulate      :: (GPU a, GPU (FTRepr' b), IsValidOutput b)    -- restriction: depth and stencil optional, arbitrary color component
                    => AccumulationContext b
                    -> FragmentFilter a
                    -> (Exp F a -> FragmentOut (NoStencilRepr b))     -- fragment shader
                    -> Exp Obj (FragmentStream layerCount a)
                    -> Exp Obj (FrameBuffer layerCount (FTRepr' b))
                    -> Exp Obj (FrameBuffer layerCount (FTRepr' b))
  -}
  "Accumulate" -> newV $ \a b n t ->
    [isValidOutput b, t ~~ TFFTRepr' b] ==>
           TAccumulationContext C b
        ~> TFragmentFilter C a
        ~> (a ~> TFragmentOut C b)
        ~> TFragmentStream C n a
        ~> TFrameBuffer C n t
        ~> TFrameBuffer C n t
  "FrameBuffer"  -> newV $ \a t t' n ->
    [t' ~~ TFFTRepr' t, CClass IsValidFrameBuffer t, TFrameBuffer C n t ~~ TFFrameBuffer a] ==> a ~> TFrameBuffer C n t'
  "ScreenOut"    -> newV $ \a b -> [] ==> TFrameBuffer C a b ~> TOutput C
  -- * Primitive Functions *
  -- Arithmetic Functions (componentwise)
  "PrimAdd"   -> newV $ \a t -> [t ~~ TFMatVecElem a, isNum t] ==> a ~> a ~> a
  "PrimAddS"  -> newV $ \a t -> [t ~~ TFMatVecScalarElem a, isNum t] ==> a ~> t ~> a
  "PrimSub"   -> newV $ \a t -> [t ~~ TFMatVecElem a, isNum t] ==> a ~> a ~> a
  "PrimSubS"  -> newV $ \a t -> [t ~~ TFMatVecScalarElem a, isNum t] ==> a ~> t ~> a
  "PrimMul"   -> newV $ \a t -> [t ~~ TFMatVecElem a, isNum t] ==> a ~> a ~> a
  "PrimMulS"  -> newV $ \a t -> [t ~~ TFMatVecScalarElem a, isNum t] ==> a ~> t ~> a
  "PrimDiv"   -> newV $ \d a t -> [isNum t, a ~~ vecS d t] ==> a ~> a ~> a
  "PrimDivS"  -> newV $ \d a t -> [isNum t, a ~~ vecS d t] ==> a ~> t ~> a
  "PrimNeg"   -> newV $ \a t -> [t ~~ TFMatVecScalarElem a, isSigned t] ==> a ~> a
  "PrimMod"   -> newV $ \d a t -> [isNum t, a ~~ vecS d t] ==> a ~> a ~> a
  "PrimModS"  -> newV $ \d a t -> [isNum t, a ~~ vecS d t] ==> a ~> t ~> a
  -- Bit-wise Functions
  "PrimBAnd"      -> newV $ \d a t -> [isIntegral t, a ~~ vecS d t] ==> a ~> a ~> a
  "PrimBAndS"     -> newV $ \d a t -> [isIntegral t, a ~~ vecS d t] ==> a ~> t ~> a
  "PrimBOr"       -> newV $ \d a t -> [isIntegral t, a ~~ vecS d t] ==> a ~> a ~> a
  "PrimBOrS"      -> newV $ \d a t -> [isIntegral t, a ~~ vecS d t] ==> a ~> t ~> a
  "PrimBXor"      -> newV $ \d a t -> [isIntegral t, a ~~ vecS d t] ==> a ~> a ~> a
  "PrimBXorS"     -> newV $ \d a t -> [isIntegral t, a ~~ vecS d t] ==> a ~> t ~> a
  "PrimBNot"      -> newV $ \d a t -> [isIntegral t, a ~~ vecS d t] ==> a ~> a
  "PrimBShiftL"   -> newV $ \d a b t -> [isIntegral t, a ~~ vecS d t, b ~~ vecS d (TWord C)] ==> a ~> b ~> a
  "PrimBShiftLS"  -> newV $ \d a t -> [isIntegral t, a ~~ vecS d t] ==> a ~> TWord C ~> a
  "PrimBShiftR"   -> newV $ \d a b t -> [isIntegral t, a ~~ vecS d t, b ~~ vecS d (TWord C)] ==> a ~> b ~> a
  "PrimBShiftRS"  -> newV $ \d a t -> [isIntegral t, a ~~ vecS d t] ==> a ~> TWord C ~> a
  -- Logic Functions
  "PrimAnd" -> [] ==> TBool C ~> TBool C ~> TBool C
  "PrimOr"  -> [] ==> TBool C ~> TBool C ~> TBool C
  "PrimXor" -> [] ==> TBool C ~> TBool C ~> TBool C
  "PrimNot" -> newV $ \d a -> [a ~~ vecS d (TBool C)] ==> a ~> a
  "PrimAny" -> newV $ \d a -> [a ~~ vecS d (TBool C)] ==> a ~> TBool C
  "PrimAll" -> newV $ \d a -> [a ~~ vecS d (TBool C)] ==> a ~> TBool C
  -- Angle and Trigonometry Functions
  "PrimACos"    -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimACosH"   -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimASin"    -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimASinH"   -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimATan"    -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimATan2"   -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a ~> a
  "PrimATanH"   -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimCos"     -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimCosH"    -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimDegrees" -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimRadians" -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimSin"     -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimSinH"    -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimTan"     -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimTanH"    -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  -- Exponential Functions
  "PrimPow"     -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a ~> a
  "PrimExp"     -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimLog"     -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimExp2"    -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimLog2"    -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimSqrt"    -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimInvSqrt" -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  -- Common Functions
  "PrimIsNan"       -> newV $ \d a b -> [a ~~ vecS d (TFloat C), b ~~ vecS d (TBool C)] ==> a ~> b
  "PrimIsInf"       -> newV $ \d a b -> [a ~~ vecS d (TFloat C), b ~~ vecS d (TBool C)] ==> a ~> b
  "PrimAbs"         -> newV $ \d a t -> [isSigned t, a ~~ vecS d t] ==> a ~> a
  "PrimSign"        -> newV $ \d a t -> [isSigned t, a ~~ vecS d t] ==> a ~> a
  "PrimFloor"       -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimTrunc"       -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimRound"       -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimRoundEven"   -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimCeil"        -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimFract"       -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimModF"        -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> TTuple C [a,a]
  "PrimMin"         -> newV $ \d a t -> [isNum t, a ~~ vecS d t] ==> a ~> a ~> a
  "PrimMinS"        -> newV $ \d a t -> [isNum t, a ~~ vecS d t] ==> a ~> t ~> a
  "PrimMax"         -> newV $ \d a t -> [isNum t, a ~~ vecS d t] ==> a ~> a ~> a
  "PrimMaxS"        -> newV $ \d a t -> [isNum t, a ~~ vecS d t] ==> a ~> t ~> a
  "PrimClamp"       -> newV $ \d a t -> [isNum t, a ~~ vecS d t] ==> a ~> a ~> a ~> a
  "PrimClampS"      -> newV $ \d a t -> [isNum t, a ~~ vecS d t] ==> a ~> t ~> t ~> a
  "PrimMix"         -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a ~> a ~> a
  "PrimMixS"        -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a ~> TFloat C ~> a
  "PrimMixB"        -> newV $ \d a b -> [a ~~ vecS d (TFloat C), b ~~ vecS d (TBool C)] ==> a ~> a ~> b ~> a
  "PrimStep"        -> newV $ \d a -> [a ~~ TFVec d (TFloat C)] ==> a ~> a ~> a
  "PrimStepS"       -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> TFloat C ~> a ~> a
  "PrimSmoothStep"  -> newV $ \d a -> [a ~~ TFVec d (TFloat C)] ==> a ~> a ~> a ~> a
  "PrimSmoothStepS" -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> TFloat C ~> TFloat C ~> a ~> a
  -- Integer/Float Conversion Functions
  "PrimFloatBitsToInt"  -> newV $ \d fv iv -> [fv ~~ vecS d (TFloat C), iv ~~ vecS d (TInt C)] ==> fv ~> iv
  "PrimFloatBitsToUInt" -> newV $ \d fv uv -> [fv ~~ vecS d (TFloat C), uv ~~ vecS d (TWord C)] ==> fv ~> uv
  "PrimIntBitsToFloat"  -> newV $ \d fv iv -> [fv ~~ vecS d (TFloat C), iv ~~ vecS d (TInt C)] ==> iv ~> fv
  "PrimUIntBitsToFloat" -> newV $ \d fv uv -> [fv ~~ vecS d (TFloat C), uv ~~ vecS d (TWord C)] ==> uv ~> fv
  -- Geometric Functions
  "PrimLength"      -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> TFloat C
  "PrimDistance"    -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a ~> TFloat C
  "PrimDot"         -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a ~> TFloat C
  "PrimCross"       -> newV $ \a -> [a ~~ vecS (TNat 3) (TFloat C)] ==> a ~> a ~> a
  "PrimNormalize"   -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimFaceForward" -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a ~> a ~> a
  "PrimReflect"     -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a ~> a
  "PrimRefract"     -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a ~> a ~> a
  -- Matrix Functions
  "PrimTranspose"     -> newV $ \a b h w -> [a ~~ TFMat h w, b ~~ TFMat w h] ==> a ~> b
  "PrimDeterminant"   -> newV $ \m s -> [m ~~ TFMat s s] ==> m ~> TFloat C
  "PrimInverse"       -> newV $ \m s -> [m ~~ TFMat s s] ==> m ~> m
  "PrimOuterProduct"  -> newV $ \m h w -> [m ~~ TFMat h w] ==> w ~> h ~> m
  "PrimMulMatVec"     -> newV $ \m h w -> [m ~~ TFMat h w] ==> m ~> w ~> h
  "PrimMulVecMat"     -> newV $ \m h w -> [m ~~ TFMat h w] ==> h ~> m ~> w
  "PrimMulMatMat"     -> newV $ \a b c i j k -> [a ~~ TFMat i j, b ~~ TFMat j k, c ~~ TFMat i k] ==> a ~> b ~> c
  -- Vector and Scalar Relational Functions
  "PrimLessThan"          -> newV $ \d a b t -> [isNum t, a ~~ vecS d t, b ~~ vecS d (TBool C)] ==> a ~> a ~> b
  "PrimLessThanEqual"     -> newV $ \d a b t -> [isNum t, a ~~ vecS d t, b ~~ vecS d (TBool C)] ==> a ~> a ~> b
  "PrimGreaterThan"       -> newV $ \d a b t -> [isNum t, a ~~ vecS d t, b ~~ vecS d (TBool C)] ==> a ~> a ~> b
  "PrimGreaterThanEqual"  -> newV $ \d a b t -> [isNum t, a ~~ vecS d t, b ~~ vecS d (TBool C)] ==> a ~> a ~> b
  "PrimEqualV"            -> newV $ \d a b t -> [isNum t, a ~~ vecS d t, b ~~ vecS d (TBool C)] ==> a ~> a ~> b
  "PrimEqual"             -> newV $ \a t -> [t ~~ TFMatVecScalarElem a] ==> a ~> a ~> TBool C
  "PrimNotEqualV"         -> newV $ \d a b t -> [isNum t, a ~~ vecS d t, b ~~ vecS d (TBool C)] ==> a ~> a ~> b
  "PrimNotEqual"          -> newV $ \a t -> [t ~~ TFMatVecScalarElem a] ==> a ~> a ~> TBool C
  -- Fragment Processing Functions
  "PrimDFdx"    -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimDFdy"    -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  "PrimFWidth"  -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> a
  -- Noise Functions
  "PrimNoise1"  -> newV $ \d a -> [a ~~ vecS d (TFloat C)] ==> a ~> TFloat C
  "PrimNoise2"  -> newV $ \d a b -> [a ~~ vecS d (TFloat C), b ~~ vecS (TNat 2) (TFloat C)] ==> a ~> b
  "PrimNoise3"  -> newV $ \d a b -> [a ~~ vecS d (TFloat C), b ~~ vecS (TNat 3) (TFloat C)] ==> a ~> b
  "PrimNoise4"  -> newV $ \d a b -> [a ~~ vecS d (TFloat C), b ~~ vecS (TNat 4) (TFloat C)] ==> a ~> b

--  a -> throwErrorUnique $ "unknown primitive: " ++ show a
  a -> nothing

inferLit :: Lit -> Unique Typing
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
    (cs, _, _, _) <- get
    case cs of
        (Just _: _) -> throwError $ head $ catMaybes $ reverse cs
        _ -> return ()

throwErrorUnique :: String -> Unique a
throwErrorUnique s = checkUnambError >> errorUnique >>= throwError . (++ s)

errorUnique :: Unique String
errorUnique = do
  (_,_,src,rl) <- get
  let sl = map mkSpan rl
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
  return $ concat sl

class NewVar a where
    type NewVarRes a :: *
    newV :: a -> Unique (NewVarRes a)

instance NewVar (Unique a) where
    type NewVarRes (Unique a) = a
    newV = id

instance NewVar a => NewVar (Ty -> a) where
    type NewVarRes (Ty -> a) = NewVarRes a
    newV f = newVar C >>= \v -> newV $ f v

newVar :: Frequency -> Unique Ty
newVar f = do
  (d, n,s,r) <- get
  put (d, n+1,s,r)
  return $ TVar f $ 't':show n
