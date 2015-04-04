{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Typing where

import Data.List
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

type Unique = StateT
    (Int,ByteString,[Range])        -- (counter, complete source{-constant-}, stack of ranges)
    (Except String)

type Range = (Delta,Delta)

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

    -- TODO: TFMatVecScalarElem

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

    TFVecScalar (TNat 1) ty | True {-TODO-} -> reduced ty
    TFVecScalar (TNat n) ty | True {-TODO-} -> reduceTF reduced fail nothing $ TFVec (TNat n) ty

{- TODO
  type family FTRepr' a :: *
    type instance FTRepr' (i1 a :+: ZZ) = a
    type instance FTRepr' (i1 a :+: i2 b :+: ZZ) = (a, b)
    type instance FTRepr' (i1 a :+: i2 b :+: i3 c :+: ZZ) = (a, b, c)
    type instance FTRepr' (i1 a :+: i2 b :+: i3 c :+: i4 d :+: ZZ) = (a, b, c, d)
    type instance FTRepr' (i1 a :+: i2 b :+: i3 c :+: i4 d :+: i5 e :+: ZZ) = (a, b, c, d, e)
    type instance FTRepr' (i1 a :+: i2 b :+: i3 c :+: i4 d :+: i5 e :+: i6 f :+: ZZ) = (a, b, c, d, e, f)
    type instance FTRepr' (i1 a :+: i2 b :+: i3 c :+: i4 d :+: i5 e :+: i6 f :+: i7 g :+: ZZ) = (a, b, c, d, e, f, g)
    type instance FTRepr' (i1 a :+: i2 b :+: i3 c :+: i4 d :+: i5 e :+: i6 f :+: i7 g :+: i8 h :+: ZZ) = (a, b, c, d, e, f, g, h)
    type instance FTRepr' (i1 a :+: i2 b :+: i3 c :+: i4 d :+: i5 e :+: i6 f :+: i7 g :+: i8 h :+: i9 i :+: ZZ) = (a, b, c, d, e, f, g, h ,i)
-}
--    TFFTRepr' (TInterpolated C (TV4F C)) -> reduced $ TV4F C

{- TODO
  type family ColorRepr a :: *
    type instance ColorRepr ZZ = ZZ
    type instance ColorRepr (a :+: b) = Color a :+: (ColorRepr b)
-}
    TFFrameBuffer (TTuple C [TImage C (TNat 1) (Depth (TFloat C)), TImage C (TNat 1) (Color (TV4F C))])
        -> reduced $ TFrameBuffer C (TNat 1) (TTuple C [TFloat C, TV4F C])
{- currently not used
  [injective] type family PrimitiveVertices (primitive :: PrimitiveType) a
    type instance PrimitiveVertices Point a             = a
    type instance PrimitiveVertices Line a              = (a,a)
    type instance PrimitiveVertices LineAdjacency a     = (a,a,a,a)
    type instance PrimitiveVertices Triangle a          = (a,a,a)
    type instance PrimitiveVertices TriangleAdjacency a = (a,a,a,a,a,a)
-}
{- TODO
  type family NoStencilRepr a :: *
    type instance NoStencilRepr ZZ = ZZ
    type instance NoStencilRepr (Stencil a :+: b) = NoStencilRepr b
    type instance NoStencilRepr (Color a :+: b) = Color a :+: NoStencilRepr b
    type instance NoStencilRepr (Depth a :+: b) = Depth a :+: NoStencilRepr b
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
    = ITMat | ITVec | ITVecScalar | NoInj
    deriving (Eq, Ord, Show)

injType :: TypeFun Ty -> InjType
injType TFMat{} = ITMat
injType TFVec{} = ITVec
injType TFVecScalar{} = ITVecScalar
injType _ = NoInj

testInj :: [TypeFun Ty] -> [[Ty]]
testInj xs@(TFMat{}:_) = transpose [[a,b] | TFMat a b <- xs]
testInj xs@(TFVec{}:_) = transpose [[a,b] | TFVec a b <- xs]
testInj xs@(TFVecScalar{}:_) = transpose [[a,b] | TFVecScalar a b <- xs]
testInj xs = []


matVecElem t a       = t ~~ TFMatVecElem a
matVecScalarElem t a = t ~~ TFMatVecScalarElem a
fTRepr' t a          = t ~~ TFFTRepr' a
colorRepr t a        = t ~~ TFColorRepr a
frameBuffer t a      = t ~~ TFFrameBuffer a
isMat m h w          = m ~~ TFMat h w
isVec d v c          = v ~~ TFVec d c
isVecScalar d v c    = v ~~ TFVecScalar d c

infix 4 ~~
(~~) :: Ty -> TypeFun Ty -> Constraint Ty
t ~~ f = CEq t f

isValidOutput      = cClass IsValidOutput
isNum              = cClass CNum
isSigned           = cClass IsSigned
isIntegral         = cClass IsIntegral
isTypeLevelNatural = cClass IsTypeLevelNatural

cClass :: Class -> Ty -> Constraint Ty
cClass c ty = CClass c ty

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
    f c t = case Map.lookup c instances of
        Nothing -> fail $ "no " ++ show c ++ " instance for " ++ show t
        Just ts -> if Set.member t ts then ok else fail $ "no " ++ show c ++ " instance for " ++ show t

instances :: Map Class (Set Ty)
instances = Map.fromList [(CNum,Set.fromList [TInt C,TFloat C])]

joinTupleType :: Ty -> Ty -> Ty
joinTupleType (TTuple f l) (TTuple _ r) = TTuple f (l ++ r)
joinTupleType l (TTuple f r) = TTuple f (l : r)
joinTupleType (TTuple f l) r = TTuple f (l ++ [r])

inferPrimFun :: (Typing -> Unique e) -> Unique e -> EName -> Unique e
inferPrimFun ok nothing = f where

 ty t = ok (mempty, mempty, t)

 infix 6 ==>
 cs ==> t = ok (mempty, cs, t)

 f = \case
  -- temporary const constructor
  "Tup"          -> do [a] <- newVars 1 C ; ty $ a ~> a
  "Const"        -> do [a] <- newVars 1 C ; ty $ a ~> a
  -- Vector/Matrix
  "True"         -> ty $ TBool C
  "False"        -> ty $ TBool C
  "V2B"          -> ty $ TBool C ~> TBool C ~> TV2B C
  "V3B"          -> ty $ TBool C ~> TBool C ~> TBool C ~> TV3B C
  "V4B"          -> ty $ TBool C ~> TBool C ~> TBool C ~> TBool C ~> TV4B C
  "V2U"          -> ty $ TWord C ~> TWord C ~> TV2U C
  "V3U"          -> ty $ TWord C ~> TWord C ~> TWord C ~> TV3U C
  "V4U"          -> ty $ TWord C ~> TWord C ~> TWord C ~> TWord C ~> TV4U C
  "V2I"          -> ty $ TInt C ~> TInt C ~> TV2I C
  "V3I"          -> ty $ TInt C ~> TInt C ~> TInt C ~> TV3I C
  "V4I"          -> ty $ TInt C ~> TInt C ~> TInt C ~> TInt C ~> TV4I C
  "V2F"          -> ty $ TFloat C ~> TFloat C ~> TV2F C
  "V3F"          -> ty $ TFloat C ~> TFloat C ~> TFloat C ~> TV3F C
  "V4F"          -> ty $ TFloat C ~> TFloat C ~> TFloat C ~> TFloat C ~> TV4F C
  "M22F"         -> ty $ TV2F C ~> TV2F C ~> TM22F C
  "M23F"         -> ty $ TV2F C ~> TV2F C ~> TV2F C ~> TM23F C
  "M24F"         -> ty $ TV2F C ~> TV2F C ~> TV2F C ~> TV2F C ~> TM24F C
  "M32F"         -> ty $ TV3F C ~> TV3F C ~> TM32F C
  "M33F"         -> ty $ TV3F C ~> TV3F C ~> TV3F C ~> TM33F C
  "M34F"         -> ty $ TV3F C ~> TV3F C ~> TV3F C ~> TV3F C ~> TM34F C
  "M42F"         -> ty $ TV4F C ~> TV4F C ~> TM42F C
  "M43F"         -> ty $ TV4F C ~> TV4F C ~> TV4F C ~> TM43F C
  "M44F"         -> ty $ TV4F C ~> TV4F C ~> TV4F C ~> TV4F C ~> TM44F C
  -- Input declaration
  "Uni"          -> do t <- newVar C ; ty $ TInput C t ~> t
  "IBool"        -> ty $ TString C ~> TInput C (TBool  C)
  "IV2B"         -> ty $ TString C ~> TInput C (TV2B   C)
  "IV3B"         -> ty $ TString C ~> TInput C (TV3B   C)
  "IV4B"         -> ty $ TString C ~> TInput C (TV4B   C)
  "IWord"        -> ty $ TString C ~> TInput C (TWord  C)
  "IV2U"         -> ty $ TString C ~> TInput C (TV2U   C)
  "IV3U"         -> ty $ TString C ~> TInput C (TV3U   C)
  "IV4U"         -> ty $ TString C ~> TInput C (TV4U   C)
  "IInt"         -> ty $ TString C ~> TInput C (TInt   C)
  "IV2I"         -> ty $ TString C ~> TInput C (TV2I   C)
  "IV3I"         -> ty $ TString C ~> TInput C (TV3I   C)
  "IV4I"         -> ty $ TString C ~> TInput C (TV4I   C)
  "IFloat"       -> ty $ TString C ~> TInput C (TFloat C)
  "IV2F"         -> ty $ TString C ~> TInput C (TV2F   C)
  "IV3F"         -> ty $ TString C ~> TInput C (TV3F   C)
  "IV4F"         -> ty $ TString C ~> TInput C (TV4F   C)
  "IM22F"        -> ty $ TString C ~> TInput C (TM22F  C)
  "IM23F"        -> ty $ TString C ~> TInput C (TM23F  C)
  "IM24F"        -> ty $ TString C ~> TInput C (TM24F  C)
  "IM32F"        -> ty $ TString C ~> TInput C (TM32F  C)
  "IM33F"        -> ty $ TString C ~> TInput C (TM33F  C)
  "IM34F"        -> ty $ TString C ~> TInput C (TM34F  C)
  "IM42F"        -> ty $ TString C ~> TInput C (TM42F  C)
  "IM43F"        -> ty $ TString C ~> TInput C (TM43F  C)
  "IM44F"        -> ty $ TString C ~> TInput C (TM44F  C)
  -- BlendingFactor
  "Zero"                   -> ty $ TBlendingFactor C
  "One"                    -> ty $ TBlendingFactor C
  "SrcColor"               -> ty $ TBlendingFactor C
  "OneMinusSrcColor"       -> ty $ TBlendingFactor C
  "DstColor"               -> ty $ TBlendingFactor C
  "OneMinusDstColor"       -> ty $ TBlendingFactor C
  "SrcAlpha"               -> ty $ TBlendingFactor C
  "OneMinusSrcAlpha"       -> ty $ TBlendingFactor C
  "DstAlpha"               -> ty $ TBlendingFactor C
  "OneMinusDstAlpha"       -> ty $ TBlendingFactor C
  "ConstantColor"          -> ty $ TBlendingFactor C
  "OneMinusConstantColor"  -> ty $ TBlendingFactor C
  "ConstantAlpha"          -> ty $ TBlendingFactor C
  "OneMinusConstantAlpha"  -> ty $ TBlendingFactor C
  "SrcAlphaSaturate"       -> ty $ TBlendingFactor C
  -- BlendEquation
  "FuncAdd"                -> ty $ TBlendEquation C
  "FuncSubtract"           -> ty $ TBlendEquation C
  "FuncReverseSubtract"    -> ty $ TBlendEquation C
  "Min"                    -> ty $ TBlendEquation C
  "Max"                    -> ty $ TBlendEquation C
  -- LogicOperation
  "Clear"        -> ty $ TLogicOperation C
  "And"          -> ty $ TLogicOperation C
  "AndReverse"   -> ty $ TLogicOperation C
  "Copy"         -> ty $ TLogicOperation C
  "AndInverted"  -> ty $ TLogicOperation C
  "Noop"         -> ty $ TLogicOperation C
  "Xor"          -> ty $ TLogicOperation C
  "Or"           -> ty $ TLogicOperation C
  "Nor"          -> ty $ TLogicOperation C
  "Equiv"        -> ty $ TLogicOperation C
  "Invert"       -> ty $ TLogicOperation C
  "OrReverse"    -> ty $ TLogicOperation C
  "CopyInverted" -> ty $ TLogicOperation C
  "OrInverted"   -> ty $ TLogicOperation C
  "Nand"         -> ty $ TLogicOperation C
  "Set"          -> ty $ TLogicOperation C
  -- StencilOperation
  "OpZero"       -> ty $ TStencilOperation C
  "OpKeep"       -> ty $ TStencilOperation C
  "OpReplace"    -> ty $ TStencilOperation C
  "OpIncr"       -> ty $ TStencilOperation C
  "OpIncrWrap"   -> ty $ TStencilOperation C
  "OpDecr"       -> ty $ TStencilOperation C
  "OpDecrWrap"   -> ty $ TStencilOperation C
  "OpInvert"     -> ty $ TStencilOperation C
  -- ComparisonFunction
  "Never"    -> ty $ TComparisonFunction C
  "Less"     -> ty $ TComparisonFunction C
  "Equal"    -> ty $ TComparisonFunction C
  "Lequal"   -> ty $ TComparisonFunction C
  "Greater"  -> ty $ TComparisonFunction C
  "Notequal" -> ty $ TComparisonFunction C
  "Gequal"   -> ty $ TComparisonFunction C
  "Always"   -> ty $ TComparisonFunction C
  -- ProvokingVertex
  "LastVertex"   -> ty $ TProvokingVertex C
  "FirstVertex"  -> ty $ TProvokingVertex C
  -- CullMode
  "CullNone"     -> ty $ TCullMode C
  "CullFront"    -> ty $ TFrontFace C ~> TCullMode C
  "CullBack"     -> ty $ TFrontFace C ~> TCullMode C
  -- FrontFace
  "CW"   -> ty $ TFrontFace C
  "CCW"  -> ty $ TFrontFace C
  -- PolygonMode
  "PolygonFill"  -> ty $ TPolygonMode C
  "PolygonPoint" -> ty $ TPointSize C ~> TPolygonMode C
  "PolygonLine"  -> ty $ TFloat C ~> TPolygonMode C
  -- PolygonOffset
  "NoOffset" -> ty $ TPolygonOffset C
  "Offset"   -> ty $ TFloat C ~> TFloat C ~> TPolygonOffset C
  -- PointSize
  "PointSize"        -> ty $ TFloat C ~> TPointSize C
  "ProgramPointSize" -> ty $ TPointSize C
  -- Fragment Out
  {-
    FragmentOut             ::                  FlatExp F a -> FragmentOut (ColorRepr a)
    FragmentOutDepth        :: Exp F Float  ->  FlatExp F a -> FragmentOut (Depth Float :+: ColorRepr a)
    FragmentOutRastDepth    ::                  FlatExp F a -> FragmentOut (Depth Float :+: ColorRepr a)
  -}
  "FragmentOut"           -> do [a,t] <- newVars 2 C ; [colorRepr a t] ==> t ~> TFragmentOut C a
  "FragmentOutDepth"      -> do [a,t] <- newVars 2 C ; [colorRepr a t] ==> TFloat C ~> t ~> TFragmentOut C (joinTupleType (Depth $ TFloat C) a)
  "FragmentOutRastDepth"  -> do [a,t] <- newVars 2 C ; [colorRepr a t] ==> t ~> TFragmentOut C (joinTupleType (Depth $ TFloat C) a)
  -- Vertex Out
  "VertexOut"    -> do [t,a] <- newVars 2 C ; [fTRepr' t a] ==> TV4F C ~> TFloat C ~> TTuple C [] ~> a ~> TVertexOut C t
  -- PointSpriteCoordOrigin
  "LowerLeft"  -> ty $ TPointSpriteCoordOrigin C
  "UpperLeft"  -> ty $ TPointSpriteCoordOrigin C
  -- Raster Context
  "TriangleCtx"  -> ty $ TCullMode C ~> TPolygonMode C ~> TPolygonOffset C ~> TProvokingVertex C ~> TRasterContext C TTriangle
  "PointCtx"     -> ty $ TPointSize C ~> TFloat C ~> TPointSpriteCoordOrigin C ~> TRasterContext C TPoint
  "LineCtx"      -> ty $ TFloat C ~> TProvokingVertex C ~> TRasterContext C TLine
  -- Fetch Primitive
  "Points"             -> ty $ TFetchPrimitive C TPoint
  "Lines"              -> ty $ TFetchPrimitive C TLine
  "Triangles"          -> ty $ TFetchPrimitive C TTriangle
  "LinesAdjacency"     -> ty $ TFetchPrimitive C TLineAdjacency
  "TrianglesAdjacency" -> ty $ TFetchPrimitive C TTriangleAdjacency
  -- Accumulation Context
  "AccumulationContext"  -> do [t,t'] <- newVars 2 C ; ty $ {-TFragmentOperation C-} t ~> TAccumulationContext C t'
  -- Image
  "ColorImage"   -> do
    [a,d,color,t] <- newVars 4 C
    [isTypeLevelNatural a, isNum t, isVecScalar d color t] ==> a ~> color ~> TImage C a (Color color)
  "DepthImage"   -> do
    [a] <- newVars 1 C
    [isTypeLevelNatural a] ==> a ~> TFloat C ~> TImage C a (Depth $ TFloat C)
  "StencilImage" -> do
    [a] <- newVars 1 C
    [isTypeLevelNatural a] ==> a ~> TInt C ~> TImage C a (Stencil $ TInt C)
  -- Interpolation
  "Smooth"         -> do t <- newVar C ; ty $ t ~> TInterpolated C t
  "Flat"           -> do t <- newVar C ; ty $ t ~> TInterpolated C t
  "NoPerspective"  -> do t <- newVar C ; ty $ t ~> TInterpolated C t
  -- Fragment Operation
  "ColorOp"    -> do
    [d,mask,c,color] <- newVars 4 C
    [isVecScalar d mask (TBool C), isVecScalar d color c, isNum c] ==> TBlending C c ~> mask ~> TFragmentOperation C (Color color)
  "DepthOp"    -> ty $ TComparisonFunction C ~> TBool C ~> TFragmentOperation C (Depth $ TFloat C)
    -- "StencilOp       :: StencilTests -> StencilOps -> StencilOps -> FragmentOperation (Stencil Int32)
  -- Blending
  "NoBlending"   -> do t <- newVar C ; ty $ TBlending C t
  "BlendLogicOp" -> do t <- newVar C ; ty $ TLogicOperation C ~> TBlending C t
  "Blend"        -> do t <- newVar C ; ty $ TTuple C [TBlendEquation C,TBlendEquation C]
                         ~> TTuple C [TTuple C [TBlendingFactor C,TBlendingFactor C],TTuple C [TBlendingFactor C,TBlendingFactor C]]
                         ~> TV4F C ~> TBlending C t --(TFloat C)
  -- Fragment Filter
  "PassAll"  -> do t <- newVar C ; ty $ TFragmentFilter C t
  "Filter"   -> do t <- newVar C ; ty $ (t ~> TBool C) ~> TFragmentFilter C t
  -- Render Operations
  "Fetch"        -> do [a,b] <- newVars 2 C ; ty $ TString C ~> TFetchPrimitive C a ~> TInput C b ~> TVertexStream C a b
  "Transform"    -> do [a,b,p] <- newVars 3 C ; ty $ (a ~> TVertexOut C b) ~> TVertexStream C p a ~> TPrimitiveStream C p (TNat 1) C b
  "Rasterize"    -> do [a,b,c] <- newVars 3 C ; ty $ TRasterContext C a ~> TPrimitiveStream C a b C c ~> TFragmentStream C b c
  {-
    Accumulate      :: (GPU a, GPU (FTRepr' b), IsValidOutput b)    -- restriction: depth and stencil optional, arbitrary color component
                    => AccumulationContext b
                    -> FragmentFilter a
                    -> (Exp F a -> FragmentOut (NoStencilRepr b))     -- fragment shader
                    -> Exp Obj (FragmentStream layerCount a)
                    -> Exp Obj (FrameBuffer layerCount (FTRepr' b))
                    -> Exp Obj (FrameBuffer layerCount (FTRepr' b))
  -}
  "Accumulate"   -> do
    [a,b,n,t] <- newVars 4 C
    [isValidOutput b, fTRepr' t b] ==>
           TAccumulationContext C b
        ~> TFragmentFilter C a
        ~> (a ~> TFragmentOut C b)
        ~> TFragmentStream C n a
        ~> TFrameBuffer C n t
        ~> TFrameBuffer C n t
  "FrameBuffer"  -> do [a,t] <- newVars 2 C ; [frameBuffer t a] ==> a ~> t
  "ScreenOut"    -> do [a,b] <- newVars 2 C ; ty $ TFrameBuffer C a b ~> TOutput C
  -- * Primitive Functions *
  -- Arithmetic Functions (componentwise)
  "PrimAdd"   -> do [a,t] <- newVars 2 C ; [matVecElem t a, isNum t] ==> a ~> a ~> a
  "PrimAddS"  -> do [a,t] <- newVars 2 C ; [matVecScalarElem t a, isNum t] ==> a ~> t ~> a
  "PrimSub"   -> do [a,t] <- newVars 2 C ; [matVecElem t a, isNum t] ==> a ~> a ~> a
  "PrimSubS"  -> do [a,t] <- newVars 2 C ; [matVecScalarElem t a, isNum t] ==> a ~> t ~> a
  "PrimMul"   -> do [a,t] <- newVars 2 C ; [matVecElem t a, isNum t] ==> a ~> a ~> a
  "PrimMulS"  -> do [a,t] <- newVars 2 C ; [matVecScalarElem t a, isNum t] ==> a ~> t ~> a
  "PrimDiv"   -> do [d,a,t] <- newVars 3 C ; [isNum t, isVecScalar d a t] ==> a ~> a ~> a
  "PrimDivS"  -> do [d,a,t] <- newVars 3 C ; [isNum t, isVecScalar d a t] ==> a ~> t ~> a
  "PrimNeg"   -> do [a,t]   <- newVars 1 C ; [matVecScalarElem t a, isSigned t] ==> a ~> a
  "PrimMod"   -> do [d,a,t] <- newVars 3 C ; [isNum t, isVecScalar d a t] ==> a ~> a ~> a
  "PrimModS"  -> do [d,a,t] <- newVars 3 C ; [isNum t, isVecScalar d a t] ==> a ~> t ~> a
  -- Bit-wise Functions
  "PrimBAnd"      -> do [d,a,t] <- newVars 3 C ; [isIntegral t, isVecScalar d a t] ==> a ~> a ~> a
  "PrimBAndS"     -> do [d,a,t] <- newVars 3 C ; [isIntegral t, isVecScalar d a t] ==> a ~> t ~> a
  "PrimBOr"       -> do [d,a,t] <- newVars 3 C ; [isIntegral t, isVecScalar d a t] ==> a ~> a ~> a
  "PrimBOrS"      -> do [d,a,t] <- newVars 3 C ; [isIntegral t, isVecScalar d a t] ==> a ~> t ~> a
  "PrimBXor"      -> do [d,a,t] <- newVars 3 C ; [isIntegral t, isVecScalar d a t] ==> a ~> a ~> a
  "PrimBXorS"     -> do [d,a,t] <- newVars 3 C ; [isIntegral t, isVecScalar d a t] ==> a ~> t ~> a
  "PrimBNot"      -> do [d,a,t] <- newVars 3 C ; [isIntegral t, isVecScalar d a t] ==> a ~> a
  "PrimBShiftL"   -> do [d,a,b,t] <- newVars 4 C ; [isIntegral t, isVecScalar d a t, isVecScalar d b (TWord C)] ==> a ~> b ~> a
  "PrimBShiftLS"  -> do [d,a,t] <- newVars 3 C ; [isIntegral t, isVecScalar d a t] ==> a ~> TWord C ~> a
  "PrimBShiftR"   -> do [d,a,b,t] <- newVars 4 C ; [isIntegral t, isVecScalar d a t, isVecScalar d b (TWord C)] ==> a ~> b ~> a
  "PrimBShiftRS"  -> do [d,a,t] <- newVars 3 C ; [isIntegral t, isVecScalar d a t] ==> a ~> TWord C ~> a
  -- Logic Functions
  "PrimAnd" -> do ty $ TBool C ~> TBool C ~> TBool C
  "PrimOr"  -> do ty $ TBool C ~> TBool C ~> TBool C
  "PrimXor" -> do ty $ TBool C ~> TBool C ~> TBool C
  "PrimNot" -> do [a,d] <- newVars 2 C ; [isVecScalar d a (TBool C)] ==> a ~> a
  "PrimAny" -> do [a,d] <- newVars 2 C ; [isVecScalar d a (TBool C)] ==> a ~> TBool C
  "PrimAll" -> do [a,d] <- newVars 2 C ; [isVecScalar d a (TBool C)] ==> a ~> TBool C
  -- Angle and Trigonometry Functions
  "PrimACos"    -> do [d,a] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a
  "PrimACosH"   -> do [d,a] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a
  "PrimASin"    -> do [d,a] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a
  "PrimASinH"   -> do [d,a] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a
  "PrimATan"    -> do [d,a] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a
  "PrimATan2"   -> do [d,a] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a ~> a
  "PrimATanH"   -> do [d,a] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a
  "PrimCos"     -> do [d,a] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a
  "PrimCosH"    -> do [d,a] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a
  "PrimDegrees" -> do [d,a] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a
  "PrimRadians" -> do [d,a] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a
  "PrimSin"     -> do [d,a] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a
  "PrimSinH"    -> do [d,a] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a
  "PrimTan"     -> do [d,a] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a
  "PrimTanH"    -> do [d,a] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a
  -- Exponential Functions
  "PrimPow"     -> do [d,a] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a ~> a
  "PrimExp"     -> do [d,a] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a
  "PrimLog"     -> do [d,a] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a
  "PrimExp2"    -> do [d,a] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a
  "PrimLog2"    -> do [d,a] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a
  "PrimSqrt"    -> do [d,a] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a
  "PrimInvSqrt" -> do [d,a] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a
  -- Common Functions
  "PrimIsNan"       -> do [d,a,b] <- newVars 3 C ; [isVecScalar d a (TFloat C), isVecScalar d b (TBool C)] ==> a ~> b
  "PrimIsInf"       -> do [d,a,b] <- newVars 3 C ; [isVecScalar d a (TFloat C), isVecScalar d b (TBool C)] ==> a ~> b
  "PrimAbs"         -> do [d,a,t] <- newVars 3 C ; [isSigned t, isVecScalar d a t] ==> a ~> a
  "PrimSign"        -> do [d,a,t] <- newVars 3 C ; [isSigned t, isVecScalar d a t] ==> a ~> a
  "PrimFloor"       -> do [d,a] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a
  "PrimTrunc"       -> do [d,a] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a
  "PrimRound"       -> do [d,a] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a
  "PrimRoundEven"   -> do [d,a] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a
  "PrimCeil"        -> do [d,a] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a
  "PrimFract"       -> do [d,a] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a
  "PrimModF"        -> do [d,a] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> TTuple C [a,a]
  "PrimMin"         -> do [d,a,t] <- newVars 3 C ; [isNum t, isVecScalar d a t] ==> a ~> a ~> a
  "PrimMinS"        -> do [d,a,t] <- newVars 3 C ; [isNum t, isVecScalar d a t] ==> a ~> t ~> a
  "PrimMax"         -> do [d,a,t] <- newVars 3 C ; [isNum t, isVecScalar d a t] ==> a ~> a ~> a
  "PrimMaxS"        -> do [d,a,t] <- newVars 3 C ; [isNum t, isVecScalar d a t] ==> a ~> t ~> a
  "PrimClamp"       -> do [d,a,t] <- newVars 3 C ; [isNum t, isVecScalar d a t] ==> a ~> a ~> a ~> a
  "PrimClampS"      -> do [d,a,t] <- newVars 3 C ; [isNum t, isVecScalar d a t] ==> a ~> t ~> t ~> a
  "PrimMix"         -> do [d,a] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a ~> a ~> a
  "PrimMixS"        -> do [d,a] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a ~> TFloat C ~> a
  "PrimMixB"        -> do [d,a,b] <- newVars 3 C ; [isVecScalar d a (TFloat C), isVecScalar d b (TBool C)] ==> a ~> a ~> b ~> a
  "PrimStep"        -> do [a,d] <- newVars 2 C ; [isVec d a (TFloat C)] ==> a ~> a ~> a
  "PrimStepS"       -> do [a,d] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> TFloat C ~> a ~> a
  "PrimSmoothStep"  -> do [a,d] <- newVars 2 C ; [isVec d a (TFloat C)] ==> a ~> a ~> a ~> a
  "PrimSmoothStepS" -> do [a,d] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> TFloat C ~> TFloat C ~> a ~> a
  -- Integer/Float Conversion Functions
  "PrimFloatBitsToInt"  -> do [d,fv,iv] <- newVars 3 C ; [isVecScalar d fv (TFloat C), isVecScalar d iv (TInt C)] ==> fv ~> iv
  "PrimFloatBitsToUInt" -> do [d,fv,uv] <- newVars 3 C ; [isVecScalar d fv (TFloat C), isVecScalar d uv (TWord C)] ==> fv ~> uv
  "PrimIntBitsToFloat"  -> do [d,fv,iv] <- newVars 3 C ; [isVecScalar d fv (TFloat C), isVecScalar d iv (TInt C)] ==> iv ~> fv
  "PrimUIntBitsToFloat" -> do [d,fv,uv] <- newVars 3 C ; [isVecScalar d fv (TFloat C), isVecScalar d uv (TWord C)] ==> uv ~> fv
  -- Geometric Functions
  "PrimLength"      -> do [a,d] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> TFloat C
  "PrimDistance"    -> do [a,d] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a ~> TFloat C
  "PrimDot"         -> do [a,d] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a ~> TFloat C
  "PrimCross"       -> do [a]   <- newVars 1 C ; [isVecScalar (TNat 3) a (TFloat C)] ==> a ~> a ~> a
  "PrimNormalize"   -> do [a,d] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a
  "PrimFaceForward" -> do [a,d] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a ~> a ~> a
  "PrimReflect"     -> do [a,d] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a ~> a
  "PrimRefract"     -> do [a,d] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a ~> a ~> a
  -- Matrix Functions
  "PrimTranspose"     -> do [a,b,h,w] <- newVars 4 C ; [isMat a h w, isMat b w h] ==> a ~> b
  "PrimDeterminant"   -> do [m,s] <- newVars 2 C ; [isMat m s s] ==> m ~> TFloat C
  "PrimInverse"       -> do [m,s] <- newVars 2 C ; [isMat m s s] ==> m ~> m
  "PrimOuterProduct"  -> do [m,h,w] <- newVars 3 C ; [isMat m h w] ==> w ~> h ~> m
  "PrimMulMatVec"     -> do [m,h,w] <- newVars 3 C ; [isMat m h w] ==> m ~> w ~> h
  "PrimMulVecMat"     -> do [m,h,w] <- newVars 3 C ; [isMat m h w] ==> h ~> m ~> w
  "PrimMulMatMat"     -> do [a,b,c,i,j,k] <- newVars 6 C ; [isMat a i j, isMat b j k, isMat c i k] ==> a ~> b ~> c
  -- Vector and Scalar Relational Functions
  "PrimLessThan"          -> do [d,a,b,t] <- newVars 2 C ; [isNum t, isVecScalar d a t, isVecScalar d b (TBool C)] ==> a ~> a ~> b
  "PrimLessThanEqual"     -> do [d,a,b,t] <- newVars 2 C ; [isNum t, isVecScalar d a t, isVecScalar d b (TBool C)] ==> a ~> a ~> b
  "PrimGreaterThan"       -> do [d,a,b,t] <- newVars 2 C ; [isNum t, isVecScalar d a t, isVecScalar d b (TBool C)] ==> a ~> a ~> b
  "PrimGreaterThanEqual"  -> do [d,a,b,t] <- newVars 2 C ; [isNum t, isVecScalar d a t, isVecScalar d b (TBool C)] ==> a ~> a ~> b
  "PrimEqualV"            -> do [d,a,b,t] <- newVars 2 C ; [isNum t, isVecScalar d a t, isVecScalar d b (TBool C)] ==> a ~> a ~> b
  "PrimEqual"             -> do [a,t] <- newVars 2 C ; [matVecScalarElem t a] ==> a ~> a ~> TBool C
  "PrimNotEqualV"         -> do [d,a,b,t] <- newVars 2 C ; [isNum t, isVecScalar d a t, isVecScalar d b (TBool C)] ==> a ~> a ~> b
  "PrimNotEqual"          -> do [a,t] <- newVars 2 C ; [matVecScalarElem t a] ==> a ~> a ~> TBool C
  -- Fragment Processing Functions
  "PrimDFdx"    -> do [a,d] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a
  "PrimDFdy"    -> do [a,d] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a
  "PrimFWidth"  -> do [a,d] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a
  -- Noise Functions
  "PrimNoise1"  -> do [a,d] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> TFloat C
  "PrimNoise2"  -> do [d,a,b] <- newVars 3 C ; [isVecScalar d a (TFloat C), isVecScalar (TNat 2) b (TFloat C)] ==> a ~> b
  "PrimNoise3"  -> do [d,a,b] <- newVars 3 C ; [isVecScalar d a (TFloat C), isVecScalar (TNat 3) b (TFloat C)] ==> a ~> b
  "PrimNoise4"  -> do [d,a,b] <- newVars 3 C ; [isVecScalar d a (TFloat C), isVecScalar (TNat 4) b (TFloat C)] ==> a ~> b

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
  ty t = return (mempty, mempty, t)

throwErrorUnique :: String -> Unique a
throwErrorUnique s = do
  (_,src,rl) <- get
  throwErrorSrc src rl s

throwErrorSrc src rl s = do
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
  throwError $ concat sl ++ s

newVars :: Int -> Frequency -> Unique [Ty]
newVars n f = replicateM n $ newVar f

newVar :: Frequency -> Unique Ty
newVar f = do
  (n,s,r) <- get
  put (n+1,s,r)
  return $ TVar f $ 't':show n
