{-# LANGUAGE PatternSynonyms #-}
module Typing where

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

isNum = CClass CNum
isSigned = CClass IsSigned
isIntegral = CClass IsIntegral

{-
simple:
  done - IsComponent
  done - IsFloating
  done - IsIntegral
  done - IsNum
  done - IsNumComponent
  done - IsSigned
fundep:
  class IsMat h w
    data Mat h w :: *
    instance IsMat M22F V2F V2F             M22F ~ Mat V2F V2F
    instance IsMat M23F V2F V3F
    instance IsMat M24F V2F V4F
    instance IsMat M32F V3F V2F
    instance IsMat M33F V3F V3F
    instance IsMat M34F V3F V4F
    instance IsMat M42F V4F V2F
    instance IsMat M43F V4F V3F
    instance IsMat M44F V4F V4F
-}
mat h w = TFun $ TFMat h w
isMat m h w = m ~~ mat h w
{-
  class IsMatVec a -- t | a -> t
    type T a :: *
    instance IsMatVec (V2 Float) Float
    instance IsMatVec (V3 Float) Float
    instance IsMatVec (V4 Float) Float
    instance IsMatVec (V2 Int32) Int32
    instance IsMatVec (V3 Int32) Int32
    instance IsMatVec (V4 Int32) Int32
    instance IsMatVec (V2 Word32) Word32
    instance IsMatVec (V3 Word32) Word32
    instance IsMatVec (V4 Word32) Word32
    instance IsMatVec (V2 Bool) Bool
    instance IsMatVec (V3 Bool) Bool
    instance IsMatVec (V4 Bool) Bool
    instance IsMatVec M22F Float
    instance IsMatVec M23F Float
    instance IsMatVec M24F Float
    instance IsMatVec M32F Float
    instance IsMatVec M33F Float
    instance IsMatVec M34F Float
    instance IsMatVec M42F Float
    instance IsMatVec M43F Float
    instance IsMatVec M44F Float
-}
matVecElem a = TFun $ TFMatVecElem a
{-
  class IsMatVecScalar a -- t | a -> t
    type T a :: *
-}
matVecScalarElem a = TFun $ TFMatVecScalarElem a
{-
  class IsVec (dim :: Nat) component
    data Vec dim component :: *
    instance IsVec 2 (V2 Float) Float
    instance IsVec 3 (V3 Float) Float
    instance IsVec 4 (V4 Float) Float
    instance IsVec 2 (V2 Int32) Int32
    instance IsVec 3 (V3 Int32) Int32
    instance IsVec 4 (V4 Int32) Int32
    instance IsVec 2 (V2 Word32) Word32
    instance IsVec 3 (V3 Word32) Word32
    instance IsVec 4 (V4 Word32) Word32
    instance IsVec 2 (V2 Bool) Bool
    instance IsVec 3 (V3 Bool) Bool
    instance IsVec 4 (V4 Bool) Bool
-}
vec d c = TFun $ TFVec d c
isVec d v c = v ~~ vec d c
{-
  [injective in both params] class IsVecScalar (dim :: Nat) component
    type VecS dim component :: *
    instance VecS 1 c = c
    instance VecS n c | n > 1 = Vec n c
-}
vecScalar d c = TFun $ TFVecScalar d c
isVecScalar d v c = v ~~ vecScalar d c

{-
type families:
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
fTRepr' a = TFun $ TFFTRepr' a
{- currently not used
  [injective] type family PrimitiveVertices (primitive :: PrimitiveType) a
    type instance PrimitiveVertices Point a             = a
    type instance PrimitiveVertices Line a              = (a,a)
    type instance PrimitiveVertices LineAdjacency a     = (a,a,a,a)
    type instance PrimitiveVertices Triangle a          = (a,a,a)
    type instance PrimitiveVertices TriangleAdjacency a = (a,a,a,a,a,a)
-}
{-
  type family ColorRepr a :: *
    type instance ColorRepr ZZ = ZZ
    type instance ColorRepr (a :+: b) = Color a :+: (ColorRepr b)

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

--reduceTF :: TName -> [Ty] -> Ty
--reduceTF n l = TFun n l

isInstance :: Class -> Ty -> Bool
isInstance IsNumComponent (TFloat _) = True
isInstance IsNumComponent (TInt _) = True
isInstance IsNumComponent (TWord _) = True
isInstance IsNumComponent (TV2F _) = True
isInstance IsNumComponent (TV3F _) = True
isInstance IsNumComponent (TV4F _) = True

isInstance IsSigned (TFloat _) = True
isInstance IsSigned (TInt _) = True

isInstance IsNum (TFloat _) = True
isInstance IsNum (TInt _) = True
isInstance IsNum (TWord _) = True

isInstance IsIntegral (TInt _) = True
isInstance IsIntegral (TWord _) = True

isInstance IsFloating (TFloat _) = True
isInstance IsFloating (TV2F   _) = True
isInstance IsFloating (TV3F   _) = True
isInstance IsFloating (TV4F   _) = True
isInstance IsFloating (TM22F  _) = True
isInstance IsFloating (TM23F  _) = True
isInstance IsFloating (TM24F  _) = True
isInstance IsFloating (TM32F  _) = True
isInstance IsFloating (TM33F  _) = True
isInstance IsFloating (TM34F  _) = True
isInstance IsFloating (TM42F  _) = True
isInstance IsFloating (TM43F  _) = True
isInstance IsFloating (TM44F  _) = True

isInstance IsComponent (TFloat _) = True
isInstance IsComponent (TInt _) = True
isInstance IsComponent (TWord _) = True
isInstance IsComponent (TBool _) = True
isInstance IsComponent (TV2F _) = True
isInstance IsComponent (TV3F _) = True
isInstance IsComponent (TV4F _) = True
isInstance c t = case Map.lookup c instances of
    Nothing -> False
    Just ts -> Set.member t ts

instances :: Map Class (Set Ty)
instances = Map.fromList [(CNum,Set.fromList [TInt C,TFloat C])]

ty :: Ty -> Unique Typing
ty t = return (mempty,mempty,t)

infix 6 ==>
cs ==> t = return (mempty, cs, t)

infix 4 ~~
a ~~ b = CEq a b

inferPrimFun :: EName -> Unique Typing
inferPrimFun a = case a of
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
  "FragmentOut"           -> do t <- newVar C ; ty $ t ~> TFragmentOut C t
  "FragmentOutDepth"      -> do t <- newVar C ; ty $ TFloat C ~> t ~> TFragmentOut C t
  "FragmentOutRastDepth"  -> do t <- newVar C ; ty $ t ~> TFragmentOut C t
  -- Vertex Out
  "VertexOut"    -> do a <- newVar C ; ty $ TV4F C ~> TFloat C ~> TTuple C [] ~> a ~> TVertexOut C (fTRepr' a)
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
  "ColorImage"   -> do [a,b] <- newVars 2 C ; ty $ a ~> b ~> TImage C a -- b
  "DepthImage"   -> do [a] <- newVars 1 C ; ty $ a ~> TFloat C ~> TImage C a -- (TFloat C)
  "StencilImage" -> do [a] <- newVars 1 C ; ty $ a ~> TInt C ~> TImage C a -- (TInt C)
  -- Interpolation
  "Smooth"         -> do t <- newVar C ; ty $ t ~> TInterpolated C t
  "Flat"           -> do t <- newVar C ; ty $ t ~> TInterpolated C t
  "NoPerspective"  -> do t <- newVar C ; ty $ t ~> TInterpolated C t
  -- Fragment Operation
  "ColorOp"    -> do [a,a'] <- newVars 2 C ; ty $ TBlending C a ~> TFragmentOperation C ({-Color-} a') -- TODO: type family needed
  "DepthOp"    -> do a <- newVar C ; ty $ TComparisonFunction C ~> TBool C ~> TFragmentOperation C a -- (TFloat C)
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
    [a,b,n] <- newVars 3 C
    [] ==>
           TAccumulationContext C b
        ~> TFragmentFilter C a
        ~> (a ~> TFragmentOut C b)
        ~> TFragmentStream C n a
        ~> TFrameBuffer C-- (fTRepr b)
        ~> TFrameBuffer C-- (fTRepr b)
  "FrameBuffer"  -> do [a,b] <- newVars 2 C ; ty $ a {-TImage C a b-} ~> TFrameBuffer C
  "ScreenOut"    -> ty $ TFrameBuffer C ~> TOutput C
  -- * Primitive Functions *
  -- Arithmetic Functions (componentwise)
  "PrimAdd"   -> do [a]   <- newVars 1 C ; [isNum (matVecElem a)] ==> a ~> a ~> a
  "PrimAddS"  -> do [a,t] <- newVars 2 C ; [t ~~ matVecScalarElem a, isNum t] ==> a ~> t ~> a
  "PrimSub"   -> do [a]   <- newVars 1 C ; [isNum (matVecElem a)] ==> a ~> a ~> a
  "PrimSubS"  -> do [a,t] <- newVars 2 C ; [t ~~ matVecScalarElem a, isNum t] ==> a ~> t ~> a
  "PrimMul"   -> do [a]   <- newVars 1 C ; [isNum (matVecElem a)] ==> a ~> a ~> a
  "PrimMulS"  -> do [a,t] <- newVars 2 C ; [t ~~ matVecScalarElem a, isNum t] ==> a ~> t ~> a
  "PrimDiv"   -> do [d,a,t] <- newVars 3 C ; [isNum t, isVecScalar d a t] ==> a ~> a ~> a
  "PrimDivS"  -> do [d,a,t] <- newVars 3 C ; [isNum t, isVecScalar d a t] ==> a ~> t ~> a
  "PrimNeg"   -> do [a]     <- newVars 1 C ; [isSigned (matVecScalarElem a)] ==> a ~> a
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
  "PrimEqual"             -> do [a,t] <- newVars 2 C ; [t ~~ matVecScalarElem a] ==> a ~> a ~> TBool C
  "PrimNotEqualV"         -> do [d,a,b,t] <- newVars 2 C ; [isNum t, isVecScalar d a t, isVecScalar d b (TBool C)] ==> a ~> a ~> b
  "PrimNotEqual"          -> do [a,t] <- newVars 2 C ; [t ~~ matVecScalarElem a] ==> a ~> a ~> TBool C
  -- Fragment Processing Functions
  "PrimDFdx"    -> do [a,d] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a
  "PrimDFdy"    -> do [a,d] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a
  "PrimFWidth"  -> do [a,d] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> a
  -- Noise Functions
  "PrimNoise1"  -> do [a,d] <- newVars 2 C ; [isVecScalar d a (TFloat C)] ==> a ~> TFloat C
  "PrimNoise2"  -> do [d,a,b] <- newVars 3 C ; [isVecScalar d a (TFloat C), isVecScalar (TNat 2) b (TFloat C)] ==> a ~> b
  "PrimNoise3"  -> do [d,a,b] <- newVars 3 C ; [isVecScalar d a (TFloat C), isVecScalar (TNat 3) b (TFloat C)] ==> a ~> b
  "PrimNoise4"  -> do [d,a,b] <- newVars 3 C ; [isVecScalar d a (TFloat C), isVecScalar (TNat 4) b (TFloat C)] ==> a ~> b

  a -> throwErrorUnique $ "unknown primitive: " ++ show a

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
