{-# LANGUAGE PatternSynonyms #-}
module Type where

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

type Unique a = StateT (Int,ByteString,[Range]) (Except String) a

type Range = (Delta,Delta)

type TName = String
type EName = String
type MonoEnv = Map EName Ty
type PolyEnv = Map EName Typing
type InstEnv = [(Constraint,Ty)]
type Typing = (MonoEnv,InstEnv,Ty)
type Env = (PolyEnv,MonoEnv,InstEnv)

data Lit
  = LInt    Integer
  | LChar   Char
  | LString String
  | LFloat  Double
  | LNat    Int
  deriving (Show,Eq,Ord)

data Exp a
  = ELit      a Lit
  | EVar      a Subst EName
  | EApp      a Subst (Exp a) (Exp a)
  | ELam      a EName (Exp a)
  | ELet      a EName (Exp a) (Exp a)
  | ETuple    a [Exp a]
--  | EFix EName Exp
  deriving (Show,Eq,Ord)

type Subst = Map TName Ty

getTag :: Show a => Exp a -> a
getTag (ELit      r _) = r
getTag (EVar      r _ _) = r
getTag (EApp      r _ _ _) = r
getTag (ELam      r _ _) = r
getTag (ELet      r _ _ _) = r
getTag (ETuple    r _) = r
--getTag x = error $ "getTag error: " ++ show x

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

data Frequency -- frequency kind
  -- frequency values
  = C
  | O
  | V
  | F
  -- type family representation
  | FVar TName
  | FMax [Frequency]
  deriving (Show,Eq,Ord)

infixr 7 ~>
a ~> b = TArr a b

type Semantic = Ty
type PrimitiveType = Ty
type Nat = Ty

data Ty -- star kind
  = TVar    Frequency TName
  | TArr    Ty Ty
  -- composit
  | TTuple  Frequency [Ty]
  | TArray  Frequency Ty
  -- type family
  | TFun    TName [Ty]
  -- primitive types
  | TChar   Frequency
  | TString Frequency
  | TBool   Frequency
  | TWord   Frequency
  | TInt    Frequency
  | TFloat  Frequency

  -- lambdacube types
  | TNat    Int

  -- Semantic
  | Depth   Ty
  | Stencil Ty
  | Color   Ty

  -- PrimitiveType
  | TTriangle
  | TLine
  | TPoint
  | TTriangleAdjacency
  | TLineAdjacency

  -- Vector/Matrix
  | TV2B    Frequency
  | TV3B    Frequency
  | TV4B    Frequency
  | TV2U    Frequency
  | TV3U    Frequency
  | TV4U    Frequency
  | TV2I    Frequency
  | TV3I    Frequency
  | TV4I    Frequency
  | TV2F    Frequency
  | TV3F    Frequency
  | TV4F    Frequency
  | TM22F   Frequency
  | TM23F   Frequency
  | TM24F   Frequency
  | TM32F   Frequency
  | TM33F   Frequency
  | TM34F   Frequency
  | TM42F   Frequency
  | TM43F   Frequency
  | TM44F   Frequency

  -- ADT
  | TCullMode               Frequency
  | TPolygonMode            Frequency
  | TPolygonOffset          Frequency
  | TProvokingVertex        Frequency
  | TFrontFace              Frequency
  | TPointSize              Frequency
  | TBlendingFactor         Frequency
  | TBlendEquation          Frequency
  | TLogicOperation         Frequency
  | TStencilOperation       Frequency
  | TComparisonFunction     Frequency
  | TPointSpriteCoordOrigin Frequency

  -- GADT
  | TAccumulationContext  Frequency Ty
  | TBlending             Frequency Ty
  | TFetchPrimitive       Frequency PrimitiveType
  | TFragmentFilter       Frequency Ty
  | TFragmentOperation    Frequency Semantic
  | TFragmentOut          Frequency Semantic
  | TFragmentStream       Frequency Nat Ty
  | TFrameBuffer          Frequency -- ???
  | TImage                Frequency Nat -- Semantic -- TODO: ignore semantic temporarly
  | TInput                Frequency Ty
  | TInterpolated         Frequency Ty -- ???
  | TOutput               Frequency
  | TPrimitiveStream      Frequency PrimitiveType Nat Frequency Ty -- ???
  | TRasterContext        Frequency PrimitiveType
  | TVertexOut            Frequency Ty -- ???
  | TVertexStream         Frequency PrimitiveType Ty
  deriving (Show,Eq,Ord)

data Constraint
  = CNum
  | CTextual
  -- lc constraints
  | IsComponent
  | IsFloating
  | IsIntegral
  | IsNum
  | IsNumComponent
  | IsSigned
  deriving (Show,Eq,Ord)
{-
simple:
  done - IsComponent
  done - IsFloating
  done - IsIntegral
  done - IsNum
  done - IsNumComponent
  done - IsSigned
fundep:
  [injective] class IsMat mat -- h w | mat -> h w
    type H mat :: *
    type W mat :: *
    instance IsMat M22F V2F V2F
    instance IsMat M23F V2F V3F
    instance IsMat M24F V2F V4F
    instance IsMat M32F V3F V2F
    instance IsMat M33F V3F V3F
    instance IsMat M34F V3F V4F
    instance IsMat M42F V4F V2F
    instance IsMat M43F V4F V3F
    instance IsMat M44F V4F V4F

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

  class IsMatVecScalar a -- t | a -> t
    type T a :: *

  [injective] class IsVec (dim :: Nat) vec component | vec -> dim component, dim component -> vec
    type VecDim vec :: Nat
    type VecComp vec :: *
    type VecDimComp d c :: *
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

  class IsVecScalar (dim :: Nat) vec component | vec -> dim component, dim component -> vec
    type VecSDim vec :: Nat
    type VecSComp vec :: *
    type VecSDimComp d c :: *
-}
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

  [injective] type family PrimitiveVertices (primitive :: PrimitiveType) a
    type instance PrimitiveVertices Point a             = a
    type instance PrimitiveVertices Line a              = (a,a)
    type instance PrimitiveVertices LineAdjacency a     = (a,a,a,a)
    type instance PrimitiveVertices Triangle a          = (a,a,a)
    type instance PrimitiveVertices TriangleAdjacency a = (a,a,a,a,a,a)

  type family ColorRepr a :: *
    type instance ColorRepr ZZ = ZZ
    type instance ColorRepr (a :+: b) = Color a :+: (ColorRepr b)

  type family NoStencilRepr a :: *
    type instance NoStencilRepr ZZ = ZZ
    type instance NoStencilRepr (Stencil a :+: b) = NoStencilRepr b
    type instance NoStencilRepr (Color a :+: b) = Color a :+: (NoStencilRepr b)
    type instance NoStencilRepr (Depth a :+: b) = Depth a :+: (NoStencilRepr b)

  - texturing -
  [injective] type family TexDataRepr arity (t :: TextureSemantics *)
    type instance TexDataRepr Red  (v a) = a
    type instance TexDataRepr RG   (v a) = V2 a
    type instance TexDataRepr RGB  (v a) = V3 a
    type instance TexDataRepr RGBA (v a) = V4 a

  type family TexArrRepr (a :: Nat) :: TextureArray
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

  [injective] type family TexSizeRepr (a :: TextureShape)
    type instance TexSizeRepr (Tex1D)   = Word32
    type instance TexSizeRepr (Tex2D)   = V2U
    type instance TexSizeRepr (TexRect) = V2U
    type instance TexSizeRepr (Tex3D)   = V3U

  [injective] type family TexelRepr sampler
    type instance TexelRepr (Sampler dim arr (v t) Red)     = t
    type instance TexelRepr (Sampler dim arr (v t) RG)      = V2 t
    type instance TexelRepr (Sampler dim arr (v t) RGB)     = V3 t
    type instance TexelRepr (Sampler dim arr (v t) RGBA)    = V4 t
-}
reduceTF :: TName -> [Ty] -> Ty
reduceTF n l = TFun n l

isInstance :: Constraint -> Ty -> Bool
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

instances :: Map Constraint (Set Ty)
instances = Map.fromList [(CNum,Set.fromList [TInt C,TFloat C])]

ty :: Ty -> Unique Typing
ty t = return (mempty,mempty,t)

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
  "VertexOut"    -> do a <- newVar C ; ty $ TV4F C ~> TFloat C ~> TTuple C [] ~> a ~> TVertexOut C a
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
  "Accumulate"   -> do [a,b,n] <- newVars 3 C ; ty $ TAccumulationContext C b ~> TFragmentFilter C a ~> (a ~> TFragmentOut C b) ~> TFragmentStream C n a ~> TFrameBuffer C ~> TFrameBuffer C
  "FrameBuffer"  -> do [a,b] <- newVars 2 C ; ty $ a {-TImage C a b-} ~> TFrameBuffer C
  "ScreenOut"    -> ty $ TFrameBuffer C ~> TOutput C
  -- * Primitive Functions *
  -- Arithmetic Functions (componentwise)
  "PrimAdd"   -> do [a,t] <- newVars 2 C ; ty $ a ~> a ~> a -- (IsNum t, IsMatVec a t) => PrimFun stage ((a,a) -> a)
  "PrimAddS"  -> do [a,t] <- newVars 2 C ; ty $ a ~> t ~> a -- (IsNum t, IsMatVecScalar a t) => PrimFun stage ((a,t) -> a)
  "PrimSub"   -> do [a,t] <- newVars 2 C ; ty $ a ~> a ~> a -- (IsNum t, IsMatVec a t) => PrimFun stage ((a,a) -> a)
  "PrimSubS"  -> do [a,t] <- newVars 2 C ; ty $ a ~> t ~> a -- (IsNum t, IsMatVecScalar a t) => PrimFun stage ((a,t) -> a)
  "PrimMul"   -> do [a,t] <- newVars 2 C ; ty $ a ~> a ~> a -- (IsNum t, IsMatVec a t) => PrimFun stage ((a,a) -> a)
  "PrimMulS"  -> do [a,t] <- newVars 2 C ; ty $ a ~> t ~> a -- (IsNum t, IsMatVecScalar a t) => PrimFun stage ((a,t) -> a)
  "PrimDiv"   -> do [a,t] <- newVars 2 C ; ty $ a ~> a ~> a -- (IsNum t, IsVecScalar d a t) => PrimFun stage ((a,a) -> a)
  "PrimDivS"  -> do [a,t] <- newVars 2 C ; ty $ a ~> t ~> a -- (IsNum t, IsVecScalar d a t) => PrimFun stage ((a,t) -> a)
  "PrimNeg"   -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- (IsSigned t, IsMatVecScalar a t) => PrimFun stage (a -> a)
  "PrimMod"   -> do [a,t] <- newVars 2 C ; ty $ a ~> a ~> a -- (IsNum t, IsVecScalar d a t) => PrimFun stage ((a,a) -> a)
  "PrimModS"  -> do [a,t] <- newVars 2 C ; ty $ a ~> t ~> a -- (IsNum t, IsVecScalar d a t) => PrimFun stage ((a,t) -> a)
  -- Bit-wise Functions
  "PrimBAnd"      -> do [a,t] <- newVars 2 C ; ty $ a ~> a ~> a -- (IsIntegral t, IsVecScalar d a t) => PrimFun stage ((a,a) -> a)
  "PrimBAndS"     -> do [a,t] <- newVars 2 C ; ty $ a ~> t ~> a -- (IsIntegral t, IsVecScalar d a t) => PrimFun stage ((a,t) -> a)
  "PrimBOr"       -> do [a,t] <- newVars 2 C ; ty $ a ~> a ~> a -- (IsIntegral t, IsVecScalar d a t) => PrimFun stage ((a,a) -> a)
  "PrimBOrS"      -> do [a,t] <- newVars 2 C ; ty $ a ~> t ~> a -- (IsIntegral t, IsVecScalar d a t) => PrimFun stage ((a,t) -> a)
  "PrimBXor"      -> do [a,t] <- newVars 2 C ; ty $ a ~> a ~> a -- (IsIntegral t, IsVecScalar d a t) => PrimFun stage ((a,a) -> a)
  "PrimBXorS"     -> do [a,t] <- newVars 2 C ; ty $ a ~> t ~> a -- (IsIntegral t, IsVecScalar d a t) => PrimFun stage ((a,t) -> a)
  "PrimBNot"      -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- (IsIntegral t, IsVecScalar d a t) => PrimFun stage (a -> a)
  "PrimBShiftL"   -> do [a,t] <- newVars 2 C ; ty $ a ~> t ~> a -- (IsIntegral t, IsVecScalar d a t, IsVecScalar d b Word32) => PrimFun stage ((a, b) -> a)
  "PrimBShiftLS"  -> do [a,t] <- newVars 2 C ; ty $ a ~> TWord C ~> a -- (IsIntegral t, IsVecScalar d a t) => PrimFun stage ((a, Word32) -> a)
  "PrimBShiftR"   -> do [a,t] <- newVars 2 C ; ty $ a ~> t ~> a -- (IsIntegral t, IsVecScalar d a t, IsVecScalar d b Word32) => PrimFun stage ((a, b) -> a)
  "PrimBShiftRS"  -> do [a,t] <- newVars 2 C ; ty $ a ~> TWord C ~> a -- (IsIntegral t, IsVecScalar d a t) => PrimFun stage ((a, Word32) -> a)
  -- Logic Functions
  "PrimAnd" -> do [a,t] <- newVars 2 C ; ty $ TBool C ~> TBool C ~> TBool C -- PrimFun stage ((Bool,Bool) -> Bool)
  "PrimOr"  -> do [a,t] <- newVars 2 C ; ty $ TBool C ~> TBool C ~> TBool C -- PrimFun stage ((Bool,Bool) -> Bool)
  "PrimXor" -> do [a,t] <- newVars 2 C ; ty $ TBool C ~> TBool C ~> TBool C -- PrimFun stage ((Bool,Bool) -> Bool)
  "PrimNot" -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- IsVecScalar d a Bool => PrimFun stage (a -> a)
  "PrimAny" -> do [a,t] <- newVars 2 C ; ty $ a ~> TBool C -- IsVecScalar d a Bool => PrimFun stage (a -> Bool)
  "PrimAll" -> do [a,t] <- newVars 2 C ; ty $ a ~> TBool C -- IsVecScalar d a Bool => PrimFun stage (a -> Bool)
  -- Angle and Trigonometry Functions
  "PrimACos"    -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- IsVecScalar d a Float => PrimFun stage (a -> a)
  "PrimACosH"   -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- IsVecScalar d a Float => PrimFun stage (a -> a)
  "PrimASin"    -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- IsVecScalar d a Float => PrimFun stage (a -> a)
  "PrimASinH"   -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- IsVecScalar d a Float => PrimFun stage (a -> a)
  "PrimATan"    -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- IsVecScalar d a Float => PrimFun stage (a -> a)
  "PrimATan2"   -> do [a,t] <- newVars 2 C ; ty $ a ~> a ~> a -- IsVecScalar d a Float => PrimFun stage ((a,a) -> a)
  "PrimATanH"   -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- IsVecScalar d a Float => PrimFun stage (a -> a)
  "PrimCos"     -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- IsVecScalar d a Float => PrimFun stage (a -> a)
  "PrimCosH"    -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- IsVecScalar d a Float => PrimFun stage (a -> a)
  "PrimDegrees" -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- IsVecScalar d a Float => PrimFun stage (a -> a)
  "PrimRadians" -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- IsVecScalar d a Float => PrimFun stage (a -> a)
  "PrimSin"     -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- IsVecScalar d a Float => PrimFun stage (a -> a)
  "PrimSinH"    -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- IsVecScalar d a Float => PrimFun stage (a -> a)
  "PrimTan"     -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- IsVecScalar d a Float => PrimFun stage (a -> a)
  "PrimTanH"    -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- IsVecScalar d a Float => PrimFun stage (a -> a)
  -- Exponential Functions
  "PrimPow"     -> do [a,t] <- newVars 2 C ; ty $ a ~> a ~> a -- IsVecScalar d a Float => PrimFun stage ((a,a) -> a)
  "PrimExp"     -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- IsVecScalar d a Float => PrimFun stage (a -> a)
  "PrimLog"     -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- IsVecScalar d a Float => PrimFun stage (a -> a)
  "PrimExp2"    -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- IsVecScalar d a Float => PrimFun stage (a -> a)
  "PrimLog2"    -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- IsVecScalar d a Float => PrimFun stage (a -> a)
  "PrimSqrt"    -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- IsVecScalar d a Float => PrimFun stage (a -> a)
  "PrimInvSqrt" -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- IsVecScalar d a Float => PrimFun stage (a -> a)
  -- Common Functions
  "PrimIsNan"       -> do [a,t] <- newVars 2 C ; ty $ a ~> t -- (IsVecScalar d a Float, IsVecScalar d b Bool) => PrimFun stage (a -> b)
  "PrimIsInf"       -> do [a,t] <- newVars 2 C ; ty $ a ~> t -- (IsVecScalar d a Float, IsVecScalar d b Bool) => PrimFun stage (a -> b)
  "PrimAbs"         -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- (IsSigned t, IsVecScalar d a t) => PrimFun stage (a -> a)
  "PrimSign"        -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- (IsSigned t, IsVecScalar d a t) => PrimFun stage (a -> a)
  "PrimFloor"       -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- IsVecScalar d a Float => PrimFun stage (a -> a)
  "PrimTrunc"       -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- IsVecScalar d a Float => PrimFun stage (a -> a)
  "PrimRound"       -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- IsVecScalar d a Float => PrimFun stage (a -> a)
  "PrimRoundEven"   -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- IsVecScalar d a Float => PrimFun stage (a -> a)
  "PrimCeil"        -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- IsVecScalar d a Float => PrimFun stage (a -> a)
  "PrimFract"       -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- IsVecScalar d a Float => PrimFun stage (a -> a)
  "PrimModF"        -> do [a,t] <- newVars 2 C ; ty $ a ~> TTuple C [a,a] -- IsVecScalar d a Float => PrimFun stage (a -> (a,a))
  "PrimMin"         -> do [a,t] <- newVars 2 C ; ty $ a ~> a ~> a -- (IsNum t, IsVecScalar d a t) => PrimFun stage ((a,a) -> a)
  "PrimMinS"        -> do [a,t] <- newVars 2 C ; ty $ a ~> t ~> a -- (IsNum t, IsVecScalar d a t) => PrimFun stage ((a,t) -> a)
  "PrimMax"         -> do [a,t] <- newVars 2 C ; ty $ a ~> a ~> a -- (IsNum t, IsVecScalar d a t) => PrimFun stage ((a,a) -> a)
  "PrimMaxS"        -> do [a,t] <- newVars 2 C ; ty $ a ~> t ~> a -- (IsNum t, IsVecScalar d a t) => PrimFun stage ((a,t) -> a)
  "PrimClamp"       -> do [a,t] <- newVars 2 C ; ty $ a ~> a ~> a ~> a -- (IsNum t, IsVecScalar d a t) => PrimFun stage ((a,a,a) -> a)
  "PrimClampS"      -> do [a,t] <- newVars 2 C ; ty $ a ~> t ~> t ~> a -- (IsNum t, IsVecScalar d a t) => PrimFun stage ((a,t,t) -> a)
  "PrimMix"         -> do [a,t] <- newVars 2 C ; ty $ a ~> a ~> a ~> a -- IsVecScalar d a Float => PrimFun stage ((a,a,a) -> a)
  "PrimMixS"        -> do [a,t] <- newVars 2 C ; ty $ a ~> a ~> TFloat C ~> a -- IsVecScalar d a Float => PrimFun stage ((a,a,Float) -> a)
  "PrimMixB"        -> do [a,t] <- newVars 2 C ; ty $ a ~> a ~> t ~> a -- (IsVecScalar d a Float, IsVecScalar d b Bool) => PrimFun stage ((a,a,b) -> a)
  "PrimStep"        -> do [a,t] <- newVars 2 C ; ty $ a ~> a ~> a -- IsVec d a Float => PrimFun stage ((a,a) -> a)
  "PrimStepS"       -> do [a,t] <- newVars 2 C ; ty $ TFloat C ~> a ~> a -- IsVecScalar d a Float => PrimFun stage ((Float,a) -> a)
  "PrimSmoothStep"  -> do [a,t] <- newVars 2 C ; ty $ a ~> a ~> a ~> a -- IsVec d a Float => PrimFun stage ((a,a,a) -> a)
  "PrimSmoothStepS" -> do [a,t] <- newVars 2 C ; ty $ TFloat C ~> TFloat C ~> a ~> a -- IsVecScalar d a Float => PrimFun stage ((Float,Float,a) -> a)
  -- Integer/Float Conversion Functions
  "PrimFloatBitsToInt"  -> do [a,t] <- newVars 2 C ; ty $ a ~> t -- (IsVecScalar d fv Float, IsVecScalar d iv Int32) => PrimFun stage (fv -> iv)
  "PrimFloatBitsToUInt" -> do [a,t] <- newVars 2 C ; ty $ a ~> t -- (IsVecScalar d fv Float, IsVecScalar d uv Word32) => PrimFun stage (fv -> uv)
  "PrimIntBitsToFloat"  -> do [a,t] <- newVars 2 C ; ty $ a ~> t -- (IsVecScalar d fv Float, IsVecScalar d iv Int32) => PrimFun stage (iv -> fv)
  "PrimUIntBitsToFloat" -> do [a,t] <- newVars 2 C ; ty $ a ~> t -- (IsVecScalar d fv Float, IsVecScalar d uv Word32) => PrimFun stage (uv -> fv)
  -- Geometric Functions
  "PrimLength"      -> do [a,t] <- newVars 2 C ; ty $ a ~> TFloat C -- IsVecScalar d a Float => PrimFun stage (a       -> Float)
  "PrimDistance"    -> do [a,t] <- newVars 2 C ; ty $ a ~> a ~> TFloat C -- IsVecScalar d a Float => PrimFun stage ((a,a)   -> Float)
  "PrimDot"         -> do [a,t] <- newVars 2 C ; ty $ a ~> a ~> TFloat C -- IsVecScalar d a Float => PrimFun stage ((a,a)   -> Float)
  "PrimCross"       -> do [a,t] <- newVars 2 C ; ty $ a ~> a ~> a -- IsVecScalar 3 a Float => PrimFun stage ((a,a)   -> a)
  "PrimNormalize"   -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- IsVecScalar d a Float => PrimFun stage (a       -> a)
  "PrimFaceForward" -> do [a,t] <- newVars 2 C ; ty $ a ~> a ~> a ~> a -- IsVecScalar d a Float => PrimFun stage ((a,a,a) -> a)
  "PrimReflect"     -> do [a,t] <- newVars 2 C ; ty $ a ~> a ~> a -- IsVecScalar d a Float => PrimFun stage ((a,a)   -> a)
  "PrimRefract"     -> do [a,t] <- newVars 2 C ; ty $ a ~> a ~> a ~> a -- IsVecScalar d a Float => PrimFun stage ((a,a,a) -> a)
  -- Matrix Functions
  "PrimTranspose"     -> do [a,t] <- newVars 2 C ; ty $ a ~> t -- (IsMat a h w, IsMat b w h)               => PrimFun stage (a       -> b)
  "PrimDeterminant"   -> do [a,t] <- newVars 2 C ; ty $ a ~> TFloat C -- IsMat m s s                              => PrimFun stage (m       -> Float)
  "PrimInverse"       -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- IsMat m s s                              => PrimFun stage (m       -> m)
  "PrimOuterProduct"  -> do [a,b,t] <- newVars 3 C ; ty $ a ~> b ~> t -- IsMat m h w                              => PrimFun stage ((w,h)   -> m)
  "PrimMulMatVec"     -> do [a,b,t] <- newVars 3 C ; ty $ a ~> b ~> t -- IsMat m h w                              => PrimFun stage ((m,w)   -> h)
  "PrimMulVecMat"     -> do [a,b,t] <- newVars 3 C ; ty $ a ~> b ~> t -- IsMat m h w                              => PrimFun stage ((h,m)   -> w)
  "PrimMulMatMat"     -> do [a,b,t] <- newVars 3 C ; ty $ a ~> b ~> t -- (IsMat a i j, IsMat b j k, IsMat c i k)  => PrimFun stage ((a,b)   -> c)
  -- Vector and Scalar Relational Functions
  "PrimLessThan"          -> do [a,t] <- newVars 2 C ; ty $ a ~> a ~> t  -- (IsNum t, IsVecScalar d a t, IsVecScalar d b Bool)   => PrimFun stage ((a,a) -> b)
  "PrimLessThanEqual"     -> do [a,t] <- newVars 2 C ; ty $ a ~> a ~> t  -- (IsNum t, IsVecScalar d a t, IsVecScalar d b Bool)   => PrimFun stage ((a,a) -> b)
  "PrimGreaterThan"       -> do [a,t] <- newVars 2 C ; ty $ a ~> a ~> t  -- (IsNum t, IsVecScalar d a t, IsVecScalar d b Bool)   => PrimFun stage ((a,a) -> b)
  "PrimGreaterThanEqual"  -> do [a,t] <- newVars 2 C ; ty $ a ~> a ~> t  -- (IsNum t, IsVecScalar d a t, IsVecScalar d b Bool)   => PrimFun stage ((a,a) -> b)
  "PrimEqualV"            -> do [a,t] <- newVars 2 C ; ty $ a ~> a ~> t  -- (IsNum t, IsVecScalar d a t, IsVecScalar d b Bool)   => PrimFun stage ((a,a) -> b)
  "PrimEqual"             -> do [a,t] <- newVars 2 C ; ty $ a ~> a ~> TBool C  -- IsMatVecScalar a t                                   => PrimFun stage ((a,a) -> Bool)
  "PrimNotEqualV"         -> do [a,t] <- newVars 2 C ; ty $ a ~> a ~> t  -- (IsNum t, IsVecScalar d a t, IsVecScalar d b Bool)   => PrimFun stage ((a,a) -> b)
  "PrimNotEqual"          -> do [a,t] <- newVars 2 C ; ty $ a ~> a ~> TBool C  -- IsMatVecScalar a t                                   => PrimFun stage ((a,a) -> Bool)
  -- Fragment Processing Functions
  "PrimDFdx"    -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- IsVecScalar d a Float => PrimFun F (a -> a)
  "PrimDFdy"    -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- IsVecScalar d a Float => PrimFun F (a -> a)
  "PrimFWidth"  -> do [a,t] <- newVars 2 C ; ty $ a ~> a -- IsVecScalar d a Float => PrimFun F (a -> a)
  -- Noise Functions
  "PrimNoise1"  -> do [a,t] <- newVars 2 C ; ty $ a ~> TFloat C -- IsVecScalar d a Float                             => PrimFun stage (a -> Float)
  "PrimNoise2"  -> do [a,t] <- newVars 2 C ; ty $ a ~> t -- (IsVecScalar d a Float, IsVecScalar 2 b Float)    => PrimFun stage (a -> b)
  "PrimNoise3"  -> do [a,t] <- newVars 2 C ; ty $ a ~> t -- (IsVecScalar d a Float, IsVecScalar 3 b Float)    => PrimFun stage (a -> b)
  "PrimNoise4"  -> do [a,t] <- newVars 2 C ; ty $ a ~> t -- (IsVecScalar d a Float, IsVecScalar 4 b Float)    => PrimFun stage (a -> b)

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
