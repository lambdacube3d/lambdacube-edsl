module LC_B_GLSLCodeGen (
    codeGenVertexShader,
    codeGenGeometryShader,
    codeGenFragmentShader,
    codeGenType
) where

import Debug.Trace

import Control.Applicative hiding (Const)
import Control.Exception
import Control.Monad.State
import Data.ByteString.Char8 (ByteString,pack,unpack)
import Data.Int
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Set (Set)
import Data.Word
import Text.PrettyPrint.HughesPJClass
import qualified Data.ByteString.Char8 as SB
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Set as Set

import LC_G_Type
import LC_G_APIType hiding (LogicOperation(..), ComparisonFunction(..))
import LC_U_APIType
import LC_U_PrimFun
import LC_U_DeBruijn hiding (ExpC(..))

import Language.GLSL.Syntax hiding (Const,InterpolationQualifier(..),TypeSpecifierNonArray(..))
import Language.GLSL.Syntax (TypeSpecifierNonArray)
import qualified Language.GLSL.Syntax as GLSL
import Language.GLSL.Pretty
import LC_B_Traversals

codeGenPrim :: PrimFun -> [InputType] -> [InputType] -> [Expr] -> [Expr]

-- Vec/Mat (de)construction
codeGenPrim PrimTupToV2             ty argTy [a,b]
    | all (==Bool)  argTy                              = [functionCall "bvec2"              [a,b]]
    | all (==Float) argTy                              = [functionCall "vec2"               [a,b]]
    | all (==Int)   argTy                              = [functionCall "ivec2"              [a,b]]
    | all (==Word)  argTy                              = [functionCall "uvec2"              [a,b]]
    | all (==V2F)   argTy                              = [functionCall "mat2"               [a,b]]
    | all (==V3F)   argTy                              = [functionCall "mat3x2"             [a,b]]
    | all (==V4F)   argTy                              = [functionCall "mat4x2"             [a,b]]
codeGenPrim PrimTupToV3             ty argTy [a,b,c]
    | all (==Bool)  argTy                              = [functionCall "bvec3"              [a,b,c]]
    | all (==Float) argTy                              = [functionCall "vec3"               [a,b,c]]
    | all (==Int)   argTy                              = [functionCall "ivec3"              [a,b,c]]
    | all (==Word)  argTy                              = [functionCall "uvec3"              [a,b,c]]
    | all (==V2F)   argTy                              = [functionCall "mat2x3"             [a,b,c]]
    | all (==V3F)   argTy                              = [functionCall "mat3"               [a,b,c]]
    | all (==V4F)   argTy                              = [functionCall "mat4x3"             [a,b,c]]
codeGenPrim PrimTupToV4             ty argTy [a,b,c,d]
    | all (==Bool)  argTy                              = [functionCall "bvec4"              [a,b,c,d]]
    | all (==Float) argTy                              = [functionCall "vec4"               [a,b,c,d]]
    | all (==Int)   argTy                              = [functionCall "ivec4"              [a,b,c,d]]
    | all (==Word)  argTy                              = [functionCall "uvec4"              [a,b,c,d]]
    | all (==V2F)   argTy                              = [functionCall "mat2x4"             [a,b,c,d]]
    | all (==V3F)   argTy                              = [functionCall "mat3x4"             [a,b,c,d]]
    | all (==V4F)   argTy                              = [functionCall "mat4"               [a,b,c,d]]

codeGenPrim PrimV2ToTup             ty argTy [a]
    | all isMatrix argTy                               = [ Bracket a (IntConstant Decimal 0)
                                                         , Bracket a (IntConstant Decimal 1)
                                                         ]
    | otherwise                                        = [ FieldSelection a "x"
                                                         , FieldSelection a "y"
                                                         ]
codeGenPrim PrimV3ToTup             ty argTy [a]
    | all isMatrix argTy                               = [ Bracket a (IntConstant Decimal 0)
                                                         , Bracket a (IntConstant Decimal 1)
                                                         , Bracket a (IntConstant Decimal 2)
                                                         ]
    | otherwise                                        = [ FieldSelection a "x"
                                                         , FieldSelection a "y"
                                                         , FieldSelection a "z"
                                                         ]
codeGenPrim PrimV4ToTup             ty argTy [a]
    | all isMatrix argTy                               = [ Bracket a (IntConstant Decimal 0)
                                                         , Bracket a (IntConstant Decimal 1)
                                                         , Bracket a (IntConstant Decimal 2)
                                                         , Bracket a (IntConstant Decimal 3)
                                                         ]
    | otherwise                                        = [ FieldSelection a "x"
                                                         , FieldSelection a "y"
                                                         , FieldSelection a "z"
                                                         , FieldSelection a "w"
                                                         ]

-- Arithmetic Functions
-- OK
codeGenPrim PrimAdd                 ty argTy [a,b]        = [Add a b]
codeGenPrim PrimAddS                ty argTy [a,b]        = [Add a b]
codeGenPrim PrimSub                 ty argTy [a,b]        = [Sub a b]
codeGenPrim PrimSubS                ty argTy [a,b]        = [Sub a b]
codeGenPrim PrimMul                 ty argTy [a,b]
    | all isMatrix argTy                               = [functionCall "matrixCompMult"     [a,b]]
    | otherwise                                        = [Mul a b]
codeGenPrim PrimMulS                ty argTy [a,b]        = [Mul a b]
codeGenPrim PrimDiv                 ty argTy [a,b]        = [Div a b]
codeGenPrim PrimDivS                ty argTy [a,b]        = [Div a b]
codeGenPrim PrimNeg                 ty argTy [a]          = [UnaryNegate a]
codeGenPrim PrimMod                 ty argTy [a,b]
    | all isIntegral argTy                             = [Mod a b]
    | otherwise                                        = [functionCall "mod"                [a,b]]
codeGenPrim PrimModS                ty argTy [a,b]
    | all isIntegral argTy                             = [Mod a b]
    | otherwise                                        = [functionCall "mod"                [a,b]]

-- Bit-wise Functions
-- OK
codeGenPrim PrimBAnd                ty argTy [a,b]        = [BitAnd a b]
codeGenPrim PrimBAndS               ty argTy [a,b]        = [BitAnd a b]
codeGenPrim PrimBOr                 ty argTy [a,b]        = [BitOr a b]
codeGenPrim PrimBOrS                ty argTy [a,b]        = [BitOr a b]
codeGenPrim PrimBXor                ty argTy [a,b]        = [BitXor a b]
codeGenPrim PrimBXorS               ty argTy [a,b]        = [BitXor a b]
codeGenPrim PrimBNot                ty argTy [a]          = [UnaryOneComplement a]
codeGenPrim PrimBShiftL             ty argTy [a,b]        = [LeftShift a b]
codeGenPrim PrimBShiftLS            ty argTy [a,b]        = [LeftShift a b]
codeGenPrim PrimBShiftR             ty argTy [a,b]        = [RightShift a b]
codeGenPrim PrimBShiftRS            ty argTy [a,b]        = [RightShift a b]

-- Logic Functions
-- OK
codeGenPrim PrimAnd                 ty argTy [a,b]        = [And a b]
codeGenPrim PrimOr                  ty argTy [a,b]        = [Or a b]
codeGenPrim PrimXor                 ty argTy [a,b]        = error "codeGenPrim PrimXor is not implemented yet!" -- TODO: implement in GLSLSyntax
codeGenPrim PrimNot                 ty argTy [a]
    | all isScalar argTy                               = [UnaryNot a]
    | otherwise                                        = [functionCall "not"                [a]]
codeGenPrim PrimAny                 ty argTy [a]          = [functionCall "any"                [a]]
codeGenPrim PrimAll                 ty argTy [a]          = [functionCall "all"                [a]]

-- Angle and Trigonometry Functions
-- OK
codeGenPrim PrimACos                ty argTy [a]          = [functionCall "acos"               [a]]
codeGenPrim PrimACosH               ty argTy [a]          = [functionCall "acosh"              [a]]
codeGenPrim PrimASin                ty argTy [a]          = [functionCall "asin"               [a]]
codeGenPrim PrimASinH               ty argTy [a]          = [functionCall "asinh"              [a]]
codeGenPrim PrimATan                ty argTy [a]          = [functionCall "atan"               [a]]
codeGenPrim PrimATan2               ty argTy [a,b]        = [functionCall "atan"               [a,b]]
codeGenPrim PrimATanH               ty argTy [a]          = [functionCall "atanh"              [a]]
codeGenPrim PrimCos                 ty argTy [a]          = [functionCall "cos"                [a]]
codeGenPrim PrimCosH                ty argTy [a]          = [functionCall "cosh"               [a]]
codeGenPrim PrimDegrees             ty argTy [a]          = [functionCall "degrees"            [a]]
codeGenPrim PrimRadians             ty argTy [a]          = [functionCall "radians"            [a]]
codeGenPrim PrimSin                 ty argTy [a]          = [functionCall "sin"                [a]]
codeGenPrim PrimSinH                ty argTy [a]          = [functionCall "sinh"               [a]]
codeGenPrim PrimTan                 ty argTy [a]          = [functionCall "tan"                [a]]
codeGenPrim PrimTanH                ty argTy [a]          = [functionCall "tanh"               [a]]

-- Exponential Functions
-- OK
codeGenPrim PrimPow                 ty argTy [a,b]        = [functionCall "pow"                [a,b]]
codeGenPrim PrimExp                 ty argTy [a]          = [functionCall "exp"                [a]]
codeGenPrim PrimLog                 ty argTy [a]          = [functionCall "log"                [a]]
codeGenPrim PrimExp2                ty argTy [a]          = [functionCall "exp2"               [a]]
codeGenPrim PrimLog2                ty argTy [a]          = [functionCall "log2"               [a]]
codeGenPrim PrimSqrt                ty argTy [a]          = [functionCall "sqrt"               [a]]
codeGenPrim PrimInvSqrt             ty argTy [a]          = [functionCall "inversesqrt"        [a]]

-- Common Functions
-- OK
codeGenPrim PrimIsNan               ty argTy [a]          = [functionCall "isnan"              [a]]
codeGenPrim PrimIsInf               ty argTy [a]          = [functionCall "isinf"              [a]]
codeGenPrim PrimAbs                 ty argTy [a]          = [functionCall "abs"                [a]]
codeGenPrim PrimSign                ty argTy [a]          = [functionCall "sign"               [a]]
codeGenPrim PrimFloor               ty argTy [a]          = [functionCall "floor"              [a]]
codeGenPrim PrimTrunc               ty argTy [a]          = [functionCall "trunc"              [a]]
codeGenPrim PrimRound               ty argTy [a]          = [functionCall "round"              [a]]
codeGenPrim PrimRoundEven           ty argTy [a]          = [functionCall "roundEven"          [a]]
codeGenPrim PrimCeil                ty argTy [a]          = [functionCall "ceil"               [a]]
codeGenPrim PrimFract               ty argTy [a]          = [functionCall "fract"              [a]]
codeGenPrim PrimModF                ty argTy [a]          = error "codeGenPrim PrimModF is not implemented yet!" -- TODO
codeGenPrim PrimMin                 ty argTy [a,b]        = [functionCall "min"                [a,b]]
codeGenPrim PrimMinS                ty argTy [a,b]        = [functionCall "min"                [a,b]]
codeGenPrim PrimMax                 ty argTy [a,b]        = [functionCall "max"                [a,b]]
codeGenPrim PrimMaxS                ty argTy [a,b]        = [functionCall "max"                [a,b]]
codeGenPrim PrimClamp               ty argTy [a,b,c]      = [functionCall "clamp"              [a,b,c]]
codeGenPrim PrimClampS              ty argTy [a,b,c]      = [functionCall "clamp"              [a,b,c]]
codeGenPrim PrimMix                 ty argTy [a,b,c]      = [functionCall "mix"                [a,b,c]]
codeGenPrim PrimMixS                ty argTy [a,b,c]      = [functionCall "mix"                [a,b,c]]
codeGenPrim PrimMixB                ty argTy [a,b,c]      = [functionCall "mix"                [a,b,c]]
codeGenPrim PrimStep                ty argTy [a,b]        = [functionCall "step"               [a,b]]
codeGenPrim PrimStepS               ty argTy [a,b]        = [functionCall "step"               [a,b]]
codeGenPrim PrimSmoothStep          ty argTy [a,b,c]      = [functionCall "smoothstep"         [a,b,c]]
codeGenPrim PrimSmoothStepS         ty argTy [a,b,c]      = [functionCall "smoothstep"         [a,b,c]]

-- Integer/Float Conversion Functions
-- OK
codeGenPrim PrimFloatBitsToInt      ty argTy [a]          = [functionCall "floatBitsToInt"     [a]]
codeGenPrim PrimFloatBitsToUInt     ty argTy [a]          = [functionCall "floatBitsToUint"    [a]]
codeGenPrim PrimIntBitsToFloat      ty argTy [a]          = [functionCall "intBitsToFloat"     [a]]
codeGenPrim PrimUIntBitsToFloat     ty argTy [a]          = [functionCall "uintBitsToFloat"    [a]]

-- Geometric Functions
-- OK
codeGenPrim PrimLength              ty argTy [a]          = [functionCall "length"             [a]]
codeGenPrim PrimDistance            ty argTy [a,b]        = [functionCall "distance"           [a,b]]
codeGenPrim PrimDot                 ty argTy [a,b]        = [functionCall "dot"                [a,b]]
codeGenPrim PrimCross               ty argTy [a,b]        = [functionCall "cross"              [a,b]]
codeGenPrim PrimNormalize           ty argTy [a]          = [functionCall "normalize"          [a]]
codeGenPrim PrimFaceForward         ty argTy [a,b,c]      = [functionCall "faceforward"        [a,b,c]]
codeGenPrim PrimReflect             ty argTy [a,b]        = [functionCall "reflect"            [a,b]]
codeGenPrim PrimRefract             ty argTy [a,b,c]      = [functionCall "refract"            [a,b,c]]

-- Matrix Functions
-- OK
codeGenPrim PrimTranspose           ty argTy [a]          = [functionCall "transpose"          [a]]
codeGenPrim PrimDeterminant         ty argTy [a]          = [functionCall "determinant"        [a]]
codeGenPrim PrimInverse             ty argTy [a]          = [functionCall "inverse"            [a]]
codeGenPrim PrimOuterProduct        ty argTy [a,b]        = [functionCall "outerProduct"       [a,b]]
codeGenPrim PrimMulMatVec           ty argTy [a,b]        = [Mul a b]
codeGenPrim PrimMulVecMat           ty argTy [a,b]        = [Mul a b]
codeGenPrim PrimMulMatMat           ty argTy [a,b]        = [Mul a b]

-- Vector and Scalar Relational Functions
-- OK
codeGenPrim PrimLessThan            ty argTy [a,b]
    | all isScalarNum argTy                            = [Lt a b]
    | otherwise                                        = [functionCall "lessThan"           [a,b]]
codeGenPrim PrimLessThanEqual       ty argTy [a,b]
    | all isScalarNum argTy                            = [Lte a b]
    | otherwise                                        = [functionCall "lessThanEqual"      [a,b]]
codeGenPrim PrimGreaterThan         ty argTy [a,b]
    | all isScalarNum argTy                            = [Gt a b]
    | otherwise                                        = [functionCall "greaterThan"        [a,b]]
codeGenPrim PrimGreaterThanEqual    ty argTy [a,b]
    | all isScalarNum argTy                            = [Gte a b]
    | otherwise                                        = [functionCall "greaterThanEqual"   [a,b]]
codeGenPrim PrimEqualV              ty argTy [a,b]
    | all isScalar argTy                               = [Equ a b]
    | otherwise                                        = [functionCall "equal"              [a,b]]
codeGenPrim PrimEqual               ty argTy [a,b]        = [Equ a b]
codeGenPrim PrimNotEqualV           ty argTy [a,b]
    | all isScalar argTy                               = [Neq a b]
    | otherwise                                        = [functionCall "notEqual"           [a,b]]
codeGenPrim PrimNotEqual            ty argTy [a,b]        = [Neq a b]

-- Fragment Processing Functions
-- OK
codeGenPrim PrimDFdx                ty argTy [a]          = [functionCall "dFdx"               [a]]
codeGenPrim PrimDFdy                ty argTy [a]          = [functionCall "dFdy"               [a]]
codeGenPrim PrimFWidth              ty argTy [a]          = [functionCall "fwidth"             [a]]

-- Noise Functions
-- OK
codeGenPrim PrimNoise1              ty argTy [a]          = [functionCall "noise1"             [a]]
codeGenPrim PrimNoise2              ty argTy [a]          = [functionCall "noise2"             [a]]
codeGenPrim PrimNoise3              ty argTy [a]          = [functionCall "noise3"             [a]]
codeGenPrim PrimNoise4              ty argTy [a]          = [functionCall "noise4"             [a]]

-- Texture Lookup Functions
codeGenPrim PrimTextureSize             ty argTy [a]          = [functionCall "textureSize"           [a]]
codeGenPrim PrimTextureSize             ty argTy [a,b]        = [functionCall "textureSize"           [a,b]]
codeGenPrim PrimTexture                 ty argTy [a,b]        = [swizzleV4 ty $ functionCall "texture"               [a,b]]
codeGenPrim PrimTexture                 ty argTy [a,b,c]      = [swizzleV4 ty $ functionCall "texture"               [a,b,c]]
codeGenPrim PrimTextureProj             ty argTy [a,b]        = [swizzleV4 ty $ functionCall "textureProj"           [a,b]]
codeGenPrim PrimTextureProj             ty argTy [a,b,c]      = [swizzleV4 ty $ functionCall "textureProj"           [a,b,c]]
codeGenPrim PrimTextureLod              ty argTy [a,b,c]      = [swizzleV4 ty $ functionCall "textureLod"            [a,b,c]]
codeGenPrim PrimTextureOffset           ty argTy [a,b,c]      = [swizzleV4 ty $ functionCall "textureOffset"         [a,b,c]]
codeGenPrim PrimTextureOffset           ty argTy [a,b,c,d]    = [swizzleV4 ty $ functionCall "textureOffset"         [a,b,c,d]]
codeGenPrim PrimTexelFetch              ty argTy [a,b]        = [swizzleV4 ty $ functionCall "texelFetch"            [a,b]]
codeGenPrim PrimTexelFetch              ty argTy [a,b,c]      = [swizzleV4 ty $ functionCall "texelFetch"            [a,b,c]]
codeGenPrim PrimTexelFetchOffset        ty argTy [a,b,c]      = [swizzleV4 ty $ functionCall "texelFetchOffset"      [a,b,c]]
codeGenPrim PrimTexelFetchOffset        ty argTy [a,b,c,d]    = [swizzleV4 ty $ functionCall "texelFetchOffset"      [a,b,c,d]]
codeGenPrim PrimTextureProjOffset       ty argTy [a,b,c]      = [swizzleV4 ty $ functionCall "textureProjOffset"     [a,b,c]]
codeGenPrim PrimTextureProjOffset       ty argTy [a,b,c,d]    = [swizzleV4 ty $ functionCall "textureProjOffset"     [a,b,c,d]]
codeGenPrim PrimTextureLodOffset        ty argTy [a,b,c,d]    = [swizzleV4 ty $ functionCall "textureLodOffset"      [a,b,c,d]]
codeGenPrim PrimTextureProjLod          ty argTy [a,b,c]      = [swizzleV4 ty $ functionCall "textureProjLod"        [a,b,c]]
codeGenPrim PrimTextureProjLodOffset    ty argTy [a,b,c,d]    = [swizzleV4 ty $ functionCall "textureProjLodOffset"  [a,b,c,d]]
codeGenPrim PrimTextureGrad             ty argTy [a,b,c,d]    = [swizzleV4 ty $ functionCall "textureGrad"           [a,b,c,d]]
codeGenPrim PrimTextureGradOffset       ty argTy [a,b,c,d,e]  = [swizzleV4 ty $ functionCall "textureGradOffset"     [a,b,c,d,e]]
codeGenPrim PrimTextureProjGrad         ty argTy [a,b,c,d]    = [swizzleV4 ty $ functionCall "textureProjGrad"       [a,b,c,d]]
codeGenPrim PrimTextureProjGradOffset   ty argTy [a,b,c,d,e]  = [swizzleV4 ty $ functionCall "textureProjGradOffset" [a,b,c,d,e]]

-- unmatched primitive function
codeGenPrim prim ty argTy params = throw $ userError $ unlines $
    [ "codeGenPrim failed: "
    , "  name: " ++ show prim
    , "  parameter types:  " ++ show ty
    , "  parameter values: " ++ show params
    ]

swizzleV4 :: [InputType] -> Expr -> Expr
swizzleV4 [ty] a
    | elem ty  [V4F, V4I, V4U]      = a
    | elem ty  [V3F, V3I, V3U]      = FieldSelection a "rgb"
    | elem ty  [V2F, V2I, V2U]      = FieldSelection a "rg"
    | elem ty  [Float, Int, Word]   = FieldSelection a "r"
    | otherwise                     = error $ "swizzleV4 - illegal type: " ++ show ty

-- glsl ast utility
functionCall :: String -> [Expr] -> Expr
functionCall name params = FunctionCall (FuncId name) (Params params)

isMatrix :: InputType -> Bool
isMatrix ty = elem ty $
    [ M22F, M23F, M24F
    , M32F, M33F, M34F
    , M42F, M43F, M44F
    ]

isIntegral :: InputType -> Bool
isIntegral ty = elem ty $
    [ Word, V2U, V3U, V4U
    , Int,  V2I, V3I, V4I
    ]

isScalarNum :: InputType -> Bool
isScalarNum ty = elem ty [Int, Word, Float]

isScalar :: InputType -> Bool
isScalar ty = elem ty [Bool, Int, Word, Float]

wordC :: Word32 -> Expr
wordC v = IntConstant Decimal (fromIntegral v)

intC :: Int32 -> Expr
intC v = IntConstant Decimal (fromIntegral v)

boolC :: Bool -> Expr
boolC v = BoolConstant v

floatC :: Float -> Expr
floatC v = FloatConstant v

v2C :: String -> (a -> Expr) -> V2 a -> Expr
v2C name f (V2 x y) = functionCall name [f x, f y]

v3C :: String -> (a -> Expr) -> V3 a -> Expr
v3C name f (V3 x y z) = functionCall name [f x, f y, f z]

v4C :: String -> (a -> Expr) -> V4 a -> Expr
v4C name f (V4 x y z w) = functionCall name [f x, f y, f z, f w]

matX2C :: String -> (v Float -> Expr) -> V2 (v Float) -> Expr
matX2C name f (V2 x y) = functionCall name [f x, f y]

matX3C :: String -> (v Float -> Expr) -> V3 (v Float) -> Expr
matX3C name f (V3 x y z) = functionCall name [f x, f y, f z]

matX4C :: String -> (v Float -> Expr) -> V4 (v Float) -> Expr
matX4C name f (V4 x y z w) = functionCall name [f x, f y, f z, f w]

codeGenConst :: Value -> Expr
codeGenConst (VBool  v) = boolC v
codeGenConst (VV2B   v) = v2C "bvec2" boolC v
codeGenConst (VV3B   v) = v3C "bvec3" boolC v
codeGenConst (VV4B   v) = v4C "bvec4" boolC v
codeGenConst (VWord  v) = wordC v
codeGenConst (VV2U   v) = v2C "uvec2" wordC v
codeGenConst (VV3U   v) = v3C "uvec3" wordC v
codeGenConst (VV4U   v) = v4C "uvec4" wordC v
codeGenConst (VInt   v) = intC v
codeGenConst (VV2I   v) = v2C "ivec2" intC v
codeGenConst (VV3I   v) = v3C "ivec3" intC v
codeGenConst (VV4I   v) = v4C "ivec4" intC v
codeGenConst (VFloat v) = floatC v
codeGenConst (VV2F   v) = v2C "vec2" floatC v
codeGenConst (VV3F   v) = v3C "vec3" floatC v
codeGenConst (VV4F   v) = v4C "vec4" floatC v
codeGenConst (VM22F  v) = matX2C "mat2"   (v2C "vec2" floatC) v
codeGenConst (VM23F  v) = matX3C "mat2x3" (v2C "vec2" floatC) v
codeGenConst (VM24F  v) = matX4C "mat2x4" (v2C "vec2" floatC) v
codeGenConst (VM32F  v) = matX2C "mat3x2" (v3C "vec3" floatC) v
codeGenConst (VM33F  v) = matX3C "mat3"   (v3C "vec3" floatC) v
codeGenConst (VM34F  v) = matX4C "mat3x4" (v3C "vec3" floatC) v
codeGenConst (VM42F  v) = matX2C "mat4x2" (v4C "vec4" floatC) v
codeGenConst (VM43F  v) = matX3C "mat4x3" (v4C "vec4" floatC) v
codeGenConst (VM44F  v) = matX4C "mat4"   (v4C "vec4" floatC) v

type CGen a = State ([Statement],IntMap [Expr]) a

store :: DAG -> Int -> Expr -> CGen [Expr]
store dag expId exp = do
    let name    = "val" ++ show expId
        newVar  = Variable name
        t       = codeGenType $ expIdType dag expId
        [ty]    = {-trace (show expId ++ " [ty]    = " ++ show t)-} t
        newStmt = varStmt name (toGLSLType ty) exp
        cnt     = expIdCount dag expId
    case cnt > 0 of
        True    -> do
            (stmt,varMap) <- get
            put (newStmt:stmt,IntMap.insert expId [newVar] varMap)
            return [newVar]
        False   -> return [exp]

-- require: input names
codeGenExp' :: DAG -> Map Exp String -> [ByteString] -> ExpId -> CGen [Expr]
codeGenExp' dag smpName inNames expId = do
    (stmt,varMap) <- get
    case IntMap.lookup expId varMap of
        Just v  -> return v
        Nothing -> case toExp dag expId of
            Const c             -> store dag expId $ codeGenConst c
            Uni n               -> return [Variable $! unpack n]
            PrimVar n           -> return [Variable $! unpack n]
            PrimApp f arg       -> do
                arg' <- codeGenExp' dag smpName inNames arg
                let argTy   = codeGenType $ expIdType dag arg
                    ty      = codeGenType $ expIdType dag expId
                    e       = codeGenPrim f ty argTy arg'
                if length e > 1 then return e else
                    store dag expId $ head e
            s@(Sampler f e t)   -> case Map.lookup s smpName of
                Just name   -> return [Variable name]
                Nothing     -> error "Unknown sampler value!"
            Cond p t e          -> do
                [p'] <- codeGenExp' dag smpName inNames p
                t' <- codeGenExp' dag smpName inNames t
                e' <- codeGenExp' dag smpName inNames e
                let branch a b  = Selection p' a b
                return $ zipWith branch t' e'
            e@(Var i li)        -> do
                let ty      = expType dag e
                    arity   = length $! codeGenType ty
                if i == 0 && length inNames == arity then return [Variable (unpack n) | n <- inNames] else throw $ userError $ unlines $
                    [ "codeGenExp failed: "
                    , "  Var " ++ show i ++ " (" ++ show li ++ ") :: " ++ show ty
                    , "  input names:  " ++ show inNames
                    , "  arity:        " ++ show arity
                    ]
            Tup t               -> concat <$> mapM (codeGenExp' dag smpName inNames) t
            p@(Prj idx e)       -> do
                let ty  = expType dag p
                e' <- codeGenExp' dag smpName inNames e
                return $ reverse . take (length $ codeGenType ty) . drop idx . reverse $ e'
{-
  required info: output variable names
  if we disable inline functions, it simplifies variable name gen
-}

codeGenVertexShader :: DAG
                    -> Map Exp String
                    -> [(ByteString,InputType)]
                    -> Exp
                    -> (ByteString, [(ByteString,GLSL.InterpolationQualifier,InputType)])
codeGenVertexShader dag smpName inVars = cvt
  where
    genExp :: ExpId -> CGen [Expr]
    genExp = codeGenExp' dag smpName (map fst inVars)

    genIExp :: Exp -> CGen (GLSL.InterpolationQualifier,[Expr],[InputType])
    genIExp (Flat e)            = (GLSL.Flat,,codeGenType $ expIdType dag e) <$> genExp e
    genIExp (Smooth e)          = (GLSL.Smooth,,codeGenType $ expIdType dag e) <$> genExp e
    genIExp (NoPerspective e)   = (GLSL.NoPerspective,,codeGenType $ expIdType dag e) <$> genExp e

    cvt :: Exp -> (ByteString, [(ByteString,GLSL.InterpolationQualifier,InputType)])
    cvt (Lam lam) = cvt $ toExp dag lam
    cvt (Body bodyExp) = (SB.unlines $!
        [ "#version 150 core"
        -- , "#pragma optimize(off)"
        , pp [uniform   (unpack n)    (toGLSLType t) | (n,t) <- uniVars]
        , pp [uniform           n     (toGLSLType t) | (n,t) <- smpVars]
        , pp [inVar     (unpack n)    (toGLSLType t) | (n,t) <- inVars]
        , pp [outVarIQ  (unpack n) iq (toGLSLType t) | n <- oNames | iq <- oQ | [t] <- oT]
        , "void main ()"
        , ppE (posE:sizeE:concat oE) ("gl_Position":"gl_PointSize":oNames)
        ], [(n,q,t) | n <- oNames | q <- oQ | [t] <- oT])
      where
        VertexOut pos size outs = toExp dag bodyExp
        ppE e a = pack $! show $! pPrint $! Compound $ reverse stmt ++ [assign (Variable (unpack n)) ex | ex <- e | n <- a]
        pp a    = pack $! show $! pPrint $! TranslationUnit a
        uniVars = Set.toList $ Set.fromList [(n,t) | u@(Uni n) <- expUniverse' dag (toExp dag bodyExp), let Single t = expType dag u]
        smpVars = Set.toList $ Set.fromList [(n,t) | s@Sampler {} <- expUniverse' dag (toExp dag bodyExp), let Single t = expType dag s, let Just n = Map.lookup s smpName]
        ((posE,sizeE,oQ,oE,oT),(stmt,_)) = runState genSrc ([],IntMap.empty)
        genSrc = do
            --[posE']      <- genExp pos
            a <- genExp pos
            let [posE'] = {-trace ("let [posE'] = " ++ show a)-} a
            [sizeE']     <- genExp size
            (oQ',oE',oT')  <- unzip3 <$> mapM genIExp (map (toExp dag) outs)
            return (posE',sizeE',oQ',oE',oT')
        oNames      = [pack $ "v" ++ show i | i <- [0..]]

-- TODO
codeGenGeometryShader samplerNameMap inVars _ = ("", inVars)

codeGenFragmentShader :: DAG
                      -> Map Exp String
                      -> [(ByteString,GLSL.InterpolationQualifier,InputType)]
                      -> Exp
                      -> Exp
                      -> (ByteString, [(ByteString,InputType)],Int)
codeGenFragmentShader dag smpName inVars ffilter = cvt
  where
    cvtF :: Exp -> CGen Expr
    cvtF (Lam lam) = cvtF $ toExp dag lam
    cvtF (Body bodyExp) = do
        [e] <- genExp bodyExp
        return e

    cvt :: Exp -> (ByteString, [(ByteString,InputType)],Int)
    cvt (Lam lam) = cvt $ toExp dag lam
    cvt (Body bodyExp) = case toExp dag bodyExp of
        FragmentOut e             -> src e []
        FragmentOutDepth de e     -> src e [("gl_FragDepth",de)]
        FragmentOutRastDepth e    -> src e []

    genExp :: ExpId -> CGen [Expr]
    genExp = codeGenExp' dag smpName [n | (n,_,_) <- inVars]

    genFExp :: ExpId -> CGen ([Expr],[InputType])
    genFExp e = (,codeGenType $ expIdType dag e) <$> genExp e

    oNames :: [ByteString]
    oNames = [pack $ "f" ++ show i | i <- [0..]]

    src :: [ExpId] -> [(ByteString,ExpId)] -> (ByteString, [(ByteString,InputType)],Int)
    src outs outs' = (SB.unlines $!
        [ "#version 150 core"
        -- , "#pragma optimize(off)"
        , pp [uniform   (unpack n)    (toGLSLType t) | (n,t) <- uniVars]
        , pp [uniform           n     (toGLSLType t) | (n,t) <- smpVars]
        , pp [inVarIQ   (unpack n) iq (toGLSLType t) | (n,iq,t) <- inVars]
        , pp [outVar    (unpack n)    (toGLSLType t) | n <- oNames | [t] <- oT]
        , "void main ()"
        , ppBody $ case ffilter of
            PassAll     -> reverse stmt ++ body
            Filter f    -> reverse fstmt ++ [SelectionStatement (UnaryNot fexpr) Discard $ Just (CompoundStatement $ Compound $ reverse stmt ++ body)]
        ], [(n,t) | n <- oNames | [t] <- oT], length outs)
      where
        assigns a e = [assign (Variable (unpack n)) ex | ex <- e | n <- a]
        ppBody l    = pack $! show $! pPrint $! Compound l
        pp a        = pack $! show $! pPrint $! TranslationUnit a
        allExps     = concat [expUniverse' dag outs, expUniverse' dag (map snd outs'), filterExps]
        filterExps  = case ffilter of
            PassAll     -> []
            Filter f    -> expUniverse' dag f
        uniVars     = Set.toList $ Set.fromList [(n,t) | u@(Uni n) <- allExps, let Single t = expType dag u]
        smpVars     = Set.toList $ Set.fromList [(n,t) | s@Sampler {} <- allExps, let Single t = expType dag s, let Just n = Map.lookup s smpName]
        body        = assigns (oN' ++ oNames) (concat oE' ++ concat oE)
        ((oE',oN',oE,oT,fstmt,fexpr),(stmt,_)) = runState genSrc ([],IntMap.empty)
        genSrc      = do
            fexpr' <- case ffilter of
                PassAll     -> return $ boolC True
                Filter f    -> cvtF $ toExp dag f
            (fstmt',s) <- get
            put ([],s)
            (oE'',oN'')   <- unzip <$> sequence [(,n) <$> genExp e | (n,e) <- outs']
            (oE',oT')     <- unzip <$> mapM genFExp outs
            return (oE'',oN'',oE',oT',fstmt',fexpr')

codeGenType :: Ty -> [InputType]
codeGenType (Single ty) = [ty]
codeGenType (Tuple l)   = concatMap codeGenType l

-- Utility functions
toGLSLType :: InputType -> TypeSpecifierNonArray
toGLSLType t = case t of
    Bool    -> GLSL.Bool
    V2B     -> GLSL.BVec2
    V3B     -> GLSL.BVec3
    V4B     -> GLSL.BVec4
    Word    -> GLSL.UInt
    V2U     -> GLSL.UVec2
    V3U     -> GLSL.UVec3
    V4U     -> GLSL.UVec4
    Int     -> GLSL.Int
    V2I     -> GLSL.IVec2
    V3I     -> GLSL.IVec3
    V4I     -> GLSL.IVec4
    Float   -> GLSL.Float
    V2F     -> GLSL.Vec2
    V3F     -> GLSL.Vec3
    V4F     -> GLSL.Vec4
    M22F    -> GLSL.Mat2
    M23F    -> GLSL.Mat2x3
    M24F    -> GLSL.Mat2x4
    M32F    -> GLSL.Mat3x2
    M33F    -> GLSL.Mat3
    M34F    -> GLSL.Mat3x4
    M42F    -> GLSL.Mat4x2
    M43F    -> GLSL.Mat4x3
    M44F    -> GLSL.Mat4
    -- shadow textures
    STexture1D          -> GLSL.Sampler1DShadow
    STexture2D          -> GLSL.Sampler2DShadow
    STextureCube        -> GLSL.SamplerCubeShadow
    STexture1DArray     -> GLSL.Sampler1DArrayShadow
    STexture2DArray     -> GLSL.Sampler2DArrayShadow
    STexture2DRect      -> GLSL.Sampler2DRectShadow
    -- float textures
    FTexture1D          -> GLSL.Sampler1D
    FTexture2D          -> GLSL.Sampler2D
    FTexture3D          -> GLSL.Sampler3D
    FTextureCube        -> GLSL.SamplerCube
    FTexture1DArray     -> GLSL.Sampler1DArray
    FTexture2DArray     -> GLSL.Sampler2DArray
    FTexture2DMS        -> GLSL.Sampler2DMS
    FTexture2DMSArray   -> GLSL.Sampler2DMSArray
    FTextureBuffer      -> GLSL.SamplerBuffer
    FTexture2DRect      -> GLSL.Sampler2DRect
    -- int textures
    ITexture1D          -> GLSL.ISampler1D
    ITexture2D          -> GLSL.ISampler2D
    ITexture3D          -> GLSL.ISampler3D
    ITextureCube        -> GLSL.ISamplerCube
    ITexture1DArray     -> GLSL.ISampler1DArray
    ITexture2DArray     -> GLSL.ISampler2DArray
    ITexture2DMS        -> GLSL.ISampler2DMS
    ITexture2DMSArray   -> GLSL.ISampler2DMSArray
    ITextureBuffer      -> GLSL.ISamplerBuffer
    ITexture2DRect      -> GLSL.ISampler2DRect
    -- uint textures
    UTexture1D          -> GLSL.USampler1D
    UTexture2D          -> GLSL.USampler2D
    UTexture3D          -> GLSL.USampler3D
    UTextureCube        -> GLSL.USamplerCube
    UTexture1DArray     -> GLSL.USampler1DArray
    UTexture2DArray     -> GLSL.USampler2DArray
    UTexture2DMS        -> GLSL.USampler2DMS
    UTexture2DMSArray   -> GLSL.USampler2DMSArray
    UTextureBuffer      -> GLSL.USamplerBuffer
    UTexture2DRect      -> GLSL.USampler2DRect

varInit :: String -> TypeSpecifierNonArray -> Maybe TypeQualifier -> Maybe Expr -> Declaration
varInit name ty tq val = InitDeclaration (TypeDeclarator varType) [InitDecl name Nothing val]
  where
    varTySpecNoPrec = TypeSpecNoPrecision ty Nothing
    varTySpec = TypeSpec Nothing varTySpecNoPrec
    varType = FullType tq varTySpec

var :: String -> TypeSpecifierNonArray -> Maybe TypeQualifier -> Declaration
var name ty tq = varInit name ty tq Nothing

uniform :: String -> TypeSpecifierNonArray -> ExternalDeclaration
uniform name ty = Declaration $ var name ty (Just $ TypeQualSto Uniform)

inVar :: String -> TypeSpecifierNonArray -> ExternalDeclaration
inVar name ty = Declaration $ var name ty (Just $ TypeQualSto In)

inVarIQ :: String -> GLSL.InterpolationQualifier -> TypeSpecifierNonArray -> ExternalDeclaration
inVarIQ name iq ty = Declaration $ var name ty (Just $ TypeQualInt iq $ Just In)

outVar :: String -> TypeSpecifierNonArray -> ExternalDeclaration
outVar name ty = Declaration $ var name ty (Just $ TypeQualSto Out)

outVarIQ :: String -> GLSL.InterpolationQualifier -> TypeSpecifierNonArray -> ExternalDeclaration
outVarIQ name iq ty = Declaration $ var name ty (Just $ TypeQualInt iq $ Just Out)
{-
attribute :: String -> TypeSpecifierNonArray -> ExternalDeclaration
attribute name ty = Declaration $ var name ty (Just $ TypeQualSto Attribute)

varying :: String -> TypeSpecifierNonArray -> ExternalDeclaration
varying name ty = Declaration $ var name ty (Just $ TypeQualSto Varying)

varyingIQ :: String -> GLSL.InterpolationQualifier -> TypeSpecifierNonArray -> ExternalDeclaration
varyingIQ name iq ty = Declaration $ var name ty (Just $ TypeQualInt iq $ Just Varying)
-}
assign :: Expr -> Expr -> Statement
assign l r = ExpressionStatement $ Just $ Equal l r

varStmt :: String -> TypeSpecifierNonArray -> Expr -> Statement
varStmt name ty val = DeclarationStatement $ varInit name ty Nothing $ Just val
