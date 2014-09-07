module LCGLSLCodeGen (
    codeGenVertexShader,
    codeGenGeometryShader,
    codeGenFragmentShader,
    codeGenTupleType
) where

import Control.Exception
import Data.Word
import Data.Int
import Data.ByteString.Char8 (ByteString,pack,unpack)
import qualified Data.ByteString.Char8 as SB
import Text.PrettyPrint.HughesPJClass
import Data.Set (Set)
import qualified Data.Set as Set

import LCType
import LCAPIType hiding (LogicOperation(..), ComparisonFunction(..),TextureDataType(..))
import LCDSLType
import LCPrimFun
import LCDeBruijn
import LCDeBruijnUtil

import GLSLSyntax hiding (Const,InterpolationQualifier(..))
import qualified GLSLSyntax as GLSL
import GLSLPretty

codeGenPrim :: PrimFun stage t -> [InputType] -> [Expr] -> [Expr]

-- Vec/Mat (de)construction
codeGenPrim PrimTupToV2             ty [a,b]
    | all (==ITBool)  ty                            = [functionCall "bvec2"              [a,b]]
    | all (==ITFloat) ty                            = [functionCall "vec2"               [a,b]]
    | all (==ITInt)   ty                            = [functionCall "ivec2"              [a,b]]
    | all (==ITWord)  ty                            = [functionCall "uvec2"              [a,b]]
    | all (==ITV2F)   ty                            = [functionCall "mat2"               [a,b]]
    | all (==ITV3F)   ty                            = [functionCall "mat3x2"             [a,b]]
    | all (==ITV4F)   ty                            = [functionCall "mat4x2"             [a,b]]
codeGenPrim PrimTupToV3             ty [a,b,c]
    | all (==ITBool)  ty                            = [functionCall "bvec3"              [a,b,c]]
    | all (==ITFloat) ty                            = [functionCall "vec3"               [a,b,c]]
    | all (==ITInt)   ty                            = [functionCall "ivec3"              [a,b,c]]
    | all (==ITWord)  ty                            = [functionCall "uvec3"              [a,b,c]]
    | all (==ITV2F)   ty                            = [functionCall "mat2x3"             [a,b,c]]
    | all (==ITV3F)   ty                            = [functionCall "mat3"               [a,b,c]]
    | all (==ITV4F)   ty                            = [functionCall "mat4x3"             [a,b,c]]
codeGenPrim PrimTupToV4             ty [a,b,c,d]
    | all (==ITBool)  ty                            = [functionCall "bvec4"              [a,b,c,d]]
    | all (==ITFloat) ty                            = [functionCall "vec4"               [a,b,c,d]]
    | all (==ITInt)   ty                            = [functionCall "ivec4"              [a,b,c,d]]
    | all (==ITWord)  ty                            = [functionCall "uvec4"              [a,b,c,d]]
    | all (==ITV2F)   ty                            = [functionCall "mat2x4"             [a,b,c,d]]
    | all (==ITV3F)   ty                            = [functionCall "mat3x4"             [a,b,c,d]]
    | all (==ITV4F)   ty                            = [functionCall "mat4"               [a,b,c,d]]
codeGenPrim PrimV2ToTup             ty [a]
    | all isMatrix ty                               = [ Bracket a (IntConstant Decimal 0)
                                                      , Bracket a (IntConstant Decimal 1)
                                                      ]
    | otherwise                                     = [ FieldSelection a "x"
                                                      , FieldSelection a "y"
                                                      ]
codeGenPrim PrimV3ToTup             ty [a]
    | all isMatrix ty                               = [ Bracket a (IntConstant Decimal 0)
                                                      , Bracket a (IntConstant Decimal 1)
                                                      , Bracket a (IntConstant Decimal 2)
                                                      ]
    | otherwise                                     = [ FieldSelection a "x"
                                                      , FieldSelection a "y"
                                                      , FieldSelection a "z"
                                                      ]
codeGenPrim PrimV4ToTup             ty [a]
    | all isMatrix ty                               = [ Bracket a (IntConstant Decimal 0)
                                                      , Bracket a (IntConstant Decimal 1)
                                                      , Bracket a (IntConstant Decimal 2)
                                                      , Bracket a (IntConstant Decimal 3)
                                                      ]
    | otherwise                                     = [ FieldSelection a "x"
                                                      , FieldSelection a "y"
                                                      , FieldSelection a "z"
                                                      , FieldSelection a "w"
                                                      ]

-- Arithmetic Functions
-- OK
codeGenPrim PrimAdd                 ty [a,b]        = [Add a b]
codeGenPrim PrimAddS                ty [a,b]        = [Add a b]
codeGenPrim PrimSub                 ty [a,b]        = [Sub a b]
codeGenPrim PrimSubS                ty [a,b]        = [Sub a b]
codeGenPrim PrimMul                 ty [a,b]
    | all isMatrix ty                               = [functionCall "matrixCompMult"     [a,b]]
    | otherwise                                     = [Mul a b]
codeGenPrim PrimMulS                ty [a,b]        = [Mul a b]
codeGenPrim PrimDiv                 ty [a,b]        = [Div a b]
codeGenPrim PrimDivS                ty [a,b]        = [Div a b]
codeGenPrim PrimNeg                 ty [a]          = [UnaryNegate a]
codeGenPrim PrimMod                 ty [a,b]
    | all isIntegral ty                             = [Mod a b]
    | otherwise                                     = [functionCall "mod"                [a,b]]
codeGenPrim PrimModS                ty [a,b]
    | all isIntegral ty                             = [Mod a b]
    | otherwise                                     = [functionCall "mod"                [a,b]]

-- Bit-wise Functions
-- OK
codeGenPrim PrimBAnd                ty [a,b]        = [BitAnd a b]
codeGenPrim PrimBAndS               ty [a,b]        = [BitAnd a b]
codeGenPrim PrimBOr                 ty [a,b]        = [BitOr a b]
codeGenPrim PrimBOrS                ty [a,b]        = [BitOr a b]
codeGenPrim PrimBXor                ty [a,b]        = [BitXor a b]
codeGenPrim PrimBXorS               ty [a,b]        = [BitXor a b]
codeGenPrim PrimBNot                ty [a]          = [UnaryOneComplement a]
codeGenPrim PrimBShiftL             ty [a,b]        = [LeftShift a b]
codeGenPrim PrimBShiftLS            ty [a,b]        = [LeftShift a b]
codeGenPrim PrimBShiftR             ty [a,b]        = [RightShift a b]
codeGenPrim PrimBShiftRS            ty [a,b]        = [RightShift a b]

-- Logic Functions
-- OK
codeGenPrim PrimAnd                 ty [a,b]        = [And a b]
codeGenPrim PrimOr                  ty [a,b]        = [Or a b]
codeGenPrim PrimXor                 ty [a,b]        = error "codeGenPrim PrimXor is not implemented yet!" -- TODO: implement in GLSLSyntax
codeGenPrim PrimNot                 ty [a]
    | all isScalar ty                               = [UnaryNot a]
    | otherwise                                     = [functionCall "not"                [a]]
codeGenPrim PrimAny                 ty [a]          = [functionCall "any"                [a]]
codeGenPrim PrimAll                 ty [a]          = [functionCall "all"                [a]]

-- Angle and Trigonometry Functions
-- OK
codeGenPrim PrimACos                ty [a]          = [functionCall "acos"               [a]]
codeGenPrim PrimACosH               ty [a]          = [functionCall "acosh"              [a]]
codeGenPrim PrimASin                ty [a]          = [functionCall "asin"               [a]]
codeGenPrim PrimASinH               ty [a]          = [functionCall "asinh"              [a]]
codeGenPrim PrimATan                ty [a]          = [functionCall "atan"               [a]]
codeGenPrim PrimATan2               ty [a,b]        = [functionCall "atan"               [a,b]]
codeGenPrim PrimATanH               ty [a]          = [functionCall "atanh"              [a]]
codeGenPrim PrimCos                 ty [a]          = [functionCall "cos"                [a]]
codeGenPrim PrimCosH                ty [a]          = [functionCall "cosh"               [a]]
codeGenPrim PrimDegrees             ty [a]          = [functionCall "degrees"            [a]]
codeGenPrim PrimRadians             ty [a]          = [functionCall "radians"            [a]]
codeGenPrim PrimSin                 ty [a]          = [functionCall "sin"                [a]]
codeGenPrim PrimSinH                ty [a]          = [functionCall "sinh"               [a]]
codeGenPrim PrimTan                 ty [a]          = [functionCall "tan"                [a]]
codeGenPrim PrimTanH                ty [a]          = [functionCall "tanh"               [a]]

-- Exponential Functions
-- OK
codeGenPrim PrimPow                 ty [a,b]        = [functionCall "pow"                [a,b]]
codeGenPrim PrimExp                 ty [a]          = [functionCall "exp"                [a]]
codeGenPrim PrimLog                 ty [a]          = [functionCall "log"                [a]]
codeGenPrim PrimExp2                ty [a]          = [functionCall "exp2"               [a]]
codeGenPrim PrimLog2                ty [a]          = [functionCall "log2"               [a]]
codeGenPrim PrimSqrt                ty [a]          = [functionCall "sgrt"               [a]]
codeGenPrim PrimInvSqrt             ty [a]          = [functionCall "inversesqrt"        [a]]

-- Common Functions
-- OK
codeGenPrim PrimIsNan               ty [a]          = [functionCall "isnan"              [a]]
codeGenPrim PrimIsInf               ty [a]          = [functionCall "isinf"              [a]]
codeGenPrim PrimAbs                 ty [a]          = [functionCall "abs"                [a]]
codeGenPrim PrimSign                ty [a]          = [functionCall "sign"               [a]]
codeGenPrim PrimFloor               ty [a]          = [functionCall "floor"              [a]]
codeGenPrim PrimTrunc               ty [a]          = [functionCall "trunc"              [a]]
codeGenPrim PrimRound               ty [a]          = [functionCall "round"              [a]]
codeGenPrim PrimRoundEven           ty [a]          = [functionCall "roundEven"          [a]]
codeGenPrim PrimCeil                ty [a]          = [functionCall "ceil"               [a]]
codeGenPrim PrimFract               ty [a]          = [functionCall "fract"              [a]]
codeGenPrim PrimModF                ty [a]          = error "codeGenPrim PrimModF is not implemented yet!" -- TODO
codeGenPrim PrimMin                 ty [a,b]        = [functionCall "min"                [a,b]]
codeGenPrim PrimMinS                ty [a,b]        = [functionCall "min"                [a,b]]
codeGenPrim PrimMax                 ty [a,b]        = [functionCall "max"                [a,b]]
codeGenPrim PrimMaxS                ty [a,b]        = [functionCall "max"                [a,b]]
codeGenPrim PrimClamp               ty [a,b,c]      = [functionCall "clamp"              [a,b,c]]
codeGenPrim PrimClampS              ty [a,b,c]      = [functionCall "clamp"              [a,b,c]]
codeGenPrim PrimMix                 ty [a,b,c]      = [functionCall "mix"                [a,b,c]]
codeGenPrim PrimMixS                ty [a,b,c]      = [functionCall "mix"                [a,b,c]]
codeGenPrim PrimMixB                ty [a,b,c]      = [functionCall "mix"                [a,b,c]]
codeGenPrim PrimStep                ty [a,b]        = [functionCall "step"               [a,b]]
codeGenPrim PrimStepS               ty [a,b]        = [functionCall "step"               [a,b]]
codeGenPrim PrimSmoothStep          ty [a,b,c]      = [functionCall "smoothstep"         [a,b,c]]
codeGenPrim PrimSmoothStepS         ty [a,b,c]      = [functionCall "smoothstep"         [a,b,c]]

-- Integer/Float Conversion Functions
-- OK
codeGenPrim PrimFloatBitsToInt      ty [a]          = [functionCall "floatBitsToInt"     [a]]
codeGenPrim PrimFloatBitsToUInt     ty [a]          = [functionCall "floatBitsToUint"    [a]]
codeGenPrim PrimIntBitsToFloat      ty [a]          = [functionCall "intBitsToFloat"     [a]]
codeGenPrim PrimUIntBitsToFloat     ty [a]          = [functionCall "uintBitsToFloat"    [a]]

-- Geometric Functions
-- OK
codeGenPrim PrimLength              ty [a]          = [functionCall "length"             [a]]
codeGenPrim PrimDistance            ty [a,b]        = [functionCall "distance"           [a,b]]
codeGenPrim PrimDot                 ty [a,b]        = [functionCall "dot"                [a,b]]
codeGenPrim PrimCross               ty [a,b]        = [functionCall "cross"              [a,b]]
codeGenPrim PrimNormalize           ty [a]          = [functionCall "normalize"          [a]]
codeGenPrim PrimFaceForward         ty [a,b,c]      = [functionCall "faceforward"        [a,b,c]]
codeGenPrim PrimReflect             ty [a,b]        = [functionCall "reflect"            [a,b]]
codeGenPrim PrimRefract             ty [a,b,c]      = [functionCall "refract"            [a,b,c]]

-- Matrix Functions
-- OK
codeGenPrim PrimTranspose           ty [a]          = [functionCall "transpose"          [a]]
codeGenPrim PrimDeterminant         ty [a]          = [functionCall "determinant"        [a]]
codeGenPrim PrimInverse             ty [a]          = [functionCall "inverse"            [a]]
codeGenPrim PrimOuterProduct        ty [a,b]        = [functionCall "outerProduct"       [a,b]]
codeGenPrim PrimMulMatVec           ty [a,b]        = [Mul a b]
codeGenPrim PrimMulVecMat           ty [a,b]        = [Mul a b]
codeGenPrim PrimMulMatMat           ty [a,b]        = [Mul a b]

-- Vector and Scalar Relational Functions
-- OK
codeGenPrim PrimLessThan            ty [a,b]
    | all isScalarNum ty                            = [Lt a b]
    | otherwise                                     = [functionCall "lessThan"           [a,b]]
codeGenPrim PrimLessThanEqual       ty [a,b]
    | all isScalarNum ty                            = [Lte a b]
    | otherwise                                     = [functionCall "lessThanEqual"      [a,b]]
codeGenPrim PrimGreaterThan         ty [a,b]
    | all isScalarNum ty                            = [Gt a b]
    | otherwise                                     = [functionCall "greaterThan"        [a,b]]
codeGenPrim PrimGreaterThanEqual    ty [a,b]
    | all isScalarNum ty                            = [Gte a b]
    | otherwise                                     = [functionCall "greaterThanEqual"   [a,b]]
codeGenPrim PrimEqualV              ty [a,b]
    | all isScalar ty                               = [Equ a b]
    | otherwise                                     = [functionCall "equal"              [a,b]]
codeGenPrim PrimEqual               ty [a,b]        = [Equ a b]
codeGenPrim PrimNotEqualV           ty [a,b]
    | all isScalar ty                               = [Neq a b]
    | otherwise                                     = [functionCall "notEqual"           [a,b]]
codeGenPrim PrimNotEqual            ty [a,b]        = [Neq a b]

-- Fragment Processing Functions
-- OK
codeGenPrim PrimDFdx                ty [a]          = [functionCall "dFdx"               [a]]
codeGenPrim PrimDFdy                ty [a]          = [functionCall "dFdy"               [a]]
codeGenPrim PrimFWidth              ty [a]          = [functionCall "fwidth"             [a]]

-- Noise Functions
-- OK
codeGenPrim PrimNoise1              ty [a]          = [functionCall "noise1"             [a]]
codeGenPrim PrimNoise2              ty [a]          = [functionCall "noise2"             [a]]
codeGenPrim PrimNoise3              ty [a]          = [functionCall "noise3"             [a]]
codeGenPrim PrimNoise4              ty [a]          = [functionCall "noise4"             [a]]

-- Texture Lookup Functions
codeGenPrim PrimTextureSize             ty [a]          = [functionCall "textureSize"           [a]]
codeGenPrim PrimTextureSize             ty [a,b]        = [functionCall "textureSize"           [a,b]]
codeGenPrim PrimTexture                 ty [a,b]        = [functionCall "texture"               [a,b]]
codeGenPrim PrimTexture                 ty [a,b,c]      = [functionCall "texture"               [a,b,c]]
codeGenPrim PrimTextureProj             ty [a,b]        = [functionCall "textureProj"           [a,b]]
codeGenPrim PrimTextureProj             ty [a,b,c]      = [functionCall "textureProj"           [a,b,c]]
codeGenPrim PrimTextureLod              ty [a,b,c]      = [functionCall "textureLod"            [a,b,c]]
codeGenPrim PrimTextureOffset           ty [a,b,c]      = [functionCall "textureOffset"         [a,b,c]]
codeGenPrim PrimTextureOffset           ty [a,b,c,d]    = [functionCall "textureOffset"         [a,b,c,d]]
codeGenPrim PrimTexelFetch              ty [a,b]        = [functionCall "texelFetch"            [a,b]]
codeGenPrim PrimTexelFetch              ty [a,b,c]      = [functionCall "texelFetch"            [a,b,c]]
codeGenPrim PrimTexelFetchOffset        ty [a,b,c]      = [functionCall "texelFetchOffset"      [a,b,c]]
codeGenPrim PrimTexelFetchOffset        ty [a,b,c,d]    = [functionCall "texelFetchOffset"      [a,b,c,d]]
codeGenPrim PrimTextureProjOffset       ty [a,b,c]      = [functionCall "textureProjOffset"     [a,b,c]]
codeGenPrim PrimTextureProjOffset       ty [a,b,c,d]    = [functionCall "textureProjOffset"     [a,b,c,d]]
codeGenPrim PrimTextureLodOffset        ty [a,b,c,d]    = [functionCall "textureLodOffset"      [a,b,c,d]]
codeGenPrim PrimTextureProjLod          ty [a,b,c]      = [functionCall "textureProjLod"        [a,b,c]]
codeGenPrim PrimTextureProjLodOffset    ty [a,b,c,d]    = [functionCall "textureProjLodOffset"  [a,b,c,d]]
codeGenPrim PrimTextureGrad             ty [a,b,c,d]    = [functionCall "textureGrad"           [a,b,c,d]]
codeGenPrim PrimTextureGradOffset       ty [a,b,c,d,e]  = [functionCall "textureGradOffset"     [a,b,c,d,e]]
codeGenPrim PrimTextureProjGrad         ty [a,b,c,d]    = [functionCall "textureProjGrad"       [a,b,c,d]]
codeGenPrim PrimTextureProjGradOffset   ty [a,b,c,d,e]  = [functionCall "textureProjGradOffset" [a,b,c,d,e]]

-- unmatched primitive function
codeGenPrim prim ty params = throw $ userError $ unlines $
    [ "codeGenPrim failed: "
    , "  name: " ++ show prim
    , "  parameter types:  " ++ show ty
    , "  parameter values: " ++ show params
    ]

-- glsl ast utility
functionCall :: String -> [Expr] -> Expr
functionCall name params = FunctionCall (FuncId name) (Params params)

isMatrix :: InputType -> Bool
isMatrix ty = elem ty $
    [ ITM22F, ITM23F, ITM24F
    , ITM32F, ITM33F, ITM34F
    , ITM42F, ITM43F, ITM44F
    ]

isIntegral :: InputType -> Bool
isIntegral ty = elem ty $
    [ ITWord, ITV2U, ITV3U, ITV4U
    , ITInt,  ITV2I, ITV3I, ITV4I
    ]

isScalarNum :: InputType -> Bool
isScalarNum ty = elem ty [ITInt, ITWord, ITFloat]

isScalar :: InputType -> Bool
isScalar ty = elem ty [ITBool, ITInt, ITWord, ITFloat]

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

codeGenConst :: Value -> [Expr]
codeGenConst (VBool  v) = [boolC v]
codeGenConst (VV2B   v) = [v2C "bvec2" boolC v]
codeGenConst (VV3B   v) = [v3C "bvec3" boolC v]
codeGenConst (VV4B   v) = [v4C "bvec4" boolC v]
codeGenConst (VWord  v) = [wordC v]
codeGenConst (VV2U   v) = [v2C "uvec2" wordC v]
codeGenConst (VV3U   v) = [v3C "uvec3" wordC v]
codeGenConst (VV4U   v) = [v4C "uvec4" wordC v]
codeGenConst (VInt   v) = [intC v]
codeGenConst (VV2I   v) = [v2C "ivec2" intC v]
codeGenConst (VV3I   v) = [v3C "ivec3" intC v]
codeGenConst (VV4I   v) = [v4C "ivec4" intC v]
codeGenConst (VFloat v) = [floatC v]
codeGenConst (VV2F   v) = [v2C "vec2" floatC v]
codeGenConst (VV3F   v) = [v3C "vec3" floatC v]
codeGenConst (VV4F   v) = [v4C "vec4" floatC v]
codeGenConst (VM22F  v) = [matX2C "mat2"   (v2C "vec2" floatC) v]
codeGenConst (VM23F  v) = [matX3C "mat2x3" (v2C "vec2" floatC) v]
codeGenConst (VM24F  v) = [matX4C "mat2x4" (v2C "vec2" floatC) v]
codeGenConst (VM32F  v) = [matX2C "mat3x2" (v3C "vec3" floatC) v]
codeGenConst (VM33F  v) = [matX3C "mat3"   (v3C "vec3" floatC) v]
codeGenConst (VM34F  v) = [matX4C "mat3x4" (v3C "vec3" floatC) v]
codeGenConst (VM42F  v) = [matX2C "mat4x2" (v4C "vec4" floatC) v]
codeGenConst (VM43F  v) = [matX3C "mat4x3" (v4C "vec4" floatC) v]
codeGenConst (VM44F  v) = [matX4C "mat4"   (v4C "vec4" floatC) v]

-- require: input names
codeGenExp :: forall stage env genv t. [ByteString] -> OpenExp stage env genv t -> [Expr]
codeGenExp inNames (PrimApp (f :: PrimFun stage (a -> r)) arg)  = codeGenPrim f (codeGenTupleType $ tupleType (undefined :: a)) (codeGenExp inNames arg)
codeGenExp inNames (Const c)                                    = concatMap codeGenConst $! codeGenTupleValue $! tupleType c
codeGenExp inNames (PrimVar v)                                  = [Variable $! unpack $! fst $! toInput v]
codeGenExp inNames (Tup t)                                      = codeGenTup inNames t
codeGenExp inNames (Uni u)                                      = [Variable $! unpack n | (n,_) <- toInputList u]
codeGenExp inNames p@(Prj idx e)
  = reverse
  . take (length $ codeGenTupleType (expType p))
  . drop (prjToInt idx (expType e))
  . reverse
  $ codeGenExp inNames e
codeGenExp inNames (Var i)
    | idx == 0 && length inNames == arity                       = [Variable (unpack n) | n <- inNames]
    | otherwise = throw $ userError $ unlines $
        [ "codeGenExp failed: "
        , "  Var " ++ show idx
        , "  input names:  " ++ show inNames
        , "  arity:        " ++ show arity
        ]
  where
    arity = length $! codeGenTupleType (tupleType (undefined::t))
    idx   = idxToInt i

codeGenExp inNames (Cond p t e)                                 = zipWith branch (codeGenExp inNames t) (codeGenExp inNames e)
  where
    [predicate] = codeGenExp inNames p
    branch a b  = Selection predicate a b

codeGenTup :: [ByteString] -> Tuple (OpenExp stage env genv) t -> [Expr]
codeGenTup _ NilTup          = []
codeGenTup inNames (t `SnocTup` e) = codeGenTup inNames t ++ codeGenExp inNames e

{-
  required info: output variable names
  if we disable inline functions, it simplifies variable name gen
-}
codeGenVertexShader :: [(ByteString,InputType)]
                    -> OpenFun OpenVertexOut env genv t
                    -> (ByteString, [(ByteString,GLSL.InterpolationQualifier,InputType)])
codeGenVertexShader inVars = cvt
  where
    genExp :: OpenExp V env genv t -> [Expr]
    genExp = codeGenExp (map fst inVars)

    genIExp :: OpenInterpolatedFlatExp V env genv t -> [(GLSL.InterpolationQualifier,[Expr],[InputType])]
    genIExp ((Flat e :: Interpolated (OpenExp V env genv) t) :. xs)           = (GLSL.Flat,genExp e,codeGenTupleType (tupleType (undefined :: t))) : genIExp xs
    genIExp ((Smooth e :: Interpolated (OpenExp V env genv) t):. xs)          = (GLSL.Smooth,genExp e,codeGenTupleType (tupleType (undefined :: t))) : genIExp xs
    genIExp ((NoPerspective e :: Interpolated (OpenExp V env genv) t):. xs)   = (GLSL.NoPerspective,genExp e,codeGenTupleType (tupleType (undefined :: t))) : genIExp xs
    genIExp ZT = []

    cvt :: OpenFun OpenVertexOut env genv t -> (ByteString, [(ByteString,GLSL.InterpolationQualifier,InputType)])
    cvt (Lam lam) = cvt lam
    cvt (Body (VertexOut pos size outs)) = (SB.unlines $!
        [ "#extension GL_EXT_gpu_shader4 : require"
        , pp [uniform   (unpack n)    (toGLSLType t) | (n,t) <- uniVars]
        , pp [attribute (unpack n)    (toGLSLType t) | (n,t) <- inVars]
        , pp [varyingIQ (unpack n) iq (toGLSLType t) | n <- oNames | iq <- oQ | [t] <- oT]
        , "void main ()"
        , ppE (posE:sizeE:concat oE) ("gl_Position":"gl_PointSize":oNames)
        ], [(n,q,t) | n <- oNames | q <- oQ | [t] <- oT])
      where
        ppE e a = pack $! show $! pPrint $! Compound [assign (Variable (unpack n)) ex | ex <- e | n <- a]
        pp a    = pack $! show $! pPrint $! TranslationUnit a
        uniVars = Set.toList $! uniformInputExp pos `Set.union` uniformInputExp size `Set.union` uniformInputInterpolatedFlatExp outs
        [posE]  = genExp pos
        [sizeE] = genExp size
        (oQ,oE,oT)  = unzip3 $! genIExp outs
        oNames      = [pack $ "v" ++ show i | i <- [0..]]

-- TODO
codeGenGeometryShader inVars _ = ("", inVars)

codeGenFragmentShader :: [(ByteString,GLSL.InterpolationQualifier,InputType)]
                      -> FragmentFilter genv a
                      -> OpenFun OpenFragmentOut env genv b
                      -> (ByteString, [(ByteString,InputType)])
codeGenFragmentShader inVars ffilter = cvt
  where
    cvtF :: OpenFun (OpenExp F) env genv a -> Expr
    cvtF (Lam lam) = cvtF lam
    cvtF (Body body) = let [e] = genExp body in e

    cvt :: OpenFun OpenFragmentOut env genv a -> (ByteString, [(ByteString,InputType)])
    cvt (Lam lam) = cvt lam
    cvt (Body body) = case body of
        FragmentOut e           -> src e []
        FragmentOutDepth de e   -> src e [("gl_FragDepth",de)]
        FragmentOutRastDepth e  -> src e []

    genExp :: OpenExp F env genv t -> [Expr]
    genExp = codeGenExp [n | (n,_,_) <- inVars]

    genFExp :: OpenFlatExp F env genv t' -> [([Expr],[InputType])]
    genFExp ((e :: OpenExp F env genv t) :. xs) = (genExp e,codeGenTupleType (tupleType (undefined :: t))) : genFExp xs
    genFExp ZT = []

    oNames :: [ByteString]
    oNames = [pack $ "f" ++ show i | i <- [0..]]

    src :: OpenFlatExp F env genv a -> [(ByteString,OpenExp F env genv t)] -> (ByteString, [(ByteString,InputType)])
    src outs outs' = (SB.unlines $!
        [ "#extension GL_EXT_gpu_shader4 : require"
        , pp [uniform   (unpack n)    (toGLSLType t) | (n,t) <- uniVars]
        , pp [varyingIQ (unpack n) iq (toGLSLType t) | (n,iq,t) <- inVars]
        , pp [outVar    (unpack n)    (toGLSLType t) | n <- oNames | [t] <- oT]
        , "void main ()"
        , ppBody $ case ffilter of
            PassAll     -> body
            Filter f    -> [SelectionStatement (UnaryNot $ cvtF f) Discard $ Just (CompoundStatement $ Compound body)]
        ], [(n,t) | n <- oNames | [t] <- oT])
      where
        assigns a e = [assign (Variable (unpack n)) ex | ex <- e | n <- a]
        ppBody l    = pack $! show $! pPrint $! Compound l
        pp a        = pack $! show $! pPrint $! TranslationUnit a
        uniVars     = Set.toList $! uniformInputFlatExp outs `Set.union` Set.unions [uniformInputExp e | (_,e) <- outs']
        (oE',oN')   = unzip $! [(genExp e,n) | (n,e) <- outs']
        (oE,oT)     = unzip $! genFExp outs
        body        = assigns (oN' ++ oNames) (concat oE' ++ concat oE)

-- Convert a tuple index into the corresponding integer. Since the internal
-- representation is flat, be sure to walk over all sub components when indexing
-- past nested tuples.
--
prjToInt :: TupleIdx t e -> TupleType a -> Int
prjToInt ZeroTupIdx     _                 = 0
prjToInt (SuccTupIdx i) (b `PairTuple` a) = length (codeGenTupleType a) + prjToInt i b
prjToInt _ _ = error "prjToInt" "inconsistent valuation"

codeGenTupleType :: TupleType a -> [InputType]
codeGenTupleType UnitTuple         = []
codeGenTupleType (SingleTuple  ty) = [toType ty]
codeGenTupleType (PairTuple t1 t0) = codeGenTupleType t1 ++ codeGenTupleType t0

codeGenTupleValue :: TupleType a -> [Value]
codeGenTupleValue UnitTuple         = []
codeGenTupleValue (SingleTuple  ty) = [toValue ty]
codeGenTupleValue (PairTuple t1 t0) = codeGenTupleValue t1 ++ codeGenTupleValue t0

-- |Reify the result types of of a scalar expression using the expression AST before tying the
-- knot.
--
expType :: OpenExp stage genv env t -> TupleType (EltRepr t)
expType e =
  case e of
    (Var _       :: OpenExp stage genv env t)      -> tupleType (undefined::t)
    (Const _     :: OpenExp stage genv env t)      -> tupleType (undefined::t)
    (PrimVar _   :: OpenExp stage genv env t)      -> tupleType (undefined::t)
    (Uni _       :: OpenExp stage genv env t)      -> tupleType (undefined::t)
    (Tup _       :: OpenExp stage genv env t)      -> tupleType (undefined::t)
    (Prj idx _   :: OpenExp stage genv env t)      -> tupleIdxType idx
    (Cond _ t _  :: OpenExp stage genv env t)      -> expType t
    (PrimApp _ _ :: OpenExp stage genv env t)      -> tupleType (undefined::t)

-- |Reify the result type of a tuple projection.
--
tupleIdxType :: forall t e. TupleIdx t e -> TupleType (EltRepr e)
tupleIdxType ZeroTupIdx       = tupleType (undefined::e)
tupleIdxType (SuccTupIdx idx) = tupleIdxType idx

-- Utility functions
toGLSLType :: InputType -> TypeSpecifierNonArray
toGLSLType ITBool   = Bool
toGLSLType ITV2B    = BVec2
toGLSLType ITV3B    = BVec3
toGLSLType ITV4B    = BVec4
toGLSLType ITWord   = UInt
toGLSLType ITV2U    = UVec2
toGLSLType ITV3U    = UVec3
toGLSLType ITV4U    = UVec4
toGLSLType ITInt    = Int
toGLSLType ITV2I    = IVec2
toGLSLType ITV3I    = IVec3
toGLSLType ITV4I    = IVec4
toGLSLType ITFloat  = Float
toGLSLType ITV2F    = Vec2
toGLSLType ITV3F    = Vec3
toGLSLType ITV4F    = Vec4
toGLSLType ITM22F   = Mat2
toGLSLType ITM23F   = Mat2x3
toGLSLType ITM24F   = Mat2x4
toGLSLType ITM32F   = Mat3x2
toGLSLType ITM33F   = Mat3
toGLSLType ITM34F   = Mat3x4
toGLSLType ITM42F   = Mat4x2
toGLSLType ITM43F   = Mat4x3
toGLSLType ITM44F   = Mat4

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

outVar :: String -> TypeSpecifierNonArray -> ExternalDeclaration
outVar name ty = Declaration $ var name ty (Just $ TypeQualSto Out)

attribute :: String -> TypeSpecifierNonArray -> ExternalDeclaration
attribute name ty = Declaration $ var name ty (Just $ TypeQualSto Attribute)

varying :: String -> TypeSpecifierNonArray -> ExternalDeclaration
varying name ty = Declaration $ var name ty (Just $ TypeQualSto Varying)

varyingIQ :: String -> GLSL.InterpolationQualifier -> TypeSpecifierNonArray -> ExternalDeclaration
varyingIQ name iq ty = Declaration $ var name ty (Just $ TypeQualInt iq $ Just Varying)

assign :: Expr -> Expr -> Statement
assign l r = ExpressionStatement $ Just $ Equal l r
