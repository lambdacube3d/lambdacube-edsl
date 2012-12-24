module LC_T_PrimFun where

import Data.Int
import Data.Word

import LC_G_LinearAlgebraTypes
import LC_T_Sampler
import LC_T_APIType

data PrimFun freq sig where

    -- Vec/Mat (de)construction
    PrimTupToV2             :: IsComponent a                                        => PrimFun freq (a:+:a:+:ZZ         -> V2 a)
    PrimTupToV3             :: IsComponent a                                        => PrimFun freq (a:+:a:+:a:+:ZZ     -> V3 a)
    PrimTupToV4             :: IsComponent a                                        => PrimFun freq (a:+:a:+:a:+:a:+:ZZ -> V4 a)
    PrimV2ToTup             :: IsComponent a                                        => PrimFun freq (V2 a         -> a:+:a:+:ZZ)
    PrimV3ToTup             :: IsComponent a                                        => PrimFun freq (V3 a     -> a:+:a:+:a:+:ZZ)
    PrimV4ToTup             :: IsComponent a                                        => PrimFun freq (V4 a -> a:+:a:+:a:+:a:+:ZZ)

    -- Arithmetic Functions (componentwise)
    PrimAdd                 :: (IsNum t, IsMatVec a t)                              => PrimFun freq (a:+:a:+:ZZ -> a)
    PrimAddS                :: (IsNum t, IsMatVecScalar a t)                        => PrimFun freq (a:+:t:+:ZZ -> a)
    PrimSub                 :: (IsNum t, IsMatVec a t)                              => PrimFun freq (a:+:a:+:ZZ -> a)
    PrimSubS                :: (IsNum t, IsMatVecScalar a t)                        => PrimFun freq (a:+:t:+:ZZ -> a)
    PrimMul                 :: (IsNum t, IsMatVec a t)                              => PrimFun freq (a:+:a:+:ZZ -> a)
    PrimMulS                :: (IsNum t, IsMatVecScalar a t)                        => PrimFun freq (a:+:t:+:ZZ -> a)
    PrimDiv                 :: (IsNum t, IsVecScalar d a t)                         => PrimFun freq (a:+:a:+:ZZ -> a)
    PrimDivS                :: (IsNum t, IsVecScalar d a t)                         => PrimFun freq (a:+:t:+:ZZ -> a)
    PrimNeg                 :: (IsSigned t, IsMatVecScalar a t)                     => PrimFun freq (a -> a)
    PrimMod                 :: (IsNum t, IsVecScalar d a t)                         => PrimFun freq (a:+:a:+:ZZ -> a)
    PrimModS                :: (IsNum t, IsVecScalar d a t)                         => PrimFun freq (a:+:t:+:ZZ -> a)

    -- Bit-wise Functions
    PrimBAnd        :: (IsIntegral t, IsVecScalar d a t)                            => PrimFun freq (a:+:a:+:ZZ -> a)
    PrimBAndS       :: (IsIntegral t, IsVecScalar d a t)                            => PrimFun freq (a:+:t:+:ZZ -> a)
    PrimBOr         :: (IsIntegral t, IsVecScalar d a t)                            => PrimFun freq (a:+:a:+:ZZ -> a)
    PrimBOrS        :: (IsIntegral t, IsVecScalar d a t)                            => PrimFun freq (a:+:t:+:ZZ -> a)
    PrimBXor        :: (IsIntegral t, IsVecScalar d a t)                            => PrimFun freq (a:+:a:+:ZZ -> a)
    PrimBXorS       :: (IsIntegral t, IsVecScalar d a t)                            => PrimFun freq (a:+:t:+:ZZ -> a)
    PrimBNot        :: (IsIntegral t, IsVecScalar d a t)                            => PrimFun freq (a -> a)
    PrimBShiftL     :: (IsIntegral t, IsVecScalar d a t, IsVecScalar d b Word32)    => PrimFun freq (a:+:b:+:ZZ         -> a)
    PrimBShiftLS    :: (IsIntegral t, IsVecScalar d a t)                            => PrimFun freq (a:+:Word32:+:ZZ    -> a)
    PrimBShiftR     :: (IsIntegral t, IsVecScalar d a t, IsVecScalar d b Word32)    => PrimFun freq (a:+:b:+:ZZ         -> a)
    PrimBShiftRS    :: (IsIntegral t, IsVecScalar d a t)                            => PrimFun freq (a:+:Word32:+:ZZ    -> a)

    -- Logic Functions
    PrimAnd                 ::                                                         PrimFun freq (Bool:+:Bool:+:ZZ   -> Bool)
    PrimOr                  ::                                                         PrimFun freq (Bool:+:Bool:+:ZZ   -> Bool)
    PrimXor                 ::                                                         PrimFun freq (Bool:+:Bool:+:ZZ   -> Bool)
    PrimNot                 :: IsVecScalar d a Bool                                 => PrimFun freq (a -> a)
    PrimAny                 :: IsVecScalar d a Bool                                 => PrimFun freq (a -> Bool)
    PrimAll                 :: IsVecScalar d a Bool                                 => PrimFun freq (a -> Bool)

    -- Angle and Trigonometry Functions
    PrimACos                :: IsVecScalar d a Float                                => PrimFun freq (a -> a)
    PrimACosH               :: IsVecScalar d a Float                                => PrimFun freq (a -> a)
    PrimASin                :: IsVecScalar d a Float                                => PrimFun freq (a -> a)
    PrimASinH               :: IsVecScalar d a Float                                => PrimFun freq (a -> a)
    PrimATan                :: IsVecScalar d a Float                                => PrimFun freq (a -> a)
    PrimATan2               :: IsVecScalar d a Float                                => PrimFun freq (a:+:a:+:ZZ -> a)
    PrimATanH               :: IsVecScalar d a Float                                => PrimFun freq (a -> a)
    PrimCos                 :: IsVecScalar d a Float                                => PrimFun freq (a -> a)
    PrimCosH                :: IsVecScalar d a Float                                => PrimFun freq (a -> a)
    PrimDegrees             :: IsVecScalar d a Float                                => PrimFun freq (a -> a)
    PrimRadians             :: IsVecScalar d a Float                                => PrimFun freq (a -> a)
    PrimSin                 :: IsVecScalar d a Float                                => PrimFun freq (a -> a)
    PrimSinH                :: IsVecScalar d a Float                                => PrimFun freq (a -> a)
    PrimTan                 :: IsVecScalar d a Float                                => PrimFun freq (a -> a)
    PrimTanH                :: IsVecScalar d a Float                                => PrimFun freq (a -> a)

    -- Exponential Functions
    PrimPow                 :: IsVecScalar d a Float                                => PrimFun freq (a:+:a:+:ZZ -> a)
    PrimExp                 :: IsVecScalar d a Float                                => PrimFun freq (a -> a)
    PrimLog                 :: IsVecScalar d a Float                                => PrimFun freq (a -> a)
    PrimExp2                :: IsVecScalar d a Float                                => PrimFun freq (a -> a)
    PrimLog2                :: IsVecScalar d a Float                                => PrimFun freq (a -> a)
    PrimSqrt                :: IsVecScalar d a Float                                => PrimFun freq (a -> a)
    PrimInvSqrt             :: IsVecScalar d a Float                                => PrimFun freq (a -> a)

    -- Common Functions
    PrimIsNan               :: (IsVecScalar d a Float, IsVecScalar d b Bool)        => PrimFun freq (a -> b)
    PrimIsInf               :: (IsVecScalar d a Float, IsVecScalar d b Bool)        => PrimFun freq (a -> b)
    PrimAbs                 :: (IsSigned t, IsVecScalar d a t)                      => PrimFun freq (a -> a)
    PrimSign                :: (IsSigned t, IsVecScalar d a t)                      => PrimFun freq (a -> a)
    PrimFloor               :: IsVecScalar d a Float                                => PrimFun freq (a -> a)
    PrimTrunc               :: IsVecScalar d a Float                                => PrimFun freq (a -> a)
    PrimRound               :: IsVecScalar d a Float                                => PrimFun freq (a -> a)
    PrimRoundEven           :: IsVecScalar d a Float                                => PrimFun freq (a -> a)
    PrimCeil                :: IsVecScalar d a Float                                => PrimFun freq (a -> a)
    PrimFract               :: IsVecScalar d a Float                                => PrimFun freq (a -> a)
    PrimModF                :: IsVecScalar d a Float                                => PrimFun freq (a -> a:+:a:+:ZZ)
    PrimMin                 :: (IsNum t, IsVecScalar d a t)                         => PrimFun freq (a:+:a:+:ZZ             -> a)
    PrimMinS                :: (IsNum t, IsVecScalar d a t)                         => PrimFun freq (a:+:t:+:ZZ             -> a)
    PrimMax                 :: (IsNum t, IsVecScalar d a t)                         => PrimFun freq (a:+:a:+:ZZ             -> a)
    PrimMaxS                :: (IsNum t, IsVecScalar d a t)                         => PrimFun freq (a:+:t:+:ZZ             -> a)
    PrimClamp               :: (IsNum t, IsVecScalar d a t)                         => PrimFun freq (a:+:a:+:a:+:ZZ         -> a)
    PrimClampS              :: (IsNum t, IsVecScalar d a t)                         => PrimFun freq (a:+:t:+:t:+:ZZ         -> a)
    PrimMix                 :: IsVecScalar d a Float                                => PrimFun freq (a:+:a:+:a:+:ZZ         -> a)
    PrimMixS                :: IsVecScalar d a Float                                => PrimFun freq (a:+:a:+:Float:+:ZZ     -> a)
    PrimMixB                :: (IsVecScalar d a Float, IsVecScalar d b Bool)        => PrimFun freq (a:+:a:+:b:+:ZZ         -> a)
    PrimStep                :: IsVecScalar d a Float                                => PrimFun freq (a:+:a:+:ZZ             -> a)
    PrimStepS               :: IsVecScalar d a Float                                => PrimFun freq (Float:+:a:+:ZZ         -> a)
    PrimSmoothStep          :: IsVecScalar d a Float                                => PrimFun freq (a:+:a:+:a:+:ZZ         -> a)
    PrimSmoothStepS         :: IsVecScalar d a Float                                => PrimFun freq (Float:+:Float:+:a:+:ZZ -> a)

    -- Integer/Float Conversion Functions
    PrimFloatBitsToInt      :: (IsVecScalar d fv Float, IsVecScalar d iv Int32)     => PrimFun freq (fv -> iv)
    PrimFloatBitsToUInt     :: (IsVecScalar d fv Float, IsVecScalar d uv Word32)    => PrimFun freq (fv -> uv)
    PrimIntBitsToFloat      :: (IsVecScalar d fv Float, IsVecScalar d iv Int32)     => PrimFun freq (iv -> fv)
    PrimUIntBitsToFloat     :: (IsVecScalar d fv Float, IsVecScalar d uv Word32)    => PrimFun freq (uv -> fv)

    -- Geometric Functions
    PrimLength              :: IsVecScalar d a Float                                => PrimFun freq (a -> Float)
    PrimDistance            :: IsVecScalar d a Float                                => PrimFun freq (a:+:a:+:ZZ     -> Float)
    PrimDot                 :: IsVecScalar d a Float                                => PrimFun freq (a:+:a:+:ZZ     -> Float)
    PrimCross               :: IsVecScalar DIM3 a Float                             => PrimFun freq (a:+:a:+:ZZ     -> a)
    PrimNormalize           :: IsVecScalar d a Float                                => PrimFun freq (a -> a)
    PrimFaceForward         :: IsVecScalar d a Float                                => PrimFun freq (a:+:a:+:a:+:ZZ -> a)
    PrimReflect             :: IsVecScalar d a Float                                => PrimFun freq (a:+:a:+:ZZ     -> a)
    PrimRefract             :: IsVecScalar d a Float                                => PrimFun freq (a:+:a:+:a:+:ZZ -> a)

    -- Matrix Functions
    PrimTranspose           :: (IsMat a h w, IsMat b w h)                           => PrimFun freq (a -> b)
    PrimDeterminant         :: IsMat m s s                                          => PrimFun freq (m -> Float)
    PrimInverse             :: IsMat m h w                                          => PrimFun freq (m -> m)
    PrimOuterProduct        :: IsMat m h w                                          => PrimFun freq (w:+:h:+:ZZ -> m)
    PrimMulMatVec           :: IsMat m h w                                          => PrimFun freq (m:+:w:+:ZZ -> h)
    PrimMulVecMat           :: IsMat m h w                                          => PrimFun freq (h:+:m:+:ZZ -> w)
    PrimMulMatMat           :: (IsMat a i j, IsMat b j k, IsMat c i k)              => PrimFun freq (a:+:b:+:ZZ -> c)

    -- Vector and Scalar Relational Functions
    PrimLessThan            :: (IsNum t, IsVecScalar d a t, IsVecScalar d b Bool)   => PrimFun freq (a:+:a:+:ZZ -> b)
    PrimLessThanEqual       :: (IsNum t, IsVecScalar d a t, IsVecScalar d b Bool)   => PrimFun freq (a:+:a:+:ZZ -> b)
    PrimGreaterThan         :: (IsNum t, IsVecScalar d a t, IsVecScalar d b Bool)   => PrimFun freq (a:+:a:+:ZZ -> b)
    PrimGreaterThanEqual    :: (IsNum t, IsVecScalar d a t, IsVecScalar d b Bool)   => PrimFun freq (a:+:a:+:ZZ -> b)
    PrimEqualV              :: (IsNum t, IsVecScalar d a t, IsVecScalar d b Bool)   => PrimFun freq (a:+:a:+:ZZ -> b)
    PrimEqual               :: IsMatVecScalar a t                                   => PrimFun freq (a:+:a:+:ZZ -> Bool)
    PrimNotEqualV           :: (IsNum t, IsVecScalar d a t, IsVecScalar d b Bool)   => PrimFun freq (a:+:a:+:ZZ -> b)
    PrimNotEqual            :: IsMatVecScalar a t                                   => PrimFun freq (a:+:a:+:ZZ -> Bool)

    -- Fragment Processing Functions
    PrimDFdx                :: IsVecScalar d a Float                                => PrimFun F (a -> a)
    PrimDFdy                :: IsVecScalar d a Float                                => PrimFun F (a -> a)
    PrimFWidth              :: IsVecScalar d a Float                                => PrimFun F (a -> a)

    -- Noise Functions
    PrimNoise1              :: IsVecScalar d a Float                                => PrimFun freq (a -> Float)
    PrimNoise2              :: (IsVecScalar d a Float, IsVecScalar DIM2 b Float)    => PrimFun freq (a -> b)
    PrimNoise3              :: (IsVecScalar d a Float, IsVecScalar DIM3 b Float)    => PrimFun freq (a -> b)
    PrimNoise4              :: (IsVecScalar d a Float, IsVecScalar DIM4 b Float)    => PrimFun freq (a -> b)

    -- Texture Lookup Functions
    PrimTextureSize             :: IsTextureSize sampler lod size                           => PrimFun freq (sampler:+:lod:+:ZZ                         -> size)
    PrimTexture                 :: IsTexture sampler coord bias                             => PrimFun freq (sampler:+:coord:+:ZZ                       -> TexelRepr sampler)
    PrimTextureB                :: IsTexture sampler coord bias                             => PrimFun F    (sampler:+:coord:+:bias:+:ZZ                -> TexelRepr sampler)
    PrimTextureProj             :: IsTextureProj sampler coord bias                         => PrimFun freq (sampler:+:coord:+:ZZ                       -> TexelRepr sampler)
    PrimTextureProjB            :: IsTextureProj sampler coord bias                         => PrimFun F    (sampler:+:coord:+:bias:+:ZZ                -> TexelRepr sampler)
    PrimTextureLod              :: IsTextureLod sampler coord lod                           => PrimFun freq (sampler:+:coord:+:lod:+:ZZ                 -> TexelRepr sampler)
    PrimTextureOffset           :: IsTextureOffset sampler coord offset bias                => PrimFun freq (sampler:+:coord:+:offset:+:ZZ              -> TexelRepr sampler)
    PrimTextureOffsetB          :: IsTextureOffset sampler coord offset bias                => PrimFun F    (sampler:+:coord:+:offset:+:bias:+:ZZ       -> TexelRepr sampler)
    PrimTexelFetch              :: IsTexelFetch sampler coord lod                           => PrimFun freq (sampler:+:coord:+:lod:+:ZZ                 -> TexelRepr sampler)
    PrimTexelFetchOffset        :: IsTexelFetchOffset sampler coord lod offset              => PrimFun freq (sampler:+:coord:+:lod:+:offset:+:ZZ        -> TexelRepr sampler)
    PrimTextureProjOffset       :: IsTextureProjOffset sampler coord offset bias            => PrimFun freq (sampler:+:coord:+:offset:+:ZZ              -> TexelRepr sampler)
    PrimTextureProjOffsetB      :: IsTextureProjOffset sampler coord offset bias            => PrimFun F    (sampler:+:coord:+:offset:+:bias:+:ZZ       -> TexelRepr sampler)
    PrimTextureLodOffset        :: IsTextureLodOffset sampler coord lod offset              => PrimFun freq (sampler:+:coord:+:lod:+:offset:+:ZZ        -> TexelRepr sampler)
    PrimTextureProjLod          :: IsTextureProjLod sampler coord lod                       => PrimFun freq (sampler:+:coord:+:lod:+:ZZ                 -> TexelRepr sampler)
    PrimTextureProjLodOffset    :: IsTextureProjLodOffset sampler coord lod offset          => PrimFun freq (sampler:+:coord:+:lod:+:offset:+:ZZ        -> TexelRepr sampler)
    PrimTextureGrad             :: IsTextureGrad sampler coord dx dy                        => PrimFun freq (sampler:+:coord:+:dx:+:dy:+:ZZ             -> TexelRepr sampler)
    PrimTextureGradOffset       :: IsTextureGradOffset sampler coord dx dy offset           => PrimFun freq (sampler:+:coord:+:dx:+:dy:+:offset:+:ZZ    -> TexelRepr sampler)
    PrimTextureProjGrad         :: IsTextureProjGrad sampler coord dx dy                    => PrimFun freq (sampler:+:coord:+:dx:+:dy:+:ZZ             -> TexelRepr sampler)
    PrimTextureProjGradOffset   :: IsTextureProjGradOffset sampler coord dx dy offset       => PrimFun freq (sampler:+:coord:+:dx:+:dy:+:offset:+:ZZ    -> TexelRepr sampler)

    -- Builtin variables
    -- hint: modeled as functions with unit input to simplify AST
    -- vertex shader
    PrimVertexID        :: PrimFun V (ZZ -> Int32)
    PrimInstanceID      :: PrimFun V (ZZ -> Int32)
    -- geometry shader
    PrimPrimitiveIDIn   :: PrimFun G (ZZ -> Int32)
    -- fragment shader
    PrimFragCoord       :: PrimFun F (ZZ -> V4F)
    PrimFrontFacing     :: PrimFun F (ZZ -> Bool)
    PrimPointCoord      :: PrimFun F (ZZ -> V2F)
    PrimPrimitiveID     :: PrimFun F (ZZ -> Int32)

    -- Texture Construction
    PrimNewTexture      :: PrimFun Obj (TextureSetting dim arr layerCount t ar :+: Array order (Image layerCount (TexDataRepr ar t)) :+: ZZ -> Texture dim arr t ar)

    -- Sampler Construction
    PrimNewSampler      :: PrimFun Obj (SamplerSetting :+: Texture dim arr t ar :+: ZZ -> Sampler dim arr t ar)
