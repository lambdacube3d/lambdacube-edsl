module LC_T_PrimFun where

import Data.Int
import Data.Word

import LC_G_Type
import LC_T_Sampler
import LC_T_APIType
import LC_T_DSLType

data PrimFun freq sig where

    -- Vec/Mat (de)construction
    PrimTupToV2             :: IsComponent a                                        => PrimFun freq ((a,a)     -> V2 a)
    PrimTupToV3             :: IsComponent a                                        => PrimFun freq ((a,a,a)   -> V3 a)
    PrimTupToV4             :: IsComponent a                                        => PrimFun freq ((a,a,a,a) -> V4 a)
    PrimV2ToTup             :: IsComponent a                                        => PrimFun freq (V2 a     -> (a,a))
    PrimV3ToTup             :: IsComponent a                                        => PrimFun freq (V3 a   -> (a,a,a))
    PrimV4ToTup             :: IsComponent a                                        => PrimFun freq (V4 a -> (a,a,a,a))

    -- Arithmetic Functions (componentwise)
    PrimAdd                 :: (IsNum t, IsMatVec a t)                              => PrimFun freq ((a,a)   -> a)
    PrimAddS                :: (IsNum t, IsMatVecScalar a t)                        => PrimFun freq ((a,t)   -> a)
    PrimSub                 :: (IsNum t, IsMatVec a t)                              => PrimFun freq ((a,a)   -> a)
    PrimSubS                :: (IsNum t, IsMatVecScalar a t)                        => PrimFun freq ((a,t)   -> a)
    PrimMul                 :: (IsNum t, IsMatVec a t)                              => PrimFun freq ((a,a)   -> a)
    PrimMulS                :: (IsNum t, IsMatVecScalar a t)                        => PrimFun freq ((a,t)   -> a)
    PrimDiv                 :: (IsNum t, IsVecScalar d a t)                         => PrimFun freq ((a,a)   -> a)
    PrimDivS                :: (IsNum t, IsVecScalar d a t)                         => PrimFun freq ((a,t)   -> a)
    PrimNeg                 :: (IsSigned t, IsMatVecScalar a t)                     => PrimFun freq (a       -> a)
    PrimMod                 :: (IsNum t, IsVecScalar d a t)                         => PrimFun freq ((a,a)   -> a)
    PrimModS                :: (IsNum t, IsVecScalar d a t)                         => PrimFun freq ((a,t)   -> a)

    -- Bit-wise Functions
    PrimBAnd        :: (IsIntegral t, IsVecScalar d a t)                            => PrimFun freq ((a,a)   -> a)
    PrimBAndS       :: (IsIntegral t, IsVecScalar d a t)                            => PrimFun freq ((a,t)   -> a)
    PrimBOr         :: (IsIntegral t, IsVecScalar d a t)                            => PrimFun freq ((a,a)   -> a)
    PrimBOrS        :: (IsIntegral t, IsVecScalar d a t)                            => PrimFun freq ((a,t)   -> a)
    PrimBXor        :: (IsIntegral t, IsVecScalar d a t)                            => PrimFun freq ((a,a)   -> a)
    PrimBXorS       :: (IsIntegral t, IsVecScalar d a t)                            => PrimFun freq ((a,t)   -> a)
    PrimBNot        :: (IsIntegral t, IsVecScalar d a t)                            => PrimFun freq (a       -> a)
    PrimBShiftL     :: (IsIntegral t, IsVecScalar d a t, IsVecScalar d b Word32)    => PrimFun freq ((a, b)      -> a)
    PrimBShiftLS    :: (IsIntegral t, IsVecScalar d a t)                            => PrimFun freq ((a, Word32) -> a)
    PrimBShiftR     :: (IsIntegral t, IsVecScalar d a t, IsVecScalar d b Word32)    => PrimFun freq ((a, b)      -> a)
    PrimBShiftRS    :: (IsIntegral t, IsVecScalar d a t)                            => PrimFun freq ((a, Word32) -> a)

    -- Logic Functions
    PrimAnd                 ::                                                         PrimFun freq ((Bool,Bool) -> Bool)
    PrimOr                  ::                                                         PrimFun freq ((Bool,Bool) -> Bool)
    PrimXor                 ::                                                         PrimFun freq ((Bool,Bool) -> Bool)
    PrimNot                 :: IsVecScalar d a Bool                                 => PrimFun freq (a           -> a)
    PrimAny                 :: IsVecScalar d a Bool                                 => PrimFun freq (a           -> Bool)
    PrimAll                 :: IsVecScalar d a Bool                                 => PrimFun freq (a           -> Bool)

    -- Angle and Trigonometry Functions
    PrimACos                :: IsVecScalar d a Float                                => PrimFun freq (a -> a)
    PrimACosH               :: IsVecScalar d a Float                                => PrimFun freq (a -> a)
    PrimASin                :: IsVecScalar d a Float                                => PrimFun freq (a -> a)
    PrimASinH               :: IsVecScalar d a Float                                => PrimFun freq (a -> a)
    PrimATan                :: IsVecScalar d a Float                                => PrimFun freq (a -> a)
    PrimATan2               :: IsVecScalar d a Float                                => PrimFun freq ((a,a) -> a)
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
    PrimPow                 :: IsVecScalar d a Float                                => PrimFun freq ((a,a) -> a)
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
    PrimModF                :: IsVecScalar d a Float                                => PrimFun freq (a               -> (a,a))
    PrimMin                 :: (IsNum t, IsVecScalar d a t)                         => PrimFun freq ((a,a)           -> a)
    PrimMinS                :: (IsNum t, IsVecScalar d a t)                         => PrimFun freq ((a,t)           -> a)
    PrimMax                 :: (IsNum t, IsVecScalar d a t)                         => PrimFun freq ((a,a)           -> a)
    PrimMaxS                :: (IsNum t, IsVecScalar d a t)                         => PrimFun freq ((a,t)           -> a)
    PrimClamp               :: (IsNum t, IsVecScalar d a t)                         => PrimFun freq ((a,a,a)         -> a)
    PrimClampS              :: (IsNum t, IsVecScalar d a t)                         => PrimFun freq ((a,t,t)         -> a)
    PrimMix                 :: IsVecScalar d a Float                                => PrimFun freq ((a,a,a)         -> a)
    PrimMixS                :: IsVecScalar d a Float                                => PrimFun freq ((a,a,Float)     -> a)
    PrimMixB                :: (IsVecScalar d a Float, IsVecScalar d b Bool)        => PrimFun freq ((a,a,b)         -> a)
    PrimStep                :: IsVecScalar d a Float                                => PrimFun freq ((a,a)           -> a)
    PrimStepS               :: IsVecScalar d a Float                                => PrimFun freq ((Float,a)       -> a)
    PrimSmoothStep          :: IsVecScalar d a Float                                => PrimFun freq ((a,a,a)         -> a)
    PrimSmoothStepS         :: IsVecScalar d a Float                                => PrimFun freq ((Float,Float,a) -> a)

    -- Integer/Float Conversion Functions
    PrimFloatBitsToInt      :: (IsVecScalar d fv Float, IsVecScalar d iv Int32)     => PrimFun freq (fv -> iv)
    PrimFloatBitsToUInt     :: (IsVecScalar d fv Float, IsVecScalar d uv Word32)    => PrimFun freq (fv -> uv)
    PrimIntBitsToFloat      :: (IsVecScalar d fv Float, IsVecScalar d iv Int32)     => PrimFun freq (iv -> fv)
    PrimUIntBitsToFloat     :: (IsVecScalar d fv Float, IsVecScalar d uv Word32)    => PrimFun freq (uv -> fv)

    -- Geometric Functions
    PrimLength              :: IsVecScalar d a Float                                => PrimFun freq (a       -> Float)
    PrimDistance            :: IsVecScalar d a Float                                => PrimFun freq ((a,a)   -> Float)
    PrimDot                 :: IsVecScalar d a Float                                => PrimFun freq ((a,a)   -> Float)
    PrimCross               :: IsVecScalar DIM3 a Float                             => PrimFun freq ((a,a)   -> a)
    PrimNormalize           :: IsVecScalar d a Float                                => PrimFun freq (a       -> a)
    PrimFaceForward         :: IsVecScalar d a Float                                => PrimFun freq ((a,a,a) -> a)
    PrimReflect             :: IsVecScalar d a Float                                => PrimFun freq ((a,a)   -> a)
    PrimRefract             :: IsVecScalar d a Float                                => PrimFun freq ((a,a,a) -> a)

    -- Matrix Functions
    PrimTranspose           :: (IsMat a h w, IsMat b w h)                           => PrimFun freq (a       -> b)
    PrimDeterminant         :: IsMat m s s                                          => PrimFun freq (m       -> Float)
    PrimInverse             :: IsMat m h w                                          => PrimFun freq (m       -> m)
    PrimOuterProduct        :: IsMat m h w                                          => PrimFun freq ((w,h)   -> m)
    PrimMulMatVec           :: IsMat m h w                                          => PrimFun freq ((m,w)   -> h)
    PrimMulVecMat           :: IsMat m h w                                          => PrimFun freq ((h,m)   -> w)
    PrimMulMatMat           :: (IsMat a i j, IsMat b j k, IsMat c i k)              => PrimFun freq ((a,b)   -> c)

    -- Vector and Scalar Relational Functions
    PrimLessThan            :: (IsNum t, IsVecScalar d a t, IsVecScalar d b Bool)   => PrimFun freq ((a,a) -> b)
    PrimLessThanEqual       :: (IsNum t, IsVecScalar d a t, IsVecScalar d b Bool)   => PrimFun freq ((a,a) -> b)
    PrimGreaterThan         :: (IsNum t, IsVecScalar d a t, IsVecScalar d b Bool)   => PrimFun freq ((a,a) -> b)
    PrimGreaterThanEqual    :: (IsNum t, IsVecScalar d a t, IsVecScalar d b Bool)   => PrimFun freq ((a,a) -> b)
    PrimEqualV              :: (IsNum t, IsVecScalar d a t, IsVecScalar d b Bool)   => PrimFun freq ((a,a) -> b)
    PrimEqual               :: IsMatVecScalar a t                                   => PrimFun freq ((a,a) -> Bool)
    PrimNotEqualV           :: (IsNum t, IsVecScalar d a t, IsVecScalar d b Bool)   => PrimFun freq ((a,a) -> b)
    PrimNotEqual            :: IsMatVecScalar a t                                   => PrimFun freq ((a,a) -> Bool)

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
    PrimTextureSize             :: IsTextureSize sampler lod size                           => PrimFun freq ((sampler,lod)                  -> size)
    PrimTexture                 :: IsTexture sampler coord bias                             => PrimFun freq ((sampler,coord)                -> TexelRepr sampler)
    PrimTextureB                :: IsTexture sampler coord bias                             => PrimFun F    ((sampler,coord,bias)           -> TexelRepr sampler)
    PrimTextureProj             :: IsTextureProj sampler coord bias                         => PrimFun freq ((sampler,coord)                -> TexelRepr sampler)
    PrimTextureProjB            :: IsTextureProj sampler coord bias                         => PrimFun F    ((sampler,coord,bias)           -> TexelRepr sampler)
    PrimTextureLod              :: IsTextureLod sampler coord lod                           => PrimFun freq ((sampler,coord,lod)            -> TexelRepr sampler)
    PrimTextureOffset           :: IsTextureOffset sampler coord offset bias                => PrimFun freq ((sampler,coord,offset)         -> TexelRepr sampler)
    PrimTextureOffsetB          :: IsTextureOffset sampler coord offset bias                => PrimFun F    ((sampler,coord,offset,bias)    -> TexelRepr sampler)
    PrimTexelFetch              :: IsTexelFetch sampler coord lod                           => PrimFun freq ((sampler,coord,lod)            -> TexelRepr sampler)
    PrimTexelFetchOffset        :: IsTexelFetchOffset sampler coord lod offset              => PrimFun freq ((sampler,coord,lod,offset)     -> TexelRepr sampler)
    PrimTextureProjOffset       :: IsTextureProjOffset sampler coord offset bias            => PrimFun freq ((sampler,coord,offset)         -> TexelRepr sampler)
    PrimTextureProjOffsetB      :: IsTextureProjOffset sampler coord offset bias            => PrimFun F    ((sampler,coord,offset,bias)    -> TexelRepr sampler)
    PrimTextureLodOffset        :: IsTextureLodOffset sampler coord lod offset              => PrimFun freq ((sampler,coord,lod,offset)     -> TexelRepr sampler)
    PrimTextureProjLod          :: IsTextureProjLod sampler coord lod                       => PrimFun freq ((sampler,coord,lod)            -> TexelRepr sampler)
    PrimTextureProjLodOffset    :: IsTextureProjLodOffset sampler coord lod offset          => PrimFun freq ((sampler,coord,lod,offset)     -> TexelRepr sampler)
    PrimTextureGrad             :: IsTextureGrad sampler coord dx dy                        => PrimFun freq ((sampler,coord,dx,dy)          -> TexelRepr sampler)
    PrimTextureGradOffset       :: IsTextureGradOffset sampler coord dx dy offset           => PrimFun freq ((sampler,coord,dx,dy,offset)   -> TexelRepr sampler)
    PrimTextureProjGrad         :: IsTextureProjGrad sampler coord dx dy                    => PrimFun freq ((sampler,coord,dx,dy)          -> TexelRepr sampler)
    PrimTextureProjGradOffset   :: IsTextureProjGradOffset sampler coord dx dy offset       => PrimFun freq ((sampler,coord,dx,dy,offset)   -> TexelRepr sampler)

    -- Builtin variables
    -- hint: modeled as functions with unit input to simplify AST
    -- vertex shader
    PrimVertexID        :: PrimFun V (() -> Int32)
    PrimInstanceID      :: PrimFun V (() -> Int32)
    -- geometry shader
    PrimPrimitiveIDIn   :: PrimFun G (() -> Int32)
    -- fragment shader
    PrimFragCoord       :: PrimFun F (() -> V4F)
    PrimFrontFacing     :: PrimFun F (() -> Bool)
    PrimPointCoord      :: PrimFun F (() -> V2F)
    PrimPrimitiveID     :: PrimFun F (() -> Int32)

    -- Texture Construction
    PrimNewTexture      :: PrimFun Obj ((TextureSetting dim arr layerCount t ar, Array order (Image layerCount (TexDataRepr ar t))) -> Texture dim arr t ar)

    -- Sampler Construction
    PrimNewSampler      :: PrimFun Obj ((SamplerSetting, Texture dim arr t ar) -> Sampler dim arr t ar)
