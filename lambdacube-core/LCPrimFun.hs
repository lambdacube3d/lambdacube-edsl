module LCPrimFun where

import Data.Int
import Data.Word

import LCType
import LCSampler

data PrimFun stage sig where

    -- Vec/Mat (de)construction
    PrimTupToV2             :: IsComponent a                            => PrimFun stage ((a,a)     -> V2 a)
    PrimTupToV3             :: IsComponent a                            => PrimFun stage ((a,a,a)   -> V3 a)
    PrimTupToV4             :: IsComponent a                            => PrimFun stage ((a,a,a,a) -> V4 a)
    PrimV2ToTup             :: IsComponent a                            => PrimFun stage (V2 a     -> (a,a))
    PrimV3ToTup             :: IsComponent a                            => PrimFun stage (V3 a   -> (a,a,a))
    PrimV4ToTup             :: IsComponent a                            => PrimFun stage (V4 a -> (a,a,a,a))

    -- Arithmetic Functions (componentwise)
    PrimAdd                 :: (IsNum t, IsMatVec a t)                              => PrimFun stage ((a,a)   -> a)
    PrimAddS                :: (IsNum t, IsMatVecScalar a t)                        => PrimFun stage ((a,t)   -> a)
    PrimSub                 :: (IsNum t, IsMatVec a t)                              => PrimFun stage ((a,a)   -> a)
    PrimSubS                :: (IsNum t, IsMatVecScalar a t)                        => PrimFun stage ((a,t)   -> a)
    PrimMul                 :: (IsNum t, IsMatVec a t)                              => PrimFun stage ((a,a)   -> a)
    PrimMulS                :: (IsNum t, IsMatVecScalar a t)                        => PrimFun stage ((a,t)   -> a)
    PrimDiv                 :: (IsNum t, IsVecScalar d a t)                         => PrimFun stage ((a,a)   -> a)
    PrimDivS                :: (IsNum t, IsVecScalar d a t)                         => PrimFun stage ((a,t)   -> a)
    PrimNeg                 :: (IsSigned t, IsMatVecScalar a t)                     => PrimFun stage (a       -> a)
    PrimMod                 :: (IsNum t, IsVecScalar d a t)                         => PrimFun stage ((a,a)   -> a)
    PrimModS                :: (IsNum t, IsVecScalar d a t)                         => PrimFun stage ((a,t)   -> a)

    -- Bit-wise Functions
    PrimBAnd        :: (IsIntegral t, IsVecScalar d a t)                            => PrimFun stage ((a,a)   -> a)
    PrimBAndS       :: (IsIntegral t, IsVecScalar d a t)                            => PrimFun stage ((a,t)   -> a)
    PrimBOr         :: (IsIntegral t, IsVecScalar d a t)                            => PrimFun stage ((a,a)   -> a)
    PrimBOrS        :: (IsIntegral t, IsVecScalar d a t)                            => PrimFun stage ((a,t)   -> a)
    PrimBXor        :: (IsIntegral t, IsVecScalar d a t)                            => PrimFun stage ((a,a)   -> a)
    PrimBXorS       :: (IsIntegral t, IsVecScalar d a t)                            => PrimFun stage ((a,t)   -> a)
    PrimBNot        :: (IsIntegral t, IsVecScalar d a t)                            => PrimFun stage (a       -> a)
    PrimBShiftL     :: (IsIntegral t, IsVecScalar d a t, IsVecScalar d b Word32)    => PrimFun stage ((a, b)      -> a)
    PrimBShiftLS    :: (IsIntegral t, IsVecScalar d a t)                            => PrimFun stage ((a, Word32) -> a)
    PrimBShiftR     :: (IsIntegral t, IsVecScalar d a t, IsVecScalar d b Word32)    => PrimFun stage ((a, b)      -> a)
    PrimBShiftRS    :: (IsIntegral t, IsVecScalar d a t)                            => PrimFun stage ((a, Word32) -> a)

    -- Logic Functions
    PrimAnd                 ::                                             PrimFun stage ((Bool,Bool) -> Bool)
    PrimOr                  ::                                             PrimFun stage ((Bool,Bool) -> Bool)
    PrimXor                 ::                                             PrimFun stage ((Bool,Bool) -> Bool)
    PrimNot                 :: IsVecScalar d a Bool                           => PrimFun stage (a           -> a)
    PrimAny                 :: IsVecScalar d a Bool                           => PrimFun stage (a           -> Bool)
    PrimAll                 :: IsVecScalar d a Bool                           => PrimFun stage (a           -> Bool)

    -- Angle and Trigonometry Functions
    PrimACos                :: IsVecScalar d a Float                          => PrimFun stage (a -> a)
    PrimACosH               :: IsVecScalar d a Float                          => PrimFun stage (a -> a)
    PrimASin                :: IsVecScalar d a Float                          => PrimFun stage (a -> a)
    PrimASinH               :: IsVecScalar d a Float                          => PrimFun stage (a -> a)
    PrimATan                :: IsVecScalar d a Float                          => PrimFun stage (a -> a)
    PrimATan2               :: IsVecScalar d a Float                          => PrimFun stage ((a,a) -> a)
    PrimATanH               :: IsVecScalar d a Float                          => PrimFun stage (a -> a)
    PrimCos                 :: IsVecScalar d a Float                          => PrimFun stage (a -> a)
    PrimCosH                :: IsVecScalar d a Float                          => PrimFun stage (a -> a)
    PrimDegrees             :: IsVecScalar d a Float                          => PrimFun stage (a -> a)
    PrimRadians             :: IsVecScalar d a Float                          => PrimFun stage (a -> a)
    PrimSin                 :: IsVecScalar d a Float                          => PrimFun stage (a -> a)
    PrimSinH                :: IsVecScalar d a Float                          => PrimFun stage (a -> a)
    PrimTan                 :: IsVecScalar d a Float                          => PrimFun stage (a -> a)
    PrimTanH                :: IsVecScalar d a Float                          => PrimFun stage (a -> a)

    -- Exponential Functions
    PrimPow                 :: IsVecScalar d a Float                          => PrimFun stage ((a,a) -> a)
    PrimExp                 :: IsVecScalar d a Float                          => PrimFun stage (a -> a)
    PrimLog                 :: IsVecScalar d a Float                          => PrimFun stage (a -> a)
    PrimExp2                :: IsVecScalar d a Float                          => PrimFun stage (a -> a)
    PrimLog2                :: IsVecScalar d a Float                          => PrimFun stage (a -> a)
    PrimSqrt                :: IsVecScalar d a Float                          => PrimFun stage (a -> a)
    PrimInvSqrt             :: IsVecScalar d a Float                          => PrimFun stage (a -> a)

    -- Common Functions
    PrimIsNan               :: (IsVecScalar d a Float, IsVecScalar d b Bool)        => PrimFun stage (a -> b)
    PrimIsInf               :: (IsVecScalar d a Float, IsVecScalar d b Bool)        => PrimFun stage (a -> b)
    PrimAbs                 :: (IsSigned t, IsVecScalar d a t)                => PrimFun stage (a -> a)
    PrimSign                :: (IsSigned t, IsVecScalar d a t)                => PrimFun stage (a -> a)
    PrimFloor               :: IsVecScalar d a Float                          => PrimFun stage (a -> a)
    PrimTrunc               :: IsVecScalar d a Float                          => PrimFun stage (a -> a)
    PrimRound               :: IsVecScalar d a Float                          => PrimFun stage (a -> a)
    PrimRoundEven           :: IsVecScalar d a Float                          => PrimFun stage (a -> a)
    PrimCeil                :: IsVecScalar d a Float                          => PrimFun stage (a -> a)
    PrimFract               :: IsVecScalar d a Float                          => PrimFun stage (a -> a)
    PrimModF                :: IsVecScalar d a Float                          => PrimFun stage (a               -> (a,a))
    PrimMin                 :: (IsNum t, IsVecScalar d a t)                   => PrimFun stage ((a,a)           -> a)
    PrimMinS                :: (IsNum t, IsVecScalar d a t)                   => PrimFun stage ((a,t)           -> a)
    PrimMax                 :: (IsNum t, IsVecScalar d a t)                   => PrimFun stage ((a,a)           -> a)
    PrimMaxS                :: (IsNum t, IsVecScalar d a t)                   => PrimFun stage ((a,t)           -> a)
    PrimClamp               :: (IsNum t, IsVecScalar d a t)                   => PrimFun stage ((a,a,a)         -> a)
    PrimClampS              :: (IsNum t, IsVecScalar d a t)                   => PrimFun stage ((a,t,t)         -> a)
    PrimMix                 :: IsVecScalar d a Float                          => PrimFun stage ((a,a,a)         -> a)
    PrimMixS                :: IsVecScalar d a Float                          => PrimFun stage ((a,a,Float)     -> a)
    PrimMixB                :: (IsVecScalar d a Float, IsVecScalar d b Bool)        => PrimFun stage ((a,a,b)         -> a)
    PrimStep                :: IsVecScalar d a Float                          => PrimFun stage ((a,a)           -> a)
    PrimStepS               :: IsVecScalar d a Float                          => PrimFun stage ((Float,a)       -> a)
    PrimSmoothStep          :: IsVecScalar d a Float                          => PrimFun stage ((a,a,a)         -> a)
    PrimSmoothStepS         :: IsVecScalar d a Float                          => PrimFun stage ((Float,Float,a) -> a)

    -- Integer/Float Conversion Functions
    PrimFloatBitsToInt      :: (IsVecScalar d fv Float, IsVecScalar d iv Int32)     => PrimFun stage (fv -> iv)
    PrimFloatBitsToUInt     :: (IsVecScalar d fv Float, IsVecScalar d uv Word32)    => PrimFun stage (fv -> uv)
    PrimIntBitsToFloat      :: (IsVecScalar d fv Float, IsVecScalar d iv Int32)     => PrimFun stage (iv -> fv)
    PrimUIntBitsToFloat     :: (IsVecScalar d fv Float, IsVecScalar d uv Word32)    => PrimFun stage (uv -> fv)

    -- Geometric Functions
    PrimLength              :: IsVecScalar d a Float                          => PrimFun stage (a       -> Float)
    PrimDistance            :: IsVecScalar d a Float                          => PrimFun stage ((a,a)   -> Float)
    PrimDot                 :: IsVecScalar d a Float                          => PrimFun stage ((a,a)   -> Float)
    PrimCross               :: IsVecScalar DIM3 a Float                       => PrimFun stage ((a,a)   -> a)
    PrimNormalize           :: IsVecScalar d a Float                          => PrimFun stage (a       -> a)
    PrimFaceForward         :: IsVecScalar d a Float                          => PrimFun stage ((a,a,a) -> a)
    PrimReflect             :: IsVecScalar d a Float                          => PrimFun stage ((a,a)   -> a)
    PrimRefract             :: IsVecScalar d a Float                          => PrimFun stage ((a,a,a) -> a)

    -- Matrix Functions
    PrimTranspose           :: (IsMat a h w, IsMat b w h)               => PrimFun stage (a       -> b)
    PrimDeterminant         :: IsMat m s s                              => PrimFun stage (m       -> Float)
    PrimInverse             :: IsMat m h w                              => PrimFun stage (m       -> m)
    PrimOuterProduct        :: IsMat m h w                              => PrimFun stage ((w,h)   -> m)
    PrimMulMatVec           :: IsMat m h w                              => PrimFun stage ((m,w)   -> h)
    PrimMulVecMat           :: IsMat m h w                              => PrimFun stage ((h,m)   -> w)
    PrimMulMatMat           :: (IsMat a i j, IsMat b j k, IsMat c i k)  => PrimFun stage ((a,b)   -> c)

    -- Vector and Scalar Relational Functions
    PrimLessThan            :: (IsNum t, IsVecScalar d a t, IsVecScalar d b Bool)   => PrimFun stage ((a,a) -> b)
    PrimLessThanEqual       :: (IsNum t, IsVecScalar d a t, IsVecScalar d b Bool)   => PrimFun stage ((a,a) -> b)
    PrimGreaterThan         :: (IsNum t, IsVecScalar d a t, IsVecScalar d b Bool)   => PrimFun stage ((a,a) -> b)
    PrimGreaterThanEqual    :: (IsNum t, IsVecScalar d a t, IsVecScalar d b Bool)   => PrimFun stage ((a,a) -> b)
    PrimEqualV              :: (IsNum t, IsVecScalar d a t, IsVecScalar d b Bool)   => PrimFun stage ((a,a) -> b)
    PrimEqual               :: IsMatVecScalar a t                             => PrimFun stage ((a,a) -> Bool)
    PrimNotEqualV           :: (IsNum t, IsVecScalar d a t, IsVecScalar d b Bool)   => PrimFun stage ((a,a) -> b)
    PrimNotEqual            :: IsMatVecScalar a t                             => PrimFun stage ((a,a) -> Bool)

    -- Fragment Processing Functions
    PrimDFdx                :: IsVecScalar d a Float                          => PrimFun stage (a -> a)
    PrimDFdy                :: IsVecScalar d a Float                          => PrimFun stage (a -> a)
    PrimFWidth              :: IsVecScalar d a Float                          => PrimFun stage (a -> a)

    -- Noise Functions
    PrimNoise1              :: IsVecScalar d a Float                          => PrimFun stage (a -> Float)
    PrimNoise2              :: (IsVecScalar d a Float, IsVecScalar DIM2 b Float)    => PrimFun stage (a -> b)
    PrimNoise3              :: (IsVecScalar d a Float, IsVecScalar DIM3 b Float)    => PrimFun stage (a -> b)
    PrimNoise4              :: (IsVecScalar d a Float, IsVecScalar DIM4 b Float)    => PrimFun stage (a -> b)

    -- Texture Lookup Functions
    PrimTextureSize             :: IsTextureSize sampler lod size                           => PrimFun stage ((sampler,lod)                       -> size)
    PrimTexture                 :: IsTexture sampler coord bias                             => PrimFun stage ((sampler,coord,bias)                -> TexelRepr sampler)
    PrimTextureProj             :: IsTextureProj sampler coord bias                         => PrimFun stage ((sampler,coord,bias)                -> TexelRepr sampler)
    PrimTextureLod              :: IsTextureLod sampler coord lod                           => PrimFun stage ((sampler,coord,lod)                 -> TexelRepr sampler)
    PrimTextureOffset           :: IsTextureOffset sampler coord offset bias                => PrimFun stage ((sampler,coord,offset,bias)         -> TexelRepr sampler)
    PrimTexelFetch              :: IsTexelFetch sampler coord lod                           => PrimFun stage ((sampler,coord,lod)                 -> TexelRepr sampler)
    PrimTexelFetchOffset        :: IsTexelFetchOffset sampler coord lod offset              => PrimFun stage ((sampler,coord,lod,offset)          -> TexelRepr sampler)
    PrimTextureProjOffset       :: IsTextureProjOffset sampler coord offset bias            => PrimFun stage ((sampler,coord,offset,bias)         -> TexelRepr sampler)
    PrimTextureLodOffset        :: IsTextureLodOffset sampler coord lod offset              => PrimFun stage ((sampler,coord,lod,offset)          -> TexelRepr sampler)
    PrimTextureProjLod          :: IsTextureProjLod sampler coord lod                       => PrimFun stage ((sampler,coord,lod)                 -> TexelRepr sampler)
    PrimTextureProjLodOffset    :: IsTextureProjLodOffset sampler coord lod offset          => PrimFun stage ((sampler,coord,lod,offset)          -> TexelRepr sampler)
    PrimTextureGrad             :: IsTextureGrad sampler coord dx dy                        => PrimFun stage ((sampler,coord,dx,dy)               -> TexelRepr sampler)
    PrimTextureGradOffset       :: IsTextureGradOffset sampler coord dx dy offset           => PrimFun stage ((sampler,coord,dx,dy,offset)        -> TexelRepr sampler)
    PrimTextureProjGrad         :: IsTextureProjGrad sampler coord dx dy                    => PrimFun stage ((sampler,coord,dx,dy)               -> TexelRepr sampler)
    PrimTextureProjGradOffset   :: IsTextureProjGradOffset sampler coord dx dy offset       => PrimFun stage ((sampler,coord,dx,dy,offset)        -> TexelRepr sampler)

showPrimFun :: PrimFun stage t -> String
showPrimFun PrimTupToV2                 = "PrimTupToV2"
showPrimFun PrimTupToV3                 = "PrimTupToV3"
showPrimFun PrimTupToV4                 = "PrimTupToV4"
showPrimFun PrimV2ToTup                 = "PrimV2ToTup"
showPrimFun PrimV3ToTup                 = "PrimV3ToTup"
showPrimFun PrimV4ToTup                 = "PrimV4ToTup"
showPrimFun PrimAdd                     = "PrimAdd"
showPrimFun PrimAddS                    = "PrimAddS"
showPrimFun PrimSub                     = "PrimSub"
showPrimFun PrimSubS                    = "PrimSubS"
showPrimFun PrimMul                     = "PrimMul"
showPrimFun PrimMulS                    = "PrimMulS"
showPrimFun PrimDiv                     = "PrimDiv"
showPrimFun PrimDivS                    = "PrimDivS"
showPrimFun PrimNeg                     = "PrimNeg"
showPrimFun PrimMod                     = "PrimMod"
showPrimFun PrimModS                    = "PrimModS"
showPrimFun PrimBAnd                    = "PrimBAnd"
showPrimFun PrimBAndS                   = "PrimBAndS"
showPrimFun PrimBOr                     = "PrimBOr"
showPrimFun PrimBOrS                    = "PrimBOrS"
showPrimFun PrimBXor                    = "PrimBXor"
showPrimFun PrimBXorS                   = "PrimBXorS"
showPrimFun PrimBNot                    = "PrimBNot"
showPrimFun PrimBShiftL                 = "PrimBShiftL"
showPrimFun PrimBShiftLS                = "PrimBShiftLS"
showPrimFun PrimBShiftR                 = "PrimBShiftR"
showPrimFun PrimBShiftRS                = "PrimBShiftRS"
showPrimFun PrimAnd                     = "PrimAnd"
showPrimFun PrimOr                      = "PrimOr"
showPrimFun PrimXor                     = "PrimXor"
showPrimFun PrimNot                     = "PrimNot"
showPrimFun PrimAny                     = "PrimAny"
showPrimFun PrimAll                     = "PrimAll"
showPrimFun PrimACos                    = "PrimACos"
showPrimFun PrimACosH                   = "PrimACosH"
showPrimFun PrimASin                    = "PrimASin"
showPrimFun PrimASinH                   = "PrimASinH"
showPrimFun PrimATan                    = "PrimATan"
showPrimFun PrimATan2                   = "PrimATan2"
showPrimFun PrimATanH                   = "PrimATanH"
showPrimFun PrimCos                     = "PrimCos"
showPrimFun PrimCosH                    = "PrimCosH"
showPrimFun PrimDegrees                 = "PrimDegrees"
showPrimFun PrimRadians                 = "PrimRadians"
showPrimFun PrimSin                     = "PrimSin"
showPrimFun PrimSinH                    = "PrimSinH"
showPrimFun PrimTan                     = "PrimTan"
showPrimFun PrimTanH                    = "PrimTanH"
showPrimFun PrimPow                     = "PrimPow"
showPrimFun PrimExp                     = "PrimExp"
showPrimFun PrimLog                     = "PrimLog"
showPrimFun PrimExp2                    = "PrimExp2"
showPrimFun PrimLog2                    = "PrimLog2"
showPrimFun PrimSqrt                    = "PrimSqrt"
showPrimFun PrimInvSqrt                 = "PrimInvSqrt"
showPrimFun PrimIsNan                   = "PrimIsNan"
showPrimFun PrimIsInf                   = "PrimIsInf"
showPrimFun PrimAbs                     = "PrimAbs"
showPrimFun PrimSign                    = "PrimSign"
showPrimFun PrimFloor                   = "PrimFloor"
showPrimFun PrimTrunc                   = "PrimTrunc"
showPrimFun PrimRound                   = "PrimRound"
showPrimFun PrimRoundEven               = "PrimRoundEven"
showPrimFun PrimCeil                    = "PrimCeil"
showPrimFun PrimFract                   = "PrimFract"
showPrimFun PrimModF                    = "PrimModF"
showPrimFun PrimMin                     = "PrimMin"
showPrimFun PrimMinS                    = "PrimMinS"
showPrimFun PrimMax                     = "PrimMax"
showPrimFun PrimMaxS                    = "PrimMaxS"
showPrimFun PrimClamp                   = "PrimClamp"
showPrimFun PrimClampS                  = "PrimClampS"
showPrimFun PrimMix                     = "PrimMix"
showPrimFun PrimMixS                    = "PrimMixS"
showPrimFun PrimMixB                    = "PrimMixB"
showPrimFun PrimStep                    = "PrimStep"
showPrimFun PrimStepS                   = "PrimStepS"
showPrimFun PrimSmoothStep              = "PrimSmoothStep"
showPrimFun PrimSmoothStepS             = "PrimSmoothStepS"
showPrimFun PrimFloatBitsToInt          = "PrimFloatBitsToInt"
showPrimFun PrimFloatBitsToUInt         = "PrimFloatBitsToUInt"
showPrimFun PrimIntBitsToFloat          = "PrimIntBitsToFloat"
showPrimFun PrimUIntBitsToFloat         = "PrimUIntBitsToFloat"
showPrimFun PrimLength                  = "PrimLength"
showPrimFun PrimDistance                = "PrimDistance"
showPrimFun PrimDot                     = "PrimDot"
showPrimFun PrimCross                   = "PrimCross"
showPrimFun PrimNormalize               = "PrimNormalize"
showPrimFun PrimFaceForward             = "PrimFaceForward"
showPrimFun PrimReflect                 = "PrimReflect"
showPrimFun PrimRefract                 = "PrimRefract"
showPrimFun PrimTranspose               = "PrimTranspose"
showPrimFun PrimDeterminant             = "PrimDeterminant"
showPrimFun PrimInverse                 = "PrimInverse"
showPrimFun PrimOuterProduct            = "PrimOuterProduct"
showPrimFun PrimMulMatVec               = "PrimMulMatVec"
showPrimFun PrimMulVecMat               = "PrimMulVecMat"
showPrimFun PrimLessThan                = "PrimLessThan"
showPrimFun PrimLessThanEqual           = "PrimLessThanEqual"
showPrimFun PrimGreaterThan             = "PrimGreaterThan"
showPrimFun PrimGreaterThanEqual        = "PrimGreaterThanEqual"
showPrimFun PrimEqualV                  = "PrimEqualV"
showPrimFun PrimEqual                   = "PrimEqual"
showPrimFun PrimNotEqualV               = "PrimNotEqualV"
showPrimFun PrimNotEqual                = "PrimNotEqual"
showPrimFun PrimDFdx                    = "PrimDFdx"
showPrimFun PrimDFdy                    = "PrimDFdy"
showPrimFun PrimFWidth                  = "PrimFWidth"
showPrimFun PrimNoise1                  = "PrimNoise1"
showPrimFun PrimNoise2                  = "PrimNoise2"
showPrimFun PrimNoise3                  = "PrimNoise3"
showPrimFun PrimNoise4                  = "PrimNoise4"
showPrimFun PrimTextureSize             = "PrimTextureSize"
showPrimFun PrimTexture                 = "PrimTexture"
showPrimFun PrimTextureProj             = "PrimTextureProj"
showPrimFun PrimTextureLod              = "PrimTextureLod"
showPrimFun PrimTextureOffset           = "PrimTextureOffset"
showPrimFun PrimTexelFetch              = "PrimTexelFetch"
showPrimFun PrimTexelFetchOffset        = "PrimTexelFetchOffset"
showPrimFun PrimTextureProjOffset       = "PrimTextureProjOffset"
showPrimFun PrimTextureLodOffset        = "PrimTextureLodOffset"
showPrimFun PrimTextureProjLod          = "PrimTextureProjLod"
showPrimFun PrimTextureProjLodOffset    = "PrimTextureProjLodOffset"
showPrimFun PrimTextureGrad             = "PrimTextureGrad"
showPrimFun PrimTextureGradOffset       = "PrimTextureGradOffset"
showPrimFun PrimTextureProjGrad         = "PrimTextureProjGrad"
showPrimFun PrimTextureProjGradOffset   = "PrimTextureProjGradOffset"

instance Show (PrimFun stage t) where
    show = showPrimFun
