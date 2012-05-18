module LCPrimFunTypeclass where

import Data.Int
import Data.Word

import LCType
import LCSampler

class PrimFun primFun where

    -- Vec/Mat (de)construction
    primTupToV2             :: IsComponent a                            => primFun stage ((a,a)     -> V2 a)
    primTupToV3             :: IsComponent a                            => primFun stage ((a,a,a)   -> V3 a)
    primTupToV4             :: IsComponent a                            => primFun stage ((a,a,a,a) -> V4 a)
    primV2ToTup             :: IsComponent a                            => primFun stage (V2 a     -> (a,a))
    primV3ToTup             :: IsComponent a                            => primFun stage (V3 a   -> (a,a,a))
    primV4ToTup             :: IsComponent a                            => primFun stage (V4 a -> (a,a,a,a))

    -- Arithmetic Functions (componentwise)
    primAdd                 :: (IsNum t, IsMatVec a t)                              => primFun stage ((a,a)   -> a)
    primAddS                :: (IsNum t, IsMatVecScalar a t)                        => primFun stage ((a,t)   -> a)
    primSub                 :: (IsNum t, IsMatVec a t)                              => primFun stage ((a,a)   -> a)
    primSubS                :: (IsNum t, IsMatVecScalar a t)                        => primFun stage ((a,t)   -> a)
    primMul                 :: (IsNum t, IsMatVec a t)                              => primFun stage ((a,a)   -> a)
    primMulS                :: (IsNum t, IsMatVecScalar a t)                        => primFun stage ((a,t)   -> a)
    primDiv                 :: (IsNum t, IsVecScalar d a t)                         => primFun stage ((a,a)   -> a)
    primDivS                :: (IsNum t, IsVecScalar d a t)                         => primFun stage ((a,t)   -> a)
    primNeg                 :: (IsSigned t, IsMatVecScalar a t)                     => primFun stage (a       -> a)
    primMod                 :: (IsNum t, IsVecScalar d a t)                         => primFun stage ((a,a)   -> a)
    primModS                :: (IsNum t, IsVecScalar d a t)                         => primFun stage ((a,t)   -> a)

    -- Bit-wise Functions
    primBAnd        :: (IsIntegral t, IsVecScalar d a t)                            => primFun stage ((a,a)   -> a)
    primBAndS       :: (IsIntegral t, IsVecScalar d a t)                            => primFun stage ((a,t)   -> a)
    primBOr         :: (IsIntegral t, IsVecScalar d a t)                            => primFun stage ((a,a)   -> a)
    primBOrS        :: (IsIntegral t, IsVecScalar d a t)                            => primFun stage ((a,t)   -> a)
    primBXor        :: (IsIntegral t, IsVecScalar d a t)                            => primFun stage ((a,a)   -> a)
    primBXorS       :: (IsIntegral t, IsVecScalar d a t)                            => primFun stage ((a,t)   -> a)
    primBNot        :: (IsIntegral t, IsVecScalar d a t)                            => primFun stage (a       -> a)
    primBShiftL     :: (IsIntegral t, IsVecScalar d a t, IsVecScalar d b Word32)    => primFun stage ((a, b)      -> a)
    primBShiftLS    :: (IsIntegral t, IsVecScalar d a t)                            => primFun stage ((a, Word32) -> a)
    primBShiftR     :: (IsIntegral t, IsVecScalar d a t, IsVecScalar d b Word32)    => primFun stage ((a, b)      -> a)
    primBShiftRS    :: (IsIntegral t, IsVecScalar d a t)                            => primFun stage ((a, Word32) -> a)

    -- Logic Functions
    primAnd                 ::                                             primFun stage ((Bool,Bool) -> Bool)
    primOr                  ::                                             primFun stage ((Bool,Bool) -> Bool)
    primXor                 ::                                             primFun stage ((Bool,Bool) -> Bool)
    primNot                 :: IsVecScalar d a Bool                           => primFun stage (a           -> a)
    primAny                 :: IsVecScalar d a Bool                           => primFun stage (a           -> Bool)
    primAll                 :: IsVecScalar d a Bool                           => primFun stage (a           -> Bool)

    -- Angle and Trigonometry Functions
    primACos                :: IsVecScalar d a Float                          => primFun stage (a -> a)
    primACosH               :: IsVecScalar d a Float                          => primFun stage (a -> a)
    primASin                :: IsVecScalar d a Float                          => primFun stage (a -> a)
    primASinH               :: IsVecScalar d a Float                          => primFun stage (a -> a)
    primATan                :: IsVecScalar d a Float                          => primFun stage (a -> a)
    primATan2               :: IsVecScalar d a Float                          => primFun stage ((a,a) -> a)
    primATanH               :: IsVecScalar d a Float                          => primFun stage (a -> a)
    primCos                 :: IsVecScalar d a Float                          => primFun stage (a -> a)
    primCosH                :: IsVecScalar d a Float                          => primFun stage (a -> a)
    primDegrees             :: IsVecScalar d a Float                          => primFun stage (a -> a)
    primRadians             :: IsVecScalar d a Float                          => primFun stage (a -> a)
    primSin                 :: IsVecScalar d a Float                          => primFun stage (a -> a)
    primSinH                :: IsVecScalar d a Float                          => primFun stage (a -> a)
    primTan                 :: IsVecScalar d a Float                          => primFun stage (a -> a)
    primTanH                :: IsVecScalar d a Float                          => primFun stage (a -> a)

    -- Exponential Functions
    primPow                 :: IsVecScalar d a Float                          => primFun stage ((a,a) -> a)
    primExp                 :: IsVecScalar d a Float                          => primFun stage (a -> a)
    primLog                 :: IsVecScalar d a Float                          => primFun stage (a -> a)
    primExp2                :: IsVecScalar d a Float                          => primFun stage (a -> a)
    primLog2                :: IsVecScalar d a Float                          => primFun stage (a -> a)
    primSqrt                :: IsVecScalar d a Float                          => primFun stage (a -> a)
    primInvSqrt             :: IsVecScalar d a Float                          => primFun stage (a -> a)

    -- Common Functions
    primIsNan               :: (IsVecScalar d a Float, IsVecScalar d b Bool)        => primFun stage (a -> b)
    primIsInf               :: (IsVecScalar d a Float, IsVecScalar d b Bool)        => primFun stage (a -> b)
    primAbs                 :: (IsSigned t, IsVecScalar d a t)                => primFun stage (a -> a)
    primSign                :: (IsSigned t, IsVecScalar d a t)                => primFun stage (a -> a)
    primFloor               :: IsVecScalar d a Float                          => primFun stage (a -> a)
    primTrunc               :: IsVecScalar d a Float                          => primFun stage (a -> a)
    primRound               :: IsVecScalar d a Float                          => primFun stage (a -> a)
    primRoundEven           :: IsVecScalar d a Float                          => primFun stage (a -> a)
    primCeil                :: IsVecScalar d a Float                          => primFun stage (a -> a)
    primFract               :: IsVecScalar d a Float                          => primFun stage (a -> a)
    primModF                :: IsVecScalar d a Float                          => primFun stage (a               -> (a,a))
    primMin                 :: (IsNum t, IsVecScalar d a t)                   => primFun stage ((a,a)           -> a)
    primMinS                :: (IsNum t, IsVecScalar d a t)                   => primFun stage ((a,t)           -> a)
    primMax                 :: (IsNum t, IsVecScalar d a t)                   => primFun stage ((a,a)           -> a)
    primMaxS                :: (IsNum t, IsVecScalar d a t)                   => primFun stage ((a,t)           -> a)
    primClamp               :: (IsNum t, IsVecScalar d a t)                   => primFun stage ((a,a,a)         -> a)
    primClampS              :: (IsNum t, IsVecScalar d a t)                   => primFun stage ((a,t,t)         -> a)
    primMix                 :: IsVecScalar d a Float                          => primFun stage ((a,a,a)         -> a)
    primMixS                :: IsVecScalar d a Float                          => primFun stage ((a,a,Float)     -> a)
    primMixB                :: (IsVecScalar d a Float, IsVecScalar d b Bool)        => primFun stage ((a,a,b)         -> a)
    primStep                :: IsVecScalar d a Float                          => primFun stage ((a,a)           -> a)
    primStepS               :: IsVecScalar d a Float                          => primFun stage ((Float,a)       -> a)
    primSmoothStep          :: IsVecScalar d a Float                          => primFun stage ((a,a,a)         -> a)
    primSmoothStepS         :: IsVecScalar d a Float                          => primFun stage ((Float,Float,a) -> a)

    -- Integer/Float Conversion Functions
    primFloatBitsToInt      :: (IsVecScalar d fv Float, IsVecScalar d iv Int32)     => primFun stage (fv -> iv)
    primFloatBitsToUInt     :: (IsVecScalar d fv Float, IsVecScalar d uv Word32)    => primFun stage (fv -> uv)
    primIntBitsToFloat      :: (IsVecScalar d fv Float, IsVecScalar d iv Int32)     => primFun stage (iv -> fv)
    primUIntBitsToFloat     :: (IsVecScalar d fv Float, IsVecScalar d uv Word32)    => primFun stage (uv -> fv)

    -- Geometric Functions
    primLength              :: IsVecScalar d a Float                          => primFun stage (a       -> Float)
    primDistance            :: IsVecScalar d a Float                          => primFun stage ((a,a)   -> Float)
    primDot                 :: IsVecScalar d a Float                          => primFun stage ((a,a)   -> Float)
    primCross               :: IsVecScalar DIM3 a Float                       => primFun stage ((a,a)   -> a)
    primNormalize           :: IsVecScalar d a Float                          => primFun stage (a       -> a)
    primFaceForward         :: IsVecScalar d a Float                          => primFun stage ((a,a,a) -> a)
    primReflect             :: IsVecScalar d a Float                          => primFun stage ((a,a)   -> a)
    primRefract             :: IsVecScalar d a Float                          => primFun stage ((a,a,a) -> a)

    -- Matrix Functions
    primTranspose           :: (IsMat a h w, IsMat b w h)               => primFun stage (a       -> b)
    primDeterminant         :: IsMat m s s                              => primFun stage (m       -> Float)
    primInverse             :: IsMat m h w                              => primFun stage (m       -> m)
    primOuterProduct        :: IsMat m h w                              => primFun stage ((w,h)   -> m)
    primMulMatVec           :: IsMat m h w                              => primFun stage ((m,w)   -> h)
    primMulVecMat           :: IsMat m h w                              => primFun stage ((h,m)   -> w)
    primMulMatMat           :: (IsMat a i j, IsMat b j k, IsMat c i k)  => primFun stage ((a,b)   -> c)

    -- Vector and Scalar Relational Functions
    primLessThan            :: (IsNum t, IsVecScalar d a t, IsVecScalar d b Bool)   => primFun stage ((a,a) -> b)
    primLessThanEqual       :: (IsNum t, IsVecScalar d a t, IsVecScalar d b Bool)   => primFun stage ((a,a) -> b)
    primGreaterThan         :: (IsNum t, IsVecScalar d a t, IsVecScalar d b Bool)   => primFun stage ((a,a) -> b)
    primGreaterThanEqual    :: (IsNum t, IsVecScalar d a t, IsVecScalar d b Bool)   => primFun stage ((a,a) -> b)
    primEqualV              :: (IsNum t, IsVecScalar d a t, IsVecScalar d b Bool)   => primFun stage ((a,a) -> b)
    primEqual               :: IsMatVecScalar a t                             => primFun stage ((a,a) -> Bool)
    primNotEqualV           :: (IsNum t, IsVecScalar d a t, IsVecScalar d b Bool)   => primFun stage ((a,a) -> b)
    primNotEqual            :: IsMatVecScalar a t                             => primFun stage ((a,a) -> Bool)

    -- Fragment Processing Functions
    primDFdx                :: IsVecScalar d a Float                          => primFun stage (a -> a)
    primDFdy                :: IsVecScalar d a Float                          => primFun stage (a -> a)
    primFWidth              :: IsVecScalar d a Float                          => primFun stage (a -> a)

    -- Noise Functions
    primNoise1              :: IsVecScalar d a Float                          => primFun stage (a -> Float)
    primNoise2              :: (IsVecScalar d a Float, IsVecScalar DIM2 b Float)    => primFun stage (a -> b)
    primNoise3              :: (IsVecScalar d a Float, IsVecScalar DIM3 b Float)    => primFun stage (a -> b)
    primNoise4              :: (IsVecScalar d a Float, IsVecScalar DIM4 b Float)    => primFun stage (a -> b)

    -- Texture Lookup Functions
    primTextureSize             :: IsTextureSize sampler lod size                           => primFun stage ((sampler,lod)                       -> size)
    primTexture                 :: IsTexture sampler coord bias                             => primFun stage ((sampler,coord,bias)                -> TexelRepr sampler)
    primTextureProj             :: IsTextureProj sampler coord bias                         => primFun stage ((sampler,coord,bias)                -> TexelRepr sampler)
    primTextureLod              :: IsTextureLod sampler coord lod                           => primFun stage ((sampler,coord,lod)                 -> TexelRepr sampler)
    primTextureOffset           :: IsTextureOffset sampler coord offset bias                => primFun stage ((sampler,coord,offset,bias)         -> TexelRepr sampler)
    primTexelFetch              :: IsTexelFetch sampler coord lod                           => primFun stage ((sampler,coord,lod)                 -> TexelRepr sampler)
    primTexelFetchOffset        :: IsTexelFetchOffset sampler coord lod offset              => primFun stage ((sampler,coord,lod,offset)          -> TexelRepr sampler)
    primTextureProjOffset       :: IsTextureProjOffset sampler coord offset bias            => primFun stage ((sampler,coord,offset,bias)         -> TexelRepr sampler)
    primTextureLodOffset        :: IsTextureLodOffset sampler coord lod offset              => primFun stage ((sampler,coord,lod,offset)          -> TexelRepr sampler)
    primTextureProjLod          :: IsTextureProjLod sampler coord lod                       => primFun stage ((sampler,coord,lod)                 -> TexelRepr sampler)
    primTextureProjLodOffset    :: IsTextureProjLodOffset sampler coord lod offset          => primFun stage ((sampler,coord,lod,offset)          -> TexelRepr sampler)
    primTextureGrad             :: IsTextureGrad sampler coord dx dy                        => primFun stage ((sampler,coord,dx,dy)               -> TexelRepr sampler)
    primTextureGradOffset       :: IsTextureGradOffset sampler coord dx dy offset           => primFun stage ((sampler,coord,dx,dy,offset)        -> TexelRepr sampler)
    primTextureProjGrad         :: IsTextureProjGrad sampler coord dx dy                    => primFun stage ((sampler,coord,dx,dy)               -> TexelRepr sampler)
    primTextureProjGradOffset   :: IsTextureProjGradOffset sampler coord dx dy offset       => primFun stage ((sampler,coord,dx,dy,offset)        -> TexelRepr sampler)
