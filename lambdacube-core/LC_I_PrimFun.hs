module LC_I_PrimFun where

import LC_T_PrimFun
import LC_U_PrimFun hiding (PrimFun)
import qualified LC_U_PrimFun as U

newtype PrimFunI stage t = PrimFunI (U.PrimFun)

instance PrimFun PrimFunI where
    -- Vec/Mat (de)construction
    primTupToV2             = PrimFunI PrimTupToV2
    primTupToV3             = PrimFunI PrimTupToV3
    primTupToV4             = PrimFunI PrimTupToV4
    primV2ToTup             = PrimFunI PrimV2ToTup
    primV3ToTup             = PrimFunI PrimV3ToTup
    primV4ToTup             = PrimFunI PrimV4ToTup

    -- Arithmetic Functions (componentwise)
    primAdd                 = PrimFunI PrimAdd
    primAddS                = PrimFunI PrimAddS
    primSub                 = PrimFunI PrimSub
    primSubS                = PrimFunI PrimSubS
    primMul                 = PrimFunI PrimMul
    primMulS                = PrimFunI PrimMulS
    primDiv                 = PrimFunI PrimDiv
    primDivS                = PrimFunI PrimDivS
    primNeg                 = PrimFunI PrimNeg
    primMod                 = PrimFunI PrimMod
    primModS                = PrimFunI PrimModS

    -- Bit-wise Functions
    primBAnd                = PrimFunI PrimBAnd
    primBAndS               = PrimFunI PrimBAndS
    primBOr                 = PrimFunI PrimBOr
    primBOrS                = PrimFunI PrimBOrS
    primBXor                = PrimFunI PrimBXor
    primBXorS               = PrimFunI PrimBXorS
    primBNot                = PrimFunI PrimBNot
    primBShiftL             = PrimFunI PrimBShiftL
    primBShiftLS            = PrimFunI PrimBShiftLS
    primBShiftR             = PrimFunI PrimBShiftR
    primBShiftRS            = PrimFunI PrimBShiftRS

    -- Logic Functions
    primAnd                 = PrimFunI PrimAnd
    primOr                  = PrimFunI PrimOr
    primXor                 = PrimFunI PrimXor
    primNot                 = PrimFunI PrimNot
    primAny                 = PrimFunI PrimAny
    primAll                 = PrimFunI PrimAll

    -- Angle and Trigonometry Functions
    primACos                = PrimFunI PrimACos
    primACosH               = PrimFunI PrimACosH
    primASin                = PrimFunI PrimASin
    primASinH               = PrimFunI PrimASinH
    primATan                = PrimFunI PrimATan
    primATan2               = PrimFunI PrimATan2
    primATanH               = PrimFunI PrimATanH
    primCos                 = PrimFunI PrimCos
    primCosH                = PrimFunI PrimCosH
    primDegrees             = PrimFunI PrimDegrees
    primRadians             = PrimFunI PrimRadians
    primSin                 = PrimFunI PrimSin
    primSinH                = PrimFunI PrimSinH
    primTan                 = PrimFunI PrimTan
    primTanH                = PrimFunI PrimTanH

    -- Exponential Functions
    primPow                 = PrimFunI PrimPow
    primExp                 = PrimFunI PrimExp
    primLog                 = PrimFunI PrimLog
    primExp2                = PrimFunI PrimExp2
    primLog2                = PrimFunI PrimLog2
    primSqrt                = PrimFunI PrimSqrt
    primInvSqrt             = PrimFunI PrimInvSqrt

    -- Common Functions
    primIsNan               = PrimFunI PrimIsNan
    primIsInf               = PrimFunI PrimIsInf
    primAbs                 = PrimFunI PrimAbs
    primSign                = PrimFunI PrimSign
    primFloor               = PrimFunI PrimFloor
    primTrunc               = PrimFunI PrimTrunc
    primRound               = PrimFunI PrimRound
    primRoundEven           = PrimFunI PrimRoundEven
    primCeil                = PrimFunI PrimCeil
    primFract               = PrimFunI PrimFract
    primModF                = PrimFunI PrimModF
    primMin                 = PrimFunI PrimMin
    primMinS                = PrimFunI PrimMinS
    primMax                 = PrimFunI PrimMax
    primMaxS                = PrimFunI PrimMaxS
    primClamp               = PrimFunI PrimClamp
    primClampS              = PrimFunI PrimClampS
    primMix                 = PrimFunI PrimMix
    primMixS                = PrimFunI PrimMixS
    primMixB                = PrimFunI PrimMixB
    primStep                = PrimFunI PrimStep
    primStepS               = PrimFunI PrimStepS
    primSmoothStep          = PrimFunI PrimSmoothStep
    primSmoothStepS         = PrimFunI PrimSmoothStepS

    -- Integer/Float Conversion Functions
    primFloatBitsToInt      = PrimFunI PrimFloatBitsToInt
    primFloatBitsToUInt     = PrimFunI PrimFloatBitsToUInt
    primIntBitsToFloat      = PrimFunI PrimIntBitsToFloat
    primUIntBitsToFloat     = PrimFunI PrimUIntBitsToFloat

    -- Geometric Functions
    primLength              = PrimFunI PrimLength
    primDistance            = PrimFunI PrimDistance
    primDot                 = PrimFunI PrimDot
    primCross               = PrimFunI PrimCross
    primNormalize           = PrimFunI PrimNormalize
    primFaceForward         = PrimFunI PrimFaceForward
    primReflect             = PrimFunI PrimReflect
    primRefract             = PrimFunI PrimRefract

    -- Matrix Functions
    primTranspose           = PrimFunI PrimTranspose
    primDeterminant         = PrimFunI PrimDeterminant
    primInverse             = PrimFunI PrimInverse
    primOuterProduct        = PrimFunI PrimOuterProduct
    primMulMatVec           = PrimFunI PrimMulMatVec
    primMulVecMat           = PrimFunI PrimMulVecMat
    primMulMatMat           = PrimFunI PrimMulMatMat

    -- Vector and Scalar Relational Functions
    primLessThan            = PrimFunI PrimLessThan
    primLessThanEqual       = PrimFunI PrimLessThanEqual
    primGreaterThan         = PrimFunI PrimGreaterThan
    primGreaterThanEqual    = PrimFunI PrimGreaterThanEqual
    primEqualV              = PrimFunI PrimEqualV
    primEqual               = PrimFunI PrimEqual
    primNotEqualV           = PrimFunI PrimNotEqualV
    primNotEqual            = PrimFunI PrimNotEqual

    -- Fragment Processing Functions
    primDFdx                = PrimFunI PrimDFdx
    primDFdy                = PrimFunI PrimDFdy
    primFWidth              = PrimFunI PrimFWidth

    -- Noise Functions
    primNoise1              = PrimFunI PrimNoise1
    primNoise2              = PrimFunI PrimNoise2
    primNoise3              = PrimFunI PrimNoise3
    primNoise4              = PrimFunI PrimNoise4

    -- Texture Lookup Functions
    primTextureSize             = PrimFunI PrimTextureSize
    primTexture                 = PrimFunI PrimTexture
    primTextureProj             = PrimFunI PrimTextureProj
    primTextureLod              = PrimFunI PrimTextureLod
    primTextureOffset           = PrimFunI PrimTextureOffset
    primTexelFetch              = PrimFunI PrimTexelFetch
    primTexelFetchOffset        = PrimFunI PrimTexelFetchOffset
    primTextureProjOffset       = PrimFunI PrimTextureProjOffset
    primTextureLodOffset        = PrimFunI PrimTextureLodOffset
    primTextureProjLod          = PrimFunI PrimTextureProjLod
    primTextureProjLodOffset    = PrimFunI PrimTextureProjLodOffset
    primTextureGrad             = PrimFunI PrimTextureGrad
    primTextureGradOffset       = PrimFunI PrimTextureGradOffset
    primTextureProjGrad         = PrimFunI PrimTextureProjGrad
    primTextureProjGradOffset   = PrimFunI PrimTextureProjGradOffset
