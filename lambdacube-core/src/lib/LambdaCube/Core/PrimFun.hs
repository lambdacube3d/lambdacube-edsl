module LambdaCube.Core.PrimFun where

data PrimFun
    -- Vec/Mat (de)construction
    = PrimTupToV2
    | PrimTupToV3
    | PrimTupToV4
    | PrimV2ToTup
    | PrimV3ToTup
    | PrimV4ToTup

    -- Arithmetic Functions (componentwise)
    | PrimAdd
    | PrimAddS
    | PrimSub
    | PrimSubS
    | PrimMul
    | PrimMulS
    | PrimDiv
    | PrimDivS
    | PrimNeg
    | PrimMod
    | PrimModS

    -- Bit-wise Functions
    | PrimBAnd
    | PrimBAndS
    | PrimBOr
    | PrimBOrS
    | PrimBXor
    | PrimBXorS
    | PrimBNot
    | PrimBShiftL
    | PrimBShiftLS
    | PrimBShiftR
    | PrimBShiftRS

    -- Logic Functions
    | PrimAnd
    | PrimOr
    | PrimXor
    | PrimNot
    | PrimAny
    | PrimAll

    -- Angle and Trigonometry Functions
    | PrimACos
    | PrimACosH
    | PrimASin
    | PrimASinH
    | PrimATan
    | PrimATan2
    | PrimATanH
    | PrimCos
    | PrimCosH
    | PrimDegrees
    | PrimRadians
    | PrimSin
    | PrimSinH
    | PrimTan
    | PrimTanH

    -- Exponential Functions
    | PrimPow
    | PrimExp
    | PrimLog
    | PrimExp2
    | PrimLog2
    | PrimSqrt
    | PrimInvSqrt

    -- Common Functions
    | PrimIsNan
    | PrimIsInf
    | PrimAbs
    | PrimSign
    | PrimFloor
    | PrimTrunc
    | PrimRound
    | PrimRoundEven
    | PrimCeil
    | PrimFract
    | PrimModF
    | PrimMin
    | PrimMinS
    | PrimMax
    | PrimMaxS
    | PrimClamp
    | PrimClampS
    | PrimMix
    | PrimMixS
    | PrimMixB
    | PrimStep
    | PrimStepS
    | PrimSmoothStep
    | PrimSmoothStepS

    -- Integer/Float Conversion Functions
    | PrimFloatBitsToInt
    | PrimFloatBitsToUInt
    | PrimIntBitsToFloat
    | PrimUIntBitsToFloat

    -- Geometric Functions
    | PrimLength
    | PrimDistance
    | PrimDot
    | PrimCross
    | PrimNormalize
    | PrimFaceForward
    | PrimReflect
    | PrimRefract

    -- Matrix Functions
    | PrimTranspose
    | PrimDeterminant
    | PrimInverse
    | PrimOuterProduct
    | PrimMulMatVec
    | PrimMulVecMat
    | PrimMulMatMat

    -- Vector and Scalar Relational Functions
    | PrimLessThan
    | PrimLessThanEqual
    | PrimGreaterThan
    | PrimGreaterThanEqual
    | PrimEqualV
    | PrimEqual
    | PrimNotEqualV
    | PrimNotEqual

    -- Fragment Processing Functions
    | PrimDFdx
    | PrimDFdy
    | PrimFWidth

    -- Noise Functions
    | PrimNoise1
    | PrimNoise2
    | PrimNoise3
    | PrimNoise4

    -- Texture Lookup Functions
    | PrimTextureSize
    | PrimTexture
    | PrimTextureProj
    | PrimTextureLod
    | PrimTextureOffset
    | PrimTexelFetch
    | PrimTexelFetchOffset
    | PrimTextureProjOffset
    | PrimTextureLodOffset
    | PrimTextureProjLod
    | PrimTextureProjLodOffset
    | PrimTextureGrad
    | PrimTextureGradOffset
    | PrimTextureProjGrad
    | PrimTextureProjGradOffset
    deriving (Eq, Ord, Show, Read)
