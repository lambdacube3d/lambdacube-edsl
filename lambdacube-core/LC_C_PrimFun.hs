module LC_C_PrimFun where

import qualified LC_T_PrimFun as T
import LC_U_PrimFun

convertPrimFun :: T.PrimFun a b -> PrimFun
convertPrimFun a = case a of
    -- Vec/Mat (de)construction
    T.PrimTupToV2                   -> PrimTupToV2
    T.PrimTupToV3                   -> PrimTupToV3
    T.PrimTupToV4                   -> PrimTupToV4
    T.PrimV2ToTup                   -> PrimV2ToTup
    T.PrimV3ToTup                   -> PrimV3ToTup
    T.PrimV4ToTup                   -> PrimV4ToTup

    -- Arithmetic Functions (componentwise)
    T.PrimAdd                       -> PrimAdd 
    T.PrimAddS                      -> PrimAddS
    T.PrimSub                       -> PrimSub 
    T.PrimSubS                      -> PrimSubS  
    T.PrimMul                       -> PrimMul 
    T.PrimMulS                      -> PrimMulS
    T.PrimDiv                       -> PrimDiv 
    T.PrimDivS                      -> PrimDivS
    T.PrimNeg                       -> PrimNeg 
    T.PrimMod                       -> PrimMod 
    T.PrimModS                      -> PrimModS

    -- Bit-wise Functions
    T.PrimBAnd                      -> PrimBAnd    
    T.PrimBAndS                     -> PrimBAndS   
    T.PrimBOr                       -> PrimBOr     
    T.PrimBOrS                      -> PrimBOrS    
    T.PrimBXor                      -> PrimBXor    
    T.PrimBXorS                     -> PrimBXorS   
    T.PrimBNot                      -> PrimBNot    
    T.PrimBShiftL                   -> PrimBShiftL 
    T.PrimBShiftLS                  -> PrimBShiftLS
    T.PrimBShiftR                   -> PrimBShiftR 
    T.PrimBShiftRS                  -> PrimBShiftRS

    -- Logic Functions
    T.PrimAnd                       -> PrimAnd
    T.PrimOr                        -> PrimOr 
    T.PrimXor                       -> PrimXor
    T.PrimNot                       -> PrimNot
    T.PrimAny                       -> PrimAny
    T.PrimAll                       -> PrimAll

    -- Angle and Trigonometry Functions
    T.PrimACos                      -> PrimACos   
    T.PrimACosH                     -> PrimACosH  
    T.PrimASin                      -> PrimASin   
    T.PrimASinH                     -> PrimASinH  
    T.PrimATan                      -> PrimATan   
    T.PrimATan2                     -> PrimATan2  
    T.PrimATanH                     -> PrimATanH  
    T.PrimCos                       -> PrimCos    
    T.PrimCosH                      -> PrimCosH   
    T.PrimDegrees                   -> PrimDegrees
    T.PrimRadians                   -> PrimRadians
    T.PrimSin                       -> PrimSin    
    T.PrimSinH                      -> PrimSinH   
    T.PrimTan                       -> PrimTan    
    T.PrimTanH                      -> PrimTanH   

    -- Exponential Functions
    T.PrimPow                       -> PrimPow    
    T.PrimExp                       -> PrimExp    
    T.PrimLog                       -> PrimLog    
    T.PrimExp2                      -> PrimExp2   
    T.PrimLog2                      -> PrimLog2   
    T.PrimSqrt                      -> PrimSqrt   
    T.PrimInvSqrt                   -> PrimInvSqrt

    -- Common Functions
    T.PrimIsNan                     -> PrimIsNan      
    T.PrimIsInf                     -> PrimIsInf      
    T.PrimAbs                       -> PrimAbs        
    T.PrimSign                      -> PrimSign       
    T.PrimFloor                     -> PrimFloor      
    T.PrimTrunc                     -> PrimTrunc      
    T.PrimRound                     -> PrimRound      
    T.PrimRoundEven                 -> PrimRoundEven  
    T.PrimCeil                      -> PrimCeil       
    T.PrimFract                     -> PrimFract      
    T.PrimModF                      -> PrimModF       
    T.PrimMin                       -> PrimMin        
    T.PrimMinS                      -> PrimMinS       
    T.PrimMax                       -> PrimMax        
    T.PrimMaxS                      -> PrimMaxS       
    T.PrimClamp                     -> PrimClamp      
    T.PrimClampS                    -> PrimClampS     
    T.PrimMix                       -> PrimMix        
    T.PrimMixS                      -> PrimMixS       
    T.PrimMixB                      -> PrimMixB       
    T.PrimStep                      -> PrimStep       
    T.PrimStepS                     -> PrimStepS      
    T.PrimSmoothStep                -> PrimSmoothStep 
    T.PrimSmoothStepS               -> PrimSmoothStepS

    -- Integer/Float Conversion Functions
    T.PrimFloatBitsToInt            -> PrimFloatBitsToInt   
    T.PrimFloatBitsToUInt           -> PrimFloatBitsToUInt  
    T.PrimIntBitsToFloat            -> PrimIntBitsToFloat   
    T.PrimUIntBitsToFloat           -> PrimUIntBitsToFloat  

    -- Geometric Functions
    T.PrimLength                    -> PrimLength     
    T.PrimDistance                  -> PrimDistance   
    T.PrimDot                       -> PrimDot        
    T.PrimCross                     -> PrimCross      
    T.PrimNormalize                 -> PrimNormalize  
    T.PrimFaceForward               -> PrimFaceForward
    T.PrimReflect                   -> PrimReflect    
    T.PrimRefract                   -> PrimRefract    

    -- Matrix Functions
    T.PrimTranspose                 -> PrimTranspose   
    T.PrimDeterminant               -> PrimDeterminant 
    T.PrimInverse                   -> PrimInverse     
    T.PrimOuterProduct              -> PrimOuterProduct
    T.PrimMulMatVec                 -> PrimMulMatVec   
    T.PrimMulVecMat                 -> PrimMulVecMat   
    T.PrimMulMatMat                 -> PrimMulMatMat   

    -- Vector and Scalar Relational Functions
    T.PrimLessThan                  -> PrimLessThan        
    T.PrimLessThanEqual             -> PrimLessThanEqual   
    T.PrimGreaterThan               -> PrimGreaterThan     
    T.PrimGreaterThanEqual          -> PrimGreaterThanEqual
    T.PrimEqualV                    -> PrimEqualV          
    T.PrimEqual                     -> PrimEqual           
    T.PrimNotEqualV                 -> PrimNotEqualV       
    T.PrimNotEqual                  -> PrimNotEqual        

    -- Fragment Processing Functions
    T.PrimDFdx                      -> PrimDFdx  
    T.PrimDFdy                      -> PrimDFdy  
    T.PrimFWidth                    -> PrimFWidth

    -- Noise Functions
    T.PrimNoise1                    -> PrimNoise1
    T.PrimNoise2                    -> PrimNoise2
    T.PrimNoise3                    -> PrimNoise3
    T.PrimNoise4                    -> PrimNoise4

    -- Texture Lookup Functions
    T.PrimTextureSize               -> PrimTextureSize
    T.PrimTexture                   -> PrimTexture               
    T.PrimTextureProj               -> PrimTextureProj           
    T.PrimTextureLod                -> PrimTextureLod            
    T.PrimTextureOffset             -> PrimTextureOffset         
    T.PrimTexelFetch                -> PrimTexelFetch            
    T.PrimTexelFetchOffset          -> PrimTexelFetchOffset      
    T.PrimTextureProjOffset         -> PrimTextureProjOffset     
    T.PrimTextureLodOffset          -> PrimTextureLodOffset      
    T.PrimTextureProjLod            -> PrimTextureProjLod        
    T.PrimTextureProjLodOffset      -> PrimTextureProjLodOffset  
    T.PrimTextureGrad               -> PrimTextureGrad           
    T.PrimTextureGradOffset         -> PrimTextureGradOffset     
    T.PrimTextureProjGrad           -> PrimTextureProjGrad       
    T.PrimTextureProjGradOffset     -> PrimTextureProjGradOffset 

