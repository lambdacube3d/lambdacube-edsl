module LC_T_Sampler where

import Data.Int
import Data.Word

import LC_G_Type
import LC_T_APIType

{-
-- shadow samplers
type Sampler1DShadow        = Sampler DIM1 Z Depth          Mip                         Shadow
type Sampler2DShadow        = Sampler DIM2 Z Depth          Mip                         Shadow
type SamplerCubeShadow      = Sampler DIM2 C Depth          Mip                 Array   Shadow
type Sampler1DArrayShadow   = Sampler DIM1 A Depth          Mip                 Array   Shadow
type Sampler2DArrayShadow   = Sampler DIM2 A Depth          Mip                 Array   Shadow
type Sampler2DRectShadow    = Sampler Rect Z Depth          NoMip                       Shadow

-- float,int,word samplers
type Sampler1D              = Sampler DIM1 Z Float          Mip     Regular
type Sampler2D              = Sampler DIM2 Z Float          Mip     Regular
type Sampler3D              = Sampler DIM3 Z Float          Mip     Regular
type SamplerCube            = Sampler DIM2 C Float          Mip     Regular     Array
type Sampler1DArray         = Sampler DIM1 A Float          Mip     Regular     Array
type Sampler2DArray         = Sampler DIM2 A Float          Mip     Regular     Array
type Sampler2DRect          = Sampler Rect Z Float          NoMip   Regular

type Sampler2DMS            = Sampler DIM2 Z FloatMS        NoMip                       -- from previous render pass only
type Sampler2DMSArray       = Sampler DIM2 A FloatMS        NoMip               Array   -- from previous render pass only
type SamplerBuffer          = Sampler DIM1 Z BufferFloat    NoMip
-}

type GSampler1D t ar        = Sampler DIM1 SingleTex t ar
type GSampler2D t ar        = Sampler DIM2 SingleTex t ar
type GSampler3D t ar        = Sampler DIM3 SingleTex t ar
type GSamplerCube t ar      = Sampler DIM2 CubeTex t ar
type GSampler1DArray t ar   = Sampler DIM1 ArrayTex t ar
type GSampler2DArray t ar   = Sampler DIM2 ArrayTex t ar
type GSampler2DRect t ar    = Sampler Rect SingleTex t ar

type family TexelRepr sampler
type instance TexelRepr (Sampler dim arr (v t) Red)     = t
type instance TexelRepr (Sampler dim arr (v t) RG)      = V2 t
type instance TexelRepr (Sampler dim arr (v t) RGB)     = V3 t
type instance TexelRepr (Sampler dim arr (v t) RGBA)    = V4 t

-- shadow samplers
type Sampler1DShadow        = GSampler1D        (Shadow Float) Red
type Sampler2DShadow        = GSampler2D        (Shadow Float) Red
type SamplerCubeShadow      = GSamplerCube      (Shadow Float) Red
type Sampler1DArrayShadow   = GSampler1DArray   (Shadow Float) Red
type Sampler2DArrayShadow   = GSampler2DArray   (Shadow Float) Red
type Sampler2DRectShadow    = GSampler2DRect    (Shadow Float) Red

-- float samplers
type  Sampler1D t ar        = GSampler1D        (Regular t) ar
type  Sampler2D t ar        = GSampler2D        (Regular t) ar
type  Sampler3D t ar        = GSampler3D        (Regular t) ar
type  SamplerCube t ar      = GSamplerCube      (Regular t) ar
type  Sampler1DArray t ar   = GSampler1DArray   (Regular t) ar
type  Sampler2DArray t ar   = GSampler2DArray   (Regular t) ar
type  Sampler2DRect t ar    = GSampler2DRect    (Regular t) ar
type  Sampler2DMS t ar      = GSampler2D        (MultiSample t) ar
type  Sampler2DMSArray t ar = GSampler2DArray   (MultiSample t) ar
type  SamplerBuffer t ar    = GSampler1D        (Buffer t) ar

-- brute force
-- arity problem: lod
-- restriction: NONE
class IsTextureSize sampler lod size | sampler -> lod size
instance IsTextureSize  (Sampler1D t ar)           Int32   Int32
instance IsTextureSize  (Sampler1DArray t ar)      Int32   V2I
instance IsTextureSize  (Sampler2D t ar)           Int32   V2I
instance IsTextureSize  (Sampler2DArray t ar)      Int32   V3I
instance IsTextureSize  (Sampler2DMS t ar)         ZZ      V2I
instance IsTextureSize  (Sampler2DMSArray t ar)    ZZ      V3I
instance IsTextureSize  (Sampler2DRect t ar)       ZZ      V2I
instance IsTextureSize  (Sampler3D t ar)           Int32   V3I
instance IsTextureSize  (SamplerCube t ar)         Int32   V2I
instance IsTextureSize  (SamplerBuffer t ar)       ZZ      Int32
instance IsTextureSize  Sampler1DArrayShadow       Int32   V2I
instance IsTextureSize  Sampler1DShadow            Int32   Int32
instance IsTextureSize  Sampler2DArrayShadow       Int32   V3I
instance IsTextureSize  Sampler2DRectShadow        ZZ      V2I
instance IsTextureSize  Sampler2DShadow            Int32   V2I
instance IsTextureSize  SamplerCubeShadow          Int32   V2I

-- arity problem: bias
-- restriction: Regular union Shadow
class IsTexture sampler coord bias | sampler -> coord bias
instance IsTexture  (Sampler1D t ar)               Float   Float
instance IsTexture  (Sampler1DArray t ar)          V2F     Float
instance IsTexture  (Sampler2D t ar)               V2F     Float
instance IsTexture  (Sampler2DArray t ar)          V3F     Float
instance IsTexture  (Sampler2DRect t ar)           V2F     ZZ   
instance IsTexture  (Sampler3D t ar)               V3F     Float
instance IsTexture  (SamplerCube t ar)             V3F     Float
instance IsTexture  Sampler1DShadow                V3F     Float
instance IsTexture  Sampler1DArrayShadow           V3F     Float
instance IsTexture  Sampler2DShadow                V3F     Float
instance IsTexture  Sampler2DArrayShadow           V4F     ZZ   
instance IsTexture  Sampler2DRectShadow            V3F     ZZ   
instance IsTexture  SamplerCubeShadow              V4F     Float

-- arity problem: bias
-- restriction: (Regular union Shadow) exclude Array
class IsTextureProj sampler coord bias | sampler coord -> bias
instance IsTextureProj  (Sampler1D t ar)           V2F     Float
instance IsTextureProj  (Sampler1D t ar)           V4F     Float
instance IsTextureProj  (Sampler2D t ar)           V3F     Float
instance IsTextureProj  (Sampler2D t ar)           V4F     Float
instance IsTextureProj  (Sampler2DRect t ar)       V3F     ZZ   
instance IsTextureProj  (Sampler2DRect t ar)       V4F     ZZ   
instance IsTextureProj  (Sampler3D t ar)           V4F     Float
instance IsTextureProj  Sampler1DShadow            V4F     Float
instance IsTextureProj  Sampler2DRectShadow        V4F     ZZ   
instance IsTextureProj  Sampler2DShadow            V4F     Float

-- arity ok
-- restriction: ((Regular union Shadow) intersection Mip) exclude (2D Shadow Array)
class IsTextureLod sampler coord lod | sampler -> coord lod
instance IsTextureLod  (Sampler1D t ar)            Float   Float
instance IsTextureLod  (Sampler1DArray t ar)       V2F     Float
instance IsTextureLod  (Sampler2D t ar)            V2F     Float
instance IsTextureLod  (Sampler2DArray t ar)       V3F     Float
instance IsTextureLod  (Sampler3D t ar)            V3F     Float
instance IsTextureLod  (SamplerCube t ar)          V3F     Float
instance IsTextureLod  Sampler1DShadow             V3F     Float
instance IsTextureLod  Sampler1DArrayShadow        V3F     Float
instance IsTextureLod  Sampler2DShadow             V3F     Float

-- arity problem: bias
-- restriction: (Regular union Shadow) excluding (Cube, 2D Shadow Array)
class IsTextureOffset sampler coord offset bias | sampler -> coord offset bias
instance IsTextureOffset  (Sampler1D t ar)         Float   Int32   Float
instance IsTextureOffset  (Sampler1DArray t ar)    V2F     Int32   Float
instance IsTextureOffset  (Sampler2D t ar)         V2F     V2I     Float
instance IsTextureOffset  (Sampler2DArray t ar)    V3F     V2I     Float
instance IsTextureOffset  (Sampler2DRect t ar)     V2F     V2I     ZZ   
instance IsTextureOffset  (Sampler3D t ar)         V3F     V3I     Float
instance IsTextureOffset  Sampler1DShadow          V3F     Int32   Float
instance IsTextureOffset  Sampler1DArrayShadow     V3F     Int32   Float
instance IsTextureOffset  Sampler2DShadow          V3F     V2I     Float
instance IsTextureOffset  Sampler2DRectShadow      V3F     V2I     ZZ   

-- arity problem: lod, sample
class IsTexelFetch sampler coord lod | sampler -> coord lod
instance IsTexelFetch  (Sampler1D t ar)            Int32   Int32
instance IsTexelFetch  (Sampler1DArray t ar)       V2I     Int32
instance IsTexelFetch  (Sampler2D t ar)            V2I     Int32
instance IsTexelFetch  (Sampler2DArray t ar)       V3I     Int32
instance IsTexelFetch  (Sampler2DMS t ar)          V2I     Int32
instance IsTexelFetch  (Sampler2DMSArray t ar)     V3I     Int32
instance IsTexelFetch  (Sampler2DRect t ar)        V2I     ZZ   
instance IsTexelFetch  (Sampler3D t ar)            V3I     Int32
instance IsTexelFetch  (SamplerBuffer t ar)        Int32   ZZ   

-- arity problem: lod
class IsTexelFetchOffset sampler coord lod offset | sampler -> coord lod offset
instance IsTexelFetchOffset  (Sampler1D t ar)      Int32   Int32   Int32
instance IsTexelFetchOffset  (Sampler1DArray t ar) V2I     Int32   Int32
instance IsTexelFetchOffset  (Sampler2D t ar)      V2I     Int32   V2I  
instance IsTexelFetchOffset  (Sampler2DArray t ar) V3I     Int32   V2I  
instance IsTexelFetchOffset  (Sampler2DRect t ar)  V2I     ZZ      V2I  
instance IsTexelFetchOffset  (Sampler3D t ar)      V3I     Int32   V3I  

-- arity problem: bias
class IsTextureProjOffset sampler coord offset bias | sampler coord -> offset bias
instance IsTextureProjOffset  (Sampler1D t ar)      V2F     Int32   Float
instance IsTextureProjOffset  (Sampler1D t ar)      V4F     Int32   Float
instance IsTextureProjOffset  (Sampler2D t ar)      V3F     V2I     Float
instance IsTextureProjOffset  (Sampler2D t ar)      V4F     V2I     Float
instance IsTextureProjOffset  (Sampler3D t ar)      V4F     V3I     Float
instance IsTextureProjOffset  (Sampler2DRect t ar)  V3F     V2I     ZZ   
instance IsTextureProjOffset  (Sampler2DRect t ar)  V4F     V2I     ZZ   
instance IsTextureProjOffset  Sampler1DShadow       V4F     Int32   Float
instance IsTextureProjOffset  Sampler2DShadow       V4F     V2I     Float
instance IsTextureProjOffset  Sampler2DRectShadow   V4F     V2I     Float

-- arity ok
class IsTextureLodOffset sampler coord lod offset | sampler -> coord lod offset
instance IsTextureLodOffset  (Sampler1D t ar)       Float   Float   Int32
instance IsTextureLodOffset  (Sampler1DArray t ar)  V2F     Float   Int32
instance IsTextureLodOffset  (Sampler2D t ar)       V2F     Float   V2I  
instance IsTextureLodOffset  (Sampler2DArray t ar)  V3F     Float   V2I  
instance IsTextureLodOffset  (Sampler3D t ar)       V3F     Float   V3I  
instance IsTextureLodOffset  Sampler1DShadow        V3F     Float   Int32
instance IsTextureLodOffset  Sampler1DArrayShadow   V3F     Float   Int32
instance IsTextureLodOffset  Sampler2DShadow        V3F     Float   V2I  

-- arity ok
class IsTextureProjLod sampler coord lod | sampler coord -> lod
instance IsTextureProjLod  (Sampler1D t ar)         V2F     Float
instance IsTextureProjLod  (Sampler1D t ar)         V4F     Float
instance IsTextureProjLod  (Sampler2D t ar)         V3F     Float
instance IsTextureProjLod  (Sampler2D t ar)         V4F     Float
instance IsTextureProjLod  (Sampler3D t ar)         V4F     Float
instance IsTextureProjLod  Sampler1DShadow          V4F     Float
instance IsTextureProjLod  Sampler2DShadow          V4F     Float

-- arity ok
class IsTextureProjLodOffset sampler coord lod offset | sampler coord -> lod offset
instance IsTextureProjLodOffset  (Sampler1D t ar)   V2F     Float   Int32
instance IsTextureProjLodOffset  (Sampler1D t ar)   V4F     Float   Int32
instance IsTextureProjLodOffset  (Sampler2D t ar)   V3F     Float   V2I  
instance IsTextureProjLodOffset  (Sampler2D t ar)   V4F     Float   V2I  
instance IsTextureProjLodOffset  (Sampler3D t ar)   V4F     Float   V3I  
instance IsTextureProjLodOffset  Sampler1DShadow    V4F     Float   Int32
instance IsTextureProjLodOffset  Sampler2DShadow    V4F     Float   V2F  

-- arity ok
class IsTextureGrad sampler coord dx dy | sampler -> coord dx dy
instance IsTextureGrad  (Sampler1D t ar)            Float   Float   Float
instance IsTextureGrad  (Sampler1DArray t ar)       V2F     Float   Float
instance IsTextureGrad  (Sampler2D t ar)            V2F     V2F     V2F  
instance IsTextureGrad  (Sampler2DArray t ar)       V3F     V2F     V2F  
instance IsTextureGrad  (Sampler2DRect t ar)        V2F     V2F     V2F  
instance IsTextureGrad  (Sampler3D t ar)            V3F     V3F     V3F  
instance IsTextureGrad  (SamplerCube t ar)          V3F     V3F     V3F  
instance IsTextureGrad  Sampler1DArrayShadow        V3F     Float   Float
instance IsTextureGrad  Sampler1DShadow             V3F     Float   Float
instance IsTextureGrad  Sampler2DArrayShadow        V4F     V2F     V2F  
instance IsTextureGrad  Sampler2DRectShadow         V3F     V2F     V2F  
instance IsTextureGrad  Sampler2DShadow             V3F     V2F     V2F  
instance IsTextureGrad  SamplerCubeShadow           V4F     V3F     V3F  

-- arity ok
class IsTextureGradOffset sampler coord dx dy offset | sampler -> coord dx dy offset
instance IsTextureGradOffset  (Sampler1D t ar)      Float   Float   Float   Int32
instance IsTextureGradOffset  (Sampler1DArray t ar) V2F     Float   Float   Int32
instance IsTextureGradOffset  (Sampler2D t ar)      V2F     V2F     V2F     V2I  
instance IsTextureGradOffset  (Sampler2DArray t ar) V3F     V2F     V2F     V2I  
instance IsTextureGradOffset  (Sampler2DRect t ar)  V2F     V2F     V2F     V2I  
instance IsTextureGradOffset  (Sampler3D t ar)      V3F     V3F     V3F     V3I  
instance IsTextureGradOffset  Sampler1DArrayShadow  V3F     Float   Float   Int32
instance IsTextureGradOffset  Sampler1DShadow       V3F     Float   Float   Int32
instance IsTextureGradOffset  Sampler2DArrayShadow  V4F     V2F     V2F     V2I  
instance IsTextureGradOffset  Sampler2DRectShadow   V3F     V2F     V2F     V2I  
instance IsTextureGradOffset  Sampler2DShadow       V3F     V2F     V2F     V2I  

-- arity ok
class IsTextureProjGrad sampler coord dx dy | sampler coord -> dx dy
instance IsTextureProjGrad  (Sampler1D t ar)        V2F     Float   Float
instance IsTextureProjGrad  (Sampler1D t ar)        V4F     Float   Float
instance IsTextureProjGrad  (Sampler2D t ar)        V3F     V2F     V2F  
instance IsTextureProjGrad  (Sampler2D t ar)        V4F     V2F     V2F  
instance IsTextureProjGrad  (Sampler2DRect t ar)    V3F     V2F     V2F  
instance IsTextureProjGrad  (Sampler2DRect t ar)    V4F     V2F     V2F  
instance IsTextureProjGrad  (Sampler3D t ar)        V4F     V3F     V3F  
instance IsTextureProjGrad  Sampler1DShadow         V4F     Float   Float
instance IsTextureProjGrad  Sampler2DRectShadow     V4F     V2F     V2F  
instance IsTextureProjGrad  Sampler2DShadow         V4F     V2F     V2F  

-- arity ok
class IsTextureProjGradOffset sampler coord dx dy offset | sampler coord -> dx dy offset
instance IsTextureProjGradOffset  (Sampler1D t ar)      V2F     Float   Float   Int32
instance IsTextureProjGradOffset  (Sampler1D t ar)      V4F     Float   Float   Int32
instance IsTextureProjGradOffset  (Sampler2D t ar)      V3F     V2F     V2F     V2I  
instance IsTextureProjGradOffset  (Sampler2D t ar)      V4F     V2F     V2F     V2I  
instance IsTextureProjGradOffset  (Sampler2DRect t ar)  V3F     V2F     V2F     V2I  
instance IsTextureProjGradOffset  (Sampler2DRect t ar)  V4F     V2F     V2F     V2I  
instance IsTextureProjGradOffset  (Sampler3D t ar)      V4F     V3F     V3F     V3I  
instance IsTextureProjGradOffset  Sampler1DShadow       V4F     Float   Float   Int32
instance IsTextureProjGradOffset  Sampler2DRectShadow   V4F     V2F     V2F     V2I  
instance IsTextureProjGradOffset  Sampler2DShadow       V4F     V2F     V2F     V2I  
