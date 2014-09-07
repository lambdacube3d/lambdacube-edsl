module LC_U_APIType where

import Data.Data
import Data.ByteString.Char8
import Data.Int
import Data.Word

import LCType

import LC_APIType

-- primitive types
data PrimitiveType  = Triangle | Line | Point deriving (Show, Eq, Ord, Data,Typeable)
data Primitive  = TriangleStrip | TriangleList | TriangleFan | LineStrip | LineList | PointList deriving (Eq,Ord,Bounded,Enum,Show, Data,Typeable)

data MipMap = Mip | NoMip | AutoMip deriving (Show,Eq,Ord, Data,Typeable)
data ColorArity = Red | RG | RGB | RGBA deriving (Show,Eq,Ord, Data,Typeable)

data Blending
    = NoBlending
    | BlendLogicOp  LogicOperation
    | Blend         (BlendEquation, BlendEquation) 
                    ((BlendingFactor, BlendingFactor), (BlendingFactor, BlendingFactor))
                    V4F
    deriving (Show,Eq,Ord, Data,Typeable)

data Interpolated e
    = Flat          e
    | Smooth        e
    | NoPerspective e
    deriving (Show, Eq, Ord, Data,Typeable)

data RasterContext
    = PointCtx
    | LineCtx       Float ProvokingVertex
    | TriangleCtx   CullMode PolygonMode PolygonOffset ProvokingVertex
    deriving (Show, Eq, Ord, Data,Typeable)

data FragmentOperation
    = DepthOp       DepthFunction Bool
    | StencilOp     StencilTests StencilOps StencilOps
    | ColorOp       Blending Value
    deriving (Show, Eq, Ord, Data,Typeable)

data Image
    = DepthImage    Int Float
    | StencilImage  Int Int32
    | ColorImage    Int Value
    deriving (Show, Eq, Ord, Data,Typeable)

data TextureDataType
    = FloatT        ColorArity
    | IntT          ColorArity
    | WordT         ColorArity
    | ShadowT
    deriving (Show, Eq, Ord, Data,Typeable)

data TextureType
    = Texture1D     TextureDataType Int
    | Texture2D     TextureDataType Int
    | Texture3D     TextureDataType
    | TextureCube   TextureDataType
    | TextureRect   TextureDataType
    | Texture2DMS   TextureDataType Int
    | TextureBuffer TextureDataType
    deriving (Show, Eq, Ord, Data,Typeable)

data Texture gp
    = TextureSlot   ByteString TextureType
    | Texture       TextureType MipMap [gp]
    deriving (Show, Eq, Ord, Data,Typeable)
