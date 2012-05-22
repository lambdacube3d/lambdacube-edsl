module LC_U_APIType where

import Data.ByteString.Char8
import Data.Int
import Data.Word

import LCType

import LC_APIType

-- primitive types
data PrimitiveType  = Triangles | Lines | Points deriving (Show, Eq, Ord)
data Primitive  = TriangleStrip | TriangleList | TriangleFan | LineStrip | LineList | PointList deriving (Eq,Ord,Bounded,Enum,Show)

data Mip = Mip | NoMip | AutoMip deriving (Show,Eq,Ord)
data ColorArity = Red | RGB | RGBA deriving (Show,Eq,Ord)

data Blending
    = NoBlending
    | BlendLogicOp  LogicOperation
    | Blend         (BlendEquation, BlendEquation) 
                    ((BlendingFactor, BlendingFactor), (BlendingFactor, BlendingFactor))
                    V4F
    deriving (Show,Eq,Ord)

data Interpolated e
    = Flat          e
    | Smooth        e
    | NoPerspective e
    deriving (Show, Eq, Ord)

data RasterContext
    = PointCtx
    | LineCtx       Float ProvokingVertex
    | TriangleCtx   CullMode PolygonMode PolygonOffset ProvokingVertex
    deriving (Show, Eq, Ord)

data FragmentOperation
    = DepthOp       DepthFunction Bool
    | StencilOp     StencilTests StencilOps StencilOps
    | ColorOp       Blending [Bool]
    deriving (Show, Eq, Ord)

data Image
    = DepthImage    Int Float
    | StencilImage  Int Int32
    | ColorImage    Int V4F
    deriving (Show, Eq, Ord)

data TextureDataType
    = Float         ColorArity
    | Int           ColorArity
    | Word          ColorArity
    | Shadow
    deriving (Show, Eq, Ord)

data TextureType
    = Texture1D     TextureDataType Int
    | Texture2D     TextureDataType Int
    | Texture3D     TextureDataType
    | TextureCube   TextureDataType
    | Texture2DMS   TextureDataType Int
    | TextureBuffer TextureDataType
    deriving (Show, Eq, Ord)

data Texture gp
    = TextureSlot   ByteString TextureType
    | Texture       TextureType Mip [gp]
    deriving (Show, Eq, Ord)
