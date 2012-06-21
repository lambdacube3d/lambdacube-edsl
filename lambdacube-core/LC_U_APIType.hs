module LC_U_APIType where

import Data.ByteString.Char8
import Data.Int
import Data.Word

import LC_G_Type

import LC_G_APIType

-- primitive types
data PrimitiveType  = Triangle | Line | Point deriving (Show, Eq, Ord)

data ColorArity = Red | RG | RGB | RGBA deriving (Show,Eq,Ord)

data Blending
    = NoBlending
    | BlendLogicOp  LogicOperation
    | Blend         (BlendEquation, BlendEquation) 
                    ((BlendingFactor, BlendingFactor), (BlendingFactor, BlendingFactor))
                    V4F
    deriving (Show,Eq,Ord)
{-
data Interpolated e
    = Flat          e
    | Smooth        e
    | NoPerspective e
    deriving (Show, Eq, Ord)
-}
data RasterContext
    = PointCtx
    | LineCtx       Float ProvokingVertex
    | TriangleCtx   CullMode PolygonMode PolygonOffset ProvokingVertex
    deriving (Show, Eq, Ord)

data FragmentOperation
    = DepthOp       DepthFunction Bool
    | StencilOp     StencilTests StencilOps StencilOps
    | ColorOp       Blending Value
    deriving (Show, Eq, Ord)

data Image
    = DepthImage    Int Float
    | StencilImage  Int Int32
    | ColorImage    Int Value
    deriving (Show, Eq, Ord)

data TextureDataType
    = FloatT        ColorArity
    | IntT          ColorArity
    | WordT         ColorArity
    | ShadowT
    deriving (Show, Eq, Ord)

data TextureType
    = Texture1D     TextureDataType Int
    | Texture2D     TextureDataType Int
    | Texture3D     TextureDataType
    | TextureCube   TextureDataType
    | TextureRect   TextureDataType
    | Texture2DMS   TextureDataType Int
    | TextureBuffer TextureDataType
    deriving (Show, Eq, Ord)

data MipMap
    = Mip           Int Int -- Base level, Max level
    | NoMip 
    | AutoMip       Int Int -- Base level, Max level
    deriving (Show,Eq,Ord)
{-
data Texture gp
    = TextureSlot   ByteString TextureType
    | Texture       TextureType Value MipMap [gp] -- hint: type, size, mip, data
    deriving (Show, Eq, Ord)
-}