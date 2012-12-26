module LC_U_APIType where

import Data.ByteString.Char8
import Data.Int
import Data.Word

import LC_G_Type

import LC_G_APIType

data OrderingType
    = Ordered
    | Unordered
    deriving (Show,Eq,Ord)

data InputType = InputType -- TODO
    deriving (Show,Eq,Ord)
data InputValue = InputValue -- TODO
    deriving (Show,Eq,Ord)

data FetchPrimitive
    = Points
    | LineStrip
    | LineLoop
    | Lines
    | TriangleStrip
    | TriangleFan
    | Triangles
    | LinesAdjacency
    | LineStripAdjacency
    | TrianglesAdjacency
    | TriangleStripAdjacency
    deriving (Show,Eq,Ord)

data OutputPrimitive
    = TrianglesOutput
    | LinesOutput
    | PointsOutput
    deriving (Show,Eq,Ord)

data ColorArity
    = Red
    | RG
    | RGB
    | RGBA
    deriving (Show,Eq,Ord)

data Blending
    = NoBlending
    | BlendLogicOp  LogicOperation
    | Blend         (BlendEquation, BlendEquation) 
                    ((BlendingFactor, BlendingFactor), (BlendingFactor, BlendingFactor))
                    V4F
    deriving (Show,Eq,Ord)

data RasterContext
    = PointCtx
    | LineCtx       Float ProvokingVertex
    | TriangleCtx   CullMode PolygonMode PolygonOffset ProvokingVertex
    deriving (Show, Eq, Ord)

data FragmentOperation
    = DepthOp       DepthFunction Bool
    | StencilOp     StencilTests StencilOps StencilOps
    | ColorOp       Blending InputValue
    deriving (Show, Eq, Ord)

data AccumulationContext
    = AccumulationContext
    { accViewportName   :: Maybe ByteString
    , accOperations     :: [FragmentOperation]
    }
    deriving (Show, Eq, Ord)

data Image
    = DepthImage    Int Float
    | StencilImage  Int Int32
    | ColorImage    Int InputValue
    deriving (Show, Eq, Ord)

data TextureDataType
    = FloatTexel    ColorArity
    | IntTexel      ColorArity
    | WordTexel     ColorArity
    | ShadowTexel
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
