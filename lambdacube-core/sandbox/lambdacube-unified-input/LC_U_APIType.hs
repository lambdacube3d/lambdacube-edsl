module LC_U_APIType where

import Data.ByteString.Char8
import Data.Int
import Data.Word

import LC_G_LinearAlgebraTypes

import LC_G_APIType

data OrderingType
    = Ordered'
    | Unordered'
    deriving (Show,Eq,Ord)

{-
  TODO:
    - reify comutational frequency
    - add ViewportSize type: fullscreen, percentage, per pixel
    - add ViewportSize to accumulation context
-}

{-
-- Exp types:
Bool'
Float'
Int32'
Word32'
V2B'
V2F'
V2I'
V2U'
V3B'
V3F'
V3I'
V3U'
V4B'
V4F'
V4I'
V4U'
M22F'
M23F'
M24F'
M32F'
M33F'
M34F'
M42F'
M43F'
M44F'
ZZ'
Array' orderType a
FragmentFilter' a
FragmentOut' a
FragmentStream' layerCount a
FrameBuffer' layerCount (t)
GeometryOut' a
GeometryShader' inputPrimitive inputAdjacency outputPrimitive layerCount a b
Image' layerCount e
Interpolated' e a
Output'
PrimitiveStream' primitive adjacency N1 V b
Sampler' dim arr t ar
SamplerSetting'
Texture' dim arr t ar
TextureSetting' dim arr layerCount t ar
Tuple' c a t
VertexOut' a
VertexStream' primitive adjacency a
-> (function)
----------------

-- Array types:
  OrderingType
    Ordered'
    Unordered'

-- GeometryShader types:
  PrimitiveType
    Triangle'
    Line'
    Point'

  AdjacencyType
    Adjacency'
    NoAdjacency'
    
-- Sampler and Texture types:
  DimensionType
    DIM1'
    DIM2'
    DIM3'
    DIM4'
    Rect'

  TextureArrayType
    SingleTex'
    ArrayTex'
    CubeTex'

  TextureType
    Regular' a
    Shadow' a
    MultiSample' a
    Buffer' a

  ColorArityType
    Red'
    RG'
    RGB'
    RGBA'
-}
data ExpType = ExpType -- TODO
    deriving (Show,Eq,Ord)
data ExpValue = ExpValue -- TODO
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
    | ColorOp       Blending ExpValue
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
    | ColorImage    Int ExpValue
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
