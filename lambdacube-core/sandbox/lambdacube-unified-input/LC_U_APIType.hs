module LC_U_APIType where

import Data.ByteString.Char8
import Data.Int
import Data.Word

import LC_G_LinearAlgebraTypes

import LC_G_APIType

data Ty = Ty FrequencyType ExpType
    deriving (Show,Eq,Ord)

data FrequencyType
    = Obj'
    | V'
    | G'
    | F'
    deriving (Show,Eq,Ord)

data OrderingType
    = Ordered'
    | Unordered'
    deriving (Show,Eq,Ord)

data PrimitiveType
    = Triangle'
    | Line'
    | Point'
    deriving (Show,Eq,Ord)

data AdjacencyType
    = Adjacency'
    | NoAdjacency'
    deriving (Show,Eq,Ord)

data DimensionType
    = DIM1'
    | DIM2'
    | DIM3'
    | DIM4'
    | Rect'
    deriving (Show,Eq,Ord)

data TextureArrayType
    = SingleTex'
    | ArrayTex'
    | CubeTex'
    deriving (Show,Eq,Ord)

{-
  TextureType
    Regular' a
    Shadow' a
    MultiSample' a
    Buffer' a
-}

data ColorArityType
    = Red'
    | RG'
    | RGB'
    | RGBA'
    deriving (Show,Eq,Ord)

{-
  TODO:
    - reify comutational frequency
    - add ViewportSize type: fullscreen, percentage, per pixel
    - add ViewportSize to accumulation context
-}
infixr 1 :->

data ExpType
    = Bool'
    | Float'
    | Int32'
    | Word32'
    | V2B'
    | V2F'
    | V2I'
    | V2U'
    | V3B'
    | V3F'
    | V3I'
    | V3U'
    | V4B'
    | V4F'
    | V4I'
    | V4U'
    | M22F'
    | M23F'
    | M24F'
    | M32F'
    | M33F'
    | M34F'
    | M42F'
    | M43F'
    | M44F'
    | ZZ'
    | Array'            OrderingType ExpType
    | Fragment'         ExpType
    | FragmentStream'   Int ExpType
{-
FrameBuffer' layerCount (t)
-}
    | GeometryShader'   PrimitiveType AdjacencyType PrimitiveType Int Int Int ExpType ExpType
{-
Image' layerCount e
-}
    | Interpolated'     ExpType
    | Output'
    | PrimitiveStream'  PrimitiveType AdjacencyType Int Int FrequencyType ExpType

{-
Sampler' dim arr t ar
-}
    | SamplerSetting'
{-
Texture' dim arr t ar
TextureSetting' dim arr layerCount t ar
-}
    | Tuple'            [ExpType]
    | Vertex'           ExpType ExpType
    | VertexStream'     PrimitiveType AdjacencyType ExpType
    | ExpType :-> ExpType
    deriving (Show,Eq,Ord)

data ExpValue -- TODO
    = Bool      Bool
    | Float     Float
    | Int32     Int32
    | Word32    Word32
    | V2B       V2B
    | V2F       V2F
    | V2I       V2I
    | V2U       V2U
    | V3B       V3B
    | V3F       V3F
    | V3I       V3I
    | V3U       V3U
    | V4B       V4B
    | V4F       V4F
    | V4I       V4I
    | V4U       V4U
    | M22F      M22F
    | M23F      M23F
    | M24F      M24F
    | M32F      M32F
    | M33F      M33F
    | M34F      M34F
    | M42F      M42F
    | M43F      M43F
    | M44F      M44F
    | ZZ
    | Array -- TODO
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
    { accDithering  :: Bool
    , accOperations :: [FragmentOperation]
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
