module LambdaCube.Core.Type where

import Data.Typeable

import Data.Int
import Data.Word

data V2 a = V2 !a !a deriving (Read,Eq,Ord,Show,Typeable)
data V3 a = V3 !a !a !a deriving (Read,Eq,Ord,Show,Typeable)
data V4 a = V4 !a !a !a !a deriving (Read,Eq,Ord,Show,Typeable)

-- matrices are stored in column major order
type M22F = V2 V2F
type M23F = V3 V2F
type M24F = V4 V2F
type M32F = V2 V3F
type M33F = V3 V3F
type M34F = V4 V3F
type M42F = V2 V4F
type M43F = V3 V4F
type M44F = V4 V4F

type V2F = V2 Float
type V3F = V3 Float
type V4F = V4 Float
type V2I = V2 Int32
type V3I = V3 Int32
type V4I = V4 Int32
type V2U = V2 Word32
type V3U = V3 Word32
type V4U = V4 Word32
type V2B = V2 Bool
type V3B = V3 Bool
type V4B = V4 Bool

-- GPU type value reification, needed for shader codegen
data Value
    = VBool     !Bool
    | VV2B      !V2B
    | VV3B      !V3B
    | VV4B      !V4B
    | VWord     !Word32
    | VV2U      !V2U
    | VV3U      !V3U
    | VV4U      !V4U
    | VInt      !Int32
    | VV2I      !V2I
    | VV3I      !V3I
    | VV4I      !V4I
    | VFloat    !Float
    | VV2F      !V2F
    | VV3F      !V3F
    | VV4F      !V4F
    | VM22F     !M22F
    | VM23F     !M23F
    | VM24F     !M24F
    | VM32F     !M32F
    | VM33F     !M33F
    | VM34F     !M34F
    | VM42F     !M42F
    | VM43F     !M43F
    | VM44F     !M44F
    deriving (Read,Typeable,Show,Eq,Ord)

data Ty
    = Single !InputType
    | Tuple [Ty]
    | Unknown String
    deriving (Read,Typeable,Show,Eq,Ord)

tySize :: Ty -> Int
tySize (Tuple a)  = sum $ map tySize a
tySize _ = 1

-- describes a stream type (in GPU side)
data InputType
    = Bool
    | V2B
    | V3B
    | V4B
    | Word
    | V2U
    | V3U
    | V4U
    | Int
    | V2I
    | V3I
    | V4I
    | Float
    | V2F
    | V3F
    | V4F
    | M22F
    | M23F
    | M24F
    | M32F
    | M33F
    | M34F
    | M42F
    | M43F
    | M44F
    -- shadow textures
    | STexture1D
    | STexture2D
    | STextureCube
    | STexture1DArray
    | STexture2DArray
    | STexture2DRect
    -- float textures
    | FTexture1D
    | FTexture2D
    | FTexture3D
    | FTextureCube
    | FTexture1DArray
    | FTexture2DArray
    | FTexture2DMS
    | FTexture2DMSArray
    | FTextureBuffer
    | FTexture2DRect
    -- int textures
    | ITexture1D
    | ITexture2D
    | ITexture3D
    | ITextureCube
    | ITexture1DArray
    | ITexture2DArray
    | ITexture2DMS
    | ITexture2DMSArray
    | ITextureBuffer
    | ITexture2DRect
    -- uint textures
    | UTexture1D
    | UTexture2D
    | UTexture3D
    | UTextureCube
    | UTexture1DArray
    | UTexture2DArray
    | UTexture2DMS
    | UTexture2DMSArray
    | UTextureBuffer
    | UTexture2DRect
    deriving (Read,Typeable,Show,Eq,Ord)

data PointSpriteCoordOrigin = LowerLeft | UpperLeft deriving (Read,Typeable,Show, Eq, Ord)
data PointSize              = PointSize Float | ProgramPointSize deriving (Read,Typeable,Eq,Ord,Show)
data PolygonOffset          = NoOffset | Offset Float Float  deriving (Read,Typeable,Eq,Ord,Show)
data FrontFace              = CCW | CW deriving (Read,Typeable,Eq,Ord,Show)
data PolygonMode            = PolygonPoint PointSize | PolygonLine Float | PolygonFill deriving (Read,Typeable,Eq,Ord,Show)
data ProvokingVertex        = FirstVertex | LastVertex deriving (Read,Typeable,Eq,Ord,Bounded,Enum,Show)
data CullMode               = CullNone | CullFront FrontFace | CullBack FrontFace deriving (Read,Typeable,Eq,Ord,Show)
type DepthFunction          = ComparisonFunction
data ComparisonFunction     = Never | Less | Equal | Lequal | Greater | Notequal | Gequal | Always deriving (Read,Typeable, Eq, Ord, Show )
data StencilOperation       = OpZero | OpKeep | OpReplace | OpIncr | OpIncrWrap | OpDecr | OpDecrWrap | OpInvert deriving (Read,Typeable, Eq, Ord, Show )
data BlendEquation          = FuncAdd | FuncSubtract | FuncReverseSubtract | Min | Max deriving (Read,Typeable, Eq, Ord, Show )
data BlendingFactor         = Zero | One | SrcColor | OneMinusSrcColor | DstColor | OneMinusDstColor | SrcAlpha | OneMinusSrcAlpha | DstAlpha | OneMinusDstAlpha | ConstantColor | OneMinusConstantColor | ConstantAlpha | OneMinusConstantAlpha | SrcAlphaSaturate deriving (Read,Typeable, Eq, Ord, Show )
data LogicOperation         = Clear | And | AndReverse | Copy | AndInverted | Noop | Xor | Or | Nor | Equiv | Invert | OrReverse | CopyInverted | OrInverted | Nand | Set deriving (Read,Typeable, Eq, Ord, Show )

data StencilOps
    = StencilOps
    { frontStencilOp    :: StencilOperation -- ^ Used for front faced triangles and other primitives.
    , backStencilOp     :: StencilOperation -- ^ Used for back faced triangles.
    } deriving (Read,Typeable,Eq,Ord,Show)

data StencilTests = StencilTests StencilTest StencilTest  deriving (Read,Typeable,Eq,Ord,Show)
data StencilTest
    = StencilTest
    { stencilComparision    :: ComparisonFunction   -- ^ The function used to compare the @stencilReference@ and the stencil buffers value with.
    , stencilReference      :: Int32                -- ^ The value to compare with the stencil buffer's value.
    , stencilMask           :: Word32               -- ^ A bit mask with ones in each position that should be compared and written to the stencil buffer.
    } deriving (Read,Typeable,Eq,Ord,Show)

-- sampler and texture specification
data Filter = PointFilter | LinearFilter    deriving (Read,Typeable,Show,Eq,Ord)
data EdgeMode = Repeat | MirroredRepeat | ClampToEdge | ClampToBorder       deriving (Read,Typeable,Show,Eq,Ord)

-- primitive types
data FetchPrimitive
    = Points
    | Lines
    | Triangles
    | LinesAdjacency
    | TrianglesAdjacency
    deriving (Read,Show,Eq,Ord)

data OutputPrimitive
    = TrianglesOutput
    | LinesOutput
    | PointsOutput
    deriving (Read,Show,Eq,Ord)

data ColorArity = Red | RG | RGB | RGBA deriving (Read,Show,Eq,Ord)

data Blending
    = NoBlending
    | BlendLogicOp  LogicOperation
    | Blend         (BlendEquation, BlendEquation) 
                    ((BlendingFactor, BlendingFactor), (BlendingFactor, BlendingFactor))
                    V4F
    deriving (Read,Show,Eq,Ord)

data RasterContext
    = PointCtx      PointSize Float PointSpriteCoordOrigin
    | LineCtx       Float ProvokingVertex
    | TriangleCtx   CullMode PolygonMode PolygonOffset ProvokingVertex
    deriving (Read,Show, Eq, Ord)

data FragmentOperation
    = DepthOp       DepthFunction Bool
    | StencilOp     StencilTests StencilOps StencilOps
    | ColorOp       Blending Value
    deriving (Read,Show, Eq, Ord)

data Image
    = DepthImage      Int Float
    | StencilImage    Int Int32
    | ColorImage      Int Value
    | UnclearedImage  Int
    deriving (Read,Show, Eq, Ord)

data TextureDataType
    = FloatT        ColorArity
    | IntT          ColorArity
    | WordT         ColorArity
    | ShadowT
    deriving (Read,Show, Eq, Ord)

data TextureType
    = Texture1D     TextureDataType Int
    | Texture2D     TextureDataType Int
    | Texture3D     TextureDataType
    | TextureCube   TextureDataType
    | TextureRect   TextureDataType
    | Texture2DMS   TextureDataType Int
    | TextureBuffer TextureDataType
    deriving (Read,Show, Eq, Ord)

data MipMap
    = Mip           Int Int -- Base level, Max level
    | NoMip 
    | AutoMip       Int Int -- Base level, Max level
    deriving (Read,Show,Eq,Ord)
