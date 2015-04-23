{-# LANGUAGE DeriveFunctor #-}
module IR where

import Data.Int
import Data.Word
import Data.Map
import Data.Vector (Vector)

data V2 a = V2 !a !a deriving (Eq,Ord,Show,Read,Functor)
data V3 a = V3 !a !a !a deriving (Eq,Ord,Show,Read,Functor)
data V4 a = V4 !a !a !a !a deriving (Eq,Ord,Show,Read,Functor)

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
    deriving (Read,Show,Eq,Ord)

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
    deriving (Read,Show,Eq,Ord)

data PointSpriteCoordOrigin = LowerLeft | UpperLeft deriving (Read,Show, Eq, Ord)
data PointSize              = PointSize Float | ProgramPointSize deriving (Read,Eq,Ord,Show)
data PolygonOffset          = NoOffset | Offset Float Float  deriving (Read,Eq,Ord,Show)
data FrontFace              = CCW | CW deriving (Read,Eq,Ord,Show)
data PolygonMode            = PolygonPoint PointSize | PolygonLine Float | PolygonFill deriving (Read,Eq,Ord,Show)
data ProvokingVertex        = FirstVertex | LastVertex deriving (Read,Eq,Ord,Bounded,Enum,Show)
data CullMode               = CullNone | CullFront FrontFace | CullBack FrontFace deriving (Read,Eq,Ord,Show)
type DepthFunction          = ComparisonFunction
data ComparisonFunction     = Never | Less | Equal | Lequal | Greater | Notequal | Gequal | Always deriving (Read, Eq, Ord, Show)
data StencilOperation       = OpZero | OpKeep | OpReplace | OpIncr | OpIncrWrap | OpDecr | OpDecrWrap | OpInvert deriving (Read, Eq, Ord, Show)
data BlendEquation          = FuncAdd | FuncSubtract | FuncReverseSubtract | Min | Max deriving (Read, Eq, Ord, Show)
data BlendingFactor         = Zero | One | SrcColor | OneMinusSrcColor | DstColor | OneMinusDstColor | SrcAlpha | OneMinusSrcAlpha | DstAlpha | OneMinusDstAlpha | ConstantColor | OneMinusConstantColor | ConstantAlpha | OneMinusConstantAlpha | SrcAlphaSaturate deriving (Read, Eq, Ord, Show)
data LogicOperation         = Clear | And | AndReverse | Copy | AndInverted | Noop | Xor | Or | Nor | Equiv | Invert | OrReverse | CopyInverted | OrInverted | Nand | Set deriving (Read, Eq, Ord, Show)

data StencilOps
    = StencilOps
    { frontStencilOp    :: StencilOperation -- ^ Used for front faced triangles and other primitives.
    , backStencilOp     :: StencilOperation -- ^ Used for back faced triangles.
    } deriving (Read,Eq,Ord,Show)

data StencilTests = StencilTests StencilTest StencilTest  deriving (Read,Eq,Ord,Show)
data StencilTest
    = StencilTest
    { stencilComparision    :: ComparisonFunction   -- ^ The function used to compare the @stencilReference@ and the stencil buffers value with.
    , stencilReference      :: Int32                -- ^ The value to compare with the stencil buffer's value.
    , stencilMask           :: Word32               -- ^ A bit mask with ones in each position that should be compared and written to the stencil buffer.
    } deriving (Read,Eq,Ord,Show)

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

data AccumulationContext
    = AccumulationContext
    { accViewportName   :: Maybe String
    , accOperations     :: [FragmentOperation]
    }
    deriving (Read,Show, Eq, Ord)

data Image
    = DepthImage    Int Float
    | StencilImage  Int Int32
    | ColorImage    Int Value
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
    | Texture2DMS   TextureDataType Int Int Bool
    | TextureBuffer TextureDataType
    deriving (Read,Show, Eq, Ord)

data MipMap
    = Mip           Int Int -- Base level, Max level
    | NoMip 
    | AutoMip       Int Int -- Base level, Max level
    deriving (Read,Show,Eq,Ord)

data Filter
    = Nearest
    | Linear
    | NearestMipmapNearest
    | NearestMipmapLinear
    | LinearMipmapNearest
    | LinearMipmapLinear
    deriving (Read,Show,Eq,Ord)

data EdgeMode
    = Repeat
    | MirroredRepeat
    | ClampToEdge
    | ClampToBorder
    deriving (Read,Show,Eq,Ord)

type ProgramName = Int
type TextureName = Int
type SamplerName = Int
type UniformName = String
type SlotName = Int
type FrameBufferComponent = Int
type TextureUnit = Int
type RenderTargetName = Int
type TextureUnitMapping = Map UniformName TextureUnit

data ImageRef
    = TextureImage  TextureName Int (Maybe Int)  -- Texture name, mip index, array index
    | Framebuffer   ImageSemantic
    deriving Show

data ImageSemantic
    = Depth
    | Stencil
    | Color
    deriving Show
{-
  TODO:
    transform feedback
    conditional rendering
-}
data Command
    = SetRasterContext          RasterContext
    | SetAccumulationContext    AccumulationContext
    | SetRenderTarget           RenderTargetName
    | SetProgram                ProgramName --TextureUnitMapping    -- adding texture unit map to set program command seems to be better solution than the current one
    | SetSamplerUniform         UniformName TextureUnit             -- hint: currently the texture unit mapping is encoded with this command
    | SetTexture                TextureUnit TextureName             -- binds texture to the specified texture unit
    | SetSampler                TextureUnit (Maybe SamplerName)     -- binds sampler to the specified texture unit
    | RenderSlot                SlotName
    | ClearRenderTarget         [(ImageSemantic,Value)]
    | GenerateMipMap            TextureUnit
    | SaveImage                 FrameBufferComponent ImageRef                            -- from framebuffer component to texture (image)
    | LoadImage                 ImageRef FrameBufferComponent                            -- from texture (image) to framebuffer component
    deriving Show
    -- | CopyImage                 ImageIndex ImageIndex               -- copy texture data from source to destination
    -- | Foreach                   ByteString [Command]            -- slot name, command to apply for each object

-- describes the static part of the network
data TextureDescriptor    -- texture size, type, array, mipmap
    = TextureDescriptor
    { textureType       :: TextureType
    , textureSize       :: Value
    , textureSemantic   :: ImageSemantic
    , textureSampler    :: SamplerDescriptor
    , textureBaseLevel  :: Int
    , textureMaxLevel   :: Int
    }
    deriving Show
{-
  texture state:
    - TEXTURE_BASE_LEVEL    - int
    - TEXTURE_MAX_LEVEL     - int
    - TEXTURE_SWIZZLE_R     - RED, GREEN, BLUE, ALPHA, ZERO, ONE
    - TEXTURE_SWIZZLE_G     - RED, GREEN, BLUE, ALPHA, ZERO, ONE
    - TEXTURE_SWIZZLE_B     - RED, GREEN, BLUE, ALPHA, ZERO, ONE
    - TEXTURE_SWIZZLE_A     - RED, GREEN, BLUE, ALPHA, ZERO, ONE
-}

data SamplerDescriptor
    = SamplerDescriptor
    { samplerWrapS          :: EdgeMode
    , samplerWrapT          :: Maybe EdgeMode
    , samplerWrapR          :: Maybe EdgeMode
    , samplerMinFilter      :: Filter
    , samplerMagFilter      :: Filter
    , samplerBorderColor    :: Value
    , samplerMinLod         :: Maybe Float
    , samplerMaxLod         :: Maybe Float
    , samplerLodBias        :: Float
    , samplerCompareFunc    :: Maybe ComparisonFunction
    }
    deriving (Read,Show,Eq,Ord)
{-
  sampler state:
    - TEXTURE_WRAP_S        - CLAMP_TO_EDGE, REPEAT, CLAMP_TO_BORDER, MIRRORED_REPEAT
    - TEXTURE_WRAP_T        - CLAMP_TO_EDGE, REPEAT, CLAMP_TO_BORDER, MIRRORED_REPEAT
    - TEXTURE_WRAP_R        - CLAMP_TO_EDGE, REPEAT, CLAMP_TO_BORDER, MIRRORED_REPEAT
    - TEXTURE_MIN_FILTER    - NEAREST, LINEAR, NEAREST_MIPMAP_NEAREST, NEAREST_MIPMAP_LINEAR, LINEAR_MIPMAP_NEAREST, LINEAR_MIPMAP_LINEAR
    - TEXTURE_MAG_FILTER    - NEAREST, LINEAR
    - TEXTURE_BORDER_COLOR  - any 4 values (float)
    - TEXTURE_MIN_LOD       - float
    - TEXTURE_MAX_LOD       - float
    - TEXTURE_LOD_BIAS      - float
    - TEXTURE_COMPARE_MODE  - NONE, COMPARE_REF_TO_TEXTURE
    - TEXTURE_COMPARE_FUNC  - LEQUAL, GEQUAL, LESS, GREATER, EQUAL, NOTEQUAL, ALWAYS, NEVER
-}

data Program    -- AST, input
    = Program
    { programUniforms   :: Map UniformName InputType    -- uniform input (value based uniforms only / no textures)
    , programStreams    :: Map UniformName (String,InputType)  -- vertex shader input attribute name -> (slot attribute name, attribute type)
    , programInTextures :: Map UniformName InputType               -- all textures (uniform textures and render textures) referenced by the program
    , programOutput     :: [(String,InputType)]
    , vertexShader      :: String
    , geometryShader    :: Maybe String
    , fragmentShader    :: String
    }
    deriving Show

data Slot       -- input, primitive type
    = Slot
    { slotName      :: String
    , slotUniforms  :: Map UniformName InputType
    , slotStreams   :: Map String InputType
    , slotPrimitive :: FetchPrimitive
    , slotPrograms  :: [ProgramName]
    }
    deriving Show

data RenderTarget
    = RenderTarget
    { renderTargets :: [(ImageSemantic,Maybe ImageRef)] -- render texture or default framebuffer (semantic, render texture for the program output)
    }
    deriving Show

data Pipeline
    = Pipeline
    { textures      :: Vector TextureDescriptor
    , samplers      :: Vector SamplerDescriptor
    , targets       :: Vector RenderTarget
    , programs      :: Vector Program
    , slots         :: Vector Slot
    , commands      :: [Command]
    }
    deriving Show

-- slot rendering
{-
    argument transformations:
        - batched constant update:  uniform block
        - batched stream update:    vertex array buffer (VAO)
        - batched rendering:        instancing
-}
