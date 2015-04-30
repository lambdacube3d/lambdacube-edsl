module IR where

import Data.Maybe
import Data.StrMap

type Int = Number
type Float = Number
type Int32 = Number
type Word32 = Number
type Bool = Boolean

data V2 a = V2 a a
data V3 a = V3 a a a
data V4 a = V4 a a a a

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
    = VBool     Bool
    | VV2B      V2B
    | VV3B      V3B
    | VV4B      V4B
    | VWord     Word32
    | VV2U      V2U
    | VV3U      V3U
    | VV4U      V4U
    | VInt      Int32
    | VV2I      V2I
    | VV3I      V3I
    | VV4I      V4I
    | VFloat    Float
    | VV2F      V2F
    | VV3F      V3F
    | VV4F      V4F
    | VM22F     M22F
    | VM23F     M23F
    | VM24F     M24F
    | VM32F     M32F
    | VM33F     M33F
    | VM34F     M34F
    | VM42F     M42F
    | VM43F     M43F
    | VM44F     M44F

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

data PointSpriteCoordOrigin = LowerLeft | UpperLeft
data PointSize              = PointSize Float | ProgramPointSize
data PolygonOffset          = NoOffset | Offset Float Float
data FrontFace              = CCW | CW
data PolygonMode            = PolygonPoint PointSize | PolygonLine Float | PolygonFill
data ProvokingVertex        = FirstVertex | LastVertex
data CullMode               = CullNone | CullFront FrontFace | CullBack FrontFace
type DepthFunction          = ComparisonFunction
data ComparisonFunction     = Never | Less | Equal | Lequal | Greater | Notequal | Gequal | Always
data StencilOperation       = OpZero | OpKeep | OpReplace | OpIncr | OpIncrWrap | OpDecr | OpDecrWrap | OpInvert
data BlendEquation          = FuncAdd | FuncSubtract | FuncReverseSubtract | Min | Max
data BlendingFactor         = Zero | One | SrcColor | OneMinusSrcColor | DstColor | OneMinusDstColor | SrcAlpha | OneMinusSrcAlpha | DstAlpha | OneMinusDstAlpha | ConstantColor | OneMinusConstantColor | ConstantAlpha | OneMinusConstantAlpha | SrcAlphaSaturate
data LogicOperation         = Clear | And | AndReverse | Copy | AndInverted | Noop | Xor | Or | Nor | Equiv | Invert | OrReverse | CopyInverted | OrInverted | Nand | Set

type StencilOps =
    { frontStencilOp    :: StencilOperation -- ^ Used for front faced triangles and other primitives.
    , backStencilOp     :: StencilOperation -- ^ Used for back faced triangles.
    }

data StencilTests = StencilTests StencilTest StencilTest
type StencilTest =
    { stencilComparision    :: ComparisonFunction   -- ^ The function used to compare the @stencilReference@ and the stencil buffers value with.
    , stencilReference      :: Int32                -- ^ The value to compare with the stencil buffer's value.
    , stencilMask           :: Word32               -- ^ A bit mask with ones in each position that should be compared and written to the stencil buffer.
    }

-- primitive types
data FetchPrimitive
    = Points
    | Lines
    | Triangles
    | LinesAdjacency
    | TrianglesAdjacency

data OutputPrimitive
    = TrianglesOutput
    | LinesOutput
    | PointsOutput

data ColorArity = Red | RG | RGB | RGBA

type BlendingFactorPair = {src :: BlendingFactor, dst :: BlendingFactor}
data Blending
    = NoBlending
    | BlendLogicOp  LogicOperation
    | Blend         {colorEq :: BlendEquation, alphaEq :: BlendEquation}
                    {colorF :: BlendingFactorPair, alphaF :: BlendingFactorPair}
                    V4F

data RasterContext
    = PointCtx      PointSize Float PointSpriteCoordOrigin
    | LineCtx       Float ProvokingVertex
    | TriangleCtx   CullMode PolygonMode PolygonOffset ProvokingVertex

data FragmentOperation
    = DepthOp       DepthFunction Bool
    | StencilOp     StencilTests StencilOps StencilOps
    | ColorOp       Blending Value

type AccumulationContext =
    { accViewportName   :: Maybe String
    , accOperations     :: [FragmentOperation]
    }

data Image
    = DepthImage    Int Float
    | StencilImage  Int Int32
    | ColorImage    Int Value

data TextureDataType
    = FloatT        ColorArity
    | IntT          ColorArity
    | WordT         ColorArity
    | ShadowT

data TextureType
    = Texture1D     TextureDataType Int
    | Texture2D     TextureDataType Int
    | Texture3D     TextureDataType
    | TextureCube   TextureDataType
    | TextureRect   TextureDataType
    | Texture2DMS   TextureDataType Int Int Bool
    | TextureBuffer TextureDataType

data MipMap
    = Mip           Int Int -- Base level, Max level
    | NoMip 
    | AutoMip       Int Int -- Base level, Max level

data Filter
    = Nearest
    | Linear
    | NearestMipmapNearest
    | NearestMipmapLinear
    | LinearMipmapNearest
    | LinearMipmapLinear

data EdgeMode
    = Repeat
    | MirroredRepeat
    | ClampToEdge
    | ClampToBorder

type ProgramName = Int
type TextureName = Int
type SamplerName = Int
type UniformName = String
type SlotName = Int
type FrameBufferComponent = Int
type TextureUnit = Int
type RenderTargetName = Int
type TextureUnitMapping = StrMap TextureUnit

data ImageRef
    = TextureImage  TextureName Int (Maybe Int)  -- Texture name, mip index, array index
    | Framebuffer   ImageSemantic

data ImageSemantic
    = Depth
    | Stencil
    | Color

data Command
    = SetRasterContext          RasterContext
    | SetAccumulationContext    AccumulationContext
    | SetRenderTarget           RenderTargetName
    | SetProgram                ProgramName --TextureUnitMapping    -- adding texture unit map to set program command seems to be better solution than the current one
    | SetSamplerUniform         UniformName TextureUnit             -- hint: currently the texture unit mapping is encoded with this command
    | SetTexture                TextureUnit TextureName             -- binds texture to the specified texture unit
    | SetSampler                TextureUnit (Maybe SamplerName)     -- binds sampler to the specified texture unit
    | RenderSlot                SlotName
    | ClearRenderTarget         [{semantic :: ImageSemantic, value :: Value}]
    | GenerateMipMap            TextureUnit
    | SaveImage                 FrameBufferComponent ImageRef                            -- from framebuffer component to texture (image)
    | LoadImage                 ImageRef FrameBufferComponent                            -- from texture (image) to framebuffer component

type TextureDescriptor =   -- texture size, type, array, mipmap
    { textureType       :: TextureType
    , textureSize       :: Value
    , textureSemantic   :: ImageSemantic
    , textureSampler    :: SamplerDescriptor
    , textureBaseLevel  :: Int
    , textureMaxLevel   :: Int
    }

type SamplerDescriptor =
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

type Program =   -- AST, input
    { programUniforms   :: StrMap InputType    -- uniform input (value based uniforms only / no textures)
    , programStreams    :: StrMap {name::String,ty::InputType}  -- vertex shader input attribute name -> (slot attribute name, attribute type)
    , programInTextures :: StrMap InputType               -- all textures (uniform textures and render textures) referenced by the program
    , programOutput     :: [{name::String,ty::InputType}]
    , vertexShader      :: String
    , geometryShader    :: Maybe String
    , fragmentShader    :: String
    }

type Slot =       -- input, primitive type
    { slotName      :: String
    , slotUniforms  :: StrMap InputType
    , slotStreams   :: StrMap InputType
    , slotPrimitive :: FetchPrimitive
    , slotPrograms  :: [ProgramName]
    }

type RenderTarget =
    { renderTargets :: [{semantic::ImageSemantic,ref::Maybe ImageRef}] -- render texture or default framebuffer (semantic, render texture for the program output)
    }

type Pipeline =
    { textures      :: [TextureDescriptor]
    , samplers      :: [SamplerDescriptor]
    , targets       :: [RenderTarget]
    , programs      :: [Program]
    , slots         :: [Slot]
    , commands      :: [Command]
    }
