module LC_B2_IR where

import Data.ByteString.Char8 (ByteString,pack,unpack)
import Data.Trie (Trie,toList,fromList)
import Data.Vector (Vector)
import Data.Aeson hiding (Value)-- (ToJSON(..),FromJSON(..))
import Data.Aeson.TH
import Control.Applicative

import LC_G_Type
import LC_G_APIType
import LC_U_APIType

type ProgramName = Int
type TextureName = Int
type SamplerName = Int
type UniformName = ByteString
type SlotName = Int
type FrameBufferComponent = Int
type TextureUnit = Int
type RenderTargetName = Int
type TextureUnitMapping = Trie TextureUnit

data TargetPlatform
    = OpenGL33
    | WebGL
    deriving (Show, Eq, Ord)

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
    deriving (Show,Eq,Ord)
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
    { programUniforms   :: Trie InputType               -- uniform input (value based uniforms only / no textures)
    , programStreams    :: Trie (ByteString,InputType)  -- vertex shader input attribute name -> (slot attribute name, attribute type)
    , programInTextures :: Trie InputType               -- all textures (uniform textures and render textures) referenced by the program
    , programOutput     :: [(ByteString,InputType)]
    , vertexShader      :: ByteString
    , geometryShader    :: Maybe ByteString
    , fragmentShader    :: ByteString
    }
    deriving Show

data Slot       -- input, primitive type
    = Slot
    { slotName      :: ByteString
    , slotUniforms  :: Trie InputType
    , slotStreams   :: Trie InputType
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
    , platform      :: TargetPlatform
    }
    deriving Show

-- slot rendering
{-
    argument transformations:
        - batched constant update:  uniform block
        - batched stream update:    vertex array buffer (VAO)
        - batched rendering:        instancing
-}
instance FromJSON ByteString where
    parseJSON v = pack <$> parseJSON v

instance ToJSON ByteString where
    toJSON v = toJSON $ unpack v

instance FromJSON a => FromJSON (Trie a) where
    parseJSON v = fromList <$> parseJSON v

instance ToJSON a => ToJSON (Trie a) where
    toJSON v = toJSON $ toList v

$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''V2)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''V3)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''V4)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''BlendEquation)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''BlendingFactor)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''LogicOperation)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''StencilOperation)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''StencilTest)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''FrontFace)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''ColorArity)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''StencilTests)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''CullMode)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''StencilOps)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''Blending)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''PolygonMode)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''EdgeMode)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''TextureDataType)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''FragmentOperation)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''PointSize)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''PolygonOffset)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''Filter)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''PointSpriteCoordOrigin)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''ProvokingVertex)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''ImageSemantic)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''TextureType)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''ComparisonFunction)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''Value)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''InputType)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''FetchPrimitive)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''AccumulationContext)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''RasterContext)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''ImageRef)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''TextureDescriptor)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''SamplerDescriptor)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''RenderTarget)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''Program)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''Slot)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''Command)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''TargetPlatform)
$(deriveJSON defaultOptions {sumEncoding = TwoElemArray} ''Pipeline)
