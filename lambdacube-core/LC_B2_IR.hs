module LC_B2_IR where

import Data.ByteString.Char8 (ByteString)
import Data.Trie (Trie)
import Data.Vector (Vector)

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

data ImageIndex
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
    | SaveImage                 FrameBufferComponent ImageIndex                          -- from framebuffer component to texture (image)
    | LoadImage                 ImageIndex FrameBufferComponent                          -- from texture (image) to framebuffer component
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
    { renderTargets :: [(ImageSemantic,Maybe ImageIndex)]   -- render texture or default framebuffer (semantic, render texture for the program output)
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
