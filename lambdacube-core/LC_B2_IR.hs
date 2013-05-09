module LC_B2_IR where

import Data.ByteString.Char8 (ByteString)
import Data.Trie (Trie)
import qualified Data.Trie as T

import LC_G_APIType
import LC_U_APIType
import LC_U_DeBruijn

data ImageIndex
    = TextureImage  ByteString Int (Maybe Int)  -- Texture name, mip index, array index
    | Framebuffer   ImageSemantic

data ImageSemantic
    = Depth
    | Stencil
    | Color

{-
  TODO:
    transform feedback
    conditional rendering
-}
data Command
    = SetRasterContext          RasterContext
    | SetAccumulationContext    AccumulationContext
    | SetRenderTarget           [(ImageSemantic,Maybe ImageIndex)]      -- render texture or default framebuffer (semantic, render texture for the program output)
    | SetProgram                ByteString [(ByteString,ByteString)]    -- program name and input texture names (mapping: texture argument name -> texture resource name)
    | RenderSlot                ByteString                              -- slot name
    | ClearRenderTarget         [(ImageSemantic,Value)]
    | GenerateMips              ByteString                              -- texture name
    | SaveImage                 Int ImageIndex                          -- from framebuffer component to texture (image)
    | LoadImage                 ImageIndex Int                          -- from texture (image) to framebuffer component

    -- | CopyImage                 ImageIndex ImageIndex               -- copy texture data from source to destination
    -- | Foreach                   ByteString [Command]            -- slot name, command to apply for each object

-- describes the static part of the network
data TextureDescriptor    -- texture size, type, array, mipmap
    = TextureDescriptor
    { textureType       :: TextureType
    , textureSize       :: Value
    , textureMipMap     :: MipMap
    , textureSemantic   :: ImageSemantic
    }

data Program    -- AST, input
    = Program
    { programUniforms   :: Trie InputType
    , programStreams    :: Trie (ByteString,InputType) -- vertex shader input attribute name -> (slot attribute name, attribute type)
    , programInTextures :: Trie InputType
    , programOutput     :: [(ByteString,InputType)]
    , vertexShader      :: ByteString
    , geometryShader    :: Maybe ByteString
    , fragmentShader    :: ByteString
    }

data Slot       -- input, primitive type
    = Slot
    { slotUniforms  :: Trie InputType
    , slotStreams   :: Trie InputType
    , slotPrimitive :: FetchPrimitive
    }

data Pipeline
    = Pipeline
    { textures      :: Trie TextureDescriptor
    , programs      :: Trie Program
    , slots         :: Trie Slot
    , commands      :: [Command]
    }

-- slot rendering
{-
    argument transformations:
        - batched constant update:  uniform block
        - batched stream update:    vertex array buffer (VAO)
        - batched rendering:        instancing
-}
