module Backend.GL.Data where

import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Data.IORef
import Data.List as L
import Data.Maybe
import Data.Trie as T
import Foreign 
--import qualified Data.IntMap as IM
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as V

--import Control.DeepSeq

import Graphics.Rendering.OpenGL.Raw.Core33
    ( GLuint
    -- * BUFFER related *
    -- buffer data
    , glBindBuffer
    , glBufferData
    , glBufferSubData
    , glGenBuffers
    , gl_ARRAY_BUFFER
    , gl_ELEMENT_ARRAY_BUFFER
    , gl_STATIC_DRAW

    -- * TEXTURE related *
    -- texture data
    , glBindTexture
    , glGenTextures
    , glGenerateMipmap
    , glPixelStorei
    , glTexImage2D
    , glTexParameteri
    , gl_CLAMP_TO_EDGE
    , gl_LINEAR
    , gl_LINEAR_MIPMAP_LINEAR
    , gl_REPEAT
    , gl_RGB
    , gl_RGBA
    , gl_RGBA8
    , gl_TEXTURE_2D
    , gl_TEXTURE_BASE_LEVEL
    , gl_TEXTURE_MAG_FILTER
    , gl_TEXTURE_MAX_LEVEL
    , gl_TEXTURE_MIN_FILTER
    , gl_TEXTURE_WRAP_S
    , gl_TEXTURE_WRAP_T
    , gl_UNPACK_ALIGNMENT
    , gl_UNSIGNED_BYTE
    )

import Data.Word
import Data.Bitmap.Pure

import Backend.GL.Type
import Backend.GL.Util

-- Buffer
compileBuffer :: [Array] -> IO Buffer
compileBuffer arrs = do
    let calcDesc (offset,setters,descs) (Array arrType cnt setter) =
          let size = cnt * sizeOfArrayType arrType
          in (size + offset, (offset,size,setter):setters, ArrayDesc arrType cnt offset size:descs)
        (bufSize,arrSetters,arrDescs) = foldl' calcDesc (0,[],[]) arrs
    bo <- alloca $! \pbo -> glGenBuffers 1 pbo >> peek pbo
    glBindBuffer gl_ARRAY_BUFFER bo
    glBufferData gl_ARRAY_BUFFER (fromIntegral bufSize) nullPtr gl_STATIC_DRAW
    forM_ arrSetters $! \(offset,size,setter) -> setter $! glBufferSubData gl_ARRAY_BUFFER (fromIntegral offset) (fromIntegral size)
    glBindBuffer gl_ARRAY_BUFFER 0
    return $! Buffer (V.fromList $! reverse arrDescs) bo

updateBuffer :: Buffer -> [(Int,Array)] -> IO ()
updateBuffer (Buffer arrDescs bo) arrs = do
    glBindBuffer gl_ARRAY_BUFFER bo
    forM arrs $ \(i,Array arrType cnt setter) -> do
        let ArrayDesc ty len offset size = arrDescs V.! i
        when (ty == arrType && cnt == len) $
            setter $! glBufferSubData gl_ARRAY_BUFFER (fromIntegral offset) (fromIntegral size)
    glBindBuffer gl_ARRAY_BUFFER 0

bufferSize :: Buffer -> Int
bufferSize = V.length . bufArrays

arraySize :: Buffer -> Int -> Int
arraySize buf arrIdx = arrLength $! bufArrays buf V.! arrIdx

arrayType :: Buffer -> Int -> ArrayType
arrayType buf arrIdx = arrType $! bufArrays buf V.! arrIdx

-- Texture

-- FIXME: Temporary implemenation
compileTexture2DRGBAF :: Bool -> Bool -> Bitmap Word8 -> IO TextureData
compileTexture2DRGBAF isMip isClamped bitmap = do
    glPixelStorei gl_UNPACK_ALIGNMENT 1
    to <- alloca $! \pto -> glGenTextures 1 pto >> peek pto
    glBindTexture gl_TEXTURE_2D to
    let (width,height) = bitmapSize bitmap
        wrapMode = case isClamped of
            True    -> gl_CLAMP_TO_EDGE
            False   -> gl_REPEAT
        (minFilter,maxLevel) = case isMip of
            False   -> (gl_LINEAR,0)
            True    -> (gl_LINEAR_MIPMAP_LINEAR, floor $ log (fromIntegral $ max width height) / log 2)
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fromIntegral wrapMode
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fromIntegral wrapMode
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral minFilter
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fromIntegral gl_LINEAR
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_BASE_LEVEL 0
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAX_LEVEL $ fromIntegral maxLevel
    withBitmap bitmap $ \(w,h) nchn 0 ptr -> do
        let internalFormat  = fromIntegral gl_RGBA8
            dataFormat      = fromIntegral $ case nchn of
                3   -> gl_RGB
                4   -> gl_RGBA
                _   -> error "unsupported texture format!"
        glTexImage2D gl_TEXTURE_2D 0 internalFormat (fromIntegral w) (fromIntegral h) 0 dataFormat gl_UNSIGNED_BYTE $ castPtr ptr
    when isMip $ glGenerateMipmap gl_TEXTURE_2D
    return $ TextureData to
