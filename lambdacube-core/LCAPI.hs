module LCAPI (
    -- language
    module LCType,
    module LCAPIType,
    module LCDSLType,
    module LCHOAS,

    -- backend
    Buffer,
    compileBuffer,
    bufferSize,
    arraySize,
    arrayType,

    Renderer,
    compileRenderer,
    slotUniform,
    slotStream,
    uniformSetter,
    render,
    dispose,

    Object,
    addObject,
    removeObject,
    objectUniformSetter,

    module TypeLevel.Number.Nat,
    module TypeLevel.Number.Nat.Num,
    Int32,
    Word32
) where

import LCType
import LCAPIType hiding (Buffer)
import LCDSLType
import LCHOAS
import LCGL
import LCGLUtil
import TypeLevel.Number.Nat
import TypeLevel.Number.Nat.Num
import Data.Int
import Data.Word

-- Backend should implement this functions
{-
---------
-- API --
---------

-- Buffer
compileBuffer   :: [Array] -> IO Buffer
bufferSize      :: Buffer -> Int        -- stored Array count
arraySize       :: Buffer -> Int -> Int -- array item count at the given index
arrayType       :: Buffer -> Int -> ArrayType

-- Renderer
compileRenderer :: H.GP (FrameBuffer c d s) -> IO Renderer
slotUniforms    :: Renderer -> Trie (Trie InputType)
slotStreams     :: Renderer -> Trie (PrimitiveType, Trie InputType)
uniformSetter   :: Renderer -> Trie InputSetter
render          :: Renderer -> IO ()
dispose        :: Renderer -> IO ()

-- Object
-- hint: user can define per object uniform and can define a mesh using vertex array or generic vertex attribute
addObject       :: Renderer -> ByteString -> Maybe (IndexStream Buffer) -> Trie (Stream Buffer) -> [ByteString] -> IO Object
removeObject    :: Renderer -> Object -> IO ()
objectUniforms  :: Object -> Trie InputSetter
-}
