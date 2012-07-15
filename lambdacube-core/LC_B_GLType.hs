module LC_B_GLType where

import Data.ByteString.Char8 (ByteString)
import Data.IORef
import Data.Word
import Data.Map (Map)
--import Data.IntMap (IntMap)
import Data.Set (Set)
import Data.Trie (Trie)
import Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as MV

import Graphics.Rendering.OpenGL.Raw.Core32 (GLint, GLuint)

import LC_G_APIType
import LC_U_APIType
import LC_U_DeBruijn

{-
    we should store:
        generic attributer setters (default values in that case when an attribute is missing from a mesh)
        question:
            should we provide default attributes or should we require a full attribute description from the user?
          answer: the latter seems better idea!
        question:
            is a mesh object constant in vertex attributes? e.g. if we'd like to change the value of an attribute buffer or generic attribute
            then we have to swap the old mesh object with a new one.
          answer: seems a good and reasonable idea, because we can customize object rendering using per object uniforms!
        PUBLIC:
            - slotUniforms  :: Trie (Trie InputType)
            - slotStreams   :: Trie (PrimitiveType, Trie InputType)
            - uniformSetter :: Trie InputSetter
            - render        :: IO ()
            - dispose       :: IO ()
        INTERNAL:
            - object sets               :: Trie (TVar ObjectSet)
            - globalUniformSetup        :: Trie (GLint -> IO ())
            - uniform setup actions     :: Trie (STM (GLint -> IO (), InputSetter))
            - attribute setup actions   :: Trie (GLuint -> StreamSetter)

    note: if we'd like to support slot sharing between different sub gfx networks,
           then we have to check that they have the same primitve type and
           there is no type collision if we build the union of they stream input
          otherwise we can not support slot sharing.
          should we use namespaces in stream input names?
           e.g "slot name/atribute name" in this case we can store stream attribute descriptions in a single trie.
           or should we use trie of tries?
    discussion:
        how to handle sub network uniform/attribute naming
            - global all
            - namespace prefix in name
            - trie of tries
        temporary decision:
            i'll use global names

    minimal restricition for (global name) uniforms and attributes:
        a name sould bound to only one type!

    TODO:
        proper handling of attribute and uniform sharing between shader programs
            two alternatives:
                - use same mapping for every program (more efficient, but requires more gl features)
                - use custom mapping per program with custom attribute/uniform setter (less efficient, but more compatible/less restricitive)
-}

---------
-- API --
---------
{-
-- Buffer
    compileBuffer   :: [Array] -> IO Buffer
    bufferSize      :: Buffer -> Int
    arraySize       :: Buffer -> Int -> Int
    arrayType       :: Buffer -> Int -> ArrayType

-- Renderer
    compileRenderer :: GPOutput -> IO Renderer
    slotUniforms    :: Renderer -> Trie (Trie InputType)
    slotStreams     :: Renderer -> Trie (PrimitiveType, Trie InputType)
    uniformSetter   :: Renderer -> Trie InputSetter
    render          :: Renderer -> IO ()
    dispose         :: Renderer -> IO ()

-- Object
    addObject           :: Renderer -> ByteString -> Primitive -> Maybe (IndexStream Buffer) -> Trie (Stream Buffer) -> [ByteString] -> IO Object
    removeObject        :: Renderer -> Object -> IO ()
    objectUniformSetter :: Object -> Trie InputSetter
-}

data Renderer -- internal type
    = Renderer
    -- public
    { slotUniform           :: Trie (Trie InputType)
    , slotStream            :: Trie (PrimitiveType, Trie InputType)
    , uniformSetter         :: Trie InputSetter         -- global uniform
    , render                :: IO ()
    , dispose               :: IO ()
    , setScreenSize         :: Word -> Word -> IO ()

    -- internal
    , mkUniformSetup        :: Trie (GLint -> IO ())    -- global unifiorm
    , slotDescriptor        :: Trie SlotDescriptor
    , renderDescriptor      :: Map Exp RenderDescriptor --Map GP RenderDescriptor
    , renderState           :: RenderState
    , objectIDSeed          :: IORef Int
    }

data RenderDescriptor
    = RenderDescriptor
    { uniformLocation   :: Trie GLint   -- Uniform name -> GLint
    , streamLocation    :: Trie GLuint  -- Attribute name -> GLuint
    , renderAction      :: IO ()
    , disposeAction     :: IO ()
    , drawObjectsIORef  :: IORef ObjectSet  -- updated internally, according objectSet
                                            -- hint: Map is required to support slot sharing across different Accumulation nodes,
                                            --       because each node requires it's own render action list: [IO ()]
    }

data SlotDescriptor
    = SlotDescriptor
    { attachedGP        :: Set Exp
    , objectSet         :: IORef (Set Object)       -- objects, added to this slot (set by user)
    }

data ObjectSet
    = ObjectSet
    { drawObject    :: IO ()                -- synthetized/sorted render action
    , drawObjectMap :: Map Object (IO ())   -- original render actions
    }

data Object -- internal type
    = Object
    { objectSlotName        :: ByteString
    , objectUniformSetter   :: Trie InputSetter
    , objectID              :: Int
    , objectEnabledIORef    :: IORef Bool
    }

instance Eq Object where
    a == b  = objectID a == objectID b

instance Ord Object where
    a `compare` b  = objectID a `compare` objectID b

data RenderState
    = RenderState
    { textureUnitState  :: IOVector Int
    }

type StreamSetter = Stream Buffer -> IO ()

data Buffer -- internal type
    = Buffer
    { bufArrays :: V.Vector ArrayDesc
    , bufGLObj  :: GLuint
    }
    deriving (Show,Eq)

data ArrayDesc
    = ArrayDesc
    { arrType   :: ArrayType
    , arrLength :: Int  -- item count
    , arrOffset :: Int  -- byte position in buffer
    , arrSize   :: Int  -- size in bytes
    }
    deriving (Show,Eq)
