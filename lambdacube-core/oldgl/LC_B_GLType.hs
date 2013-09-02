module LC_B_GLType where

--import Data.IntMap (IntMap)
import Data.ByteString.Char8 (ByteString)
import Data.IORef
import Data.Int
import Data.Map (Map)
import Data.Set (Set)
import Data.Trie (Trie)
import Data.Vector.Unboxed.Mutable (IOVector)
import Data.Word
import Foreign.Ptr
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as MV

import Graphics.Rendering.OpenGL.Raw.Core32 (GLint, GLuint)

import LC_G_Type
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
    , slotStream            :: Trie (FetchPrimitive, Trie InputType)
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
    , fragmentOutCount  :: Int
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

----------------------
-- input related types
----------------------

data TextureData
    = TextureData
    { textureObject :: GLuint
    }

data Primitive
    = TriangleStrip
    | TriangleList
    | TriangleFan
    | LineStrip
    | LineList
    | PointList
    | TriangleStripAdjacency
    | TriangleListAdjacency
    | LineStripAdjacency
    | LineListAdjacency
    deriving (Eq,Ord,Bounded,Enum,Show)

type SetterFun a = a -> IO ()

-- user will provide scalar input data via this type
data InputSetter
    = SBool  (SetterFun Bool)
    | SV2B   (SetterFun V2B)
    | SV3B   (SetterFun V3B)
    | SV4B   (SetterFun V4B)
    | SWord  (SetterFun Word32)
    | SV2U   (SetterFun V2U)
    | SV3U   (SetterFun V3U)
    | SV4U   (SetterFun V4U)
    | SInt   (SetterFun Int32)
    | SV2I   (SetterFun V2I)
    | SV3I   (SetterFun V3I)
    | SV4I   (SetterFun V4I)
    | SFloat (SetterFun Float)
    | SV2F   (SetterFun V2F)
    | SV3F   (SetterFun V3F)
    | SV4F   (SetterFun V4F)
    | SM22F  (SetterFun M22F)
    | SM23F  (SetterFun M23F)
    | SM24F  (SetterFun M24F)
    | SM32F  (SetterFun M32F)
    | SM33F  (SetterFun M33F)
    | SM34F  (SetterFun M34F)
    | SM42F  (SetterFun M42F)
    | SM43F  (SetterFun M43F)
    | SM44F  (SetterFun M44F)
    -- shadow textures
    | SSTexture1D
    | SSTexture2D
    | SSTextureCube
    | SSTexture1DArray
    | SSTexture2DArray
    | SSTexture2DRect
    -- float textures
    | SFTexture1D
    | SFTexture2D           (SetterFun TextureData)
    | SFTexture3D
    | SFTextureCube
    | SFTexture1DArray
    | SFTexture2DArray
    | SFTexture2DMS
    | SFTexture2DMSArray
    | SFTextureBuffer
    | SFTexture2DRect
    -- int textures
    | SITexture1D
    | SITexture2D
    | SITexture3D
    | SITextureCube
    | SITexture1DArray
    | SITexture2DArray
    | SITexture2DMS
    | SITexture2DMSArray
    | SITextureBuffer
    | SITexture2DRect
    -- uint textures
    | SUTexture1D
    | SUTexture2D
    | SUTexture3D
    | SUTextureCube
    | SUTexture1DArray
    | SUTexture2DArray
    | SUTexture2DMS
    | SUTexture2DMSArray
    | SUTextureBuffer
    | SUTexture2DRect

-- buffer handling
{-
    user can fills a buffer (continuous memory region)
    each buffer have a data descriptor, what describes the
    buffer content. e.g. a buffer can contain more arrays of stream types
-}

-- user will provide stream data using this setup function
type BufferSetter = (Ptr () -> IO ()) -> IO ()

-- specifies array component type (stream type in storage side)
--  this type can be overridden in GPU side, e.g ArrWord8 can be seen as TFloat or TWord also
data ArrayType
    = ArrWord8
    | ArrWord16
    | ArrWord32
    | ArrInt8
    | ArrInt16
    | ArrInt32
    | ArrFloat
    | ArrHalf     -- Hint: half float is not supported in haskell
    deriving (Show,Eq,Ord)

sizeOfArrayType :: ArrayType -> Int
sizeOfArrayType ArrWord8  = 1
sizeOfArrayType ArrWord16 = 2
sizeOfArrayType ArrWord32 = 4
sizeOfArrayType ArrInt8   = 1
sizeOfArrayType ArrInt16  = 2
sizeOfArrayType ArrInt32  = 4
sizeOfArrayType ArrFloat  = 4
sizeOfArrayType ArrHalf   = 2

-- describes an array in a buffer
data Array  -- array type, element count (NOT byte size!), setter
    = Array ArrayType Int BufferSetter

-- dev hint: this should be InputType
--              we restrict StreamType using type class
-- subset of InputType, describes a stream type (in GPU side)
data StreamType
    = TWord
    | TV2U
    | TV3U
    | TV4U
    | TInt
    | TV2I
    | TV3I
    | TV4I
    | TFloat
    | TV2F
    | TV3F
    | TV4F
    | TM22F
    | TM23F
    | TM24F
    | TM32F
    | TM33F
    | TM34F
    | TM42F
    | TM43F
    | TM44F
    deriving (Show,Eq,Ord)

toStreamType :: InputType -> Maybe StreamType
toStreamType Word     = Just TWord
toStreamType V2U      = Just TV2U
toStreamType V3U      = Just TV3U
toStreamType V4U      = Just TV4U
toStreamType Int      = Just TInt
toStreamType V2I      = Just TV2I
toStreamType V3I      = Just TV3I
toStreamType V4I      = Just TV4I
toStreamType Float    = Just TFloat
toStreamType V2F      = Just TV2F
toStreamType V3F      = Just TV3F
toStreamType V4F      = Just TV4F
toStreamType M22F     = Just TM22F
toStreamType M23F     = Just TM23F
toStreamType M24F     = Just TM24F
toStreamType M32F     = Just TM32F
toStreamType M33F     = Just TM33F
toStreamType M34F     = Just TM34F
toStreamType M42F     = Just TM42F
toStreamType M43F     = Just TM43F
toStreamType M44F     = Just TM44F
toStreamType _          = Nothing

fromStreamType :: StreamType -> InputType
fromStreamType TWord    = Word
fromStreamType TV2U     = V2U
fromStreamType TV3U     = V3U
fromStreamType TV4U     = V4U
fromStreamType TInt     = Int
fromStreamType TV2I     = V2I
fromStreamType TV3I     = V3I
fromStreamType TV4I     = V4I
fromStreamType TFloat   = Float
fromStreamType TV2F     = V2F
fromStreamType TV3F     = V3F
fromStreamType TV4F     = V4F
fromStreamType TM22F    = M22F
fromStreamType TM23F    = M23F
fromStreamType TM24F    = M24F
fromStreamType TM32F    = M32F
fromStreamType TM33F    = M33F
fromStreamType TM34F    = M34F
fromStreamType TM42F    = M42F
fromStreamType TM43F    = M43F
fromStreamType TM44F    = M44F

-- user can specify streams using Stream type
-- a stream can be constant (ConstXXX) or can came from a buffer
data Stream b
    = ConstWord  Word32
    | ConstV2U   V2U
    | ConstV3U   V3U
    | ConstV4U   V4U
    | ConstInt   Int32
    | ConstV2I   V2I
    | ConstV3I   V3I
    | ConstV4I   V4I
    | ConstFloat Float
    | ConstV2F   V2F
    | ConstV3F   V3F
    | ConstV4F   V4F
    | ConstM22F  M22F
    | ConstM23F  M23F
    | ConstM24F  M24F
    | ConstM32F  M32F
    | ConstM33F  M33F
    | ConstM34F  M34F
    | ConstM42F  M42F
    | ConstM43F  M43F
    | ConstM44F  M44F
    | Stream 
        { streamType    :: StreamType
        , streamBuffer  :: b
        , streamArrIdx  :: Int
        , streamStart   :: Int
        , streamLength  :: Int
        }

-- stream of index values (for index buffer)
data IndexStream b
    = IndexStream
    { indexBuffer   :: b
    , indexArrIdx   :: Int
    , indexStart    :: Int
    , indexLength   :: Int
    }
