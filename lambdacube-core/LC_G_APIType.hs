module LC_G_APIType where

import Data.Data
import Data.Int
import Data.Word
import Foreign.Ptr

import LC_G_Type

import Graphics.Rendering.OpenGL.Raw.Core32 (GLuint)

data TextureData
    = TextureData
    { textureObject :: GLuint
    }

data Primitive  = TriangleStrip | TriangleList | TriangleFan | LineStrip | LineList | PointList deriving (Eq,Ord,Bounded,Enum,Show, Data,Typeable)

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
    deriving (Show,Eq,Ord, Data,Typeable)

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
    deriving (Show,Eq,Ord, Data,Typeable)

data Ty
    = Single !InputType
    | Tuple [Ty]
    | FrameBuffer'
    | Image'
    | PrimitiveStream'
    | VertexStream'
    | FragmentStream'
    | Unknown
    deriving (Show,Eq,Ord, Data,Typeable)

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
    deriving (Show,Eq,Ord, Data,Typeable)

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

data PointSpriteCoordOrigin = LowerLeft | UpperLeft deriving (Show, Eq, Ord)
data PointSize              = PointSize Float | ProgramPointSize deriving (Eq,Ord,Show, Data,Typeable)
data PolygonOffset          = NoOffset | Offset Float Float  deriving (Eq,Ord,Show, Data,Typeable)
data FrontFace              = CCW | CW deriving (Eq,Ord,Show, Data,Typeable)
data PolygonMode            = PolygonPoint PointSize | PolygonLine Float | PolygonFill deriving (Eq,Ord,Show, Data,Typeable)
data ProvokingVertex        = FirstVertex | LastVertex deriving (Eq,Ord,Bounded,Enum,Show, Data,Typeable)
data CullMode               = CullNone | CullFront FrontFace | CullBack FrontFace deriving (Eq,Ord,Show, Data,Typeable)
type DepthFunction          = ComparisonFunction
data ComparisonFunction     = Never | Less | Equal | Lequal | Greater | Notequal | Gequal | Always deriving ( Eq, Ord, Show, Data,Typeable )
data StencilOperation       = OpZero | OpKeep | OpReplace | OpIncr | OpIncrWrap | OpDecr | OpDecrWrap | OpInvert deriving ( Eq, Ord, Show, Data,Typeable )
data BlendEquation          = FuncAdd | FuncSubtract | FuncReverseSubtract | Min | Max deriving ( Eq, Ord, Show, Data,Typeable )
data BlendingFactor         = Zero | One | SrcColor | OneMinusSrcColor | DstColor | OneMinusDstColor | SrcAlpha | OneMinusSrcAlpha | DstAlpha | OneMinusDstAlpha | ConstantColor | OneMinusConstantColor | ConstantAlpha | OneMinusConstantAlpha | SrcAlphaSaturate deriving ( Eq, Ord, Show, Data,Typeable )
data LogicOperation         = Clear | And | AndReverse | Copy | AndInverted | Noop | Xor | Or | Nor | Equiv | Invert | OrReverse | CopyInverted | OrInverted | Nand | Set deriving ( Eq, Ord, Show, Data,Typeable )

data StencilOps
    = StencilOps
    { frontStencilOp    :: StencilOperation -- ^ Used for front faced triangles and other primitives.
    , backStencilOp     :: StencilOperation -- ^ Used for back faced triangles.
    } deriving (Eq,Ord,Show, Data,Typeable)

data StencilTests = StencilTests StencilTest StencilTest  deriving (Eq,Ord,Show, Data,Typeable)
data StencilTest
    = StencilTest
    { stencilComparision    :: ComparisonFunction   -- ^ The function used to compare the @stencilReference@ and the stencil buffers value with.
    , stencilReference      :: Int32                -- ^ The value to compare with the stencil buffer's value.
    , stencilMask           :: Word32               -- ^ A bit mask with ones in each position that should be compared and written to the stencil buffer.
    } deriving (Eq,Ord,Show, Data,Typeable)

-- sampler and texture specification
data Filter = PointFilter | LinearFilter    deriving (Show,Eq,Ord, Data,Typeable)
data EdgeMode = Wrap | Mirror | Clamp       deriving (Show,Eq,Ord, Data,Typeable)
