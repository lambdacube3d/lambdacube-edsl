module LC_APIType where

import Control.Concurrent.STM
import Data.ByteString.Char8
import Data.Int
import Data.Word
import Foreign.Ptr

import LCType

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
    deriving (Show,Eq,Ord)

type SetterFun a = a -> STM ()

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
    | SFTexture2D
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

-- describes a stream type (in GPU side)
data InputType
    = ITBool
    | ITV2B
    | ITV3B
    | ITV4B
    | ITWord
    | ITV2U
    | ITV3U
    | ITV4U
    | ITInt
    | ITV2I
    | ITV3I
    | ITV4I
    | ITFloat
    | ITV2F
    | ITV3F
    | ITV4F
    | ITM22F
    | ITM23F
    | ITM24F
    | ITM32F
    | ITM33F
    | ITM34F
    | ITM42F
    | ITM43F
    | ITM44F
    -- shadow textures
    | ITSTexture1D
    | ITSTexture2D
    | ITSTextureCube
    | ITSTexture1DArray
    | ITSTexture2DArray
    | ITSTexture2DRect
    -- float textures
    | ITFTexture1D
    | ITFTexture2D
    | ITFTexture3D
    | ITFTextureCube
    | ITFTexture1DArray
    | ITFTexture2DArray
    | ITFTexture2DMS
    | ITFTexture2DMSArray
    | ITFTextureBuffer
    | ITFTexture2DRect
    -- int textures
    | ITITexture1D
    | ITITexture2D
    | ITITexture3D
    | ITITextureCube
    | ITITexture1DArray
    | ITITexture2DArray
    | ITITexture2DMS
    | ITITexture2DMSArray
    | ITITextureBuffer
    | ITITexture2DRect
    -- uint textures
    | ITUTexture1D
    | ITUTexture2D
    | ITUTexture3D
    | ITUTextureCube
    | ITUTexture1DArray
    | ITUTexture2DArray
    | ITUTexture2DMS
    | ITUTexture2DMSArray
    | ITUTextureBuffer
    | ITUTexture2DRect
    deriving (Eq,Ord)

toStreamType :: InputType -> Maybe StreamType
toStreamType ITWord     = Just TWord
toStreamType ITV2U      = Just TV2U
toStreamType ITV3U      = Just TV3U
toStreamType ITV4U      = Just TV4U
toStreamType ITInt      = Just TInt
toStreamType ITV2I      = Just TV2I
toStreamType ITV3I      = Just TV3I
toStreamType ITV4I      = Just TV4I
toStreamType ITFloat    = Just TFloat
toStreamType ITV2F      = Just TV2F
toStreamType ITV3F      = Just TV3F
toStreamType ITV4F      = Just TV4F
toStreamType ITM22F     = Just TM22F
toStreamType ITM23F     = Just TM23F
toStreamType ITM24F     = Just TM24F
toStreamType ITM32F     = Just TM32F
toStreamType ITM33F     = Just TM33F
toStreamType ITM34F     = Just TM34F
toStreamType ITM42F     = Just TM42F
toStreamType ITM43F     = Just TM43F
toStreamType ITM44F     = Just TM44F
toStreamType _          = Nothing

fromStreamType :: StreamType -> InputType
fromStreamType TWord    = ITWord
fromStreamType TV2U     = ITV2U
fromStreamType TV3U     = ITV3U
fromStreamType TV4U     = ITV4U
fromStreamType TInt     = ITInt
fromStreamType TV2I     = ITV2I
fromStreamType TV3I     = ITV3I
fromStreamType TV4I     = ITV4I
fromStreamType TFloat   = ITFloat
fromStreamType TV2F     = ITV2F
fromStreamType TV3F     = ITV3F
fromStreamType TV4F     = ITV4F
fromStreamType TM22F    = ITM22F
fromStreamType TM23F    = ITM23F
fromStreamType TM24F    = ITM24F
fromStreamType TM32F    = ITM32F
fromStreamType TM33F    = ITM33F
fromStreamType TM34F    = ITM34F
fromStreamType TM42F    = ITM42F
fromStreamType TM43F    = ITM43F
fromStreamType TM44F    = ITM44F

instance Show InputType where
    show ITBool  = "Bool"
    show ITV2B   = "V2B"
    show ITV3B   = "V3B"
    show ITV4B   = "V4B"
    show ITWord  = "Word"
    show ITV2U   = "V2U"
    show ITV3U   = "V3U"
    show ITV4U   = "V4U"
    show ITInt   = "Int"
    show ITV2I   = "V2I"
    show ITV3I   = "V3I"
    show ITV4I   = "V4I"
    show ITFloat = "Float"
    show ITV2F   = "V2F"
    show ITV3F   = "V3F"
    show ITV4F   = "V4F"
    show ITM22F  = "M22F"
    show ITM23F  = "M23F"
    show ITM24F  = "M24F"
    show ITM32F  = "M32F"
    show ITM33F  = "M33F"
    show ITM34F  = "M34F"
    show ITM42F  = "M42F"
    show ITM43F  = "M43F"
    show ITM44F  = "M44F"
    -- shadow textures
    show ITSTexture1D           = "STexture1D"
    show ITSTexture2D           = "STexture2D"
    show ITSTextureCube         = "STextureCube"
    show ITSTexture1DArray      = "STexture1DArray"
    show ITSTexture2DArray      = "STexture2DArray"
    show ITSTexture2DRect       = "STexture2DRect"
    -- float textures
    show ITFTexture1D           = "FTexture1D"
    show ITFTexture2D           = "FTexture2D"
    show ITFTexture3D           = "FTexture3D"
    show ITFTextureCube         = "FTextureCube"
    show ITFTexture1DArray      = "FTexture1DArray"
    show ITFTexture2DArray      = "FTexture2DArray"
    show ITFTexture2DMS         = "FTexture2DMS"
    show ITFTexture2DMSArray    = "FTexture2DMSArray"
    show ITFTextureBuffer       = "FTextureBuffer"
    show ITFTexture2DRect       = "FTexture2DRect"
    -- int textures
    show ITITexture1D           = "ITexture1D"
    show ITITexture2D           = "ITexture2D"
    show ITITexture3D           = "ITexture3D"
    show ITITextureCube         = "ITextureCube"
    show ITITexture1DArray      = "ITexture1DArray"
    show ITITexture2DArray      = "ITexture2DArray"
    show ITITexture2DMS         = "ITexture2DMS"
    show ITITexture2DMSArray    = "ITexture2DMSArray"
    show ITITextureBuffer       = "ITextureBuffer"
    show ITITexture2DRect       = "ITexture2DRect"
    -- uint textures
    show ITUTexture1D           = "UTexture1D"
    show ITUTexture2D           = "UTexture2D"
    show ITUTexture3D           = "UTexture3D"
    show ITUTextureCube         = "UTextureCube"
    show ITUTexture1DArray      = "UTexture1DArray"
    show ITUTexture2DArray      = "UTexture2DArray"
    show ITUTexture2DMS         = "UTexture2DMS"
    show ITUTexture2DMSArray    = "UTexture2DMSArray"
    show ITUTextureBuffer       = "UTextureBuffer"
    show ITUTexture2DRect       = "UTexture2DRect"

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

data PointSize          = PointSize Float | PointSizeRast deriving (Eq,Ord,Show)
data PolygonOffset      = NoOffset | Offset Float Float  deriving (Eq,Ord,Show)
data FrontFace          = CCW | CW deriving (Eq,Ord,Show)
data PolygonMode        = PolygonPoint PointSize | PolygonLine Float | PolygonFill deriving (Eq,Ord,Show)
data ProvokingVertex    = FirstVertex | LastVertex deriving (Eq,Ord,Bounded,Enum,Show)
data CullMode           = CullNone | CullFront FrontFace | CullBack FrontFace deriving (Eq,Ord,Show)
type DepthFunction      = ComparisonFunction
data ComparisonFunction = Never | Less | Equal | Lequal | Greater | Notequal | Gequal | Always deriving ( Eq, Ord, Show )
data StencilOperation   = OpZero | OpKeep | OpReplace | OpIncr | OpIncrWrap | OpDecr | OpDecrWrap | OpInvert deriving ( Eq, Ord, Show )
data BlendEquation      = FuncAdd | FuncSubtract | FuncReverseSubtract | Min | Max deriving ( Eq, Ord, Show )
data BlendingFactor     = Zero | One | SrcColor | OneMinusSrcColor | DstColor | OneMinusDstColor | SrcAlpha | OneMinusSrcAlpha | DstAlpha | OneMinusDstAlpha | ConstantColor | OneMinusConstantColor | ConstantAlpha | OneMinusConstantAlpha | SrcAlphaSaturate deriving ( Eq, Ord, Show )
data LogicOperation     = Clear | And | AndReverse | Copy | AndInverted | Noop | Xor | Or | Nor | Equiv | Invert | OrReverse | CopyInverted | OrInverted | Nand | Set deriving ( Eq, Ord, Show )

data StencilOps
    = StencilOps
    { frontStencilOp    :: StencilOperation -- ^ Used for front faced triangles and other primitives.
    , backStencilOp     :: StencilOperation -- ^ Used for back faced triangles.
    } deriving (Eq,Ord,Show)

data StencilTests = StencilTests StencilTest StencilTest  deriving (Eq,Ord,Show)
data StencilTest
    = StencilTest
    { stencilComparision    :: ComparisonFunction   -- ^ The function used to compare the @stencilReference@ and the stencil buffers value with.
    , stencilReference      :: Int32                -- ^ The value to compare with the stencil buffer's value.
    , stencilMask           :: Word32               -- ^ A bit mask with ones in each position that should be compared and written to the stencil buffer.
    } deriving (Eq,Ord,Show)

-- sampler and texture specification
data Filter = PointFilter | LinearFilter    deriving (Show,Eq,Ord)
data EdgeMode = Wrap | Mirror | Clamp       deriving (Show,Eq,Ord)
