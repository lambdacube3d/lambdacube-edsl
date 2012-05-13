module LCAPIType where

import Control.Concurrent.STM
import Data.ByteString.Char8
import Data.Int
import Data.Typeable
import Data.Word
import Foreign.Ptr

import TypeLevel.Number.Nat
import TypeLevel.Number.Nat.Num

import LCType

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

-- user can define stream input using InputTuple type class
class InputTuple tup where
    type InputTupleRepr tup
    toInputList :: tup -> [(ByteString,InputType)]

instance InputTuple (Input a) where
    type InputTupleRepr (Input a) = a
    toInputList a = [toInput a]

instance InputTuple (Input a, Input b) where
    type InputTupleRepr (Input a, Input b) = (a, b)
    toInputList (a, b) = [toInput a, toInput b]

instance InputTuple (Input a, Input b, Input c) where
    type InputTupleRepr (Input a, Input b, Input c) = (a, b, c)
    toInputList (a, b, c) = [toInput a, toInput b, toInput c]

instance InputTuple (Input a, Input b, Input c, Input d) where
    type InputTupleRepr (Input a, Input b, Input c, Input d) = (a, b, c, d)
    toInputList (a, b, c, d) = [toInput a, toInput b, toInput c, toInput d]

instance InputTuple (Input a, Input b, Input c, Input d, Input e) where
    type InputTupleRepr (Input a, Input b, Input c, Input d, Input e) = (a, b, c, d, e)
    toInputList (a, b, c, d, e) = [toInput a, toInput b, toInput c, toInput d, toInput e]

instance InputTuple (Input a, Input b, Input c, Input d, Input e, Input f) where
    type InputTupleRepr (Input a, Input b, Input c, Input d, Input e, Input f) = (a, b, c, d, e, f)
    toInputList (a, b, c, d, e, f) = [toInput a, toInput b, toInput c, toInput d, toInput e, toInput f]

instance InputTuple (Input a, Input b, Input c, Input d, Input e, Input f, Input g) where
    type InputTupleRepr (Input a, Input b, Input c, Input d, Input e, Input f, Input g) = (a, b, c, d, e, f, g)
    toInputList (a, b, c, d, e, f, g) = [toInput a, toInput b, toInput c, toInput d, toInput e, toInput f, toInput g]

instance InputTuple (Input a, Input b, Input c, Input d, Input e, Input f, Input g, Input h) where
    type InputTupleRepr (Input a, Input b, Input c, Input d, Input e, Input f, Input g, Input h) = (a, b, c, d, e, f, g, h)
    toInputList (a, b, c, d, e, f, g, h) = [toInput a, toInput b, toInput c, toInput d, toInput e, toInput f, toInput g, toInput h]

instance InputTuple (Input a, Input b, Input c, Input d, Input e, Input f, Input g, Input h, Input i) where
    type InputTupleRepr (Input a, Input b, Input c, Input d, Input e, Input f, Input g, Input h, Input i) = (a, b, c, d, e, f, g, h, i)
    toInputList (a, b, c, d, e, f, g, h, i) = [toInput a, toInput b, toInput c, toInput d, toInput e, toInput f, toInput g, toInput h, toInput i]

-- we should define all of input types
-- supported stream input types (the ByteString argument is the input slot name)
data Input a where
    IBool   :: ByteString -> Input Bool
    IV2B    :: ByteString -> Input V2B
    IV3B    :: ByteString -> Input V3B
    IV4B    :: ByteString -> Input V4B
    IWord   :: ByteString -> Input Word32
    IV2U    :: ByteString -> Input V2U
    IV3U    :: ByteString -> Input V3U
    IV4U    :: ByteString -> Input V4U
    IInt    :: ByteString -> Input Int32
    IV2I    :: ByteString -> Input V2I
    IV3I    :: ByteString -> Input V3I
    IV4I    :: ByteString -> Input V4I
    IFloat  :: ByteString -> Input Float
    IV2F    :: ByteString -> Input V2F
    IV3F    :: ByteString -> Input V3F
    IV4F    :: ByteString -> Input V4F
    IM22F   :: ByteString -> Input M22F
    IM23F   :: ByteString -> Input M23F
    IM24F   :: ByteString -> Input M24F
    IM32F   :: ByteString -> Input M32F
    IM33F   :: ByteString -> Input M33F
    IM34F   :: ByteString -> Input M34F
    IM42F   :: ByteString -> Input M42F
    IM43F   :: ByteString -> Input M43F
    IM44F   :: ByteString -> Input M44F

toInput :: Input a -> (ByteString,InputType)
toInput (IBool  n) = (n, ITBool)
toInput (IV2B   n) = (n, ITV2B)
toInput (IV3B   n) = (n, ITV3B)
toInput (IV4B   n) = (n, ITV4B)
toInput (IWord  n) = (n, ITWord)
toInput (IV2U   n) = (n, ITV2U)
toInput (IV3U   n) = (n, ITV3U)
toInput (IV4U   n) = (n, ITV4U)
toInput (IInt   n) = (n, ITInt)
toInput (IV2I   n) = (n, ITV2I)
toInput (IV3I   n) = (n, ITV3I)
toInput (IV4I   n) = (n, ITV4I)
toInput (IFloat n) = (n, ITFloat)
toInput (IV2F   n) = (n, ITV2F)
toInput (IV3F   n) = (n, ITV3F)
toInput (IV4F   n) = (n, ITV4F)
toInput (IM22F  n) = (n, ITM22F)
toInput (IM23F  n) = (n, ITM23F)
toInput (IM24F  n) = (n, ITM24F)
toInput (IM32F  n) = (n, ITM32F)
toInput (IM33F  n) = (n, ITM33F)
toInput (IM34F  n) = (n, ITM34F)
toInput (IM42F  n) = (n, ITM42F)
toInput (IM43F  n) = (n, ITM43F)
toInput (IM44F  n) = (n, ITM44F)

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

-- primitive types
-- TODO: primitive types should be simplified using GADTs
data PrimitiveType  = Triangles | Lines | Points deriving (Show, Eq, Ord)

data Triangle   = Triangle deriving (Show, Eq, Ord)
data Line       = Line     deriving (Show, Eq, Ord)
data Point      = Point    deriving (Show, Eq, Ord)

data Primitive  = TriangleStrip | TriangleList | TriangleFan | LineStrip | LineList | PointList deriving (Eq,Ord,Bounded,Enum,Show)

class Show p => IsPrimitive p where
    toPrimitive :: p -> PrimitiveType

instance IsPrimitive Triangle where
    toPrimitive _ = Triangles
instance IsPrimitive Line where
    toPrimitive _ = Lines
instance IsPrimitive Point where
    toPrimitive _ = Points

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

data Blending c where
    NoBlending      :: Blending c

    BlendLogicOp    :: IsIntegral c
                    => LogicOperation
                    -> Blending c

    -- FIXME: restrict BlendingFactor at some BlendEquation
    Blend           :: (BlendEquation, BlendEquation) 
                    -> ((BlendingFactor, BlendingFactor), (BlendingFactor, BlendingFactor))
                    -> V4F
                    -> Blending Float

-- abstract types, used in language AST
data VertexStream prim t
data PrimitiveStream prim t
data FragmentStream layerCount t


-- flat tuple, another internal tuple representation

-- means unit
data ZZ = ZZ deriving (Typeable, Show)

-- used for tuple type description
infixr 1 :+:
data tail :+: head = !tail :+: !head deriving (Typeable, Show)

-- used for tuple value description
infixr 1 :.
data FlatTuple c a t where
    ZT      :: FlatTuple c a ZZ

    (:.)    :: c t
            => a t
            -> FlatTuple c a t'
            -> FlatTuple c a (t :+: t')

-- vertex attribute interpolation
data Interpolated e a where
    Flat            :: e a -> Interpolated e a

    Smooth          :: IsFloating a
                    => e a -> Interpolated e a

    NoPerspective   :: IsFloating a
                    => e a -> Interpolated e a

-- framebuffer data / fragment output semantic
data Color a    deriving Typeable
data Depth a    deriving Typeable
data Stencil a  deriving Typeable

-- TODO: needed to describe geometry shader input 
data PrimitiveVertices prim a


-- raster context description
-- TODO: add context parameters
data RasterContext t where
    PointCtx    :: RasterContext Point      -- TODO: PointSize, POINT_FADE_THRESHOLD_SIZE, POINT_SPRITE_COORD_ORIGIN

    LineCtx     :: 
        { ctxLineWidth          :: Float
        , ctxProvokingVertex'   :: ProvokingVertex
        } -> RasterContext Line

    TriangleCtx ::
        { ctxCullMode           :: CullMode
        , ctxPolygonMode        :: PolygonMode
        , ctxPolygonOffset      :: PolygonOffset
        , ctxProvokingVertex    :: ProvokingVertex
        } -> RasterContext Triangle

-- default triangle raster context
triangleCtx :: RasterContext Triangle
triangleCtx = TriangleCtx CullNone PolygonFill NoOffset LastVertex

type FrameBuffer layerCount t = FlatTuple Typeable (Image layerCount) t
type FragmentContext t = FlatTuple Typeable FragmentOperation t

-- Fragment Operation
data FragmentOperation ty where
    DepthOp         :: DepthFunction
                    -> Bool     -- depth write
                    -> FragmentOperation (Depth Float)

    StencilOp       :: StencilTests
                    -> StencilOps
                    -> StencilOps
                    -> FragmentOperation (Stencil Int32)

    ColorOp         :: (IsVecScalar d mask Bool, IsVecScalar d color c, IsNum c)
                    => Blending c   -- blending type
                    -> mask         -- write mask
                    -> FragmentOperation (Color color)
    deriving Typeable

-- specifies an empty image (pixel rectangle)
-- hint: framebuffer is composed from images
data Image layerCount t where
    DepthImage      :: Nat layerCount
                    => layerCount
                    -> Float    -- initial value
                    -> Image layerCount (Depth Float)

    StencilImage    :: Nat layerCount
                    => layerCount
                    -> Int32    -- initial value
                    -> Image layerCount (Stencil Int32)

    ColorImage      :: (IsNum t, IsVecScalar d color t, Nat layerCount)
                    => layerCount
                    -> color    -- initial value
                    -> Image layerCount (Color color)
    deriving Typeable

-- restriction for framebuffer structure according content semantic
-- supported configurations: optional stencil + optional depth + [zero or more color]
class IsColorOutput a
instance IsColorOutput ZZ
instance (IsColorOutput b) => IsColorOutput (Color c :+: b)

class IsValidOutput a
instance (IsColorOutput a) => IsValidOutput (Color c :+: a)
instance (IsColorOutput a) => IsValidOutput (Depth d :+: a)
instance (IsColorOutput a) => IsValidOutput (Stencil s :+: a)
instance (IsColorOutput a) => IsValidOutput (Stencil s :+: Depth d :+: a)

-- helper class (type level function), used in language AST
-- converts FlatTuple type to ordinary tuple type
type family FTRepr a :: *
type instance FTRepr (a :+: ZZ) = a
type instance FTRepr (a :+: b :+: ZZ) = (a, b)
type instance FTRepr (a :+: b :+: c :+: ZZ) = (a, b, c)
type instance FTRepr (a :+: b :+: c :+: d :+: ZZ) = (a, b, c, d)
type instance FTRepr (a :+: b :+: c :+: d :+: e :+: ZZ) = (a, b, c, d, e)
type instance FTRepr (a :+: b :+: c :+: d :+: e :+: f :+: ZZ) = (a, b, c, d, e, f)
type instance FTRepr (a :+: b :+: c :+: d :+: e :+: f :+: g :+: ZZ) = (a, b, c, d, e, f, g)

-- helper type level function, used in language AST
type family FTRepr' a :: *
type instance FTRepr' (i1 a :+: ZZ) = a
type instance FTRepr' (i1 a :+: i2 b :+: ZZ) = (a, b)
type instance FTRepr' (i1 a :+: i2 b :+: i3 c :+: ZZ) = (a, b, c)
type instance FTRepr' (i1 a :+: i2 b :+: i3 c :+: i4 d :+: ZZ) = (a, b, c, d)
type instance FTRepr' (i1 a :+: i2 b :+: i3 c :+: i4 d :+: i5 e :+: ZZ) = (a, b, c, d, e)
type instance FTRepr' (i1 a :+: i2 b :+: i3 c :+: i4 d :+: i5 e :+: i6 f :+: ZZ) = (a, b, c, d, e, f)
type instance FTRepr' (i1 a :+: i2 b :+: i3 c :+: i4 d :+: i5 e :+: i6 f :+: i7 g :+: ZZ) = (a, b, c, d, e, f, g)

-- helper type level function, used in language AST
type family ColorRepr a :: *
type instance ColorRepr ZZ = ZZ
type instance ColorRepr (a :+: b) = Color a :+: (ColorRepr b)

-- helper type level function, used in language AST
type family NoStencilRepr a :: *
type instance NoStencilRepr ZZ = ZZ
type instance NoStencilRepr (Stencil a :+: b) = NoStencilRepr b
type instance NoStencilRepr (Color a :+: b) = Color a :+: (NoStencilRepr b)
type instance NoStencilRepr (Depth a :+: b) = Depth a :+: (NoStencilRepr b)

-- sampler and texture specification
data Filter = PointFilter | LinearFilter    deriving (Eq,Ord)
data EdgeMode = Wrap | Mirror | Clamp       deriving (Eq,Ord)

data Sampler dim layerCount t ar deriving Typeable
instance Show (Sampler dim layerCount t ar) where
    show _ = "Sampler dim layerCount t ar"

data Mip     = Mip
data NoMip   = NoMip
data AutoMip = AutoMip

class IsMip a   -- MipMap option
instance IsMip Mip
instance IsMip NoMip

data Rect deriving Typeable

data Regular a      deriving Typeable
data Shadow a       deriving Typeable
data MultiSample a  deriving Typeable
data Buffer a       deriving Typeable

-- helper type level function, used in language AST
type family TexDataRepr arity t
type instance TexDataRepr Red  (v a) = a
type instance TexDataRepr RG   (v a) = V2 a
type instance TexDataRepr RGB  (v a) = V3 a
type instance TexDataRepr RGBA (v a) = V4 a

-- describes texel (texture component) type
data TextureDataType t arity where
    Float   :: IsColorArity a
            => a
            -> TextureDataType (Regular Float) a

    Int     :: IsColorArity a
            => a
            -> TextureDataType (Regular Int) a

    Word    :: IsColorArity a
            => a
            -> TextureDataType (Regular Word) a

    Shadow  :: TextureDataType (Shadow Float) Red   -- TODO: add params required by shadow textures

data SingleTex deriving Typeable    -- singleton texture
data ArrayTex  deriving Typeable    -- array texture
data CubeTex   deriving Typeable    -- cube texture = array with size 6

-- helper type level function for texture specification
-- tells whether a texture is a single or an array texture
type family TexArrRepr a
type instance TexArrRepr N1 = SingleTex
type instance TexArrRepr (Greater t N1 => t) = ArrayTex

-- supported texture component arities
data Red    = Red  deriving Typeable
data RG     = RG   deriving Typeable
data RGB    = RGB  deriving Typeable
data RGBA   = RGBA deriving Typeable

class IsColorArity a
instance IsColorArity Red
instance IsColorArity RG
instance IsColorArity RGB
instance IsColorArity RGBA

-- component arity specification (Red,RG,RGB,RGBA)
--          hint: there is an interference with Shadow component format
--                  alternatives:
--                      A: move Shadow from TextureDataType to TextureType, this will introduce some new TextureType constructors (1D,2D,Cube,Rect)
--                      B: restrict ColorArity for Shadow
--                      C: add color arity definition to TextureDataType, this will solve the problem (best solution)

-- fully describes a texture type
data TextureType dim mip arr layerCount t ar where -- hint: arr - single or array texture, ar - arity (Red,RG,RGB,..)
    Texture1D       :: Nat layerCount
                    => TextureDataType t ar
                    -> layerCount
                    -> TextureType DIM1 Mip (TexArrRepr layerCount) layerCount t ar

    Texture2D       :: Nat layerCount
                    => TextureDataType t ar
                    -> layerCount
                    -> TextureType DIM2 Mip (TexArrRepr layerCount) layerCount t ar

    Texture3D       :: TextureDataType (Regular t) ar
                    -> TextureType DIM3 Mip SingleTex N1 (Regular t) ar

    TextureCube     :: TextureDataType t ar
                    -> TextureType DIM2 Mip CubeTex N1 t ar

    TextureRect     :: TextureDataType t ar
                    -> TextureType Rect NoMip SingleTex N1 t ar

    Texture2DMS     :: Nat layerCount
                    => TextureDataType (Regular t) ar
                    -> layerCount
                    -> TextureType DIM2 NoMip (TexArrRepr layerCount) layerCount (MultiSample t) ar

    TextureBuffer   :: TextureDataType (Regular t) ar
                    -> TextureType DIM1 NoMip SingleTex N1 (Buffer t) ar

-- definies a texture
data Texture (gp :: * -> *) dim layerCount t ar where
    TextureSlot     :: IsValidTextureSlot t
                    => ByteString -- texture slot name
                    -> TextureType dim mip arr layerCount t ar
                    -> Texture gp dim arr t ar

    -- TODO:
    --  add texture internal format specification
    Texture         :: TextureType dim (MipRepr mip) arr layerCount t ar
                    -- -> TexSizeRepr dim
                    -> mip
                    -> TexRepr dim mip gp layerCount (TexDataRepr ar t) -- FIXME: for cube it will give wrong type
                    -> Texture gp dim arr t ar
{-
    -- TODO:
    --  swizzling (arity conversion)
    --  integral -> floating casting (floating -> integral casting if possible)
    ConvertTexture  :: Texture gp dim arr t ar
                    -> Texture gp dim arr t' ar'
-}
-- restriction for texture types what can be specified as texture slots, e.g. multisample textures cannot be created im this way
class IsValidTextureSlot a
instance IsValidTextureSlot (Regular a)
instance IsValidTextureSlot (Shadow a)
instance IsValidTextureSlot (Buffer a)

-- type level hepler function, used for texture specification
type family TexSizeRepr a
type instance TexSizeRepr (DIM1) = Word32
type instance TexSizeRepr (DIM2) = V2U
type instance TexSizeRepr (Rect) = V2U
type instance TexSizeRepr (DIM3) = V3U

-- type level hepler function, used for texture specification
type family TexRepr dim mip (gp :: * -> *) layerCount t :: *
type instance TexRepr DIM1 NoMip   gp layerCount t = gp (Image layerCount t)
type instance TexRepr DIM1 AutoMip gp layerCount t = gp (Image layerCount t)
type instance TexRepr DIM1 Mip     gp layerCount t = [gp (Image layerCount t)]

type instance TexRepr DIM2 NoMip   gp layerCount t = gp (Image layerCount t)
type instance TexRepr DIM2 AutoMip gp layerCount t = gp (Image layerCount t)
type instance TexRepr DIM2 Mip     gp layerCount t = [gp (Image layerCount t)]

type instance TexRepr DIM3 NoMip   gp layerCount t = [gp (Image layerCount t)]
type instance TexRepr DIM3 AutoMip gp layerCount t = [gp (Image layerCount t)]
type instance TexRepr DIM3 Mip     gp layerCount t = [[gp (Image layerCount t)]] -- 3D layers contain mipmap

-- type level hepler function, used for texture specification
type family MipRepr a
type instance MipRepr Mip       = Mip
type instance MipRepr AutoMip   = Mip
type instance MipRepr NoMip     = NoMip

-- shader stage tags: vertex, geometry, fragment
-- used in language AST, for primfun restriction and in shader codegen
data V
data G
data F
