module LCDSL where

import Data.Int
import Data.Data
import Data.Word

import Data.ByteString.Char8

import TypeLevel.Number.Nat
import TypeLevel.Number.Nat.Num

-- constructors are required for texture specification
data DIM1 = DIM1 deriving (Eq,Ord,Typeable)
data DIM2 = DIM2 deriving (Eq,Ord,Typeable)
data DIM3 = DIM3 deriving (Eq,Ord,Typeable)
data DIM4 deriving Typeable

data V2 a = V2 !a !a deriving (Eq,Ord,Show,Data,Typeable)
data V3 a = V3 !a !a !a deriving (Eq,Ord,Show,Data,Typeable)
data V4 a = V4 !a !a !a !a deriving (Eq,Ord,Show,Data,Typeable)

-- matrices are stored in column major order
type M22F = V2 V2F
type M23F = V3 V2F
type M24F = V4 V2F
type M32F = V2 V3F
type M33F = V3 V3F
type M34F = V4 V3F
type M42F = V2 V4F
type M43F = V3 V4F
type M44F = V4 V4F

type V2F = V2 Float
type V3F = V3 Float
type V4F = V4 Float
type V2I = V2 Int32
type V3I = V3 Int32
type V4I = V4 Int32
type V2U = V2 Word32
type V3U = V3 Word32
type V4U = V4 Word32
type V2B = V2 Bool
type V3B = V3 Bool
type V4B = V4 Bool

-- vector types: V2, V3, V4
class IsVec dim vec component | vec -> dim component
instance IsVec DIM2 (V2 Float) Float
instance IsVec DIM3 (V3 Float) Float
instance IsVec DIM4 (V4 Float) Float
instance IsVec DIM2 (V2 Int32) Int32
instance IsVec DIM3 (V3 Int32) Int32
instance IsVec DIM4 (V4 Int32) Int32
instance IsVec DIM2 (V2 Word32) Word32
instance IsVec DIM3 (V3 Word32) Word32
instance IsVec DIM4 (V4 Word32) Word32
instance IsVec DIM2 (V2 Bool) Bool
instance IsVec DIM3 (V3 Bool) Bool
instance IsVec DIM4 (V4 Bool) Bool

-- scalar and vector types: scalar, V2, V3, V4
class IsVecScalar dim vec component | vec -> dim component
instance IsVecScalar DIM1 Float Float
instance IsVecScalar DIM2 (V2 Float) Float
instance IsVecScalar DIM3 (V3 Float) Float
instance IsVecScalar DIM4 (V4 Float) Float
instance IsVecScalar DIM1 Int32 Int32
instance IsVecScalar DIM2 (V2 Int32) Int32
instance IsVecScalar DIM3 (V3 Int32) Int32
instance IsVecScalar DIM4 (V4 Int32) Int32
instance IsVecScalar DIM1 Word32 Word32
instance IsVecScalar DIM2 (V2 Word32) Word32
instance IsVecScalar DIM3 (V3 Word32) Word32
instance IsVecScalar DIM4 (V4 Word32) Word32
instance IsVecScalar DIM1 Bool Bool
instance IsVecScalar DIM2 (V2 Bool) Bool
instance IsVecScalar DIM3 (V3 Bool) Bool
instance IsVecScalar DIM4 (V4 Bool) Bool

-- matrix types of dimension [2..4] x [2..4]
class IsMat mat h w | mat -> h w
instance IsMat M22F V2F V2F
instance IsMat M23F V2F V3F
instance IsMat M24F V2F V4F
instance IsMat M32F V3F V2F
instance IsMat M33F V3F V3F
instance IsMat M34F V3F V4F
instance IsMat M42F V4F V2F
instance IsMat M43F V4F V3F
instance IsMat M44F V4F V4F

-- matrix, vector and scalar types
class IsMatVecScalar a t | a -> t
instance IsMatVecScalar Float Float
instance IsMatVecScalar (V2 Float) Float
instance IsMatVecScalar (V3 Float) Float
instance IsMatVecScalar (V4 Float) Float
instance IsMatVecScalar Int32 Int32
instance IsMatVecScalar (V2 Int32) Int32
instance IsMatVecScalar (V3 Int32) Int32
instance IsMatVecScalar (V4 Int32) Int32
instance IsMatVecScalar Word32 Word32
instance IsMatVecScalar (V2 Word32) Word32
instance IsMatVecScalar (V3 Word32) Word32
instance IsMatVecScalar (V4 Word32) Word32
instance IsMatVecScalar Bool Bool
instance IsMatVecScalar (V2 Bool) Bool
instance IsMatVecScalar (V3 Bool) Bool
instance IsMatVecScalar (V4 Bool) Bool
instance IsMatVecScalar M22F Float
instance IsMatVecScalar M23F Float
instance IsMatVecScalar M24F Float
instance IsMatVecScalar M32F Float
instance IsMatVecScalar M33F Float
instance IsMatVecScalar M34F Float
instance IsMatVecScalar M42F Float
instance IsMatVecScalar M43F Float
instance IsMatVecScalar M44F Float

-- matrix and vector types
class IsMatVec a t | a -> t
instance IsMatVec (V2 Float) Float
instance IsMatVec (V3 Float) Float
instance IsMatVec (V4 Float) Float
instance IsMatVec (V2 Int32) Int32
instance IsMatVec (V3 Int32) Int32
instance IsMatVec (V4 Int32) Int32
instance IsMatVec (V2 Word32) Word32
instance IsMatVec (V3 Word32) Word32
instance IsMatVec (V4 Word32) Word32
instance IsMatVec (V2 Bool) Bool
instance IsMatVec (V3 Bool) Bool
instance IsMatVec (V4 Bool) Bool
instance IsMatVec M22F Float
instance IsMatVec M23F Float
instance IsMatVec M24F Float
instance IsMatVec M32F Float
instance IsMatVec M33F Float
instance IsMatVec M34F Float
instance IsMatVec M42F Float
instance IsMatVec M43F Float
instance IsMatVec M44F Float

-- matrix or vector component type
class IsComponent a
instance IsComponent Float
instance IsComponent Int32
instance IsComponent Word32
instance IsComponent Bool
instance IsComponent V2F
instance IsComponent V3F
instance IsComponent V4F

-- matrix or vector number component type
class IsNumComponent a
instance IsNumComponent Float
instance IsNumComponent Int32
instance IsNumComponent Word32
instance IsNumComponent V2F
instance IsNumComponent V3F
instance IsNumComponent V4F

class IsSigned a
instance IsSigned Float
instance IsSigned Int

class Real a => IsNum a
instance IsNum Float
instance IsNum Int32
instance IsNum Word32

class IsIntegral a
instance IsIntegral Int32
instance IsIntegral Word32

class IsFloating a
instance IsFloating Float
instance IsFloating V2F
instance IsFloating V3F
instance IsFloating V4F
instance IsFloating M22F
instance IsFloating M23F
instance IsFloating M24F
instance IsFloating M32F
instance IsFloating M33F
instance IsFloating M34F
instance IsFloating M42F
instance IsFloating M43F
instance IsFloating M44F

data PointSize          = PointSize Float | PointSizeRast deriving (Eq,Ord,Show, Data,Typeable)
data PolygonOffset      = NoOffset | Offset Float Float  deriving (Eq,Ord,Show, Data,Typeable)
data FrontFace          = CCW | CW deriving (Eq,Ord,Show, Data,Typeable)
data PolygonMode        = PolygonPoint PointSize | PolygonLine Float | PolygonFill deriving (Eq,Ord,Show, Data,Typeable)
data ProvokingVertex    = FirstVertex | LastVertex deriving (Eq,Ord,Bounded,Enum,Show, Data,Typeable)
data CullMode           = CullNone | CullFront FrontFace | CullBack FrontFace deriving (Eq,Ord,Show, Data,Typeable)
type DepthFunction      = ComparisonFunction
data ComparisonFunction = Never | Less | Equal | Lequal | Greater | Notequal | Gequal | Always deriving ( Eq, Ord, Show, Data,Typeable )
data StencilOperation   = OpZero | OpKeep | OpReplace | OpIncr | OpIncrWrap | OpDecr | OpDecrWrap | OpInvert deriving ( Eq, Ord, Show, Data,Typeable )
data BlendEquation      = FuncAdd | FuncSubtract | FuncReverseSubtract | Min | Max deriving ( Eq, Ord, Show, Data,Typeable )
data BlendingFactor     = Zero | One | SrcColor | OneMinusSrcColor | DstColor | OneMinusDstColor | SrcAlpha | OneMinusSrcAlpha | DstAlpha | OneMinusDstAlpha | ConstantColor | OneMinusConstantColor | ConstantAlpha | OneMinusConstantAlpha | SrcAlphaSaturate deriving ( Eq, Ord, Show, Data,Typeable )
data LogicOperation     = Clear | And | AndReverse | Copy | AndInverted | Noop | Xor | Or | Nor | Equiv | Invert | OrReverse | CopyInverted | OrInverted | Nand | Set deriving ( Eq, Ord, Show, Data,Typeable )

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

--------

data Rect deriving Typeable

data Red    = Red  deriving (Eq,Ord,Typeable)
data RG     = RG   deriving (Eq,Ord,Typeable)
data RGB    = RGB  deriving (Eq,Ord,Typeable)
data RGBA   = RGBA deriving (Eq,Ord,Typeable)

data Regular a      deriving Typeable
data Shadow a       deriving Typeable
data MultiSample a  deriving Typeable
data Buffer a       deriving Typeable

data SingleTex deriving Typeable    -- singleton texture
data ArrayTex  deriving Typeable    -- array texture
data CubeTex   deriving Typeable    -- cube texture = array with size 6

data Sampler dim layerCount t ar deriving Typeable
    
-- IsScalar means here that the related type is not a tuple, but a GPU primitive type
class GPU a => IsScalar a
class GPU a => SGPU a
class GPU a


-- primitive types
data Triangle   = Triangle deriving (Show, Eq, Ord)
data Line       = Line     deriving (Show, Eq, Ord)
data Point      = Point    deriving (Show, Eq, Ord)

class Show p => IsPrimitive p where

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
data PrimitiveStream prim layerCount stage t
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
data AccumulationContext t
    = AccumulationContext
    { accViewportName   :: Maybe ByteString
    , accOperations     :: FlatTuple Typeable FragmentOperation t
    }

-- Fragment Operation
data FragmentOperation ty where
    DepthOp         :: DepthFunction
                    -> Bool     -- depth write
                    -> FragmentOperation (Depth Float)

    StencilOp       :: StencilTests
                    -> StencilOps
                    -> StencilOps
                    -> FragmentOperation (Stencil Int32)

    ColorOp         :: (IsVecScalar d mask Bool, IsVecScalar d color c, IsNum c, IsScalar mask)
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

    ColorImage      :: (IsNum t, IsVecScalar d color t, Nat layerCount, IsScalar color)
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

data Mip
data NoMip

data MipMap t where
    NoMip   :: MipMap NoMip

    Mip     :: Int  -- base level
            -> Int  -- max level
            -> MipMap Mip

    AutoMip :: Int  -- base level
            -> Int  -- max level
            -> MipMap Mip

-- helper type level function, used in language AST
type family TexDataRepr arity t
type instance TexDataRepr Red  (v a) = a
type instance TexDataRepr RG   (v a) = V2 a
type instance TexDataRepr RGB  (v a) = V3 a
type instance TexDataRepr RGBA (v a) = V4 a

-- describes texel (texture component) type
data TextureDataType t arity where
    Float   :: (IsColorArity a)
            => a
            -> TextureDataType (Regular Float) a

    Int     :: (IsColorArity a)
            => a
            -> TextureDataType (Regular Int) a

    Word    :: (IsColorArity a)
            => a
            -> TextureDataType (Regular Word) a

    Shadow  :: TextureDataType (Shadow Float) Red   -- TODO: add params required by shadow textures


-- helper type level function for texture specification
-- tells whether a texture is a single or an array texture
type family TexArrRepr a
type instance TexArrRepr N1 = SingleTex
type instance TexArrRepr (Greater t N1 => t) = ArrayTex

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

-- supported texture component arities

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
    Texture1D       :: (Nat layerCount)
                    => TextureDataType t ar
                    -> layerCount
                    -> TextureType DIM1 Mip (TexArrRepr layerCount) layerCount t ar

    Texture2D       :: (Nat layerCount)
                    => TextureDataType t ar
                    -> layerCount
                    -> TextureType DIM2 Mip (TexArrRepr layerCount) layerCount t ar

    Texture3D       :: TextureDataType (Regular t) ar
                    -> TextureType DIM3 Mip SingleTex N1 (Regular t) ar

    TextureCube     :: TextureDataType t ar
                    -> TextureType DIM2 Mip CubeTex N1 t ar

    TextureRect     :: TextureDataType t ar
                    -> TextureType Rect NoMip SingleTex N1 t ar

    Texture2DMS     :: (Nat layerCount)
                    => TextureDataType (Regular t) ar
                    -> layerCount
                    -> TextureType DIM2 NoMip (TexArrRepr layerCount) layerCount (MultiSample t) ar

    TextureBuffer   :: TextureDataType (Regular t) ar
                    -> TextureType DIM1 NoMip SingleTex N1 (Buffer t) ar


-- defines a texture
data Texture (gp :: * -> *) dim arr t ar where
    TextureSlot     :: (IsValidTextureSlot t)
                    => ByteString -- texture slot name
                    -> TextureType dim mip arr layerCount t ar
                    -> Texture gp dim arr t ar
    -- TODO:
    --  add texture internal format specification
    Texture         :: (IsScalar (TexSizeRepr dim), IsMipValid canMip mip)
                    => TextureType dim canMip arr layerCount t ar
                    -> TexSizeRepr dim
                    -> MipMap mip
--                    -> TexRepr dim mip gp layerCount (TexDataRepr ar t) -- FIXME: for cube it will give wrong type
                    -> [gp (Image layerCount (TexDataRepr ar t))]
                    -> Texture gp dim arr t ar
{-
    -- TODO:
    --  swizzling (arity conversion)
    --  integral -> floating casting (floating -> integral casting if possible)
    ConvertTexture  :: Texture gp dim arr t ar
                    -> Texture gp dim arr t' ar'
-}

-- MipMap validation
class IsMipValid canMip mip
instance IsMipValid Mip Mip
instance IsMipValid Mip NoMip
instance IsMipValid NoMip NoMip

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

-- shader stage tags: vertex, geometry, fragment
-- used in language AST, for primfun restriction and in shader codegen
data V
data G
data F

data Exp stage t where
    -- constant value
    Const   :: (GPU t,IsScalar t)
            => t
            -> Exp stage t
{-
    -- uniform value
    Uni     :: GPU t
            => Input t
            -> Exp stage t

    PrimApp :: (GPU a, GPU r)
            => PrimFun stage (a -> r)
            -> Exp stage a
            -> Exp stage r

-}
    -- sampler support
    Sampler :: GPU (Sampler dim arr t ar)
            => Filter
            -> EdgeMode
            -> Texture GP dim arr t ar
            -> Exp stage (Sampler dim arr t ar)

    -- loop support
    Loop    :: (GPU s, GPU a)
            => (Exp stage s -> Exp stage s)     -- state transform function
            -> (Exp stage s -> Exp stage Bool)  -- loop condition function
            -> (Exp stage s -> Exp stage a)     -- state to result transform function
            -> Exp stage s                      -- initial state
            -> Exp stage a                      -- result

type InterpolatedFlatExp stage a = FlatTuple GPU (Interpolated (Exp stage)) a
type FlatExp stage a = FlatTuple GPU (Exp stage) a

data VertexOut t where
    VertexOut   :: Exp V V4F      -- position
                -> Exp V Float    -- point size
                -> InterpolatedFlatExp V a
                -> VertexOut a

data GeometryShader primIn primOut layerNum a b where
    GeometryShader      :: (GPU (PrimitiveVertices primIn a), GPU i, GPU j, GPU b, IsPrimitive primIn, IsPrimitive primOut, Nat layerNum)
                        => layerNum                                                 -- geometry shader:
                        -> primOut                                                  -- output primitive
                        -> Int                                                      -- max amount of generated vertices
                        -> (Exp G (PrimitiveVertices primIn a) -> Exp G (i,Int32))  -- how many primitives?
                        -> (Exp G i -> Exp G (i,j,Int32))                           -- how many vertices?
                        -> (Exp G j -> GeometryOut (j,b))                           -- generate vertices
                        -> GeometryShader primIn primOut layerNum a b

data GeometryOut t where
    GeometryOut :: Exp G V4F      -- position
                -> Exp G Float    -- point size
                -> Exp G Int32    -- primitive ID
                -> Exp G Int32    -- layer
                -> Exp G j
                -> InterpolatedFlatExp G a
                -> GeometryOut (j,a)

data FragmentOut t where
    FragmentOut             :: FlatExp F a
                            -> FragmentOut (ColorRepr a)

    FragmentOutDepth        :: Exp F Float
                            -> FlatExp F a
                            -> FragmentOut (Depth Float :+: ColorRepr a)

    FragmentOutRastDepth    :: FlatExp F a
                            -> FragmentOut (Depth Float :+: ColorRepr a)

data FragmentFilter a where
    PassAll :: FragmentFilter a

    Filter  :: (Exp F a -> Exp F Bool)
            -> FragmentFilter a

data GP t where
    -- Needed for conversion to de Bruijn form
    GPtag           :: Typeable a
                    => Int
                    -> GP a -- FIXME: restrict valid types to shareable types
{-
    Fetch           :: (InputTuple a, SGPU (InputTupleRepr a), IsPrimitive prim)
                    => ByteString
                    -> prim
                    -> a
                    -> GP (VertexStream prim (InputTupleRepr a))
-}
    Transform       :: (GPU a, GPU b)
                    => (Exp V a -> VertexOut b)                       -- vertex shader
                    -> GP (VertexStream prim a)
                    -> GP (PrimitiveStream prim N1 V b)

    Reassemble      :: GeometryShader primIn primOut layerCount a b
                    -> GP (PrimitiveStream primIn N1 V a)
                    -> GP (PrimitiveStream primOut layerCount G b)

    Rasterize       :: RasterContext prim
                    -> GP (PrimitiveStream prim layerCount stage a)
                    -> GP (FragmentStream layerCount a)

    FrameBuffer     :: FrameBuffer layerCount t
                    -> GP (FrameBuffer layerCount (DropSemantic t))

    Accumulate      :: (GPU a, GPU (DropSemantic b), IsValidOutput b)    -- restriction: depth and stencil optional, arbitrary color component
                    => AccumulationContext b
                    -> FragmentFilter a
                    -> (Exp F a -> FragmentOut (NoStencilRepr b))     -- fragment shader
                    -> GP (FragmentStream layerCount a)
                    -> GP (FrameBuffer layerCount (DropSemantic b))
                    -> GP (FrameBuffer layerCount (DropSemantic b))

    -- dynamic extension support
    AccumulateSet   :: GPU a
                    => ByteString
                    -> GP (FrameBuffer layerCount a)
                    -> GP (FrameBuffer layerCount a)


data GPOutput where
    ImageOut    :: ByteString
                -> GP (Image layerCount t)
                -> GPOutput

    ScreenOut   :: GP (Image N1 t)
                -> GPOutput


type family DropSemantic a :: *
type instance DropSemantic ZZ = ZZ
type instance DropSemantic (Color a :+: b) = a :+: (DropSemantic b)
type instance DropSemantic (Depth a :+: b) = a :+: (DropSemantic b)
type instance DropSemantic (Stencil a :+: b) = a :+: (DropSemantic b)

