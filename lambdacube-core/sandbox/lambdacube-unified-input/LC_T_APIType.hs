module LC_T_APIType where

import Data.ByteString.Char8
import Data.Int
import Data.Typeable
import Data.Word
import Foreign.Ptr

import TypeLevel.Number.Nat
import TypeLevel.Number.Nat.Num
import TypeLevel.Number.Classes

import LC_G_LinearAlgebraTypes
import LC_G_APIType

-- internal tuple representation

-- means unit
data ZZ deriving Typeable

-- used for tuple type description
infixr 1 :+:
data tail :+: head = !tail :+: !head deriving (Typeable, Show)

-- used for tuple value description
infixr 1 :.
data Tuple c a t where
    ZT      :: Tuple c a ZZ

    (:.)    :: c t
            => a t
            -> Tuple c a t'
            -> Tuple c a (t :+: t')

-- primitive types
data Adjacency
data NoAdjacency

data Triangle
data Line
data Point

data FetchPrimitive primitive adjacency where
    Points                  :: FetchPrimitive Point     NoAdjacency
    LineStrip               :: FetchPrimitive Line      NoAdjacency
    LineLoop                :: FetchPrimitive Line      NoAdjacency
    Lines                   :: FetchPrimitive Line      NoAdjacency
    TriangleStrip           :: FetchPrimitive Triangle  NoAdjacency
    TriangleFan             :: FetchPrimitive Triangle  NoAdjacency
    Triangles               :: FetchPrimitive Triangle  NoAdjacency
    LinesAdjacency          :: FetchPrimitive Line      Adjacency
    LineStripAdjacency      :: FetchPrimitive Line      Adjacency
    TrianglesAdjacency      :: FetchPrimitive Triangle  Adjacency
    TriangleStripAdjacency  :: FetchPrimitive Triangle  Adjacency

data OutputPrimitive primitive where
    TrianglesOutput :: OutputPrimitive Triangle
    LinesOutput     :: OutputPrimitive Line
    PointsOutput    :: OutputPrimitive Point

-- describe geometry shader input 
type family PrimitiveVertices primitive adajcency a
type instance PrimitiveVertices Point NoAdjacency a     = a
type instance PrimitiveVertices Line NoAdjacency a      = a:+:a:+:ZZ
type instance PrimitiveVertices Line Adjacency a        = a:+:a:+:a:+:a:+:ZZ
type instance PrimitiveVertices Triangle NoAdjacency a  = a:+:a:+:a:+:ZZ
type instance PrimitiveVertices Triangle Adjacency a    = a:+:a:+:a:+:a:+:a:+:a:+:ZZ

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
data VertexStream primitive adjacency t
data PrimitiveStream primitive adjacency layerCount freq t
data FragmentStream layerCount t

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

type FrameBuffer layerCount t = Tuple Typeable (Image layerCount) t
data AccumulationContext t
    = AccumulationContext
    { accViewportName   :: Maybe ByteString
    , accOperations     :: Tuple Typeable FragmentOperation t
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
    Float   :: ColorArity a
            -> TextureDataType (Regular Float) a

    Int     :: ColorArity a
            -> TextureDataType (Regular Int) a

    Word    :: ColorArity a
            -> TextureDataType (Regular Word) a

    Shadow  :: TextureDataType (Shadow Float) Red   -- TODO: add params required by shadow textures


-- helper type level function for texture specification
-- tells whether a texture is a single or an array texture
type family TexArrRepr a
type instance TexArrRepr N1 = SingleTex
type instance TexArrRepr (Greater t N1 => t) = ArrayTex

-- supported texture component arities

data ColorArity a where
    Red     :: ColorArity Red
    RG      :: ColorArity RG
    RGB     :: ColorArity RGB
    RGBA    :: ColorArity RGBA

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
{-
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
-}

data TextureSetting dim arr layerCount t ar
data SamplerSetting
data Texture dim arr t ar

-- shader freq tags: vertex, geometry, fragment
-- used in language AST, for primfun restriction and in shader codegen
data Obj
data V
data G
data F

data Ordered
data Unordered
data Array orderType a

-- type sets
data Rect deriving Typeable

data Red
data RG
data RGB
data RGBA

data Regular a      deriving Typeable
data Shadow a       deriving Typeable
data MultiSample a  deriving Typeable
data Buffer a       deriving Typeable

data SingleTex deriving Typeable    -- singleton texture
data ArrayTex  deriving Typeable    -- array texture
data CubeTex   deriving Typeable    -- cube texture = array with size 6

data Sampler dim layerCount t ar deriving Typeable

instance Show (Sampler dim layerCount t ar) where
    show _ = "Sampler dim layerCount t ar"

-- GPU type restriction, the functions are used in shader codegen
class GPU a where

-- Float
instance Typeable a => GPU (Sampler DIM1 SingleTex (Regular Float) a) where
instance Typeable a => GPU (Sampler DIM2 SingleTex (Regular Float) a) where
instance Typeable a => GPU (Sampler DIM3 SingleTex (Regular Float) a) where
instance Typeable a => GPU (Sampler DIM2 CubeTex   (Regular Float) a) where
instance Typeable a => GPU (Sampler DIM1 ArrayTex  (Regular Float) a) where
instance Typeable a => GPU (Sampler DIM2 ArrayTex  (Regular Float) a) where
instance Typeable a => GPU (Sampler DIM2 SingleTex (MultiSample Float) a) where
instance Typeable a => GPU (Sampler DIM2 ArrayTex  (MultiSample Float) a) where
instance Typeable a => GPU (Sampler DIM1 SingleTex (Buffer Float) a) where
instance Typeable a => GPU (Sampler Rect SingleTex (Regular Float) a) where

-- Int
instance Typeable a => GPU (Sampler DIM1 SingleTex (Regular Int) a) where
instance Typeable a => GPU (Sampler DIM2 SingleTex (Regular Int) a) where
instance Typeable a => GPU (Sampler DIM3 SingleTex (Regular Int) a) where
instance Typeable a => GPU (Sampler DIM2 CubeTex   (Regular Int) a) where
instance Typeable a => GPU (Sampler DIM1 ArrayTex  (Regular Int) a) where
instance Typeable a => GPU (Sampler DIM2 ArrayTex  (Regular Int) a) where
instance Typeable a => GPU (Sampler DIM2 SingleTex (MultiSample Int) a) where
instance Typeable a => GPU (Sampler DIM2 ArrayTex  (MultiSample Int) a) where
instance Typeable a => GPU (Sampler DIM1 SingleTex (Buffer Int) a) where
instance Typeable a => GPU (Sampler Rect SingleTex (Regular Int) a) where

-- Word
instance Typeable a => GPU (Sampler DIM1 SingleTex (Regular Word) a) where
instance Typeable a => GPU (Sampler DIM2 SingleTex (Regular Word) a) where
instance Typeable a => GPU (Sampler DIM3 SingleTex (Regular Word) a) where
instance Typeable a => GPU (Sampler DIM2 CubeTex   (Regular Word) a) where
instance Typeable a => GPU (Sampler DIM1 ArrayTex  (Regular Word) a) where
instance Typeable a => GPU (Sampler DIM2 ArrayTex  (Regular Word) a) where
instance Typeable a => GPU (Sampler DIM2 SingleTex (MultiSample Word) a) where
instance Typeable a => GPU (Sampler DIM2 ArrayTex  (MultiSample Word) a) where
instance Typeable a => GPU (Sampler DIM1 SingleTex (Buffer Word) a) where
instance Typeable a => GPU (Sampler Rect SingleTex (Regular Word) a) where

-- Shadow
instance Typeable a => GPU (Sampler DIM1 SingleTex (Shadow Float) a) where
instance Typeable a => GPU (Sampler DIM2 SingleTex (Shadow Float) a) where
instance Typeable a => GPU (Sampler DIM2 CubeTex   (Shadow Float) a) where
instance Typeable a => GPU (Sampler DIM1 ArrayTex  (Shadow Float) a) where
instance Typeable a => GPU (Sampler DIM2 ArrayTex  (Shadow Float) a) where
instance Typeable a => GPU (Sampler Rect SingleTex (Shadow Float) a) where

instance GPU Bool where
instance GPU Float where
instance GPU Int32 where
instance GPU Word32 where
instance GPU V2B where
instance GPU V2F where
instance GPU V2I where
instance GPU V2U where
instance GPU V3B where
instance GPU V3F where
instance GPU V3I where
instance GPU V3U where
instance GPU V4B where
instance GPU V4F where
instance GPU V4I where
instance GPU V4U where
instance GPU M22F where
instance GPU M23F where
instance GPU M24F where
instance GPU M32F where
instance GPU M33F where
instance GPU M34F where
instance GPU M42F where
instance GPU M43F where
instance GPU M44F where
instance GPU ZZ where
instance (GPU a, GPU b) => GPU (a :+: b) where

-- stream type restriction, these types can be used in vertex shader input
class GPU a => SGPU a
instance SGPU Int32
instance SGPU Word32
instance SGPU Float
instance SGPU M22F
instance SGPU M23F
instance SGPU M24F
instance SGPU M32F
instance SGPU M33F
instance SGPU M34F
instance SGPU M42F
instance SGPU M43F
instance SGPU M44F
instance SGPU V2F
instance SGPU V3F
instance SGPU V4F
instance SGPU V2I
instance SGPU V3I
instance SGPU V4I
instance SGPU V2U
instance SGPU V3U
instance SGPU V4U
instance SGPU ZZ where
instance (SGPU a, SGPU b) => SGPU (a :+: b) where
