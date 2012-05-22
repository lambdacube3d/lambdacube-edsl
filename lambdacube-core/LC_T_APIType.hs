module LC_T_APIType where

import Data.ByteString.Char8
import Data.Int
import Data.Typeable
import Data.Word

import TypeLevel.Number.Nat
import TypeLevel.Number.Nat.Num

import LCType

import LC_APIType
import LC_T_DSLType
import qualified LC_U_APIType as U

-- |Type-safe projection indicies for tuples.
--
-- NB: We index tuples by starting to count from the *right*!
--
class TupleIdx tupleIdx where
    zeroTupIdx :: GPU s =>        tupleIdx (t, s) s
    succTupIdx :: tupleIdx t e -> tupleIdx (t, s) e

-- user can define stream input using InputTuple type class
class InputTuple tup where
    type InputTupleRepr tup
    toInputList :: tup -> [(ByteString,InputType)]
{-
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
-}
-- we should define all of input types
-- supported stream input types (the ByteString argument is the input slot name)
class Input input where
    iBool   :: ByteString -> input Bool
    iV2B    :: ByteString -> input V2B
    iV3B    :: ByteString -> input V3B
    iV4B    :: ByteString -> input V4B
    iWord   :: ByteString -> input Word32
    iV2U    :: ByteString -> input V2U
    iV3U    :: ByteString -> input V3U
    iV4U    :: ByteString -> input V4U
    iInt    :: ByteString -> input Int32
    iV2I    :: ByteString -> input V2I
    iV3I    :: ByteString -> input V3I
    iV4I    :: ByteString -> input V4I
    iFloat  :: ByteString -> input Float
    iV2F    :: ByteString -> input V2F
    iV3F    :: ByteString -> input V3F
    iV4F    :: ByteString -> input V4F
    iM22F   :: ByteString -> input M22F
    iM23F   :: ByteString -> input M23F
    iM24F   :: ByteString -> input M24F
    iM32F   :: ByteString -> input M32F
    iM33F   :: ByteString -> input M33F
    iM34F   :: ByteString -> input M34F
    iM42F   :: ByteString -> input M42F
    iM43F   :: ByteString -> input M43F
    iM44F   :: ByteString -> input M44F

data Triangle
data Line
data Point

class Primitive primitive where
    triangle    :: primitive Triangle
    line        :: primitive Line
    point       :: primitive Point

class Blending blending where
    noBlending      :: blending c

    blendLogicOp    :: IsIntegral c
                    => LogicOperation
                    -> blending c

    -- FIXME: restrict BlendingFactor at some BlendEquation
    blend           :: (BlendEquation, BlendEquation) 
                    -> ((BlendingFactor, BlendingFactor), (BlendingFactor, BlendingFactor))
                    -> V4F
                    -> blending Float

-- abstract types, used in language AST
data VertexStream prim t
data PrimitiveStream prim t
data FragmentStream layerCount t
data FrameBuffer layerCount t
--data Image layerCount t

-- flat tuple, another internal tuple representation
-- means unit
data ZZ = ZZ deriving (Typeable, Show)

-- used for tuple type description
infixr 1 :+:
data tail :+: head = !tail :+: !head deriving (Typeable, Show)

-- used for tuple value description
infixr 1 .:.
class FlatTuple (a :: * -> *) flatTuple where
    zt      :: flatTuple c a ZZ

    (.:.)   :: c t
            => a t
            -> flatTuple c a t'
            -> flatTuple c a (t :+: t')

-- vertex attribute interpolation
class Interpolated interpolated where
    flat            :: e a -> interpolated e a

    smooth          :: IsFloating a
                    => e a -> interpolated e a

    noPerspective   :: IsFloating a
                    => e a -> interpolated e a

-- framebuffer data / fragment output semantic
data Color a    deriving Typeable
data Depth a    deriving Typeable
data Stencil a  deriving Typeable

-- TODO: needed to describe geometry shader input 
data PrimitiveVertices prim a


-- raster context description
-- TODO: add context parameters
class RasterContext rasterContext where
    pointCtx    :: rasterContext Point      -- TODO: PointSize, POINT_FADE_THRESHOLD_SIZE, POINT_SPRITE_COORD_ORIGIN

    lineCtx     :: Float
                -> ProvokingVertex
                -> rasterContext Line

    triangleCtx :: CullMode
                -> PolygonMode
                -> PolygonOffset
                -> ProvokingVertex
                -> rasterContext Triangle

-- default triangle raster context
defaultTriangleCtx :: RasterContext rasterContext => rasterContext Triangle
defaultTriangleCtx = triangleCtx CullNone PolygonFill NoOffset LastVertex

--type FrameBuffer layerCount t = (Image reprI, FlatTuple (reprI layerCount) reprFT) => reprFT Typeable (reprI layerCount) t
--type AccumulationContext t = (FragmentOperation reprFO, FlatTuple reprFO reprFT) => reprFT Typeable reprFO t

-- Fragment Operation
class FragmentOperation fragmentOperation where
    type FragmentOperation_Blending fragmentOperation :: * -> *

    depthOp         :: DepthFunction
                    -> Bool     -- depth write
                    -> fragmentOperation (Depth Float)

    stencilOp       :: StencilTests
                    -> StencilOps
                    -> StencilOps
                    -> fragmentOperation (Stencil Int32)

    colorOp         :: (IsScalar mask, IsVecScalar d mask Bool, IsVecScalar d color c, IsNum c
                       ,blending ~ FragmentOperation_Blending fragmentOperation)
                    => blending c   -- blending type
                    -> mask         -- write mask
                    -> fragmentOperation (Color color)

-- specifies an empty image (pixel rectangle)
-- hint: framebuffer is composed from images
class Image image where
    depthImage      :: Nat layerCount
                    => layerCount
                    -> Float    -- initial value
                    -> image layerCount (Depth Float)

    stencilImage    :: Nat layerCount
                    => layerCount
                    -> Int32    -- initial value
                    -> image layerCount (Stencil Int32)

    colorImage      :: (IsScalar color, IsNum t, IsVecScalar d color t, Nat layerCount)
                    => layerCount
                    -> color    -- initial value
                    -> image layerCount (Color color)

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

-- texture specification

data Mip
data NoMip
data AutoMip

class MipMap mipMap where
    mip     :: mipMap Mip
    noMip   :: mipMap NoMip
    autoMip :: mipMap AutoMip

data Rect deriving Typeable

data Regular a      deriving Typeable
data Shadow a       deriving Typeable
data MultiSample a  deriving Typeable
data Buffer a       deriving Typeable

-- supported texture component arities
data Red
data RG
data RGB
data RGBA

class ColorArity colorArity where
    red     :: colorArity Red
    rg      :: colorArity RG
    rgb     :: colorArity RGB
    rgba    :: colorArity RGBA

-- helper type level function, used in language AST
type family TexDataRepr arity t
type instance TexDataRepr Red  (v a) = a
type instance TexDataRepr RG   (v a) = V2 a
type instance TexDataRepr RGB  (v a) = V3 a
type instance TexDataRepr RGBA (v a) = V4 a

-- describes texel (texture component) type
class TextureDataType textureDataType where
    type TextureDataType_ColorArity textureDataType :: * -> *

    float   :: (colorArity ~ TextureDataType_ColorArity textureDataType)
            => colorArity a
            -> textureDataType (Regular Float) a

    int     :: (colorArity ~ TextureDataType_ColorArity textureDataType)
            => colorArity a
            -> textureDataType (Regular Int) a

    word    :: (colorArity ~ TextureDataType_ColorArity textureDataType)
            => colorArity a
            -> textureDataType (Regular Word) a

    shadow  :: textureDataType (Shadow Float) Red   -- TODO: add params required by shadow textures

data SingleTex deriving Typeable    -- singleton texture
data ArrayTex  deriving Typeable    -- array texture
data CubeTex   deriving Typeable    -- cube texture = array with size 6

-- helper type level function for texture specification
-- tells whether a texture is a single or an array texture
type family TexArrRepr a
type instance TexArrRepr N1 = SingleTex
type instance TexArrRepr (Greater t N1 => t) = ArrayTex

-- component arity specification (Red,RG,RGB,RGBA)
--          hint: there is an interference with Shadow component format
--                  alternatives:
--                      A: move Shadow from TextureDataType to TextureType, this will introduce some new TextureType constructors (1D,2D,Cube,Rect)
--                      B: restrict ColorArity for Shadow
--                      C: add color arity definition to TextureDataType, this will solve the problem (best solution)

-- fully describes a texture type
class TextureType textureType where -- hint: arr - single or array texture, ar - arity (Red,RG,RGB,..)
    type TextureType_TextureDataType textureType :: * -> * -> *

    texture1D       :: (Nat layerCount
                       ,textureDataType ~ TextureType_TextureDataType textureType)
                    => textureDataType t ar
                    -> layerCount
                    -> textureType DIM1 Mip (TexArrRepr layerCount) layerCount t ar

    texture2D       :: (Nat layerCount
                       ,textureDataType ~ TextureType_TextureDataType textureType)
                    => textureDataType t ar
                    -> layerCount
                    -> textureType DIM2 Mip (TexArrRepr layerCount) layerCount t ar

    texture3D       :: (textureDataType ~ TextureType_TextureDataType textureType)
                    => textureDataType (Regular t) ar
                    -> textureType DIM3 Mip SingleTex N1 (Regular t) ar

    textureCube     :: (textureDataType ~ TextureType_TextureDataType textureType)
                    => textureDataType t ar
                    -> textureType DIM2 Mip CubeTex N1 t ar

    textureRect     :: (textureDataType ~ TextureType_TextureDataType textureType)
                    => textureDataType t ar
                    -> textureType Rect NoMip SingleTex N1 t ar

    texture2DMS     :: (Nat layerCount
                       ,textureDataType ~ TextureType_TextureDataType textureType)
                    => textureDataType (Regular t) ar
                    -> layerCount
                    -> textureType DIM2 NoMip (TexArrRepr layerCount) layerCount (MultiSample t) ar

    textureBuffer   :: (textureDataType ~ TextureType_TextureDataType textureType)
                    => textureDataType (Regular t) ar
                    -> textureType DIM1 NoMip SingleTex N1 (Buffer t) ar

-- defines a texture
class Texture (gp :: * -> *) texture where
    type Texture_TextureType texture :: * -> * -> * -> * -> * -> * -> *
    type Texture_Image texture :: * -> * -> *
    type Texture_MipMap texture :: * -> *

    textureSlot     :: (IsValidTextureSlot t
                       ,textureType ~ Texture_TextureType texture)
                    => ByteString -- texture slot name
                    -> textureType dim mip arr layerCount t ar
                    -> texture gp dim arr t ar
    -- TODO:
    --  add texture internal format specification
    texture         :: (textureType ~ Texture_TextureType texture
                       ,image ~ Texture_Image texture
                       ,mipMap ~ Texture_MipMap texture)
                    => textureType dim (MipRepr mip) arr layerCount t ar
                    -- -> TexSizeRepr dim
                    -> mipMap mip
                    -> TexRepr dim mip gp image layerCount (TexDataRepr ar t) -- FIXME: for cube it will give wrong type
                    -> texture gp dim arr t ar

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
type family TexRepr dim mip (gp :: * -> *) (image :: * -> * -> *) layerCount t :: *
type instance TexRepr DIM1 NoMip   gp image layerCount t = gp (image layerCount t)
type instance TexRepr DIM1 AutoMip gp image layerCount t = gp (image layerCount t)
type instance TexRepr DIM1 Mip     gp image layerCount t = [gp (image layerCount t)]

type instance TexRepr DIM2 NoMip   gp image layerCount t = gp (image layerCount t)
type instance TexRepr DIM2 AutoMip gp image layerCount t = gp (image layerCount t)
type instance TexRepr DIM2 Mip     gp image layerCount t = [gp (image layerCount t)]

type instance TexRepr DIM3 NoMip   gp image layerCount t = [gp (image layerCount t)]
type instance TexRepr DIM3 AutoMip gp image layerCount t = [gp (image layerCount t)]
type instance TexRepr DIM3 Mip     gp image layerCount t = [[gp (image layerCount t)]] -- 3D layers contain mipmap

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
data VertexOut
data GeometryOut
data FragmentOut
