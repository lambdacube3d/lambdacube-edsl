{-# LANGUAGE UndecidableInstances #-}
module LC_T_APIType where

import GHC.TypeLits

import Data.ByteString.Char8
import Data.Int
import Data.Word

import LC_G_Type
import LC_G_APIType hiding (InputType(..))
import LC_G_APIType (InputType)
import qualified LC_G_APIType as U
import qualified LC_U_APIType as U
import LC_T_DSLType hiding (Shadow)
import qualified LC_T_DSLType as T

data NatNum :: Nat -> * where
    N0 :: NatNum 0
    N1 :: NatNum 1
    N2 :: NatNum 2
    N3 :: NatNum 3
    N4 :: NatNum 4
    N5 :: NatNum 5
    N6 :: NatNum 6
    N7 :: NatNum 7
    N8 :: NatNum 8
    N9 :: NatNum 9

n0 = N0
n1 = N1
n2 = N2
n3 = N3
n4 = N4
n5 = N5
n6 = N6
n7 = N7
n8 = N8
n9 = N9

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
toInput (IBool  n) = (n, U.Bool)
toInput (IV2B   n) = (n, U.V2B)
toInput (IV3B   n) = (n, U.V3B)
toInput (IV4B   n) = (n, U.V4B)
toInput (IWord  n) = (n, U.Word)
toInput (IV2U   n) = (n, U.V2U)
toInput (IV3U   n) = (n, U.V3U)
toInput (IV4U   n) = (n, U.V4U)
toInput (IInt   n) = (n, U.Int)
toInput (IV2I   n) = (n, U.V2I)
toInput (IV3I   n) = (n, U.V3I)
toInput (IV4I   n) = (n, U.V4I)
toInput (IFloat n) = (n, U.Float)
toInput (IV2F   n) = (n, U.V2F)
toInput (IV3F   n) = (n, U.V3F)
toInput (IV4F   n) = (n, U.V4F)
toInput (IM22F  n) = (n, U.M22F)
toInput (IM23F  n) = (n, U.M23F)
toInput (IM24F  n) = (n, U.M24F)
toInput (IM32F  n) = (n, U.M32F)
toInput (IM33F  n) = (n, U.M33F)
toInput (IM34F  n) = (n, U.M34F)
toInput (IM42F  n) = (n, U.M42F)
toInput (IM43F  n) = (n, U.M43F)
toInput (IM44F  n) = (n, U.M44F)

-- primitive types
data PrimitiveType
    = Triangle
    | Line
    | Point
    | TriangleAdjacency
    | LineAdjacency

data FetchPrimitive :: PrimitiveType -> * where
    Points                  :: FetchPrimitive Point
    Lines                   :: FetchPrimitive Line
    Triangles               :: FetchPrimitive Triangle
    LinesAdjacency          :: FetchPrimitive LineAdjacency
    TrianglesAdjacency      :: FetchPrimitive TriangleAdjacency

data OutputPrimitive :: PrimitiveType -> * where
    TrianglesOutput :: OutputPrimitive Triangle
    LinesOutput     :: OutputPrimitive Line
    PointsOutput    :: OutputPrimitive Point

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

blend = Blend (FuncAdd,FuncAdd) ((SrcAlpha,OneMinusSrcAlpha),(SrcAlpha,OneMinusSrcAlpha)) (V4 1 1 1 1)

-- abstract types, used in language AST
data VertexStream (primitive :: PrimitiveType) t
data PrimitiveStream (primitive :: PrimitiveType) clipDistances (layerCount :: Nat) (freq :: Frequency) t
data FragmentStream (layerCount :: Nat) t


-- flat tuple, another internal tuple representation

-- means unit
data ZZ = ZZ deriving (Show)

-- used for tuple type description
infixr 1 :+:
data tail :+: head = !tail :+: !head deriving (Show)

-- used for tuple value description
infixr 1 :.
data FlatTuple c a t where
    ZT      :: FlatTuple c a ZZ

    (:.)    :: c t
            => a t
            -> FlatTuple c a t'
            -> FlatTuple c a (t :+: t')

class IsFloatTuple a
instance IsFloatTuple ZZ
instance IsFloatTuple l => IsFloatTuple (Float :+: l)

-- vertex attribute interpolation
data Interpolated e a where
    Flat            :: e a -> Interpolated e a

    Smooth          :: IsFloating a
                    => e a -> Interpolated e a

    NoPerspective   :: IsFloating a
                    => e a -> Interpolated e a

-- framebuffer data / fragment output semantic
data Color a
data Depth a
data Stencil a

-- describe geometry shader input 
type family PrimitiveVertices (primitive :: PrimitiveType) a
type instance PrimitiveVertices Point a             = a
type instance PrimitiveVertices Line a              = (a,a)
type instance PrimitiveVertices LineAdjacency a     = (a,a,a,a)
type instance PrimitiveVertices Triangle a          = (a,a,a)
type instance PrimitiveVertices TriangleAdjacency a = (a,a,a,a,a,a)

-- raster context description
data RasterContext t where
    PointCtx    ::
        { ctxPointSize          :: PointSize
        , ctxFadeThresholdSize  :: Float
        , ctxSpriteCoordOrigin  :: PointSpriteCoordOrigin
        } -> RasterContext Point

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

class NoConstraint a
instance NoConstraint a

type FrameBuffer layerCount t = FlatTuple NoConstraint (Image layerCount) t
data AccumulationContext t
    = AccumulationContext
    { accViewportName   :: Maybe ByteString
    , accOperations     :: FlatTuple NoConstraint FragmentOperation t
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

-- specifies an empty image (pixel rectangle)
-- hint: framebuffer is composed from images
data Image (layerCount :: Nat) t where
    DepthImage      :: KnownNat layerCount
                    => NatNum layerCount
                    -> Float    -- initial value
                    -> Image layerCount (Depth Float)

    StencilImage    :: KnownNat layerCount
                    => NatNum layerCount
                    -> Int32    -- initial value
                    -> Image layerCount (Stencil Int32)

    ColorImage      :: (IsNum t, IsVecScalar d color t, IsScalar color, KnownNat layerCount)
                    => NatNum layerCount
                    -> color    -- initial value
                    -> Image layerCount (Color color)

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
type instance FTRepr ZZ = ()
type instance FTRepr (a :+: ZZ) = a
type instance FTRepr (a :+: b :+: ZZ) = (a, b)
type instance FTRepr (a :+: b :+: c :+: ZZ) = (a, b, c)
type instance FTRepr (a :+: b :+: c :+: d :+: ZZ) = (a, b, c, d)
type instance FTRepr (a :+: b :+: c :+: d :+: e :+: ZZ) = (a, b, c, d, e)
type instance FTRepr (a :+: b :+: c :+: d :+: e :+: f :+: ZZ) = (a, b, c, d, e, f)
type instance FTRepr (a :+: b :+: c :+: d :+: e :+: f :+: g :+: ZZ) = (a, b, c, d, e, f, g)
type instance FTRepr (a :+: b :+: c :+: d :+: e :+: f :+: g :+: h :+: ZZ) = (a, b, c, d, e, f, g, h)
type instance FTRepr (a :+: b :+: c :+: d :+: e :+: f :+: g :+: h :+: i :+: ZZ) = (a, b, c, d, e, f, g, h, i)

-- helper type level function, used in language AST
type family FTRepr' a :: *
type instance FTRepr' (i1 a :+: ZZ) = a
type instance FTRepr' (i1 a :+: i2 b :+: ZZ) = (a, b)
type instance FTRepr' (i1 a :+: i2 b :+: i3 c :+: ZZ) = (a, b, c)
type instance FTRepr' (i1 a :+: i2 b :+: i3 c :+: i4 d :+: ZZ) = (a, b, c, d)
type instance FTRepr' (i1 a :+: i2 b :+: i3 c :+: i4 d :+: i5 e :+: ZZ) = (a, b, c, d, e)
type instance FTRepr' (i1 a :+: i2 b :+: i3 c :+: i4 d :+: i5 e :+: i6 f :+: ZZ) = (a, b, c, d, e, f)
type instance FTRepr' (i1 a :+: i2 b :+: i3 c :+: i4 d :+: i5 e :+: i6 f :+: i7 g :+: ZZ) = (a, b, c, d, e, f, g)
type instance FTRepr' (i1 a :+: i2 b :+: i3 c :+: i4 d :+: i5 e :+: i6 f :+: i7 g :+: i8 h :+: ZZ) = (a, b, c, d, e, f, g, h)
type instance FTRepr' (i1 a :+: i2 b :+: i3 c :+: i4 d :+: i5 e :+: i6 f :+: i7 g :+: i8 h :+: i9 i :+: ZZ) = (a, b, c, d, e, f, g, h ,i)

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
data TextureMipMap
    = TexMip
    | TexNoMip

data MipMap (t :: TextureMipMap) where
    NoMip   :: MipMap TexNoMip

    Mip     :: Int  -- base level
            -> Int  -- max level
            -> MipMap TexMip

    AutoMip :: Int  -- base level
            -> Int  -- max level
            -> MipMap TexMip

-- helper type level function, used in language AST
type family TexDataRepr arity (t :: TextureSemantics *)
type instance TexDataRepr Red  (v a) = a
type instance TexDataRepr RG   (v a) = V2 a
type instance TexDataRepr RGB  (v a) = V3 a
type instance TexDataRepr RGBA (v a) = V4 a

-- describes texel (texture component) type
data TextureDataType (kind :: TextureSemantics *) arity where
    Float   :: IsColorArity a
            => a
            -> TextureDataType (Regular Float) a

    Int     :: IsColorArity a
            => a
            -> TextureDataType (Regular Int) a

    Word    :: IsColorArity a
            => a
            -> TextureDataType (Regular Word) a

    Shadow  :: TextureDataType (T.Shadow Float) Red   -- TODO: add params required by shadow textures


-- helper type level function for texture specification
-- tells whether a texture is a single or an array texture
type family TexArrRepr (a :: Nat) :: TextureArray
{-
type instance TexArrRepr 1 = SingleTex
type instance TexArrRepr ((2 <= t) => t) = ArrayTex
-}
-- FIXME: implement properly
type instance TexArrRepr 1 = SingleTex
type instance TexArrRepr 2 = ArrayTex
type instance TexArrRepr 3 = ArrayTex
type instance TexArrRepr 4 = ArrayTex
type instance TexArrRepr 5 = ArrayTex
type instance TexArrRepr 6 = ArrayTex
type instance TexArrRepr 7 = ArrayTex
type instance TexArrRepr 8 = ArrayTex
type instance TexArrRepr 9 = ArrayTex

-- supported texture component arities
class IsColorArity a where
    toColorArity :: a -> U.ColorArity

instance IsColorArity Red where
    toColorArity _  = U.Red
instance IsColorArity RG where
    toColorArity _  = U.RG
instance IsColorArity RGB where
    toColorArity _  = U.RGB
instance IsColorArity RGBA where
    toColorArity _  = U.RGBA

-- component arity specification (Red,RG,RGB,RGBA)
--          hint: there is an interference with Shadow component format
--                  alternatives:
--                      A: move Shadow from TextureDataType to TextureType, this will introduce some new TextureType constructors (1D,2D,Cube,Rect)
--                      B: restrict ColorArity for Shadow
--                      C: add color arity definition to TextureDataType, this will solve the problem (best solution)

-- fully describes a texture type
data TextureType :: TextureShape -> TextureMipMap -> TextureArray -> Nat -> TextureSemantics * -> * -> * where -- hint: arr - single or array texture, ar - arity (Red,RG,RGB,..)
    Texture1D       :: KnownNat layerCount
                    => TextureDataType t ar
                    -> NatNum layerCount
                    -> TextureType Tex1D TexMip (TexArrRepr layerCount) layerCount t ar

    Texture2D       :: KnownNat layerCount
                    => TextureDataType t ar
                    -> NatNum layerCount
                    -> TextureType Tex2D TexMip (TexArrRepr layerCount) layerCount t ar

    Texture3D       :: TextureDataType (Regular t) ar
                    -> TextureType Tex3D TexMip SingleTex 1 (Regular t) ar

    TextureCube     :: TextureDataType t ar
                    -> TextureType Tex2D TexMip CubeTex 6 t ar

    TextureRect     :: TextureDataType t ar
                    -> TextureType TexRect TexNoMip SingleTex 1 t ar

    Texture2DMS     :: KnownNat layerCount
                    => TextureDataType (Regular t) ar
                    -> NatNum layerCount
                    -> TextureType Tex2D TexNoMip (TexArrRepr layerCount) layerCount (MultiSample t) ar

    TextureBuffer   :: TextureDataType (Regular t) ar
                    -> TextureType Tex1D TexNoMip SingleTex 1 (Buffer t) ar


-- defines a texture
data Texture (gp :: * -> *) (dim :: TextureShape) (arr :: TextureArray) (t :: TextureSemantics *) ar where
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
class IsMipValid (canMip :: TextureMipMap) (mip :: TextureMipMap)
instance IsMipValid TexMip TexMip
instance IsMipValid TexMip TexNoMip
instance IsMipValid TexNoMip TexNoMip

-- restriction for texture types what can be specified as texture slots, e.g. multisample textures cannot be created im this way
class IsValidTextureSlot (a :: TextureSemantics *)
instance IsValidTextureSlot (Regular a)
instance IsValidTextureSlot (T.Shadow a)
instance IsValidTextureSlot (Buffer a)

-- type level hepler function, used for texture specification
type family TexSizeRepr (a :: TextureShape)
type instance TexSizeRepr (Tex1D)   = Word32
type instance TexSizeRepr (Tex2D)   = V2U
type instance TexSizeRepr (TexRect) = V2U
type instance TexSizeRepr (Tex3D)   = V3U
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

-- shader stage tags: vertex, geometry, fragment
-- used in language AST, for primfun restriction and in shader codegen
data Frequency
    = Obj
    | V
    | G
    | F

data OutputType
    = SingleOutput
    | MultiOutput
