module LCDSL where

import Data.Int
import Data.Data
import Data.Word

import Data.ByteString.Char8

import TypeLevel.Number.Nat
import TypeLevel.Number.Nat.Num

---------------------
-- LINEAR ALGEBRA
---------------------

-- constructors are required for texture specification
data DIM1 = DIM1
data DIM2 = DIM2
data DIM3 = DIM3
data DIM4

data V2 a = V2 a a
data V3 a = V3 a a a
data V4 a = V4 a a a a

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
type V2I = V2 Int
type V3I = V3 Int
type V4I = V4 Int
type V2U = V2 Word
type V3U = V3 Word
type V4U = V4 Word
type V2B = V2 Bool
type V3B = V3 Bool
type V4B = V4 Bool

-- vector types: V2, V3, V4
class IsVec dim vec component | vec -> dim component
instance IsVec DIM2 (V2 Float) Float
instance IsVec DIM3 (V3 Float) Float
instance IsVec DIM4 (V4 Float) Float
instance IsVec DIM2 (V2 Int) Int
instance IsVec DIM3 (V3 Int) Int
instance IsVec DIM4 (V4 Int) Int
instance IsVec DIM2 (V2 Word) Word
instance IsVec DIM3 (V3 Word) Word
instance IsVec DIM4 (V4 Word) Word
instance IsVec DIM2 (V2 Bool) Bool
instance IsVec DIM3 (V3 Bool) Bool
instance IsVec DIM4 (V4 Bool) Bool

-- scalar and vector types: scalar, V2, V3, V4
class IsVecScalar dim vec component | vec -> dim component
instance IsVecScalar DIM1 Float Float
instance IsVecScalar DIM2 (V2 Float) Float
instance IsVecScalar DIM3 (V3 Float) Float
instance IsVecScalar DIM4 (V4 Float) Float
instance IsVecScalar DIM1 Int Int
instance IsVecScalar DIM2 (V2 Int) Int
instance IsVecScalar DIM3 (V3 Int) Int
instance IsVecScalar DIM4 (V4 Int) Int
instance IsVecScalar DIM1 Word Word
instance IsVecScalar DIM2 (V2 Word) Word
instance IsVecScalar DIM3 (V3 Word) Word
instance IsVecScalar DIM4 (V4 Word) Word
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
instance IsMatVecScalar Int Int
instance IsMatVecScalar (V2 Int) Int
instance IsMatVecScalar (V3 Int) Int
instance IsMatVecScalar (V4 Int) Int
instance IsMatVecScalar Word Word
instance IsMatVecScalar (V2 Word) Word
instance IsMatVecScalar (V3 Word) Word
instance IsMatVecScalar (V4 Word) Word
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
instance IsMatVec (V2 Int) Int
instance IsMatVec (V3 Int) Int
instance IsMatVec (V4 Int) Int
instance IsMatVec (V2 Word) Word
instance IsMatVec (V3 Word) Word
instance IsMatVec (V4 Word) Word
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
instance IsComponent Int
instance IsComponent Word
instance IsComponent Bool
instance IsComponent V2F
instance IsComponent V3F
instance IsComponent V4F

-- matrix or vector number component type
class IsNumComponent a
instance IsNumComponent Float
instance IsNumComponent Int
instance IsNumComponent Word
instance IsNumComponent V2F
instance IsNumComponent V3F
instance IsNumComponent V4F

class IsSigned a
instance IsSigned Float
instance IsSigned Int

class Real a => IsNum a
instance IsNum Float
instance IsNum Int
instance IsNum Word

class IsIntegral a
instance IsIntegral Int
instance IsIntegral Word

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

---------------------
-- CONTEXT RELATED
---------------------
data PointSize          = PointSize Float | PointSizeRast
data PolygonOffset      = NoOffset | Offset Float Float
data FrontFace          = CCW | CW
data PolygonMode        = PolygonPoint PointSize | PolygonLine Float | PolygonFill
data ProvokingVertex    = FirstVertex | LastVertex
data CullMode           = CullNone | CullFront FrontFace | CullBack FrontFace
type DepthFunction      = ComparisonFunction
data ComparisonFunction = Never | Less | Equal | Lequal | Greater | Notequal | Gequal | Always
data StencilOperation   = OpZero | OpKeep | OpReplace | OpIncr | OpIncrWrap | OpDecr | OpDecrWrap | OpInvert
data BlendEquation      = FuncAdd | FuncSubtract | FuncReverseSubtract | Min | Max
data BlendingFactor     = Zero | One | SrcColor | OneMinusSrcColor | DstColor | OneMinusDstColor | SrcAlpha | OneMinusSrcAlpha | DstAlpha | OneMinusDstAlpha | ConstantColor | OneMinusConstantColor | ConstantAlpha | OneMinusConstantAlpha | SrcAlphaSaturate
data LogicOperation     = Clear | And | AndReverse | Copy | AndInverted | Noop | Xor | Or | Nor | Equiv | Invert | OrReverse | CopyInverted | OrInverted | Nand | Set

data StencilOps
    = StencilOps
    { frontStencilOp    :: StencilOperation -- ^ Used for front faced triangles and other primitives.
    , backStencilOp     :: StencilOperation -- ^ Used for back faced triangles.
    }

data StencilTests = StencilTests StencilTest StencilTest
data StencilTest
    = StencilTest
    { stencilComparision    :: ComparisonFunction   -- ^ The function used to compare the @stencilReference@ and the stencil buffers value with.
    , stencilReference      :: Int                -- ^ The value to compare with the stencil buffer's value.
    , stencilMask           :: Word               -- ^ A bit mask with ones in each position that should be compared and written to the stencil buffer.
    }

-- primitive types
data Triangle   = Triangle
data Line       = Line
data Point      = Point

class Show p => IsPrimitive p where

-- needed to describe geometry shader input 
type family PrimitiveVertices prim a
type instance PrimitiveVertices Triangle a  = (a,a,a)
type instance PrimitiveVertices Line a      = (a,a)
type instance PrimitiveVertices Point a     = a

-- raster context description
-- TODO: add context parameters
data RasterContext t where
    PointCtx    :: RasterContext Point      -- TODO: PointSize, POINT_FADE_THRESHOLD_SIZE, POINT_SPRITE_COORD_ORIGIN

    LineCtx     :: Float
                -> ProvokingVertex
                -> RasterContext Line

    TriangleCtx :: CullMode
                -> PolygonMode
                -> PolygonOffset
                -> ProvokingVertex
                -> RasterContext Triangle

-- default triangle raster context
defaultTriangleCtx :: RasterContext Triangle
defaultTriangleCtx = TriangleCtx CullNone PolygonFill NoOffset LastVertex

-- framebuffer data / fragment output semantic
data Color a
data Depth a
data Stencil a

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

-- Fragment Operation
data FragmentOperation ty where
    DepthOp         :: DepthFunction
                    -> Bool     -- depth write
                    -> FragmentOperation (Depth Float)

    StencilOp       :: StencilTests
                    -> StencilOps
                    -> StencilOps
                    -> FragmentOperation (Stencil Int)

    ColorOp         :: (IsVecScalar d mask Bool, IsVecScalar d color c, IsNum c, IsScalar mask)
                    => Blending c   -- blending type
                    -> mask         -- write mask
                    -> FragmentOperation (Color color)

data AccumulationContext t
    = AccumulationContext
    { accViewportName   :: Maybe ByteString
    , accOperations     :: t
    }

---------------------
-- TEXTURE RELATED
---------------------

-- sampler and texture specification
data Filter = PointFilter | LinearFilter
data EdgeMode = Wrap | Mirror | Clamp

data Rect

data Red    = Red
data RG     = RG
data RGB    = RGB
data RGBA   = RGBA

data Regular a
data Shadow a
data MultiSample a
data Buffer a

data SingleTex  -- singleton texture
data ArrayTex   -- array texture
data CubeTex    -- cube texture = array with size 6

data Sampler dim layerCount t ar

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
data Texture dim arr t ar where
    TextureSlot     :: (IsValidTextureSlot t)
                    => ByteString -- texture slot name
                    -> TextureType dim mip arr layerCount t ar
                    -> Texture dim arr t ar
    -- TODO:
    --  add texture internal format specification
    Texture         :: (IsScalar (TexSizeRepr dim), IsMipValid canMip mip)
                    => TextureType dim canMip arr layerCount t ar
                    -> TexSizeRepr dim
                    -> MipMap mip
--                    -> TexRepr dim mip gp layerCount (TexDataRepr ar t) -- FIXME: for cube it will give wrong type
                    -> [Image layerCount (TexDataRepr ar t)]
                    -> Texture dim arr t ar
{-
    -- TODO:
    --  swizzling (arity conversion)
    --  integral -> floating casting (floating -> integral casting if possible)
    ConvertTexture  :: Texture dim arr t ar
                    -> Texture dim arr t' ar'
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
type instance TexSizeRepr (DIM1) = Word
type instance TexSizeRepr (DIM2) = V2U
type instance TexSizeRepr (Rect) = V2U
type instance TexSizeRepr (DIM3) = V3U
    
------------------
-- used for tuple type description
infixr 1 :+:
data tail :+: head = !tail :+: !head

-- vertex attribute interpolation
data Interpolated a

flat        :: a :@ F
            -> Interpolated a :@ F

smooth      :: IsFloating a
            => a :@ F
            -> Interpolated a :@ F

linear      :: IsFloating a
            => a :@ F
            -> Interpolated a :@ F

flat        = primop
smooth      = primop
linear      = primop

---------------------
class IsFrameBuffer a
instance IsFrameBuffer (Image layerCount t)

--type FrameBuffer layerCount t = T Typeable (Image layerCount) t
data FrameBuffer layerCount t

--------------------

data Image layerCount a where
    NewImage    :: layerCount -> a -> Image layerCount a

-- shader stage tags: vertex, geometry, fragment
-- used in language AST, for primfun restriction and in shader codegen
data C
data V
data G
data F
data VertexOut
data GeometryOut
data FragmentOut

data t :@ freq

class IsFragOut a
instance IsFragOut (t :@ F)

class IsInterpolated a

-- IsScalar means here that the related type is not a tuple, but a GPU primitive type
class GPU a => IsScalar a
class GPU a => SGPU a
class GPU a

-- abstract types, used in language AST
data VertexStream prim t
data PrimitiveStream prim layerCount stage t
data FragmentStream layerCount t

data GeometryShader primIn primOut layerNum a b where
    GeometryShader      :: (GPU (PrimitiveVertices primIn a), GPU i, GPU j, GPU b, IsPrimitive primIn, IsPrimitive primOut, Nat layerNum)
                        => layerNum                                                 -- geometry shader:
                        -> primOut                                                  -- output primitive
                        -> Int                                                      -- max amount of generated vertices
                        -> ((PrimitiveVertices primIn a) :@ G -> (i,Int) :@ G )   -- how many primitives?
                        -> (i :@ G -> (i,j,Int) :@ G )                            -- how many vertices?
                        -> (j :@ G -> (j,b) :@ GeometryOut)                         -- generate vertices
                        -> GeometryShader primIn primOut layerNum a b

vertexOut               :: IsInterpolated a
                        => V4F :@ V
                        -> Float :@ V
                        -> a :@ V
                        -> a :@ VertexOut

geometryOut             :: IsInterpolated a
                        => V4F :@ G     -- position
                        -> Float :@ G   -- point size
                        -> Int :@ G   -- primitive ID
                        -> Int :@ G   -- layer
                        -> j :@ G
                        -> a :@ G
                        -> (j,a) :@ GeometryOut

fragmentOut             :: IsFragOut a
                        => a
                        -> (ColorRepr a) :@ FragmentOut

fragmentOutDepth        :: IsFragOut a
                        => Float :@ F
                        -> a
                        -> (Depth Float :+: ColorRepr a) :@ FragmentOut

fragmentOutRastDepth    :: IsFragOut a
                        => a
                        -> (Depth Float :+: ColorRepr a) :@ FragmentOut

-- helper type level function, used in language AST
type family ColorRepr a :: *
type instance ColorRepr (a :+: b) = Color a :+: (ColorRepr b)

vertexOut               = primop
geometryOut             = primop
fragmentOut             = primop
fragmentOutDepth        = primop
fragmentOutRastDepth    = primop

data FragmentFilter a where
    PassAll :: FragmentFilter a

    Filter  :: (a :@ F -> Bool :@ F)
            -> FragmentFilter a

sampler             :: GPU (Sampler dim arr t ar)
                    => Filter
                    -> EdgeMode
                    -> Texture dim arr t ar
                    -> (Sampler dim arr t ar) :@ freq

input               :: (GPU t, IsScalar t)
                    => t
                    -> t :@ freq

transform           :: (GPU a, GPU b)
                    => (a :@ V -> b :@ VertexOut)                       -- vertex shader
                    -> VertexStream prim a :@ C
                    -> PrimitiveStream prim N1 V b :@ C

reassemble          :: GeometryShader primIn primOut layerCount a b
                    -> PrimitiveStream primIn N1 V a :@ C
                    -> PrimitiveStream primOut layerCount G b :@ C

rasterize           :: RasterContext prim
                    -> PrimitiveStream prim layerCount stage a :@ C
                    -> FragmentStream layerCount a :@ C

accumulate          :: (GPU a, GPU (DropSemantic b))    -- restriction: depth and stencil optional, arbitrary color component
                    => AccumulationContext b
                    -> FragmentFilter a
                    -> (a :@ F -> (NoStencilRepr b) :@ FragmentOut)     -- fragment shader
                    -> FragmentStream layerCount a :@ C
                    -> FrameBuffer layerCount (DropSemantic b) :@ C
                    -> FrameBuffer layerCount (DropSemantic b) :@ C

-- helper type level function, used in language AST
type family NoStencilRepr a :: *
type instance NoStencilRepr (Stencil a :+: b) = NoStencilRepr b
type instance NoStencilRepr (Color a :+: b) = Color a :+: (NoStencilRepr b)
type instance NoStencilRepr (Depth a :+: b) = Depth a :+: (NoStencilRepr b)

accumulateSet       :: GPU a
                    => FrameBuffer layerCount a :@ C
                    -> FrameBuffer layerCount a :@ C

sampler             = primop
input               = primop
transform           = primop
reassemble          = primop
rasterize           = primop
accumulate          = primop
accumulateSet       = primop

primop :: a
primop = undefined

data Output

imageOut            :: Image layerCount t
                    -> Output

screenOut           :: Image N1 t
                    -> Output

imageOut            = primop
screenOut           = primop

type family DropSemantic a :: *
type instance DropSemantic (Color a :+: b) = a :+: (DropSemantic b)
type instance DropSemantic (Depth a :+: b) = a :+: (DropSemantic b)
type instance DropSemantic (Stencil a :+: b) = a :+: (DropSemantic b)

-- restriction for framebuffer structure according content semantic
-- supported configurations: optional stencil + optional depth + [zero or more color]
data CS
data DS
data SS

data OK

type family ChkCDS sem :: *
type instance ChkCDS CS = OK
type instance ChkCDS DS = OK
type instance ChkCDS (CS :+: a) = ChkCDS a
type instance ChkCDS (DS :+: a) = ChkCS a
type instance ChkCDS (SS :+: a) = ChkCD a

type family ChkCD sem :: *
type instance ChkCD CS = OK
type instance ChkCD DS = OK
type instance ChkCD (CS :+: a) = ChkCD a
type instance ChkCD (DS :+: a) = ChkC a

type family ChkCS sem :: *
type instance ChkCS CS = OK
type instance ChkCS SS = OK
type instance ChkCS (CS :+: a) = ChkCS a
type instance ChkCS (SS :+: a) = ChkC a

type family ChkC sem :: *
type instance ChkC CS = OK
type instance ChkC (CS :+: a) = ChkC a

testChk :: ChkCDS a ~ OK => a -> ()
testChk = undefined

ok :: ()
ok = testChk (undefined :: CS :+: SS :+: CS :+: CS :+: DS :+: CS :+: CS :+: CS)
