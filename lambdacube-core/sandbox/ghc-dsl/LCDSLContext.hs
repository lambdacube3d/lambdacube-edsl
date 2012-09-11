module LCDSLContext where

import Data.Word
import Data.ByteString.Char8

import LCDSLLinAlg

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

class IsPrimitive p
instance IsPrimitive Triangle
instance IsPrimitive Line
instance IsPrimitive Point

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

    ColorOp         :: (IsVecScalar d mask Bool, IsVecScalar d color c, IsNum c)
                    => Blending c   -- blending type
                    -> mask         -- write mask
                    -> FragmentOperation (Color color)

data AccumulationContext t
    = AccumulationContext
    { accViewportName   :: Maybe ByteString
    , accOperations     :: t
    }
