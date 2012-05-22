module LCHOAS where

import Data.ByteString.Char8
import Data.Typeable
import Data.Int

import TypeLevel.Number.Nat
import TypeLevel.Number.Nat.Num

import LCType
import LCAPIType
import LCDSLType
import LCPrimFun

-- Common Exp, describes shader functions
data Exp stage t where
    -- Needed for conversion to de Bruijn form
    Tag     :: GPU t
            => Int
            -> Exp stage t
                 -- environment size at defining occurrence

    -- constant value
    Const   :: GPU t
            => t
            -> Exp stage t

    -- builtin variable
    PrimVar :: GPU t
            => Input t
            -> Exp stage t

    -- uniform value
    Uni     :: (InputTuple t, GPU (InputTupleRepr t))
            => t
            -> Exp stage (InputTupleRepr t)

    -- conditional expression
    Cond    :: GPU t
            => Exp stage Bool
            -> Exp stage t
            -> Exp stage t
            -> Exp stage t

    PrimApp :: (GPU a, GPU r)
            => PrimFun stage (a -> r)
            -> Exp stage a
            -> Exp stage r

    -- tuple support
    Tup     :: (GPU t, IsTuple t)
            => Tuple (Exp stage) (TupleRepr t)
            -> Exp stage t

    Prj     :: (GPU e, GPU t, IsTuple t)
            => TupleIdx (TupleRepr t) e
            -> Exp stage t
            -> Exp stage e

    -- sampler support
    Sampler :: GPU (Sampler dim arr t ar)
            => Filter
            -> EdgeMode
            -> Texture GP dim arr t ar
            -> Exp stage (Sampler dim arr t ar)

type InterpolatedFlatExp stage a = FlatTuple GPU (Interpolated (Exp stage)) a
type FlatExp stage a = FlatTuple GPU (Exp stage) a

-- Vertex
{-
    Vertex shader builtin output:
            gl_PerVertex {
                vec4  gl_Position
                float gl_PointSize
                float gl_ClipDistance[]
            }
-}
-- TODO: add support for gl_ClipDistance setup
-- result of a vertex shader function
data VertexOut t where
    VertexOut   :: Exp V V4F      -- position
                -> Exp V Float    -- point size
                -> InterpolatedFlatExp V a
                -> VertexOut (FTRepr a)

-- Geometry
-- describes a geometry shader
data GeometryShader primIn primOut layerNum a b where
    NoGeometryShader    :: GeometryShader prim prim N1 a a

    GeometryShader      :: (GPU (PrimitiveVertices primIn a), GPU i, GPU j, GPU b, IsPrimitive primIn, IsPrimitive primOut, Nat layerNum)
                        => layerNum                                                 -- geometry shader:
                        -> primOut                                                  -- output primitive
                        -> Int                                                      -- max amount of generated vertices
                        -> (Exp G (PrimitiveVertices primIn a) -> Exp G (i,Int32))  -- how many primitives?
                        -> (Exp G i -> Exp G (i,j,Int32))                           -- how many vertices?
                        -> (Exp G j -> GeometryOut (j,b))                           -- generate vertices
                        -> GeometryShader primIn primOut layerNum a b

{-
    Geometry shader builtin output:
            gl_PerVertex {
                vec4  gl_Position
                float gl_PointSize
                float gl_ClipDistance[]
            }
            int gl_PrimitiveID
            int gl_Layer
-}
-- result of a geometry shader function
data GeometryOut t where
    GeometryOut :: Exp G V4F      -- position
                -> Exp G Float    -- point size
                -> Exp G Int32    -- primitive ID
                -> Exp G Int32    -- layer
                -> Exp G j
                -> InterpolatedFlatExp G a
                -> GeometryOut (j,(FTRepr a))

-- Fragment
{-
    Fragment shader builtin output:
            float gl_FragDepth  -- Optional
-}
-- result of a fragment shader function
data FragmentOut t where
    FragmentOut             :: FlatExp F a
                            -> FragmentOut (ColorRepr a)

    FragmentOutDepth        :: Exp F Float
                            -> FlatExp F a
                            -> FragmentOut (Depth Float :+: ColorRepr a)

    FragmentOutRastDepth    :: FlatExp F a
                            -> FragmentOut (Depth Float :+: ColorRepr a)

-- fragment filter function, we express discard using a filter function
data FragmentFilter a where
    PassAll :: FragmentFilter a

    Filter  :: (Exp F a -> Exp F Bool)
            -> FragmentFilter a

-- hint: GP stands for Graphics Pipeline
-- GP AST
data GP t where
    -- Needed for conversion to de Bruijn form
    GPtag           :: Typeable a
                    => Int
                    -> GP a -- FIXME: restrict valid types to shareable types

    Fetch           :: (InputTuple a, SGPU (InputTupleRepr a), IsPrimitive prim)
                    => ByteString
                    -> prim
                    -> a
                    -> GP (VertexStream prim (InputTupleRepr a))

    Transform       :: (GPU a, GPU b)
                    => (Exp V a -> VertexOut b)                       -- vertex shader
                    -> GP (VertexStream prim a)
                    -> GP (PrimitiveStream prim b)

    Rasterize       :: RasterContext primOut
                    -> GeometryShader primIn primOut layerNum a b
                    -> GP (PrimitiveStream primIn a)
                    -> GP (FragmentStream layerNum b)

    FrameBuffer     :: V2U                                          -- size: width, height
                    -> FrameBuffer layerCount t
                    -> GP (FrameBuffer layerCount (FTRepr' t))

    Accumulate      :: (GPU a, GPU (FTRepr' b), IsValidOutput b)    -- restriction: depth and stencil optional, arbitrary color component
                    => AccumulationContext b
                    -> FragmentFilter a
                    -> (Exp F a -> FragmentOut (NoStencilRepr b))     -- fragment shader
                    -> GP (FragmentStream layerCount a)
                    -> GP (FrameBuffer layerCount (FTRepr' b))
                    -> GP (FrameBuffer layerCount (FTRepr' b))

    PrjFrameBuffer  :: ByteString                       -- internal image output (can be allocated on request)
                    -> TupleIdx (EltRepr b) t
                    -> GP (FrameBuffer layerCount b)
                    -> GP (Image layerCount t)

    PrjImage        :: (LesserEq idx layerCount)
                    => ByteString                       -- internal image output (can be allocated on request)
                    -> idx
                    -> GP (Image layerCount t)
                    -> GP (Image N1 t)

deriving instance Typeable1 GP
