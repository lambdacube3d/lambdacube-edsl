module LC_T_HOAS where

import Data.ByteString.Char8
import Data.Typeable
import Data.Int
import Data.Word

import TypeLevel.Number.Nat
import TypeLevel.Number.Nat.Num

import LC_G_Type
import LC_G_APIType (Filter(..),EdgeMode(..))
import LC_T_APIType
import LC_T_DSLType
import LC_T_PrimFun

-- all LC supported types including all types of every computation frequency (Obj,V,G,F)
class LCType a

-- types supported on V,G,F
class GPUType a

{-
User Input New Feature:
    - support tuples
    - support arrays
-}
data InputType a where
    Bool    :: InputType Bool
    V2B     :: InputType V2B
    V3B     :: InputType V3B
    V4B     :: InputType V4B
    Word    :: InputType Word32
    V2U     :: InputType V2U
    V3U     :: InputType V3U
    V4U     :: InputType V4U
    Int     :: InputType Int32
    V2I     :: InputType V2I
    V3I     :: InputType V3I
    V4I     :: InputType V4I
    Float   :: InputType Float
    V2F     :: InputType V2F
    V3F     :: InputType V3F
    V4F     :: InputType V4F
    M22F    :: InputType M22F
    M23F    :: InputType M23F
    M24F    :: InputType M24F
    M32F    :: InputType M32F
    M33F    :: InputType M33F
    M34F    :: InputType M34F
    M42F    :: InputType M42F
    M43F    :: InputType M43F
    M44F    :: InputType M44F

    Tuple   :: FlatTuple Typeable InputType t
            -> InputType t -- TODO: accept only at least two long flat tuples

    Array   :: ordering
            -> InputType t
            -> InputType (Array ordering t)

    deriving Typeable


--TODO: check whether we should distinct size limited arrays and arbitrary sized arrays.
--          former is due to GLSL and GPU restrictions, latter are stored in CPU RAM

-- Common Exp, describes shader functions
data Exp freq t where
    -- Needed for conversion to de Bruijn form
    Tag     :: LCType t
            => Int
            -> TypeRep
            -> Exp freq t
                 -- environment size at defining occurrence

    -- constant value
    -- TODO: support constants for all LCTypes
    Const   :: (LCType t,IsScalar t)
            => t
            -> Exp freq t

    -- User input constant variable
    Var     :: LCType t
            => ByteString
            -> InputType t
            -> Exp freq t

    -- Lift Obj expressions to higher frequencies
    Use     :: Exp Obj t
            -> Exp freq t

    -- conditional expression
    Cond    :: LCType t
            => Exp freq Bool
            -> Exp freq t
            -> Exp freq t
            -> Exp freq t

    PrimApp :: (LCType a, LCType r)
            => PrimFun freq (a -> r)
            -> Exp freq a
            -> Exp freq r

    -- tuple support
    -- TODO: replace Tuple and TupleIdx with FlatTuple and Nat
    Tup     :: (LCType t, IsTuple t)
            => Tuple (Exp freq) (TupleRepr t)
            -> Exp freq t

    Prj     :: (LCType e, LCType t, IsTuple t)
            => TupleIdx (TupleRepr t) e
            -> Exp freq t
            -> Exp freq e

    -- sampler support
    Sampler :: LCType (Sampler dim arr t ar)
            => Filter
            -> EdgeMode
            -> Texture (Exp Obj) dim arr t ar
            -> Exp freq (Sampler dim arr t ar)

    -- loop support
    Loop    :: (LCType s, LCType a)
            => (Exp freq s -> Exp freq s)     -- state transform function
            -> (Exp freq s -> Exp freq Bool)  -- loop condition function
            -> (Exp freq s -> Exp freq a)     -- state to result transform function
            -> Exp freq s                      -- initial state
            -> Exp freq a                      -- result

    -- Array operations
    -- Construction
    ArrayReplicate  :: Exp Obj Int32
                    -> Exp Obj a
                    -> Exp Obj (Array order a)

    ArrayGenerate   :: Exp Obj Int32
                    -> (Exp Obj Int32 -> Exp Obj a)
                    -> Exp Obj (Array order a)

    ArrayIterateN   :: Exp Obj Int32
                    -> (Exp Obj a -> Exp Obj a)
                    -> Exp Obj a
                    -> Exp Obj (Array order a)
    -- Elementwise operations
    ArrayIndex      :: Exp Obj Int32
                    -> Exp Obj (Array order a)
                    -> Exp Obj a

    ArrayFilter     :: (Exp freq a -> Exp freq Bool)
                    -> Exp freq (Array order a)
                    -> Exp freq (Array order a)

    ArrayMap        :: (Exp freq a -> Exp freq b)
                    -> Exp freq (Array order a)
                    -> Exp freq (Array order b)

    ArrayZipWith    :: (Exp freq a -> Exp freq b -> Exp freq c)
                    -> Exp freq (Array order a)
                    -> Exp freq (Array order b)
                    -> Exp freq (Array order c)

    ArrayAccumulate :: (Exp freq a -> Exp freq b -> Exp freq a)
                    -> Exp freq a
                    -> Exp freq (Array order b)
                    -> Exp freq a

    -- Graphics pipeline extensibility
    -- dynamic extension support
    AccumulateSet   :: GPU a
                    => ByteString
                    -> Exp Obj (FrameBuffer layerCount a)
                    -> Exp Obj (FrameBuffer layerCount a)

    -- GPU pipeline model
    Fetch           :: (SGPU a, IsPrimitive prim)
                    => prim
                    -> Exp Obj (Array order a)
                    -> Maybe (Exp Obj (Array order Int32))
                    -> Exp Obj (VertexStream prim a)

    Transform       :: (GPU a, GPU b)
                    => (Exp V a -> VertexOut b)                       -- vertex shader
                    -> Exp Obj (VertexStream prim a)
                    -> Exp Obj (PrimitiveStream prim N1 V b)

    Reassemble      :: GeometryShader primIn primOut layerCount a b
                    -> Exp Obj (PrimitiveStream primIn N1 V a)
                    -> Exp Obj (PrimitiveStream primOut layerCount G b)

    Rasterize       :: RasterContext prim
                    -> Exp Obj (PrimitiveStream prim layerCount freq a)
                    -> Exp Obj (FragmentStream layerCount a)

    FrameBuffer     :: FrameBuffer layerCount t
                    -> Exp Obj (FrameBuffer layerCount (FTRepr' t))

    Accumulate      :: (GPU a, GPU (FTRepr' b), IsValidOutput b)    -- restriction: depth and stencil optional, arbitrary color component
                    => AccumulationContext b
                    -> FragmentFilter a
                    -> (Exp F a -> FragmentOut (NoStencilRepr b))     -- fragment shader
                    -> Exp Obj (FragmentStream layerCount a)
                    -> Exp Obj (FrameBuffer layerCount (FTRepr' b))
                    -> Exp Obj (FrameBuffer layerCount (FTRepr' b))

    -- Transform feedback support
    ArrayFromStream :: Exp Obj (PrimitiveStream prim layerCount freq a)
                    -> Exp Obj (Array order a)

    -- FrameBuffer and Image helpers
    PrjFrameBuffer  :: ByteString                       -- internal image output (can be allocated on request)
                    -> TupleIdx (EltRepr b) t
                    -> Exp Obj (FrameBuffer layerCount b)
                    -> Exp Obj (Image layerCount t)

    PrjImage        :: (Nat idx, LesserEq idx layerCount)
                    => ByteString                       -- internal image output (can be allocated on request)
                    -> idx
                    -> Exp Obj (Image layerCount t)
                    -> Exp Obj (Image N1 t)



type InterpolatedFlatExp freq a = FlatTuple GPU (Interpolated (Exp freq)) a
type FlatExp freq a = FlatTuple GPU (Exp freq) a

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


data GPOutput where
    ImageOut    :: ByteString
                -> Exp Obj (Image layerCount t)
                -> GPOutput

    ScreenOut   :: Exp Obj (Image N1 t)
                -> GPOutput
