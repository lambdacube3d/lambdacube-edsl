module LC_T_HOAS where

import Data.ByteString.Char8
import Data.Typeable
import Data.Int
import Data.Word

import TypeLevel.Number.Nat
import TypeLevel.Number.Nat.Num

import LC_G_LinearAlgebraTypes
import LC_G_APIType
import LC_T_APIType
import LC_T_PrimFun

--TODO: check whether we should distinct size limited arrays and arbitrary sized arrays.
--          former is due to GLSL and GPU restrictions, latter are stored in CPU RAM

type InterpolatedExpTuple freq a = Tuple LCType (Interpolated (Exp freq)) a
type ExpTuple freq a = Tuple LCType (Exp freq) a

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
    Const   :: LCType t
            => InputValue t
            -> Exp freq t

    -- User input constant variable
    Var     :: LCType t
            => ByteString
            -> InputType t
            -> Exp Obj t

    -- Lift Obj expressions to higher frequencies
    Use     :: LCType t
            => Exp Obj t
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

    Tup     :: LCType t
            => ExpTuple freq t
            -> Exp freq t

    Prj     :: (e ~ PrjTup idx t, LCType t, LCType e, Nat idx)
            => idx
            -> Exp freq t
            -> Exp freq e

    -- loop support
    Loop    :: (LCType s, LCType a)
            => (Exp freq s -> Exp freq s)     -- state transform function
            -> (Exp freq s -> Exp freq Bool)  -- loop condition function
            -> (Exp freq s -> Exp freq a)     -- state to result transform function
            -> Exp freq s                      -- initial state
            -> Exp freq a                      -- result

    -- Array operations
    -- Construction
    ArrayFromList   :: LCType (Array order a)
                    => [Exp Obj a]
                    -> Exp Obj (Array order a)

    ArrayReplicate  :: LCType (Array order a)
                    => Exp Obj Int32
                    -> Exp Obj a
                    -> Exp Obj (Array order a)

    ArrayGenerate   :: LCType (Array order a)
                    => Exp Obj Int32
                    -> (Exp Obj Int32 -> Exp Obj a)
                    -> Exp Obj (Array order a)

    ArrayIterateN   :: LCType (Array order a)
                    => Exp Obj Int32
                    -> (Exp Obj a -> Exp Obj a)
                    -> Exp Obj a
                    -> Exp Obj (Array order a)

    -- Elementwise operations
    ArrayIndex      :: LCType (Array order a)
                    => Exp Obj Int32
                    -> Exp Obj (Array order a)
                    -> Exp Obj a

    ArrayFilter     :: LCType (Array order a)
                    => (Exp freq a -> Exp freq Bool)
                    -> Exp freq (Array order a)
                    -> Exp freq (Array order a)

    ArrayMap        :: (LCType (Array order a), LCType (Array order b))
                    => (Exp freq a -> Exp freq b)
                    -> Exp freq (Array order a)
                    -> Exp freq (Array order b)

    ArrayZipWith    :: (LCType (Array order a), LCType (Array order b), LCType (Array order c))
                    => (Exp freq a -> Exp freq b -> Exp freq c)
                    -> Exp freq (Array order a)
                    -> Exp freq (Array order b)
                    -> Exp freq (Array order c)

    ArrayAccumulate :: (LCType a, LCType (Array order b))
                    => (Exp freq a -> Exp freq b -> Exp freq a)
                    -> Exp freq a
                    -> Exp freq (Array order b)
                    -> Exp freq a

    -- Graphics pipeline extensibility
    -- dynamic extension support
    AccumulateSet   :: LCType (FrameBuffer layerCount a)
                    => ByteString
                    -> Exp Obj (FrameBuffer layerCount a)
                    -> Exp Obj (FrameBuffer layerCount a)

    -- GPU pipeline model
    Fetch           :: AttributeType a
                    => FetchPrimitive primitive adjacency 
                    -> Exp Obj (Array order a)
                    -> Maybe (Exp Obj (Array order Int32))
                    -> Exp Obj (VertexStream primitive adjacency a)

    Transform       :: (LCType a, LCType b)
                    => (Exp V a -> VertexOut b)                       -- vertex shader
                    -> Exp Obj (VertexStream primitive adjacency a)
                    -> Exp Obj (PrimitiveStream primitive adjacency N1 V b)

    Reassemble      :: GeometryShader inputPrimitive inputAdjacency outputPrimitive layerCount a b
                    -> Exp Obj (PrimitiveStream inputPrimitive inputAdjacency N1 V a)
                    -> Exp Obj (PrimitiveStream outputPrimitive NoAdjacency layerCount G b)

    Rasterize       :: RasterContext primitive
                    -> Exp Obj (PrimitiveStream primitive adjacency layerCount freq a)
                    -> Exp Obj (FragmentStream layerCount a)

    FrameBuffer     :: FrameBuffer layerCount t
                    -> Exp Obj (FrameBuffer layerCount (t))--TODO ftrepr'

    Accumulate      :: (LCType a, LCType b, IsValidOutput b)    -- restriction: depth and stencil optional, arbitrary color component
                    => AccumulationContext b
                    -> FragmentFilter a
                    -> (Exp F a -> FragmentOut (NoStencilRepr b))     -- fragment shader
                    -> Exp Obj (FragmentStream layerCount a)
                    -> Exp Obj (FrameBuffer layerCount (b))--TODO ftrepr'
                    -> Exp Obj (FrameBuffer layerCount (b))--TODO ftrepr'

    -- Transform feedback support
    ArrayFromStream :: Exp Obj (PrimitiveStream primitive adjacency layerCount freq a)
                    -> Exp Obj (Array order a)

    -- FrameBuffer and Image helpers
    PrjFrameBuffer  :: Nat idx
                    => idx
                    -> Exp Obj (FrameBuffer layerCount b)
                    -> Exp Obj (Image layerCount (PrjTup idx t))

    PrjImage        :: (Nat idx, LesserEq idx layerCount)
                    => idx
                    -> Exp Obj (Image layerCount t)
                    -> Exp Obj (Image N1 t)

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
                -> InterpolatedExpTuple V a
                -> VertexOut a

-- Geometry
-- describes a geometry shader
data GeometryShader inputPrimitive inputAdjacency outputPrimitive layerCount a b where
    GeometryShader      :: (LCType (PrimitiveVertices inputPrimitive inputAdjacency a), LCType i, LCType j, LCType b, Nat layerCount)
                        => layerCount                                                                               -- geometry shader:
                        -> OutputPrimitive outputPrimitive                                                          -- output primitive
                        -> Int                                                                                      -- max amount of generated vertices
                        -> (Exp G (PrimitiveVertices inputPrimitive inputAdjacency a) -> Exp G (i:+:Int32:+:ZZ))    -- how many primitives?
                        -> (Exp G i -> Exp G (i:+:j:+:Int32:+:ZZ))                                                  -- how many vertices?
                        -> (Exp G j -> GeometryOut (j:+:b:+:ZZ))                                                    -- generate vertices
                        -> GeometryShader inputPrimitive inputAdjacency outputPrimitive layerCount a b

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
                -> InterpolatedExpTuple G a
                -> GeometryOut (j:+:a:+:ZZ)

-- Fragment
{-
    Fragment shader builtin output:
            float gl_FragDepth  -- Optional
-}
-- result of a fragment shader function
data FragmentOut t where
    FragmentOut             :: ExpTuple F a
                            -> FragmentOut (ColorRepr a)

    FragmentOutDepth        :: Exp F Float
                            -> ExpTuple F a
                            -> FragmentOut (Depth Float :+: ColorRepr a)

    FragmentOutRastDepth    :: ExpTuple F a
                            -> FragmentOut (Depth Float :+: ColorRepr a)

-- fragment filter function, we express discard using a filter function
data FragmentFilter a where
    PassAll :: FragmentFilter a

    Filter  :: (Exp F a -> Exp F Bool)
            -> FragmentFilter a


data Output where
    ImageOut    :: ByteString
                -> Exp Obj (Image layerCount t)
                -> Output

    ScreenOut   :: Exp Obj (Image N1 t)
                -> Output
