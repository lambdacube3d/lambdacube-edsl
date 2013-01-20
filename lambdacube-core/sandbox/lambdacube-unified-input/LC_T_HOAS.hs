module LC_T_HOAS where

import Data.ByteString.Char8
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

-- vertex attribute interpolation
data Interpolated e a where
    Flat            :: e a -> Interpolated e a

    Smooth          :: IsFloating a
                    => e a -> Interpolated e a

    NoPerspective   :: IsFloating a
                    => e a -> Interpolated e a

type InterpolatedExpTuple freq a = Tuple (LCType freq) (Interpolated (Exp freq)) a
type ExpTuple freq a = Tuple (LCType freq) (Exp freq) a

class IsFrameBuffer t
instance IsFrameBuffer (Image layerCount a :+: ZZ)
instance IsFrameBuffer (Image layerCount b :+: tail) => IsFrameBuffer (Image layerCount a :+: Image layerCount b :+: tail)

-- Common Exp, describes shader functions
data Exp freq t where
    -- Needed for conversion to de Bruijn form
    Tag             :: LCType freq t
                    => Int
                    -> Exp freq t
                 -- environment size at defining occurrence

    -- constant value
    -- TODO: support constants for all LCTypes
    Const           :: LCType C t
                    => InputValue C t
                    -> Exp C t

    -- User input constant
    ConstInput      :: LCType C t
                    => ByteString
                    -> InputType C t
                    -> Exp C t

    -- User input variable
    Input           :: LCType Obj t
                    => ByteString
                    -> InputType Obj t
                    -> Exp Obj t

    -- Lift Obj expressions to higher frequencies
    Use             :: ( LCType freq t
                       , LCType Obj t
                       )
                    => Exp Obj t
                    -> Exp freq t

    -- conditional expression
    {-
        structural types (Output, GeometryShader, Vertex, etc..) can be ecaulated only in C constant frequency
    -}
    Cond            :: LCType freq t
                    => Exp freq Bool
                    -> Exp freq t
                    -> Exp freq t
                    -> Exp freq t

    PrimApp         :: LCType freq a
                    => PrimFun freq (a -> r)
                    -> Exp freq a
                    -> Exp freq r

    Tup             :: LCType freq t
                    => ExpTuple freq t
                    -> Exp freq t

    Prj             :: ( Nat idx
                       , LCType freq t
                       , e ~ PrjTup idx t
                       )
                    => idx
                    -> Exp freq t
                    -> Exp freq e

    -- loop support
    Loop            :: ( LCType freq s
                       , LCType freq a
                       )
                    => (Exp freq s -> Exp freq s)     -- state transform function
                    -> (Exp freq s -> Exp freq Bool)  -- loop condition function
                    -> (Exp freq s -> Exp freq a)     -- state to result transform function
                    -> Exp freq s                      -- initial state
                    -> Exp freq a                      -- result

    -- Array operations
    -- Construction
    ArrayFromList   :: LCType Obj a
                    => [Exp Obj a]
                    -> Exp Obj (Array order a)

    ArrayReplicate  :: LCType Obj a
                    => Exp Obj Int32
                    -> Exp Obj a
                    -> Exp Obj (Array order a)

    ArrayGenerate   :: LCType Obj a
                    => Exp Obj Int32
                    -> (Exp Obj Int32 -> Exp Obj a)
                    -> Exp Obj (Array order a)

    ArrayIterateN   :: LCType Obj a
                    => Exp Obj Int32
                    -> (Exp Obj a -> Exp Obj a)
                    -> Exp Obj a
                    -> Exp Obj (Array order a)

    -- Elementwise operations
    ArrayIndex      :: LCType Obj (Array order a)
                    => Exp Obj Int32
                    -> Exp Obj (Array order a)
                    -> Exp Obj a

    ArrayFilter     :: ( LCType freq a
                       , LCType freq (Array order a)
                       )
                    => (Exp freq a -> Exp freq Bool)
                    -> Exp freq (Array order a)
                    -> Exp freq (Array order a)

    ArrayMap        :: ( LCType freq a
                       , LCType freq b
                       , LCType freq (Array order a)
                       )
                    => (Exp freq a -> Exp freq b)
                    -> Exp freq (Array order a)
                    -> Exp freq (Array order b)

    ArrayZipWith    :: ( LCType freq a
                       , LCType freq b
                       , LCType freq c
                       , LCType freq (Array order a)
                       , LCType freq (Array order b)
                       )
                    => (Exp freq a -> Exp freq b -> Exp freq c)
                    -> Exp freq (Array order a)
                    -> Exp freq (Array order b)
                    -> Exp freq (Array order c)

    ArrayAccumulate :: ( LCType freq a
                       , LCType freq b
                       , LCType freq (Array order b)
                       )
                    => (Exp freq a -> Exp freq b -> Exp freq a)
                    -> Exp freq a
                    -> Exp freq (Array order b)
                    -> Exp freq a

    -- GPU pipeline model
    Fetch           :: ( LCType Obj (Array order a)
                       , LCType Obj (Array order Int32)
                       )
                    => FetchPrimitive primitive adjacency 
                    -> Exp Obj (Array order a)
                    -> Maybe (Exp Obj (Array order Int32))
                    -> Exp Obj (VertexStream primitive adjacency a)

    Transform       :: ( LCType V a
                       , LCType V (Vertex clipDistances b)
                       , LCType Obj (VertexStream primitive adjacency a)
                       )
                    => (Exp V a -> Exp V (Vertex clipDistances b))                       -- vertex shader
                    -> Exp Obj (VertexStream primitive adjacency a)
                    -> Exp Obj (PrimitiveStream primitive adjacency clipDistances N1 V b)

    Reassemble      :: ( LCType Obj (GeometryShader inputPrimitive inputAdjacency outputPrimitive inputClipDistances outputClipDistances layerCount a b)
                       , LCType Obj (PrimitiveStream inputPrimitive inputAdjacency inputClipDistances N1 V a)
                       )
                    => Exp Obj (GeometryShader inputPrimitive inputAdjacency outputPrimitive inputClipDistances outputClipDistances layerCount a b)
                    -> Exp Obj (PrimitiveStream inputPrimitive inputAdjacency inputClipDistances N1 V a)
                    -> Exp Obj (PrimitiveStream outputPrimitive NoAdjacency outputClipDistances layerCount G b)

    Rasterize       :: LCType Obj (PrimitiveStream primitive adjacency clipDistances layerCount freq a)
                    => RasterContext primitive
                    -> (Exp Obj V2I -> Exp Obj V4I) -- calculates viewport position and size from framebuffer size
                    -> Maybe (Exp Obj V2F)          -- Just: depth range (near,far) value or Nothing: depth clamp is disabled
                    -> Exp Obj (PrimitiveStream primitive adjacency clipDistances layerCount freq a)
                    -> Exp Obj (FragmentStream layerCount a)

    Accumulate      :: ( LCType F a
                       -- , LCType F (Fragment (NoStencilRepr b))
                       , LCType Obj (FragmentStream layerCount a)
                       , LCType Obj framebuffer
                       , IsValidOutput semantic    -- restriction: depth and stencil optional, arbitrary color component
                       , IsFrameBuffer framebuffer
                       --TODO: , IsMatchOutput semantic framebuffer b
                       )
                    => AccumulationContext semantic
                    -> Maybe (Exp Obj V2I -> Exp Obj V4I)           -- calculates scissor position and size from framebuffer size (optional)
                    -> Maybe (Exp F a -> Exp F Bool)                -- fragment filter function, we express discard using a filter function
                    -> DepthOutput hasDepth (Exp F a -> Exp F Float)-- depth value function
                    -> (Exp F a -> Exp F b)                         -- fragment shader
                    -> Exp Obj (FragmentStream layerCount a)
                    -> Exp Obj framebuffer
                    -> Exp Obj (framebuffer :+: ToOcclusionQuery hasDepth :+: ZZ)

    -- Transform feedback support
    ArrayFromStream :: LCType Obj (PrimitiveStream primitive adjacency clipDistances layerCount freq a)
                    => Exp Obj (PrimitiveStream primitive adjacency clipDistances layerCount freq a)
                    -> Exp Obj (Array order a)

    -- Image operations
    -- Construction
    -- specifies an empty image (pixel rectangle)
    -- hint: framebuffer is composed from images
    Image           :: (IsNum t, IsVecScalar d color t, Nat layerCount)
                    => layerCount
                    -> Exp Obj color -- initial value
                    -> Exp Obj (Image layerCount color)

    -- Layer projection
    PrjImage        :: ( Nat idx
                       , LesserEq idx layerCount
                       , LCType Obj (Image layerCount t)
                       )
                    => idx
                    -> Exp Obj (Image layerCount t)
                    -> Exp Obj (Image N1 t)

    -- Vertex
    -- result of a vertex or geometry shader function
    Vertex          :: IsFloatTuple clipDistances
                    => Exp freq V4F                 -- position
                    -> Exp freq Float               -- point size
                    -> ExpTuple freq clipDistances  -- clip distance []
                    -> InterpolatedExpTuple freq a
                    -> Exp freq (Vertex clipDistances a)

    -- Geometry
    -- describes a geometry shader
    GeometryShader  :: ( inputVertex ~ (V4F:+:Float:+:clipDistances:+:a:+:ZZ)
                       , input ~ PrimitiveVertices inputPrimitive inputAdjacency inputVertex
                       , LCType G input
                       , LCType G (i:+:Int32:+:ZZ)
                       , LCType G i
                       , LCType G (i:+:j:+:Int32:+:ZZ)
                       , LCType G j
                       , LCType G (Vertex outputClipDistances b)
                       , Nat layerCount
                       )
                    => layerCount                                                   -- geometry shader:
                    -> OutputPrimitive outputPrimitive                              -- output primitive
                    -> Int                                                          -- max amount of generated vertices
                    -> (Exp G input -> Exp G (i:+:Int32:+:ZZ))                      -- how many primitives?
                    -> (Exp G i -> Exp G (Int32:+:Int32:+:i:+:j:+:Int32:+:ZZ))      -- how many vertices? primtive loop, out:
                                                                                    --   gl_PrimitiveID; gl_Layer; loop var; vertex loop seed; vertex loop iteration count)
                    -> (Exp G j -> Exp G (j:+:Vertex outputClipDistances b:+:ZZ))   -- generate vertices
                    -> Exp Obj (GeometryShader inputPrimitive inputAdjacency outputPrimitive inputClipDistances outputClipDistances layerCount a b)

    -- Output
    Output          :: LCType Obj t
                    => ByteString
                    -> Exp Obj t
                    -> Exp Obj Output

    ScreenOutput    :: LCType Obj (Image N1 t)
                    => Exp Obj (Image N1 t)
                    -> Exp Obj Output
