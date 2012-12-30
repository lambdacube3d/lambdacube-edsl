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

-- Common Exp, describes shader functions
data Exp freq t where
    -- Needed for conversion to de Bruijn form
    Tag     :: LCType freq t
            => Int
            -> Exp freq t
                 -- environment size at defining occurrence

    -- constant value
    -- TODO: support constants for all LCTypes
    Const   :: LCType freq t
            => InputValue freq t
            -> Exp freq t

    -- User input constant variable
    Input   :: LCType freq t
            => ByteString
            -> InputType freq t
            -> Exp Obj t

    -- Lift Obj expressions to higher frequencies
    Use     :: ( LCType freq t
               , LCType Obj t
               )
            => Exp Obj t
            -> Exp freq t

    -- conditional expression
    Cond    :: LCType freq t
            => Exp freq Bool
            -> Exp freq t
            -> Exp freq t
            -> Exp freq t

    PrimApp :: LCType freq a
            => PrimFun freq (a -> r)
            -> Exp freq a
            -> Exp freq r

    Tup     :: LCType freq t
            => ExpTuple freq t
            -> Exp freq t

    Prj     :: ( Nat idx
               , LCType freq t
               , e ~ PrjTup idx t
               )
            => idx
            -> Exp freq t
            -> Exp freq e

    -- loop support
    Loop    :: ( LCType freq s
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
{-
    -- TODO: should be done through input api
    -- Graphics pipeline extensibility
    -- dynamic extension support
    AccumulateSet   :: LCType freq (FrameBuffer layerCount a)
                    => ByteString
                    -> Exp Obj (FrameBuffer layerCount a)
                    -> Exp Obj (FrameBuffer layerCount a)
-}
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

    FrameBuffer     :: FrameBuffer layerCount t
                    -> Exp Obj (FrameBuffer layerCount (t))--TODO ftrepr'

    Accumulate      :: ( LCType F a
                       , LCType F (Fragment (NoStencilRepr b))
                       , LCType Obj (FragmentStream layerCount a)
                       , LCType Obj (FrameBuffer layerCount b)
                       , IsValidOutput b    -- restriction: depth and stencil optional, arbitrary color component
                       )
                    => AccumulationContext b
                    -> Maybe (Exp Obj V2I -> Exp Obj V4I)   -- calculates scissor position and size from framebuffer size (optional)
                    -> Maybe (Exp F a -> Exp F Bool)        -- fragment filter function, we express discard using a filter function
                    -> (Exp F a -> Exp F (Fragment (NoStencilRepr b)))     -- fragment shader
                    -> Exp Obj (FragmentStream layerCount a)
                    -> Exp Obj (FrameBuffer layerCount (b))--TODO ftrepr'
                    -> Exp Obj (FrameBuffer layerCount (b))--TODO ftrepr'

    -- Transform feedback support
    ArrayFromStream :: LCType Obj (PrimitiveStream primitive adjacency clipDistances layerCount freq a)
                    => Exp Obj (PrimitiveStream primitive adjacency clipDistances layerCount freq a)
                    -> Exp Obj (Array order a)

    -- FrameBuffer and Image helpers
    PrjFrameBuffer  :: ( Nat idx
                       , e ~ PrjTup idx t
                       , LCType Obj (FrameBuffer layerCount b)
                       )
                    => idx
                    -> Exp Obj (FrameBuffer layerCount b)
                    -> Exp Obj (Image layerCount e)

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

    -- Fragment
    -- result of a fragment shader function
    Fragment            :: ExpTuple F a
                        -> Exp F (Fragment (ColorRepr a))

    FragmentDepth       :: Exp F Float
                        -> ExpTuple F a
                        -> Exp F (Fragment (Depth Float :+: ColorRepr a))

    FragmentRastDepth   :: ExpTuple F a
                        -> Exp F (Fragment (Depth Float :+: ColorRepr a))

    -- Output
    ImageOut        :: LCType Obj (Image layerCount t)
                    => ByteString
                    -> Exp Obj (Image layerCount t)
                    -> Exp Obj Output

    ScreenOut       :: LCType Obj (Image N1 t)
                    => Exp Obj (Image N1 t)
                    -> Exp Obj Output
