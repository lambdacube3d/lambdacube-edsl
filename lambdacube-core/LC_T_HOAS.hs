module LC_T_HOAS where

import GHC.TypeLits

import Data.ByteString.Char8
import Data.Int

import LC_G_Type
import LC_T_APIType
import LC_T_DSLType
import LC_T_PrimFun

-- Common Exp, describes shader functions
data Exp :: Frequency -> * -> * where
    -- Needed for conversion to de Bruijn form
    Tag     :: GPU t
            => Int
            -> String
            -> Exp stage t
                 -- environment size at defining occurrence
{-
    -- function support
    Lam     :: (GPU a, GPU b)
            => (Exp stage a -> Exp stage b)
            -> Exp stage (a -> b)

    App     :: (GPU a, GPU b)
            => Exp stage a
            -> Exp stage (a -> b)
            -> Exp stage b
-}
    -- constant value
    Const   :: (GPU t,IsScalar t)
            => t
            -> Exp stage t

    -- builtin variable
    PrimVar :: GPU t
            => Input t
            -> Exp stage t

    -- uniform value
    Uni     :: GPU t
            => Input t
            -> Exp stage t

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
    Sampler :: (GPU (Sampler dim arr t ar), IsMipValid canMip mip, IsEdgeMode dim edgeMode, ConvertEdgeMode edgeMode, borderColor ~ TexDataRepr ar t, IsScalar borderColor)
            => { smpMinFilter   :: Filter mip
               , smpMagFilter   :: Filter TexNoMip
               , smpEdgeMode    :: edgeMode     -- Wrap S,T,R
               , smpBorderColor :: borderColor
               , smpMinLod      :: Maybe Float
               , smpMaxLod      :: Maybe Float
               , smpLodBias     :: Float
               , smpCompareFunc :: CompareMode t
               , smpTexture     :: Texture (Exp Obj) dim arr t ar canMip
               }
            -> Exp stage (Sampler dim arr t ar)

    -- loop support
    Loop    :: (GPU s, GPU a)
            => (Exp stage s -> Exp stage s)     -- state transform function
            -> (Exp stage s -> Exp stage Bool)  -- loop condition function
            -> (Exp stage s -> Exp stage a)     -- state to result transform function
            -> Exp stage s                      -- initial state
            -> Exp stage a                      -- result
    -- GP
    -- hint: GP stands for Graphics Pipeline
    Fetch           :: (InputTuple a, SGPU (InputTupleRepr a))
                    => ByteString
                    -> FetchPrimitive primitive
                    -> a
                    -> Exp Obj (VertexStream primitive (InputTupleRepr a))

    Transform       :: (GPU a, GPU b)
                    => (Exp V a -> VertexOut clipDistances b)                       -- vertex shader
                    -> Exp Obj (VertexStream primitive a)
                    -> Exp Obj (PrimitiveStream primitive clipDistances 1 V b)

    Reassemble      :: GeometryShader inputPrimitive outputPrimitive inputClipDistances outputClipDistances layerCount a b
                    -> Exp Obj (PrimitiveStream inputPrimitive inputClipDistances 1 V a)
                    -> Exp Obj (PrimitiveStream outputPrimitive outputClipDistances layerCount G b)

    Rasterize       :: RasterContext primitive
                    -> Exp Obj (PrimitiveStream primitive clipDistances layerCount freq a)
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

    PrjFrameBuffer  :: ByteString                       -- internal image output (can be allocated on request)
                    -> TupleIdx (EltRepr b) t
                    -> Exp Obj (FrameBuffer layerCount b)
                    -> Exp Obj (Image layerCount t)

    PrjImage        :: ((idx + 1) <= layerCount, 2 <= layerCount, SingI idx)
                    => ByteString                       -- internal image output (can be allocated on request)
                    -> NatNum idx
                    -> Exp Obj (Image layerCount t)
                    -> Exp Obj (Image 1 t)
{-
    -- dynamic extension support
    AccumulateSet   :: GPU a
                    => ByteString
                    -> Exp Obj (FrameBuffer layerCount a)
                    -> Exp Obj (FrameBuffer layerCount a)
-}

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
-- result of a vertex or geometry shader function
data VertexOut clipDistances t where
    VertexOut   :: IsFloatTuple clipDistances
                => Exp V V4F      -- position
                -> Exp V Float    -- point size
                -> FlatExp V clipDistances   -- clip distance []
                -> InterpolatedFlatExp V a
                -> VertexOut (FTRepr clipDistances) (FTRepr a)

-- Geometry
-- describes a geometry shader
data GeometryShader (inPrimitive :: PrimitiveType) (outPrimitive :: PrimitiveType) inClipDistances outClipDistances (layerCount :: Nat) a b where
    GeometryShader  :: (GPU j, GPU i, GPU b, GPU outputClipDistances, GPU input, SingI layerCount
                       , inputVertex ~ (V4F,Float,inputClipDistances,a)
                       , input ~ PrimitiveVertices inputPrimitive inputVertex
                       )
                    => NatNum layerCount                                            -- geometry shader:
                    -> OutputPrimitive outputPrimitive                              -- output primitive
                    -> Int                                                          -- max amount of generated vertices
                    -> (Exp G input -> Exp G (i,Int32))                             -- how many primitives?
                    -> (Exp G i -> Exp G (Int32,Int32,i,j,Int32))                   -- how many vertices? primtive loop, out:
                                                                                    --   gl_PrimitiveID; gl_Layer; loop var; vertex loop seed; vertex loop iteration count)
                    -> (Exp G j -> GeometryOut j outputClipDistances b)             -- generate vertices
                    -> GeometryShader inputPrimitive outputPrimitive inputClipDistances outputClipDistances layerCount a b
{-
    GeometryShader      :: (GPU (PrimitiveVertices primIn a), GPU i, GPU j, GPU b, IsPrimitive primIn, IsPrimitive primOut, SingI layerNum)
                        => NatNum layerNum                                          -- geometry shader:
                        -> primOut                                                  -- output primitive
                        -> Int                                                      -- max amount of generated vertices
                        -> (Exp G (PrimitiveVertices primIn a) -> Exp G (i,Int32))  -- how many primitives?
                        -> (Exp G i -> Exp G (i,j,Int32))                           -- how many vertices?
                        -> (Exp G j -> GeometryOut (j,b))                           -- generate vertices
                        -> GeometryShader primIn primOut layerNum a b
-}

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
data GeometryOut i clipDistances t where
    GeometryOut :: IsFloatTuple clipDistances
                => Exp G i
                -> Exp G V4F      -- position
                -> Exp G Float    -- point size
                -> FlatExp G clipDistances   -- clip distance []
                -> InterpolatedFlatExp G a
                -> GeometryOut i (FTRepr clipDistances) (FTRepr a)

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


data GPOutput (o :: OutputType) where
    ImageOut    :: ByteString
                -> V2U
                -> Exp Obj (Image 1 t)
                -> GPOutput SingleOutput

    ScreenOut   :: Exp Obj (Image 1 t)
                -> GPOutput SingleOutput

    MultiOut    :: [GPOutput SingleOutput]
                -> GPOutput MultiOutput
