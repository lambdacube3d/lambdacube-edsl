module LC_GADT_HOAS where

--import GHC.Exts

import Data.ByteString.Char8
import Data.Typeable
import Data.Int

import TypeLevel.Number.Nat
import TypeLevel.Number.Nat.Num

import LCType

import LC_APIType
import LC_I_APIType
import LC_I_PrimFun
import LC_T_APIType hiding (Texture, Interpolated, FlatTuple)
import LC_T_DSLType

infixr 1 :.
data FlatTuple c a stage t where
    ZT      :: FlatTuple c a stage ZZ

    (:.)    :: c t
            => a (stage t)
            -> FlatTuple c a stage t'
            -> FlatTuple c a stage (t :+: t')

data Texture dim arr t ar where
    TextureSlot     :: (IsValidTextureSlot t)
                    => ByteString -- texture slot name
                    -> TextureTypeI dim mip arr layerCount t ar
                    -> Texture dim arr t ar
    -- TODO:
    --  add texture internal format specification
    Texture         :: TextureTypeI dim (MipRepr mip) arr layerCount t ar
                    -> MipMapI mip
                    -> [GP (FrameImage layerCount t)] -- backend must check the length of list
                    -> Texture dim arr t ar

data Interpolated a where
    Flat            :: Exp a -> Interpolated a

    Smooth          :: IsFloating a
                    => Exp a -> Interpolated a

    NoPerspective   :: IsFloating a
                    => Exp a -> Interpolated a

data Exp t where
    -- Needed for conversion to de Bruijn form
    Tag     :: GPU t
            => Int
            -> Exp (stage t)
                 -- environment size at defining occurrence

    -- constant value
    Const   :: IsScalar t
            => t
            -> Exp (stage t)

    -- builtin variable
    PrimVar :: GPU t
            => InputI t
            -> Exp (stage t)

    -- uniform value
    Uni     :: GPU t
            => InputI t
            -> Exp (stage t)

    -- conditional expression
    Cond    :: GPU t
            => Exp (stage Bool)
            -> Exp (stage t)
            -> Exp (stage t)
            -> Exp (stage t)

    PrimApp :: (GPU a, GPU r)
            => PrimFunI stage (a -> r)
            -> Exp (stage a)
            -> Exp (stage r)

    -- tuple support
    Tup     :: (GPU t, IsTuple t)
            => FlatTuple GPU Exp stage t
            -> Exp (stage t)

    Prj     :: (GPU e, GPU t, IsTuple t)
            => TupleIdxI (TupleRepr t) e
            -> Exp (stage t)
            -> Exp (stage e)

-- TODO: add support for gl_ClipDistance setup
-- result of a vertex shader function
{-
    Vertex shader builtin output:
            gl_PerVertex {
                vec4  gl_Position
                float gl_PointSize
                float gl_ClipDistance[]
            }

    Geometry shader builtin output:
            gl_PerVertex {
                vec4  gl_Position
                float gl_PointSize
                float gl_ClipDistance[]
            }
            int gl_PrimitiveID
            int gl_Layer

    Fragment shader builtin output:
            float gl_FragDepth  -- Optional
-}
    VertexOut               :: Exp (V V4F)      -- position
                            -> Exp (V Float)    -- point size
                            -> FlatTuple GPU Interpolated V a
                            -> Exp (VertexOut (FTRepr a))

    GeometryOut             :: Exp (G V4F)      -- position
                            -> Exp (G Float)    -- point size
                            -> Exp (G Int32)    -- primitive ID
                            -> Exp (G Int32)    -- layer
                            -> Exp (G j)
                            -> FlatTuple GPU Interpolated G a
                            -> Exp (GeometryOut (j,(FTRepr a)))

    FragmentOut             :: FlatTuple GPU Exp F a
                            -> Exp (FragmentOut (ColorRepr a))

    FragmentOutDepth        :: Exp (F Float)
                            -> FlatTuple GPU Exp F a
                            -> Exp (FragmentOut (Depth Float :+: ColorRepr a))

    FragmentOutRastDepth    :: FlatTuple GPU Exp F a
                            -> Exp (FragmentOut (Depth Float :+: ColorRepr a))

    -- sampler support
    Sampler                 :: (GPU (Sampler dim arr t ar))
                            => Filter
                            -> EdgeMode
                            -> Texture dim arr t ar
                            -> Exp (stage (Sampler dim arr t ar))

-- Geometry
-- describes a geometry shader
data GeometryShader primIn primOut layerNum a b where
    NoGeometryShader    :: GeometryShader prim prim N1 a a

    GeometryShader      :: (GPU (PrimitiveVertices primIn a), GPU i, GPU j, GPU b, Nat layerNum)
                        => layerNum                                                     -- geometry shader:
                        -> PrimitiveI primOut                                           -- output primitive
                        -> Int                                                          -- max amount of generated vertices
                        -> (Exp (G (PrimitiveVertices primIn a)) -> Exp (G (i,Int32)))  -- how many primitives?
                        -> (Exp (G i) -> Exp (G (i,j,Int32)))                           -- how many vertices?
                        -> (Exp (G j) -> Exp (GeometryOut (j,b)))                       -- generate vertices
                        -> GeometryShader primIn primOut layerNum a b

-- fragment filter function, we express discard using a filter function
data FragmentFilter a where
    PassAll :: FragmentFilter a

    Filter  :: (Exp (F a) -> Exp (F Bool))
            -> FragmentFilter a

-- hint: GP stands for Graphics Pipeline
-- GP AST
data GP a where
    -- Needed for conversion to de Bruijn form
    GPTag           :: Typeable a
                    => Int
                    -> GP a -- FIXME: restrict valid types to shareable types

    Fetch           :: (InputTuple a, SGPU (InputTupleRepr a))
                    => ByteString
                    -> PrimitiveI prim
                    -> a
                    -> GP (VertexStream prim (InputTupleRepr a))

    Transform       :: (GPU a, GPU b)
                    => (Exp (V a) -> Exp (VertexOut b))                       -- vertex shader
                    -> GP (VertexStream prim a)
                    -> GP (PrimitiveStream prim b)

    Rasterize       :: RasterContextI primOut
                    -> GeometryShader primIn primOut layerNum a b
                    -> GP (PrimitiveStream primIn a)
                    -> GP (FragmentStream layerNum b)

    FrameBuffer     :: V2U                                          -- size: width, height
                    -> FlatTuple Typeable (ImageI layerCount) GFX t
                    -> GP (FrameBuffer layerCount (FTRepr' t))

    Accumulate      :: (GPU a, GPU (FTRepr' b), IsValidOutput b)    -- restriction: depth and stencil optional, arbitrary color component
                    => FlatTuple Typeable FragmentOperationI GFX b
                    -> FragmentFilter a
                    -> (Exp (F a) -> Exp (FragmentOut (NoStencilRepr b)))     -- fragment shader
                    -> GP (FragmentStream layerCount a)
                    -> GP (FrameBuffer layerCount (FTRepr' b))
                    -> GP (FrameBuffer layerCount (FTRepr' b))

    PrjFrameBuffer  :: ByteString                       -- internal image output (can be allocated on request)
                    -> TupleIdxI (EltRepr b) t
                    -> GP (FrameBuffer layerCount b)
                    -> GP (FrameImage layerCount t)

    PrjImage        :: (Nat idx, LesserEq idx layerCount)
                    => ByteString                       -- internal image output (can be allocated on request)
                    -> idx
                    -> GP (FrameImage layerCount t)
                    -> GP (FrameImage N1 t)
