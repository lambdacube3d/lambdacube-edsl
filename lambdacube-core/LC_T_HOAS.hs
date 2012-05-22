module LC_T_HOAS where

import GHC.Exts

import Data.ByteString.Char8
import Data.Typeable
import Data.Int

import TypeLevel.Number.Nat
import TypeLevel.Number.Nat.Num

import LCType

import LC_APIType
import LC_T_APIType
import LC_T_DSLType
import LC_T_PrimFun

class Exp exp where
    type Exp_GP exp :: * -> *
    type Exp_Input exp :: * -> *
    type Exp_PrimFun exp :: * -> * -> *
    type Exp_Texture exp :: (* -> *) -> * -> * -> * -> * -> *
    type Exp_FlatTuple exp :: (* -> Constraint) -> (* -> *) -> * -> *
    type Exp_InterpolatedFlatTuple exp :: (* -> Constraint) -> (* -> *) -> * -> *
    type Exp_TupleIdx exp :: * -> * -> *

    -- Needed for conversion to de Bruijn form
    tag     :: GPU t
            => Int
            -> exp stage t
                 -- environment size at defining occurrence

    lam     :: (exp stageA a -> exp stageB b)
            -> exp (stageA -> stageB) (a -> b)

    -- constant value
    cnst    :: IsScalar t
            => t
            -> exp stage t

    -- builtin variable
    primVar :: (GPU t
               ,input ~ Exp_Input exp)
            => input t
            -> exp stage t

    -- uniform value
    uni     :: (GPU t
               ,input ~ Exp_Input exp)
            => input t
            -> exp stage t

    -- conditional expression
    cond    :: GPU t
            => exp stage Bool
            -> exp stage t
            -> exp stage t
            -> exp stage t

    primApp :: (GPU a, GPU r
               ,primFun ~ Exp_PrimFun exp)
            => primFun stage (a -> r)
            -> exp stage a
            -> exp stage r

    -- tuple support

    tup     :: (GPU t, IsTuple t
               ,flatTuple ~ Exp_FlatTuple exp)
            => flatTuple GPU (exp stage) t
            -> exp stage t

    prj     :: (GPU e, GPU t, IsTuple t
               ,tupleIdx ~ Exp_TupleIdx exp)
            => tupleIdx (TupleRepr t) e
            -> exp stage t
            -> exp stage e

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
    vertexOut               :: (Interpolated interpolated
                               ,flatTuple ~ Exp_InterpolatedFlatTuple exp)
                            => exp V V4F      -- position
                            -> exp V Float    -- point size
                            -> flatTuple GPU (interpolated (exp V)) a
                            -> exp VertexOut (FTRepr a)

    geometryOut             :: (Interpolated interpolated
                               ,flatTuple ~ Exp_InterpolatedFlatTuple exp)
                            => exp G V4F      -- position
                            -> exp G Float    -- point size
                            -> exp G Int32    -- primitive ID
                            -> exp G Int32    -- layer
                            -> exp G j
                            -> flatTuple GPU (interpolated (exp G)) a
                            -> exp GeometryOut (j,(FTRepr a))

    fragmentOut             :: (flatTuple ~ Exp_FlatTuple exp)
                            => flatTuple GPU (exp F) a
                            -> exp FragmentOut (ColorRepr a)

    fragmentOutDepth        :: (flatTuple ~ Exp_FlatTuple exp)
                            => exp F Float
                            -> flatTuple GPU (exp F) a
                            -> exp FragmentOut (Depth Float :+: ColorRepr a)

    fragmentOutRastDepth    :: (flatTuple ~ Exp_FlatTuple exp)
                            => flatTuple GPU (exp F) a
                            -> exp FragmentOut (Depth Float :+: ColorRepr a)

    -- sampler support
    sampler                 :: (GPU (Sampler dim arr t ar)
                               ,gp ~ Exp_GP exp
                               ,texture ~ Exp_Texture exp)
                            => Filter
                            -> EdgeMode
                            -> texture gp dim arr t ar
                            -> exp stage (Sampler dim arr t ar)

-- Geometry
-- describes a geometry shader
class GeometryShader geometryShader where
    type GeometryShader_Exp geometryShader :: * -> * -> *
    type GeometryShader_Primitive geometryShader :: * -> *

    noGeometryShader    :: geometryShader prim prim N1 a a

    geometryShader      :: (GPU (PrimitiveVertices primIn a), GPU i, GPU j, GPU b, Nat layerNum
                           ,primitive ~ GeometryShader_Primitive geometryShader
                           ,exp ~ GeometryShader_Exp geometryShader)
                        => layerNum                                                 -- geometry shader:
                        -> primitive primOut                                        -- output primitive
                        -> Int                                                      -- max amount of generated vertices
                        -> (exp (G -> G) (PrimitiveVertices primIn a -> (i,Int32))) -- how many primitives?
                        -> (exp (G -> G) (i -> (i,j,Int32)))                        -- how many vertices?
                        -> (exp (G -> GeometryOut) (j -> (j,b)))                    -- generate vertices
                        -> geometryShader primIn primOut layerNum a b

-- fragment filter function, we express discard using a filter function
class FragmentFilter fragmentFilter where
    type FragmentFilter_Exp fragmentFilter :: * -> * -> *

    passAll :: fragmentFilter a

    filter  :: (exp ~ FragmentFilter_Exp fragmentFilter)
            => (exp (F -> F) (a -> Bool))
            -> fragmentFilter a

-- hint: GP stands for Graphics Pipeline
-- GP AST
class GP gp where
    type GP_Exp gp :: * -> * -> *
    type GP_GeometryShader gp :: * -> * -> * -> * -> * -> *
    type GP_RasterContext gp :: * -> *
    type GP_FragmentFilter gp :: * -> *
    type GP_FrameBuffer gp :: * -> * -> *
    type GP_AccumulationContext gp :: * -> *
    type GP_Primitive gp :: * -> *
    type GP_Image gp :: * -> * -> *
    type GP_TupleIdx gp :: * -> * -> *
    type GP_FlatTupleImage gp :: (* -> Constraint) -> (* -> *) -> * -> *
    type GP_FragmentOperation gp :: * -> *
    type GP_FlatTupleFragmentOperation gp :: (* -> Constraint) -> (* -> *) -> * -> *

    -- Needed for conversion to de Bruijn form
    gpTag           :: Typeable a
                    => Int
                    -> gp a -- FIXME: restrict valid types to shareable types

    fetch           :: (InputTuple a, SGPU (InputTupleRepr a)
                       ,primitive ~ GP_Primitive gp)
                    => ByteString
                    -> primitive prim
                    -> a
                    -> gp (VertexStream prim (InputTupleRepr a))

    transform       :: (GPU a, GPU b
                       ,exp ~ GP_Exp gp)
                    => (exp (V -> VertexOut) (a -> b))                       -- vertex shader
                    -> gp (VertexStream prim a)
                    -> gp (PrimitiveStream prim b)

    rasterize       :: (geometryShader ~ GP_GeometryShader gp
                       ,rasterContext ~ GP_RasterContext gp)
                    => rasterContext primOut
                    -> geometryShader primIn primOut layerNum a b
                    -> gp (PrimitiveStream primIn a)
                    -> gp (FragmentStream layerNum b)

    frameBuffer     :: (Image image, image ~ GP_Image gp
                       ,FlatTuple (image layerCount) flatTuple, flatTuple ~ GP_FlatTupleImage gp)
                    => V2U                                          -- size: width, height
                    -> flatTuple Typeable (image layerCount) t
                    -> gp (FrameBuffer layerCount (FTRepr' t))

    accumulate      :: (GPU a, GPU (FTRepr' b), IsValidOutput b
                       ,exp ~ GP_Exp gp
                       ,FragmentOperation fragmentOperation, fragmentOperation ~ GP_FragmentOperation gp
                       ,FlatTuple fragmentOperation flatTuple, flatTuple ~ GP_FlatTupleFragmentOperation gp
                       ,fragmentFilter ~ GP_FragmentFilter gp)    -- restriction: depth and stencil optional, arbitrary color component
                    => flatTuple Typeable fragmentOperation b
                    -> fragmentFilter a
                    -> (exp (F -> FragmentOut) (a -> (NoStencilRepr b)))     -- fragment shader
                    -> gp (FragmentStream layerCount a)
                    -> gp (FrameBuffer layerCount (FTRepr' b))
                    -> gp (FrameBuffer layerCount (FTRepr' b))

    prjFrameBuffer  :: (tupleIdx ~ GP_TupleIdx gp
                       ,image ~ GP_Image gp)
                    => ByteString                       -- internal image output (can be allocated on request)
                    -> tupleIdx (EltRepr b) t
                    -> gp (FrameBuffer layerCount b)
                    -> gp (image layerCount t)

    prjImage        :: (Nat idx, LesserEq idx layerCount
                       ,image ~ GP_Image gp)
                    => ByteString                       -- internal image output (can be allocated on request)
                    -> idx
                    -> gp (image layerCount t)
                    -> gp (image N1 t)
