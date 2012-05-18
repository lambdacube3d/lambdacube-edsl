module LCHOASTypeclass where

import Data.ByteString.Char8
import Data.Typeable
import Data.Int

import TypeLevel.Number.Nat
import TypeLevel.Number.Nat.Num

import LCType
import LCAPITypeTypeclass
import LCDSLTypeTypeclass
import LCPrimFunTypeclass

import qualified LCHOASUntyped as HU -- HOAS Untyped
import qualified LCPrimFunUntyped as HU -- HOAS Untyped

{-
class Exp repr where
   constant :: Int -> repr Int
   variable :: String -> repr Int
   add      :: repr Int -> repr Int -> repr Int

newtype ExpI t = ExpI (ExpI.Exp)
-- * Question: why do we need the wrapper?

instance Exp ExpI where
    constant = ExpI . ExpI.Constant
    variable = ExpI . ExpI.Variable
    add (ExpI x) (ExpI y) = ExpI (ExpI.Add x y)
-}

class Exp exp where
    -- Needed for conversion to de Bruijn form
    tag     :: GPU t
            => Int
            -> exp stage t
                 -- environment size at defining occurrence

    -- constant value
    const   :: IsScalar t
            => t
            -> exp stage t

    -- builtin variable
    primVar :: (GPU t, Input input)
            => input t
            -> exp stage t

    -- uniform value
    uni     :: (GPU t, Input input)
            => input t
            -> exp stage t

    -- conditional expression
    cond    :: GPU t
            => exp stage Bool
            -> exp stage t
            -> exp stage t
            -> exp stage t

    primApp :: (GPU a, GPU r, PrimFun primFun)
            => primFun stage (a -> r)
            -> exp stage a
            -> exp stage r

    -- tuple support
    tup     :: (GPU t, IsTuple t, Tuple tuple)
            => tuple (exp stage) (TupleRepr t)
            -> exp stage t

    prj     :: (GPU e, GPU t, IsTuple t, TupleIdx tupleIdx)
            => tupleIdx (TupleRepr t) e
            -> exp stage t
            -> exp stage e

    -- sampler support
    sampler :: (GPU (Sampler dim arr t ar), GP gp, Texture texture)
            => Filter
            -> EdgeMode
            -> texture gp dim arr t ar
            -> exp stage (Sampler dim arr t ar)

type InterpolatedFlatExp repr stage a = (Interpolated reprI, FlatTuple reprFT) => reprFT GPU (reprI (repr stage)) a
type FlatExp repr stage a = FlatTuple reprFT => reprFT GPU (repr stage) a

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

class VertexOut vertexOut where
    vertexOut   :: Exp exp
                => exp V V4F      -- position
                -> exp V Float    -- point size
                -> InterpolatedFlatExp exp V a
                -> vertexOut (FTRepr a)

-- Geometry
-- describes a geometry shader
class GeometryShader geometryShader where
    noGeometryShader    :: geometryShader prim prim N1 a a

    geometryShader      :: (GPU (PrimitiveVertices primIn a), GPU i, GPU j, GPU b, IsPrimitive primIn, IsPrimitive primOut, Nat layerNum,
                            Exp exp, GeometryOut geometryOut)
                        => layerNum                                                 -- geometry shader:
                        -> primOut                                                  -- output primitive
                        -> Int                                                      -- max amount of generated vertices
                        -> (exp G (PrimitiveVertices primIn a) -> exp G (i,Int32))  -- how many primitives?
                        -> (exp G i -> exp G (i,j,Int32))                           -- how many vertices?
                        -> (exp G j -> geometryOut (j,b))                           -- generate vertices
                        -> geometryShader primIn primOut layerNum a b

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
class GeometryOut geometryOut where
    geometryOut :: Exp exp
                => exp G V4F      -- position
                -> exp G Float    -- point size
                -> exp G Int32    -- primitive ID
                -> exp G Int32    -- layer
                -> exp G j
                -> InterpolatedFlatExp exp G a
                -> geometryOut (j,(FTRepr a))

-- Fragment
{-
    Fragment shader builtin output:
            float gl_FragDepth  -- Optional
-}
-- result of a fragment shader function
class FragmentOut fragmentOut where
    fragmentOut             :: Exp exp
                            => FlatExp exp F a
                            -> fragmentOut (ColorRepr a)

    fragmentOutDepth        :: Exp exp
                            => exp F Float
                            -> FlatExp exp F a
                            -> fragmentOut (Depth Float :+: ColorRepr a)

    fragmentOutRastDepth    :: Exp exp
                            => FlatExp exp F a
                            -> fragmentOut (Depth Float :+: ColorRepr a)

-- fragment filter function, we express discard using a filter function
class FragmentFilter fragmentFilter where
    passAll :: fragmentFilter a

    filter  :: Exp exp
            => (exp F a -> exp F Bool)
            -> fragmentFilter a

-- hint: GP stands for Graphics Pipeline
-- GP AST
class GP gp where
    -- Needed for conversion to de Bruijn form
    gpTag           :: Typeable a
                    => Int
                    -> gp a -- FIXME: restrict valid types to shareable types

    fetch           :: (InputTuple a, SGPU (InputTupleRepr a), IsPrimitive prim)
                    => ByteString
                    -> prim
                    -> a
                    -> gp (VertexStream prim (InputTupleRepr a))

    transform       :: (GPU a, GPU b, Exp exp, VertexOut vertexOut)
                    => (exp V a -> vertexOut b)                       -- vertex shader
                    -> gp (VertexStream prim a)
                    -> gp (PrimitiveStream prim b)

    rasterize       :: (GeometryShader geometryShader, RasterContext rasterContext)
                    => rasterContext primOut
                    -> geometryShader primIn primOut layerNum a b
                    -> gp (PrimitiveStream primIn a)
                    -> gp (FragmentStream layerNum b)

    frameBuffer     :: V2U                                          -- size: width, height
                    -> FrameBuffer layerCount t
                    -> gp (FrameBuffer layerCount (FTRepr' t))

    accumulate      :: (GPU a, GPU (FTRepr' b), IsValidOutput b,
                        Exp exp, FragmentOut fragmentOut, FragmentFilter fragmentFilter)    -- restriction: depth and stencil optional, arbitrary color component
                    => FragmentContext b
                    -> fragmentFilter a
                    -> (exp F a -> fragmentOut (NoStencilRepr b))     -- fragment shader
                    -> gp (FragmentStream layerCount a)
                    -> gp (FrameBuffer layerCount (FTRepr' b))
                    -> gp (FrameBuffer layerCount (FTRepr' b))

    prjFrameBuffer  :: (TupleIdx tupleIdx, Image image)
                    => ByteString                       -- internal image output (can be allocated on request)
                    -> tupleIdx (EltRepr b) t
                    -> gp (FrameBuffer layerCount b)
                    -> gp (image layerCount t)

    prjImage        :: (LesserEq idx layerCount, Image image)
                    => ByteString                       -- internal image output (can be allocated on request)
                    -> idx
                    -> gp (image layerCount t)
                    -> gp (image N1 t)

--deriving instance Typeable1 GP

{-
data Exp
    = Tag Int
    | Const Value
    | PrimVar ByteString InputType
    | Uni ByteString InputType
    | Cond Exp Exp Exp
    | PrimApp PrimFun Exp
--    | Tup
-}
newtype ExpI stage t = ExpI (HU.Exp)
newtype PrimFunI stage t = PrimFunI (HU.PrimFun)

instance Exp ExpI where
    tag     = ExpI . HU.Tag
    const   = ExpI . HU.Const . toValue
    primVar = ExpI . uncurry HU.PrimVar . toInput
    uni     = ExpI . uncurry HU.Uni . toInput
    cond (ExpI c) (ExpI t) (ExpI e) = ExpI (HU.Cond c t e)
--    primApp (PrimFunI f) (ExpI a) = ExpI $ HU.PrimApp f a
{-
    primApp :: (GPU a, GPU r, PrimFun reprPF)
            => reprPF stage (a -> r)
            -> repr stage a
            -> repr stage r
-}
