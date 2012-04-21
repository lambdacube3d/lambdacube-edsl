module LCDeBruijn where

import Data.ByteString.Char8
import Data.Set (Set)
import Data.Typeable
import qualified Data.Set as Set
import Data.Int

import TypeLevel.Number.Nat
import TypeLevel.Number.Nat.Num

import LCType
import LCAPIType
import LCDSLType
import LCPrimFun
import LCDeBruijnUtil

-- Common Exp and Fun

-- |Parametrised open function abstraction
--
data OpenFun e env genv t where
    Body    :: e env genv t
            -> OpenFun e env genv t

    Lam     :: OpenFun e (env, a) genv t
            -> OpenFun e env genv (a -> t)

-- |Vanilla function without free scalar variables
--
type Fun e = OpenFun e ()

-- Embedded expressions
-- --------------------
data OpenExp stage env genv t where
    Var     :: GPU t
            => Idx env t
            -> OpenExp stage env genv t

    -- constant value
    Const   :: GPU t
            => t
            -> OpenExp stage env genv t

    -- builtin variable
    PrimVar :: GPU t
            => Input t
            -> OpenExp stage env genv t

    -- uniform value
    Uni     :: (InputTuple t, GPU (InputTupleRepr t))
            => t
            -> OpenExp stage env genv (InputTupleRepr t)

    -- tuple support
    Tup     :: (GPU t, IsTuple t)
            => Tuple (OpenExp stage env genv) (TupleRepr t)
            -> OpenExp stage env genv t

    Prj     :: (GPU e, GPU t, IsTuple t)
            => TupleIdx (TupleRepr t) e
            -> OpenExp stage env genv t
            -> OpenExp stage env genv e

    -- conditional expression
    Cond    :: GPU t
            => OpenExp stage env genv Bool
            -> OpenExp stage env genv t
            -> OpenExp stage env genv t
            -> OpenExp stage env genv t

    -- primitive operations
    PrimApp :: (GPU a, GPU r)
            => PrimFun stage (a -> r)
            -> OpenExp stage env genv a
            -> OpenExp stage env genv r

    -- sampler support
    Sampler :: GPU (Sampler dim arr t ar)
            => Filter
            -> EdgeMode
--            -> Texture GP dim arr t ar    -- FIXME: this should be ok as well, possible GHC bug??
            -> Texture (OpenGP ()) dim arr t ar
            -> OpenExp stage env genv (Sampler dim arr t ar)

type Exp stage genv t = OpenExp stage () genv t
type OpenFlatExp stage env genv a = FlatTuple GPU (OpenExp stage env genv) a
type OpenInterpolatedFlatExp stage env genv a = FlatTuple GPU (Interpolated (OpenExp stage env genv)) a

-- Fragment
data OpenFragmentOut env genv t where
    FragmentOut             :: OpenFlatExp F env genv a
                            -> OpenFragmentOut env genv (ColorRepr a)

    FragmentOutDepth        :: OpenExp F env genv Float
                            -> OpenFlatExp F env genv a
                            -> OpenFragmentOut env genv (Depth Float :+: ColorRepr a)

    FragmentOutRastDepth    :: OpenFlatExp F env genv a
                            -> OpenFragmentOut env genv (Depth Float :+: ColorRepr a)

data FragmentFilter genv a where
    PassAll :: FragmentFilter genv a

    Filter  :: Fun (OpenExp F) genv (a -> Bool)
            -> FragmentFilter genv a

-- Vertex
data OpenVertexOut env genv t where
    VertexOut   :: OpenExp V env genv V4F      -- position
                -> OpenExp V env genv Float    -- point size
                -> OpenInterpolatedFlatExp V env genv a
                -> OpenVertexOut env genv (FTRepr a)

-- Geometry
data OpenGeometryOut env genv t where
    GeometryOut :: OpenExp G env genv V4F      -- position
                -> OpenExp G env genv Float    -- point size
                -> OpenExp G env genv Int32    -- primitive ID
                -> OpenExp G env genv Int32    -- layer
                -> OpenExp G env genv j
                -> OpenInterpolatedFlatExp G env genv a
                -> OpenGeometryOut env genv (j,(FTRepr a))

data GeometryShader genv primIn primOut layerNum a b where
    NoGeometryShader    :: GeometryShader genv prim prim N0 a a

    GeometryShader      :: (GPU (PrimitiveVertices primIn a), GPU i, GPU j, GPU b, IsPrimitive primIn, IsPrimitive primOut, Nat layerNum)
                        => layerNum                                                         -- geometry shader:
                        -> primOut                                                          -- output primitive
                        -> Int                                                              -- max amount of generated vertices
                        -> Fun (OpenExp G) genv ((PrimitiveVertices primIn a) -> (i,Int32)) -- how many primitives?
                        -> Fun (OpenExp G) genv (i -> (i,j,Int32))                          -- how many vertices?
                        -> Fun OpenGeometryOut genv (j -> (j,b))                            -- generate vertices
                        -> GeometryShader genv primIn primOut layerNum a b

-- GP and GPFun
data OpenGPfun genv t where
    GPbody  :: OpenGP genv t
            -> OpenGPfun genv t

    GPlam   :: OpenGPfun (genv, a) t
            -> OpenGPfun genv (s -> t)

type GPfun = OpenGPfun ()

data OpenGP genv t where
    -- the following required only for sharing
    Let             :: OpenGP genv        bnd               -- bound expression
                    -> OpenGP (genv, bnd) body              -- the bound expr's scope
                    -> OpenGP genv        body

    GPvar           :: Idx genv t
                    -> OpenGP genv t

    Apply           :: GPfun (a -> b)
                    -> OpenGP genv a
                    -> OpenGP genv b

    Fetch           :: (InputTuple a, SGPU (InputTupleRepr a), IsPrimitive prim)
                    => ByteString
                    -> prim
                    -> a
                    -> OpenGP genv (VertexStream prim (InputTupleRepr a))

    Transform       :: (GPU a, GPU b)
                    => Fun OpenVertexOut genv (a -> b)                      -- vertex shader
                    -> OpenGP genv (VertexStream prim a)
                    -> OpenGP genv (PrimitiveStream prim b)

    Rasterize       :: RasterContext primOut
                    -> GeometryShader genv primIn primOut layerNum a b
                    -> OpenGP genv (PrimitiveStream primIn a)
                    -> OpenGP genv (FragmentStream layerNum b)

    FrameBuffer     :: V2U
                    -> FrameBuffer sh t
                    -> OpenGP genv (FrameBuffer sh (FTRepr' t))

    Accumulate      :: (GPU a, GPU (FTRepr' b), IsValidOutput b)        -- restriction: depth and stencil optional, arbitrary color component
                    => FragmentContext b
                    -> FragmentFilter genv a
                    -> Fun OpenFragmentOut genv (a -> (NoStencilRepr b))    -- fragment shader
                    -> OpenGP genv (FragmentStream sh a)
                    -> OpenGP genv (FrameBuffer sh (FTRepr' b))
                    -> OpenGP genv (FrameBuffer sh (FTRepr' b))

    PrjFrameBuffer  :: ByteString                       -- internal image output (can be allocated on request)
                    -> TupleIdx (EltRepr b) t
                    -> GP (FrameBuffer sh b)
                    -> OpenGP genv (Image sh t)

    PrjImage        :: (LesserEq idx sh)
                    => ByteString                       -- internal image output (can be allocated on request)
                    -> idx
                    -> GP (Image sh t)
                    -> OpenGP genv (Image N0 t)

type GP t = OpenGP () t
deriving instance Typeable2 OpenGP

-- utility functions
{-
data StreamInput -- slot name, primitive type, stream input name and type
    = StreamInput ByteString PrimitiveType [(ByteString,InputType)] deriving Show

streamInput :: GP t -> [StreamInput]
streamInput (Fetch n p a)               = [StreamInput n (toPrimitive p) (toInputList a)]
streamInput (Transform _ vs)            = streamInput vs
streamInput (Rasterize _ _ ps)          = streamInput ps
streamInput (Accumulate _ _ _ fs fb)    = streamInput fs ++ streamInput fb
streamInput _                           = []
-}

uniformInputExp :: OpenExp stage env genv t -> Set (ByteString,InputType)
uniformInputExp (Uni a)         = Set.fromList $! toInputList a
uniformInputExp (Tup a)         = collect a
  where
    collect :: Tuple (OpenExp stage env genv) t' -> Set (ByteString,InputType)
    collect NilTup          = Set.empty
    collect (SnocTup tup e) = uniformInputExp e `Set.union` collect tup
uniformInputExp (Prj _ a)       = uniformInputExp a
uniformInputExp (Cond a b c)    = uniformInputExp a `Set.union` uniformInputExp b `Set.union` uniformInputExp c
uniformInputExp (PrimApp _ a)   = uniformInputExp a
uniformInputExp _               = Set.empty

uniformInputInterpolatedFlatExp :: OpenInterpolatedFlatExp stage env genv t -> Set (ByteString,InputType)
uniformInputInterpolatedFlatExp (Flat e :. xs)          = uniformInputExp e `Set.union` uniformInputInterpolatedFlatExp xs
uniformInputInterpolatedFlatExp (Smooth e :. xs)        = uniformInputExp e `Set.union` uniformInputInterpolatedFlatExp xs
uniformInputInterpolatedFlatExp (NoPerspective e :. xs) = uniformInputExp e `Set.union` uniformInputInterpolatedFlatExp xs
uniformInputInterpolatedFlatExp _ = Set.empty

uniformInputFlatExp :: OpenFlatExp stage env genv t -> Set (ByteString,InputType)
uniformInputFlatExp (e :. xs)   = uniformInputExp e `Set.union` uniformInputFlatExp xs
uniformInputFlatExp _           = Set.empty

--uniformInputGS = undefined
--uniformInputFun = undefined
{-
uniformInputFun :: OpenFun OpenExp env genv a -> Set (ByteString,InputType)
uniformInputFun (Lam f)   = uniformInputFun f
uniformInputFun (Body e)  = uniformInputExp e
-}
