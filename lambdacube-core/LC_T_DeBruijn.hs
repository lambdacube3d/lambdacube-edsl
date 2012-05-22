module LC_T_DeBruijn where

import GHC.Exts

import Data.ByteString.Char8
import Data.Set (Set)
import Data.Typeable
import qualified Data.Set as Set
import Data.Int

import TypeLevel.Number.Nat
import TypeLevel.Number.Nat.Num

import LCType

import LC_APIType
import LC_T_APIType
import LC_T_DSLType
import LC_T_PrimFun
import LC_T_DeBruijnUtil

-- Common Exp and Fun

-- |Parametrised open function abstraction
--
class OpenFun openFun where
    body    :: e env genv t
            -> openFun e env genv t

    lam     :: openFun e (env, a) genv t
            -> openFun e env genv (a -> t)

-- Embedded expressions
-- --------------------
class OpenExp openExp where
    type OpenExp_Idx openExp :: * -> * -> *
    type OpenExp_Input openExp :: * -> *
    type OpenExp_Tuple openExp :: (* -> *) -> * -> *
    type OpenExp_TupleIdx openExp :: * -> * -> *
    type OpenExp_PrimFun openExp :: * -> * -> *
    type OpenExp_Texture openExp :: (* -> *) -> * -> * -> * -> * -> *
    type OpenExp_OpenGP openExp :: * -> * -> *

    var     :: (GPU t, Idx idx, idx ~ OpenExp_Idx openExp)
            => idx env t
            -> openExp stage env genv t

    -- constant value
    cnst    :: IsScalar t
            => t
            -> openExp stage env genv t

    -- builtin variable
    primVar :: (GPU t
               , Input input, input ~ OpenExp_Input openExp)
            => input t
            -> openExp stage env genv t

    -- uniform value
    uni     :: (GPU t
               , Input input, input ~ OpenExp_Input openExp)
            => input t
            -> openExp stage env genv t

    -- tuple support
    tup     :: (GPU t, IsTuple t
               , Tuple tuple, tuple ~ OpenExp_Tuple openExp)
            => tuple (openExp stage env genv) (TupleRepr t)
            -> openExp stage env genv t

    prj     :: (GPU e, GPU t, IsTuple t
               , TupleIdx tupleIdx, tupleIdx ~ OpenExp_TupleIdx openExp)
            => tupleIdx (TupleRepr t) e
            -> openExp stage env genv t
            -> openExp stage env genv e

    -- conditional expression
    cond    :: GPU t
            => openExp stage env genv Bool
            -> openExp stage env genv t
            -> openExp stage env genv t
            -> openExp stage env genv t

    -- primitive operations
    primApp :: (GPU a, GPU r
               , PrimFun primFun, primFun ~ OpenExp_PrimFun openExp)
            => primFun stage (a -> r)
            -> openExp stage env genv a
            -> openExp stage env genv r

    -- sampler support
    sampler :: ( GPU (Sampler dim arr t ar)
               , Texture (openGP ()) texture, texture ~ OpenExp_Texture openExp
               , OpenGP openGP, openGP ~ OpenExp_OpenGP openExp)
            => Filter
            -> EdgeMode
            -> texture (openGP ()) dim arr t ar
            -> openExp stage env genv (Sampler dim arr t ar)

-- Fragment
class OpenFragmentOut openFragmentOut where
    type OpenFragmentOut_OpenExp openFragmentOut :: * -> * -> * -> * -> *
    type OpenFragmentOut_FlatTuple openFragmentOut :: (* -> Constraint) -> (* -> *) -> * -> *
    fragmentOut             :: (OpenExp openExp, openExp ~ OpenFragmentOut_OpenExp openFragmentOut
                               ,FlatTuple (openExp F env genv) flatTuple, flatTuple ~ OpenFragmentOut_FlatTuple openFragmentOut)
                            => flatTuple GPU (openExp F env genv) a
                            -> openFragmentOut env genv (ColorRepr a)

    fragmentOutDepth        :: (OpenExp openExp, openExp ~ OpenFragmentOut_OpenExp openFragmentOut
                               ,FlatTuple (openExp F env genv) flatTuple, flatTuple ~ OpenFragmentOut_FlatTuple openFragmentOut)
                            => openExp F env genv Float
                            -> flatTuple GPU (openExp F env genv) a
                            -> openFragmentOut env genv (Depth Float :+: ColorRepr a)

    fragmentOutRastDepth    :: (OpenExp openExp, openExp ~ OpenFragmentOut_OpenExp openFragmentOut
                               ,FlatTuple (openExp F env genv) flatTuple, flatTuple ~ OpenFragmentOut_FlatTuple openFragmentOut)
                            => flatTuple GPU (openExp F env genv) a
                            -> openFragmentOut env genv (Depth Float :+: ColorRepr a)

class FragmentFilter fragmentFilter where
    type FragmentFilter_OpenExp fragmentFilter :: * -> * -> * -> * -> *

    passAll :: fragmentFilter genv a

    filter  :: ( OpenExp openExp, openExp ~ OpenFragmentOut_OpenExp openFragmentOut
               , OpenFun openFun)
            => openFun (openExp F) genv () (a -> Bool)
            -> fragmentFilter genv a

-- Vertex
class OpenVertexOut openVertexOut where
    type OpenVertexOut_OpenExp openVertexOut :: * -> * -> * -> * -> *

    vertexOut   :: (OpenExp openExp, openExp ~ OpenVertexOut_OpenExp openVertexOut
                   ,FlatTuple (interpolated (openExp V env genv)) flatTuple
                   ,Interpolated interpolated)
                => openExp V env genv V4F      -- position
                -> openExp V env genv Float    -- point size
                -> flatTuple GPU (interpolated (openExp V env genv)) a
                -> openVertexOut env genv (FTRepr a)

-- Geometry
class OpenGeometryOut openGeometryOut where
    type OpenGeometryOut_OpenExp openGeometryOut :: * -> * -> * -> * -> *

    geometryOut :: (OpenExp openExp, openExp ~ OpenGeometryOut_OpenExp openGeometryOut
                   ,FlatTuple (interpolated (openExp G env genv)) flatTuple
                   ,Interpolated interpolated)
                => openExp G env genv V4F      -- position
                -> openExp G env genv Float    -- point size
                -> openExp G env genv Int32    -- primitive ID
                -> openExp G env genv Int32    -- layer
                -> openExp G env genv j
                -> flatTuple GPU (interpolated (openExp G env genv)) a
                -> openGeometryOut env genv (j,(FTRepr a))

class GeometryShader geometryShader where
    type GeometryShader_OpenExp geometryShader :: * -> * -> * -> * -> *
    type GeometryShader_OpenGeometryOut geometryShader :: * -> * -> * -> *

    noGeometryShader    :: geometryShader genv prim prim N1 a a

    geometryShader      :: (GPU (PrimitiveVertices primIn a), GPU i, GPU j, GPU b, Nat layerNum
                           , Primitive primitive
                           , OpenExp openExp, openExp ~ GeometryShader_OpenExp geometryShader
                           , OpenGeometryOut openGeometryOut, openGeometryOut ~ GeometryShader_OpenGeometryOut geometryShader
                           , OpenFun openFun)
                        => layerNum                                                         -- geometry shader:
                        -> primitive primOut                                                -- output primitive
                        -> Int                                                              -- max amount of generated vertices
                        -> openFun (openExp G) genv () ((PrimitiveVertices primIn a) -> (i,Int32)) -- how many primitives?
                        -> openFun (openExp G) genv () (i -> (i,j,Int32))                          -- how many vertices?
                        -> openFun openGeometryOut genv () (j -> (j,b))                            -- generate vertices
                        -> geometryShader genv primIn primOut layerNum a b

-- GP and GPFun
class OpenGPfun openGPfun where
    type OpenGPfun_OpenGP openGPfun :: * -> * -> *

    gpBody  :: (OpenGP openGP, openGP ~ OpenGPfun_OpenGP openGPfun)
            => openGP genv t
            -> openGPfun genv t

    gpLam   :: openGPfun (genv, a) t
            -> openGPfun genv (s -> t)

--type GPfun = OpenGPfun openGPfun => openGPfun ()

class OpenGP openGP where
    type OpenGP_Idx openGP :: * -> * -> *
    type OpenGP_OpenVertexOut openGP :: * -> * -> * -> *
    type OpenGP_GeometryShader openGP :: * -> * -> * -> * -> * -> * -> *
    type OpenGP_FragmentFilter openGP :: * -> * -> *
    type OpenGP_OpenFragmentOut openGP :: * -> * -> * -> *
--    type OpenGP_AccumulationContext openGP :: 
    type OpenGP_TupleIdx openGP :: * -> * -> *
    type OpenGP_Image openGP :: * -> * -> *
    type OpenGP_RasterContext openGP :: * -> *

    -- the following required only for sharing
    gpLet           :: openGP genv        bnd               -- bound expression
                    -> openGP (genv, bnd) body              -- the bound expr's scope
                    -> openGP genv        body

    gpVar           :: (Idx idx, idx ~ OpenGP_Idx openGP)
                    => idx genv t
                    -> openGP genv t

    apply           :: (OpenGPfun openGPfun)
                    => openGPfun () (a -> b)
                    -> openGP genv a
                    -> openGP genv b

    fetch           :: (InputTuple a, SGPU (InputTupleRepr a), Primitive primitive)
                    => ByteString
                    -> primitive prim
                    -> a
                    -> openGP genv (VertexStream prim (InputTupleRepr a))

    transform       :: (GPU a, GPU b
                       , OpenVertexOut openVertexOut, openVertexOut ~ OpenGP_OpenVertexOut openGP
                       , OpenFun openFun)
                    => openFun openVertexOut genv () (a -> b)                      -- vertex shader
                    -> openGP genv (VertexStream prim a)
                    -> openGP genv (PrimitiveStream prim b)

    rasterize       :: (GeometryShader geometryShader, geometryShader ~ OpenGP_GeometryShader openGP
                       , RasterContext rasterContext, rasterContext ~ OpenGP_RasterContext openGP)
                    => rasterContext primOut
                    -> geometryShader genv primIn primOut layerNum a b
                    -> openGP genv (PrimitiveStream primIn a)
                    -> openGP genv (FragmentStream layerNum b)

    frameBuffer     :: V2U
                    -> FrameBuffer sh t
                    -> openGP genv (FrameBuffer sh (FTRepr' t))

    accumulate      :: (GPU a, GPU (FTRepr' b), IsValidOutput b
                       , FragmentFilter fragmentFilter, fragmentFilter ~ OpenGP_FragmentFilter openGP
                       , OpenFragmentOut openFragmentOut, openFragmentOut ~ OpenGP_OpenFragmentOut openGP
                       , OpenFun openFun)        -- restriction: depth and stencil optional, arbitrary color component
                    => AccumulationContext b
                    -> fragmentFilter genv a
                    -> openFun openFragmentOut genv () (a -> (NoStencilRepr b))    -- fragment shader
                    -> openGP genv (FragmentStream sh a)
                    -> openGP genv (FrameBuffer sh (FTRepr' b))
                    -> openGP genv (FrameBuffer sh (FTRepr' b))

    prjFrameBuffer  :: ( TupleIdx tupleIdx, tupleIdx ~ OpenGP_TupleIdx openGP
                       , Image image, image ~ OpenGP_Image image)
                    => ByteString                       -- internal image output (can be allocated on request)
                    -> tupleIdx (EltRepr b) t
                    -> openGP () (FrameBuffer sh b)
                    -> openGP genv (image sh t)

    prjImage        :: ( LesserEq idx sh
                       , Image image, image ~ OpenGP_Image image)
                    => ByteString                       -- internal image output (can be allocated on request)
                    -> idx
                    -> openGP () (image sh t)
                    -> openGP genv (image N1 t)

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
{-
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
-}
--uniformInputGS = undefined
--uniformInputFun = undefined
{-
uniformInputFun :: OpenFun OpenExp env genv a -> Set (ByteString,InputType)
uniformInputFun (Lam f)   = uniformInputFun f
uniformInputFun (Body e)  = uniformInputExp e
-}
