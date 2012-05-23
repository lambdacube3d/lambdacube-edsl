module LC_T_DeBruijn where

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

-- Typed de Bruijn indices
-- -----------------------

-- De Bruijn variable index projecting a specific type from a type
-- environment.  Type environments are nested pairs (..((), t1), t2, ..., tn).
--
class Idx idx where
    zeroIdx ::              idx (env, t) t
    succIdx :: idx env t -> idx (env, s) t


-- Environments
-- ------------

-- Valuation for an environment
--
class Val val where
    empty :: val ()
    push  :: val env -> t -> val (env, t)

-- Common Exp and Fun

-- |Parametrised open function abstraction
--
class OpenFun openFun where
    type OpenFun_OpenExp openFun :: * -> * -> * -> *
    body    :: (openExp ~ OpenFun_OpenExp openFun)
            => openExp env genv t
            -> openFun env genv t

    lam     :: openFun (env, a) genv t
            -> openFun env genv (a -> t)

-- Embedded expressions
-- --------------------
class OpenExp openExp where
    type OpenExp_FlatTuple exp :: (* -> Constraint) -> (* -> *) -> (* -> *) -> * -> *
    type OpenExp_Idx openExp :: * -> * -> *
    type OpenExp_Input openExp :: * -> *
    type OpenExp_InterpolatedFlatTuple exp :: (* -> Constraint) -> (* -> *) -> (* -> *) -> * -> *
    type OpenExp_OpenFun openExp :: * -> * -> * -> *
    type OpenExp_PrimFun openExp :: (* -> *) -> * -> *
    type OpenExp_Texture openExp :: * -> * -> * -> * -> *
    type OpenExp_TupleIdx openExp :: * -> * -> *

    let_    :: openExp env genv        bnd               -- bound expression
            -> openExp (env, bnd) genv body              -- the bound expr's scope
            -> openExp env genv        body

    var     :: (GPU t
               ,idx ~ OpenExp_Idx openExp)
            => idx env t
            -> openExp env genv t

    apply   :: (openFun ~ OpenExp_OpenFun openExp)
            => openFun () genv (a -> b)
            -> openExp env genv a
            -> openExp env genv b

    -- constant value
    cnst    :: IsScalar t
            => t
            -> openExp env genv (stage t)

    -- builtin variable
    primVar :: (GPU t
               ,input ~ OpenExp_Input openExp)
            => input t
            -> openExp env genv (stage t)

    -- uniform value
    uni     :: (GPU t
               ,input ~ OpenExp_Input openExp)
            => input t
            -> openExp env genv (stage t)

    -- tuple support
    tup     :: (GPU t, IsTuple t
               ,flatTuple ~ OpenExp_FlatTuple openExp)
            => flatTuple GPU (openExp env genv) stage t
            -> openExp env genv (stage t)

    prj     :: (GPU e, GPU t, IsTuple t
               ,tupleIdx ~ OpenExp_TupleIdx openExp)
            => tupleIdx (TupleRepr t) e
            -> openExp env genv (stage t)
            -> openExp env genv (stage e)

    -- conditional expression
    cond    :: GPU t
            => openExp env genv (stage Bool)
            -> openExp env genv (stage t)
            -> openExp env genv (stage t)
            -> openExp env genv (stage t)

    -- primitive operations
    primApp :: (GPU a, GPU r
               ,primFun ~ OpenExp_PrimFun openExp)
            => primFun stage (a -> r)
            -> openExp env genv (stage a)
            -> openExp env genv (stage r)

    -- special expressions
    vertexOut               :: (Interpolated interpolated
                               ,flatTuple ~ OpenExp_InterpolatedFlatTuple openExp)
                            => openExp env genv (V V4F)      -- position
                            -> openExp env genv (V Float)    -- point size
                            -> flatTuple GPU (interpolated (openExp env genv)) V a
                            -> openExp env genv (VertexOut (FTRepr a))

    geometryOut             :: (Interpolated interpolated
                               ,flatTuple ~ OpenExp_InterpolatedFlatTuple openExp)
                            => openExp env genv (G V4F)      -- position
                            -> openExp env genv (G Float)    -- point size
                            -> openExp env genv (G Int32)    -- primitive ID
                            -> openExp env genv (G Int32)    -- layer
                            -> openExp env genv (G j)
                            -> flatTuple GPU (interpolated (openExp env genv)) G a
                            -> openExp env genv (GeometryOut (j,(FTRepr a)))

    fragmentOut             :: (flatTuple ~ OpenExp_FlatTuple openExp)
                            => flatTuple GPU (openExp env genv) F a
                            -> openExp env genv (FragmentOut (ColorRepr a))

    fragmentOutDepth        :: (flatTuple ~ OpenExp_FlatTuple openExp)
                            => openExp env genv (F Float)
                            -> flatTuple GPU (openExp env genv) F a
                            -> openExp env genv (FragmentOut (Depth Float :+: ColorRepr a))

    fragmentOutRastDepth    :: (flatTuple ~ OpenExp_FlatTuple openExp)
                            => flatTuple GPU (openExp env genv) F a
                            -> openExp env genv (FragmentOut (Depth Float :+: ColorRepr a))

    -- sampler support
    sampler                 :: (GPU (Sampler dim arr t ar)
                               ,texture ~ OpenExp_Texture openExp)
                            => Filter
                            -> EdgeMode
                            -> texture dim arr t ar
                            -> openExp env genv (stage (Sampler dim arr t ar))

class FragmentFilter fragmentFilter where
    type FragmentFilter_OpenFun fragmentFilter :: * -> * -> * -> *

    passAll :: fragmentFilter genv a

    filter  :: (openFun ~ FragmentFilter_OpenFun fragmentFilter)
            => openFun () genv (F a -> F Bool)
            -> fragmentFilter genv a

class GeometryShader geometryShader where
    type GeometryShader_OpenFun geometryShader :: * -> * -> * -> *
    type GeometryShader_Primitive geometryShader :: * -> *

    noGeometryShader    :: geometryShader genv prim prim N1 a a

    geometryShader      :: (GPU (PrimitiveVertices primIn a), GPU i, GPU j, GPU b, Nat layerNum
                           ,primitive ~ GeometryShader_Primitive geometryShader
                           ,openFun ~ GeometryShader_OpenFun geometryShader)
                        => layerNum                                                                 -- geometry shader:
                        -> primitive primOut                                                        -- output primitive
                        -> Int                                                                      -- max amount of generated vertices
                        -> openFun () genv (G (PrimitiveVertices primIn a) -> G (i,Int32))  -- how many primitives?
                        -> openFun () genv (G i -> G (i,j,Int32))                           -- how many vertices?
                        -> openFun () genv (G j -> GeometryOut (j,b))               -- generate vertices
                        -> geometryShader genv primIn primOut layerNum a b

-- GP and GPFun
class OpenGPFun openGPFun where
    type OpenGPFun_OpenGP openGPFun :: * -> * -> *

    gpBody  :: (openGP ~ OpenGPFun_OpenGP openGPFun)
            => openGP genv t
            -> openGPFun genv t

    gpLam   :: openGPFun (genv, a) t
            -> openGPFun genv (s -> t)

class OpenGP openGP where
    type OpenGP_FlatTupleFragmentOperation openGP :: (* -> Constraint) -> (* -> *) -> (* -> *) -> * -> *
    type OpenGP_FlatTupleImage openGP :: (* -> Constraint) -> (* -> *) -> (* -> *) -> * -> *
    type OpenGP_FragmentFilter openGP :: * -> * -> *
    type OpenGP_FragmentOperation openGP :: * -> *
    type OpenGP_GeometryShader openGP :: * -> * -> * -> * -> * -> * -> *
    type OpenGP_Idx openGP :: * -> * -> *
    type OpenGP_Image openGP :: * -> * -> *
    type OpenGP_RasterContext openGP :: * -> *
    type OpenGP_TupleIdx openGP :: * -> * -> *
    type OpenGP_OpenGPFun openGP :: * -> * -> *
    type OpenGP_OpenFun openGP :: * -> * -> * -> *
    type OpenGP_Primitive openGP :: * -> *

    -- the following required only for sharing
    gpLet           :: openGP genv        bnd               -- bound expression
                    -> openGP (genv, bnd) body              -- the bound expr's scope
                    -> openGP genv        body

    gpVar           :: (idx ~ OpenGP_Idx openGP)
                    => idx genv t
                    -> openGP genv t

    gpApply         :: (openGPfun ~ OpenGP_OpenGPFun openGP)
                    => openGPfun () (a -> b)
                    -> openGP genv a
                    -> openGP genv b

    fetch           :: (InputTuple a, SGPU (InputTupleRepr a)
                       ,primitive ~ OpenGP_Primitive openGP)
                    => ByteString
                    -> primitive prim
                    -> a
                    -> openGP genv (VertexStream prim (InputTupleRepr a))

    transform       :: (GPU a, GPU b
                       ,openFun ~ OpenGP_OpenFun openGP)
                    => openFun () genv (V a -> V b)                      -- vertex shader
                    -> openGP genv (VertexStream prim a)
                    -> openGP genv (PrimitiveStream prim b)

    rasterize       :: (geometryShader ~ OpenGP_GeometryShader openGP
                       ,rasterContext ~ OpenGP_RasterContext openGP)
                    => rasterContext primOut
                    -> geometryShader genv primIn primOut layerNum a b
                    -> openGP genv (PrimitiveStream primIn a)
                    -> openGP genv (FragmentStream layerNum b)

    frameBuffer     :: (Image image, image ~ OpenGP_Image openGP
                       ,FlatTuple (image layerCount) flatTuple, flatTuple ~ OpenGP_FlatTupleImage openGP)
                    => V2U                                          -- size: width, height
                    -> flatTuple Typeable (image layerCount) GFX t
                    -> openGP genv (FrameBuffer layerCount (FTRepr' t))

    accumulate      :: (GPU a, GPU (FTRepr' b), IsValidOutput b
                       ,fragmentFilter ~ OpenGP_FragmentFilter openGP
                       ,fragmentOperation ~ OpenGP_FragmentOperation openGP
                       ,flatTuple ~ OpenGP_FlatTupleFragmentOperation openGP
                       ,openFun ~ OpenGP_OpenFun openGP)        -- restriction: depth and stencil optional, arbitrary color component
                    => flatTuple Typeable fragmentOperation GFX b
                    -> fragmentFilter genv a
                    -> openFun () genv (F a -> F (NoStencilRepr b))    -- fragment shader
                    -> openGP genv (FragmentStream sh a)
                    -> openGP genv (FrameBuffer sh (FTRepr' b))
                    -> openGP genv (FrameBuffer sh (FTRepr' b))

    prjFrameBuffer  :: (tupleIdx ~ OpenGP_TupleIdx openGP
                       ,image ~ OpenGP_Image image)
                    => ByteString                       -- internal image output (can be allocated on request)
                    -> tupleIdx (EltRepr b) t
                    -> openGP () (FrameBuffer sh b)
                    -> openGP genv (image sh t)

    prjImage        :: (Nat idx, LesserEq idx sh
                       ,image ~ OpenGP_Image image)
                    => ByteString                       -- internal image output (can be allocated on request)
                    -> idx
                    -> openGP () (image sh t)
                    -> openGP genv (image N1 t)
