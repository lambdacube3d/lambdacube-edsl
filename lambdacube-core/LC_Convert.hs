module LC_Convert where

import Data.Typeable

import LC_APIType
import LC_T_APIType hiding (Texture,Interpolated,FlatTuple)
import LC_I_APIType (FlatTupleI,InterpolatedI)
import LC_T_DSLType
--import LCDSLType
--import LCDeBruijnUtil
import LC_I_DeBruijn
import LC_T_DeBruijn (Idx(..))
import LC_GADT_HOAS as H
import LC_T_DeBruijn as D
import qualified LC_U_DeBruijn as D

-- Layouts
-- -------

-- A layout of an environment has an entry for each entry of the environment.
-- Each entry in the layout holds the deBruijn index that refers to the
-- corresponding entry in the environment.
--
data Layout env env' where
    EmptyLayout :: Layout env ()
    PushLayout  :: Typeable t
                => Layout env env' -> IdxI env t -> Layout env (env', t)

-- Project the nth index out of an environment layout.
--
prjIdx :: Typeable t => Int -> Layout env env' -> IdxI env t
prjIdx 0 (PushLayout _ ix) = case gcast ix of
                               Just ix' -> ix'
                               Nothing  -> error "prjIdx" "type mismatch"
prjIdx n (PushLayout l _)  = prjIdx (n - 1) l
prjIdx _ EmptyLayout       = error "prjIdx" "inconsistent valuation"

size :: Layout env env' -> Int
size EmptyLayout        = 0
size (PushLayout lyt _) = size lyt + 1

-- Add an entry to a layout, incrementing all indices
--
incLayout :: Layout env env' -> Layout (env, t) env'
incLayout EmptyLayout         = EmptyLayout
incLayout (PushLayout lyt ix) = PushLayout (incLayout lyt) (succIdx ix)

-- GP
convertGP :: H.GP t -> OpenGPI () t
convertGP = convertOpenGP EmptyLayout

convertOpenGP :: Layout genv genv -> H.GP t -> OpenGPI genv t
convertOpenGP = cvt
  where
    cvt :: Layout genv genv -> H.GP t -> OpenGPI genv t
    cvt glyt (H.GPTag i)                    = D.gpVar (prjIdx i glyt)
    cvt glyt (H.Fetch n p i)                = D.fetch n p i
    cvt glyt (H.Transform vs ps)            = D.transform (convertFun1 glyt vs) (cvt glyt ps)
    cvt glyt (H.Rasterize ctx sh ps)        = D.rasterize ctx (convertGeometryShader glyt sh) (cvt glyt ps)
    cvt glyt (H.FrameBuffer s fb)           = D.frameBuffer s fb
    cvt glyt (H.Accumulate ctx f sh fs fb)  = D.accumulate ctx (convertFragmentFilter glyt f) (convertFun1 glyt sh) (cvt glyt fs) (cvt glyt fb)
    cvt glyt (H.PrjFrameBuffer n idx fb)    = D.prjFrameBuffer n idx (convertGP fb)
    cvt glyt (H.PrjImage n idx img)         = D.prjImage n idx (convertGP img)

convertFlatTuple :: forall c a stage t .FlatTuple c a stage t -> 
-- Fragment
convertFragmentFilter :: GPU a
                      => Layout genv genv
                      -> H.FragmentFilter a
                      -> FragmentFilterI genv a
convertFragmentFilter = cvt
  where
    cvt :: GPU a => Layout genv genv -> H.FragmentFilter a -> FragmentFilterI genv a
    cvt glyt H.PassAll      = D.passAll
    cvt glyt (H.Filter f)   = D.filter (convertFun1 glyt f)

-- Geometry
convertGeometryShader :: Layout genv genv
                      -> H.GeometryShader primIn primOut layerNum a b
                      -> GeometryShaderI genv primIn primOut layerNum a b
convertGeometryShader = cvt
  where
    cvt :: Layout genv genv -> H.GeometryShader primIn primOut layerNum a b -> GeometryShaderI genv primIn primOut layerNum a b
    cvt glyt H.NoGeometryShader                 = D.noGeometryShader
    cvt glyt (H.GeometryShader a b c e1 e2 e3)  = D.geometryShader a b c (convertFun1 glyt e1) (convertFun1 glyt e2) (convertFun1 glyt e3)

--newtype FlatTupleI e (c :: * -> Constraint) (a :: * -> *) (stage :: * -> *) t = FlatTupleI [e]
--newtype InterpolatedI a (b :: * -> *) (c :: * -> *) = InterpolatedI (U.Interpolated a)
-- Common
convertOpenInterpolatedFlatExp :: forall stage t env genv.
                              Layout env  env       -- scalar environment
                           -> Layout genv genv      -- array environment
                           -> H.Interpolated t               -- expression to be converted
                           -> FlatTupleI (Interpolated D.Exp) GPU (InterpolatedI D.Exp (OpenExpI env genv)) stage t
convertOpenInterpolatedFlatExp lyt glyt = cvt
  where
    cvt :: H.Interpolated t' -> FlatTupleI (Interpolated D.Exp) GPU (InterpolatedI D.Exp (OpenExpI env genv)) stage t'
    cvt (ZT)    = zt
    cvt (e:.xs) = cvt' e :. cvt xs

    cvt' :: Interpolated (H.Exp satge) t' -> Interpolated (D.OpenExp satge env genv) t'
    cvt' (Flat e)           = flat          (convertOpenExp lyt glyt e)
    cvt' (Smooth e)         = smooth        (convertOpenExp lyt glyt e)
    cvt' (NoPerspective e)  = noPerspective (convertOpenExp lyt glyt e)

type FlatExp a b = FlatTuple GPU Exp a b
convertOpenFlatExp :: forall stage t env genv.
                      Layout env  env       -- scalar environment
                   -> Layout genv genv      -- array environment
                   -> FlatExp stage t               -- expression to be converted
                   -> FlatTupleI D.Exp GPU (OpenExpI env genv) stage t
convertOpenFlatExp lyt glyt = cvt
  where
    cvt :: FlatExp stage t' -> FlatTupleI D.Exp GPU (OpenExpI env genv) stage t'
    cvt (ZT)    = zt
    cvt (e:.xs) = convertOpenExp lyt glyt e :. cvt xs

convertOpenExp :: forall t env genv.
                  Layout env  env       -- scalar environment
               -> Layout genv genv      -- array environment
               -> H.Exp t               -- expression to be converted
               -> OpenExpI env genv t
convertOpenExp lyt glyt = cvt
  where
    cvt :: H.Exp t' -> OpenExpI env genv t'
    cvt (H.Tag i)               = D.var (prjIdx i lyt)
    cvt (H.Const v)             = D.cnst v
    cvt (H.PrimVar v)           = D.primVar v
    cvt (H.Uni v)               = D.uni v
    cvt (H.Tup tup)             = D.tup (convertTuple lyt glyt tup)
    cvt (H.Prj idx e)           = D.prj idx (cvt e)
    cvt (H.Cond e1 e2 e3)       = D.cond (cvt e1) (cvt e2) (cvt e3)
    cvt (H.PrimApp p e)         = D.primApp p (cvt e)
    cvt (H.VertexOut e1 e2 ie)  = D.vertexOut (convertOpenExp lyt glyt e1) (convertOpenExp lyt glyt e2) (convertOpenInterpolatedFlatExp lyt glyt ie)
    cvt (H.GeometryOut e1 e2 e3
                      e4 e5 ie) = D.geometryOut (convertOpenExp lyt glyt e1)
                                                (convertOpenExp lyt glyt e2)
                                                (convertOpenExp lyt glyt e3)
                                                (convertOpenExp lyt glyt e4)
                                                (convertOpenExp lyt glyt e5)
                                                (convertOpenInterpolatedFlatExp lyt glyt ie)
    cvt (H.FragmentOut fe)          = D.fragmentOut (convertOpenFlatExp lyt glyt fe)
    cvt (H.FragmentOutDepth e fe)   = D.fragmentOutDepth (convertOpenExp lyt glyt e) (convertOpenFlatExp lyt glyt fe)
    cvt (H.FragmentOutRastDepth fe) = D.fragmentOutRastDepth (convertOpenFlatExp lyt glyt fe)
    cvt (H.Sampler f em t)          = D.sampler f em (convertTexture t)

convertFun1 :: forall a b genv.
                Layout genv genv 
               -> (H.Exp a -> H.Exp b) 
               -> OpenFunI () genv (a -> b)
convertFun1 = convertFun1' convertOpenExp

{-
convertFun1' :: (GPU a, Typeable a)
            => (Layout (env, a) ((), a) -> Layout genv genv -> he b -> de (env, a) genv b)
            -> Layout genv genv
            -> (H.Exp a -> he b)
            -> D.OpenFun de env genv (a -> b)
-}
convertFun1' cvt glyt f = D.lam (D.body openF)
  where
    a     = H.Tag 0
    lyt   = EmptyLayout 
            `PushLayout` 
            zeroIdx
            --(ZeroIdx :: Idx ((), EltRepr a) (EltRepr a))
    openF = cvt lyt glyt (f a)

convertTexture :: Texture dim arr t ar
               -> TextureI dim arr t ar
convertTexture (TextureSlot n t) = textureSlot n t
--convertTexture (Texture t m d) = texture t m undefined --TODO
{-
convertTuple :: Layout env env 
             -> Layout genv genv 
             -> Tuple (H.Exp stage) t 
             -> Tuple (D.OpenExp stage env genv) t
convertTuple _lyt _glyt NilTup          = NilTup
convertTuple lyt  glyt (es `SnocTup` e) = convertTuple lyt glyt es `SnocTup` convertOpenExp lyt glyt e
-}
convertTuple = undefined
convertExp :: Layout genv genv      -- array environment
           -> H.Exp t          -- expression to be converted
           -> OpenExpI () genv t
convertExp glyt = convertOpenExp EmptyLayout glyt
