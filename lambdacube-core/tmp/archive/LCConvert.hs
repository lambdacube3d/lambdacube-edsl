module LCConvert where

import Data.Typeable

import LCAPIType
import LCDSLType
import LCDeBruijnUtil
import qualified LCHOAS as H
import qualified LCDeBruijn as D

-- Layouts
-- -------

-- A layout of an environment has an entry for each entry of the environment.
-- Each entry in the layout holds the deBruijn index that refers to the
-- corresponding entry in the environment.
--
data Layout env env' where
    EmptyLayout :: Layout env ()
    PushLayout  :: Typeable t
                => Layout env env' -> Idx env t -> Layout env (env', t)

-- Project the nth index out of an environment layout.
--
prjIdx :: Typeable t => Int -> Layout env env' -> Idx env t
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
incLayout (PushLayout lyt ix) = PushLayout (incLayout lyt) (SuccIdx ix)

-- GP
convertGP :: H.GP t -> D.GP t
convertGP = convertOpenGP EmptyLayout

convertOpenGP :: Layout genv genv -> H.GP t -> D.OpenGP genv t
convertOpenGP = cvt
  where
    cvt :: Layout genv genv -> H.GP t -> D.OpenGP genv t
    cvt glyt (H.GPtag i)                    = D.GPvar (prjIdx i glyt)
    cvt glyt (H.Fetch n p i)                = D.Fetch n p i
    cvt glyt (H.Transform vs ps)            = D.Transform (convertFun1Vert glyt vs) (cvt glyt ps)
    cvt glyt (H.Rasterize ctx sh ps)        = D.Rasterize ctx (convertGeometryShader glyt sh) (cvt glyt ps)
    cvt glyt (H.FrameBuffer s fb)           = D.FrameBuffer s fb
    cvt glyt (H.Accumulate ctx f sh fs fb)  = D.Accumulate ctx (convertFragmentFilter glyt f) (convertFun1Frag glyt sh) (cvt glyt fs) (cvt glyt fb)
    cvt glyt (H.PrjFrameBuffer n idx fb)    = D.PrjFrameBuffer n idx (convertGP fb)
    cvt glyt (H.PrjImage n idx img)         = D.PrjImage n idx (convertGP img)

-- Vertex
convertOpenVertexOut :: forall t env genv.
                        Layout env  env       -- scalar environment
                     -> Layout genv genv      -- array environment
                     -> H.VertexOut t               -- expression to be converted
                     -> D.OpenVertexOut env genv t
convertOpenVertexOut lyt glyt = cvt
  where
    cvt :: H.VertexOut t' -> D.OpenVertexOut env genv t'
    cvt (H.VertexOut e1 e2 ie)  = D.VertexOut (convertOpenExp lyt glyt e1) (convertOpenExp lyt glyt e2) (convertOpenInterpolatedFlatExp lyt glyt ie)

-- Fragment
convertOpenFragmentOut :: forall t env genv.
                          Layout env  env       -- scalar environment
                       -> Layout genv genv      -- array environment
                       -> H.FragmentOut t               -- expression to be converted
                       -> D.OpenFragmentOut env genv t
convertOpenFragmentOut lyt glyt = cvt
  where
    cvt :: H.FragmentOut t' -> D.OpenFragmentOut env genv t'
    cvt (H.FragmentOut fe)          = D.FragmentOut (convertOpenFlatExp lyt glyt fe)
    cvt (H.FragmentOutDepth e fe)   = D.FragmentOutDepth (convertOpenExp lyt glyt e) (convertOpenFlatExp lyt glyt fe)
    cvt (H.FragmentOutRastDepth fe) = D.FragmentOutRastDepth (convertOpenFlatExp lyt glyt fe)

convertFragmentFilter :: GPU a
                      => Layout genv genv
                      -> H.FragmentFilter a
                      -> D.FragmentFilter genv a
convertFragmentFilter = cvt
  where
    cvt :: GPU a => Layout genv genv -> H.FragmentFilter a -> D.FragmentFilter genv a
    cvt glyt H.PassAll      = D.PassAll
    cvt glyt (H.Filter f)   = D.Filter (convertFun1Exp glyt f)

-- Geometry
convertOpenGeometryOut :: forall t env genv.
                          Layout env  env       -- scalar environment
                       -> Layout genv genv      -- array environment
                       -> H.GeometryOut t               -- expression to be converted
                       -> D.OpenGeometryOut env genv t
convertOpenGeometryOut lyt glyt = cvt
  where
    cvt :: H.GeometryOut t' -> D.OpenGeometryOut env genv t'
    cvt (H.GeometryOut e1 e2 e3 e4 e5 ie) = D.GeometryOut (convertOpenExp lyt glyt e1)
                                                          (convertOpenExp lyt glyt e2)
                                                          (convertOpenExp lyt glyt e3)
                                                          (convertOpenExp lyt glyt e4)
                                                          (convertOpenExp lyt glyt e5)
                                                          (convertOpenInterpolatedFlatExp lyt glyt ie)

convertGeometryShader :: Layout genv genv
                      -> H.GeometryShader primIn primOut layerNum a b
                      -> D.GeometryShader genv primIn primOut layerNum a b
convertGeometryShader = cvt
  where
    cvt ::  Layout genv genv -> H.GeometryShader primIn primOut layerNum a b -> D.GeometryShader genv primIn primOut layerNum a b
    cvt glyt H.NoGeometryShader                 = D.NoGeometryShader
    cvt glyt (H.GeometryShader a b c e1 e2 e3)  = D.GeometryShader a b c (convertFun1Exp glyt e1) (convertFun1Exp glyt e2) (convertFun1Geom glyt e3)

-- Common
convertOpenInterpolatedFlatExp :: forall stage t env genv.
                              Layout env  env       -- scalar environment
                           -> Layout genv genv      -- array environment
                           -> H.InterpolatedFlatExp stage t               -- expression to be converted
                           -> D.OpenInterpolatedFlatExp stage env genv t
convertOpenInterpolatedFlatExp lyt glyt = cvt
  where
    cvt :: H.InterpolatedFlatExp satge t' -> D.OpenInterpolatedFlatExp satge env genv t'
    cvt (ZT)    = ZT
    cvt (e:.xs) = cvt' e :. cvt xs

    cvt' :: Interpolated (H.Exp satge) t' -> Interpolated (D.OpenExp satge env genv) t'
    cvt' (Flat e)           = Flat          (convertOpenExp lyt glyt e)
    cvt' (Smooth e)         = Smooth        (convertOpenExp lyt glyt e)
    cvt' (NoPerspective e)  = NoPerspective (convertOpenExp lyt glyt e)

convertOpenFlatExp :: forall stage t env genv.
                      Layout env  env       -- scalar environment
                   -> Layout genv genv      -- array environment
                   -> H.FlatExp stage t               -- expression to be converted
                   -> D.OpenFlatExp stage env genv t
convertOpenFlatExp lyt glyt = cvt
  where
    cvt :: H.FlatExp stage t' -> D.OpenFlatExp stage env genv t'
    cvt (ZT)    = ZT
    cvt (e:.xs) = convertOpenExp lyt glyt e :. cvt xs

convertOpenExp :: forall stage t env genv.
                  Layout env  env       -- scalar environment
               -> Layout genv genv      -- array environment
               -> H.Exp stage t               -- expression to be converted
               -> D.OpenExp stage env genv t
convertOpenExp lyt glyt = cvt
  where
    cvt :: H.Exp stage t' -> D.OpenExp stage env genv t'
    cvt (H.Tag i)           = D.Var (prjIdx i lyt)
    cvt (H.Const v)         = D.Const v
    cvt (H.PrimVar v)       = D.PrimVar v
    cvt (H.Uni v)           = D.Uni v
    cvt (H.Tup tup)         = D.Tup (convertTuple lyt glyt tup)
    cvt (H.Prj idx e)       = D.Prj idx (cvt e)
    cvt (H.Cond e1 e2 e3)   = D.Cond (cvt e1) (cvt e2) (cvt e3)
    cvt (H.PrimApp p e)     = D.PrimApp p (cvt e)
    cvt (H.Sampler f em t)  = D.Sampler f em (convertTexture t)

convertFun1Vert :: forall a b genv. GPU a
                => Layout genv genv 
                -> (H.Exp V a -> H.VertexOut b) 
                -> D.Fun D.OpenVertexOut genv (a -> b)
convertFun1Vert = convertFun1 convertOpenVertexOut

convertFun1Geom :: forall a b genv. GPU a
                => Layout genv genv 
                -> (H.Exp G a -> H.GeometryOut b) 
                -> D.Fun D.OpenGeometryOut genv (a -> b)
convertFun1Geom = convertFun1 convertOpenGeometryOut

convertFun1Frag :: forall a b genv. GPU a
                => Layout genv genv 
                -> (H.Exp F a -> H.FragmentOut b) 
                -> D.Fun D.OpenFragmentOut genv (a -> b)
convertFun1Frag = convertFun1 convertOpenFragmentOut

convertFun1Exp :: forall stage a b genv. GPU a
               => Layout genv genv 
               -> (H.Exp stage a -> H.Exp stage b) 
               -> D.Fun (D.OpenExp stage) genv (a -> b)
convertFun1Exp = convertFun1 convertOpenExp

{-
convertFun1 :: (GPU a, Typeable a)
            => (Layout (env, a) ((), a) -> Layout genv genv -> he b -> de (env, a) genv b)
            -> Layout genv genv
            -> (H.Exp a -> he b)
            -> D.OpenFun de env genv (a -> b)
-}
convertFun1 cvt glyt f = D.Lam (D.Body openF)
  where
    a     = H.Tag 0
    lyt   = EmptyLayout 
            `PushLayout` 
            ZeroIdx
            --(ZeroIdx :: Idx ((), EltRepr a) (EltRepr a))
    openF = cvt lyt glyt (f a)

convertTexture :: Texture (H.GP) dim arr t ar
               -> Texture (D.OpenGP ()) dim arr t ar
convertTexture (TextureSlot n t) = TextureSlot n t
--convertTexture (Texture t m d) = Texture t m undefined --TODO

convertTuple :: Layout env env 
             -> Layout genv genv 
             -> Tuple (H.Exp stage) t 
             -> Tuple (D.OpenExp stage env genv) t
convertTuple _lyt _glyt NilTup          = NilTup
convertTuple lyt  glyt (es `SnocTup` e) = convertTuple lyt glyt es `SnocTup` convertOpenExp lyt glyt e

convertExp :: Layout genv genv      -- array environment
           -> H.Exp stage t          -- expression to be converted
           -> D.Exp stage genv t
convertExp glyt = convertOpenExp EmptyLayout glyt
