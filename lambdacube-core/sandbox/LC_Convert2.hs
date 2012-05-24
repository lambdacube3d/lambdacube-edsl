module LC_Convert2 where

import Data.Typeable

import LCAPIType (FlatTuple(..),V,G,F)
import LCDSLType (GPU,Tuple(..))
import qualified LCAPIType as T
import qualified LCDSLType as T
import LCDeBruijnUtil
import qualified LCHOAS as H
import LC_U_DeBruijn
import LC_U_APIType

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
convertGP :: H.GP t -> GP
convertGP = convertOpenGP EmptyLayout

convertOpenGP :: Layout genv genv -> H.GP t -> GP
convertOpenGP = cvt
  where
    cvt :: Layout genv genv -> H.GP t -> GP
    cvt glyt (H.GPtag i)                    = GPVar (prjIdx i glyt)
    cvt glyt (H.Fetch n p i)                = Fetch n p i
    cvt glyt (H.Transform vs ps)            = Transform (convertFun1Vert glyt vs) (cvt glyt ps)
    cvt glyt (H.Rasterize ctx sh ps)        = Rasterize ctx (convertGeometryShader glyt sh) (cvt glyt ps)
    cvt glyt (H.FrameBuffer s fb)           = FrameBuffer s fb
    cvt glyt (H.Accumulate ctx f sh fs fb)  = Accumulate ctx (convertFragmentFilter glyt f) (convertFun1Frag glyt sh) (cvt glyt fs) (cvt glyt fb)
    cvt glyt (H.PrjFrameBuffer n idx fb)    = PrjFrameBuffer n idx (convertGP fb)
    cvt glyt (H.PrjImage n idx img)         = PrjImage n idx (convertGP img)

-- Vertex
convertOpenVertexOut :: forall t env genv.
                        Layout env  env       -- scalar environment
                     -> Layout genv genv      -- array environment
                     -> H.VertexOut t               -- expression to be converted
                     -> Exp
convertOpenVertexOut lyt glyt = cvt
  where
    cvt :: H.VertexOut t' -> Exp
    cvt (H.VertexOut e1 e2 ie)  = VertexOut (convertOpenExp lyt glyt e1) (convertOpenExp lyt glyt e2) (convertOpenInterpolatedFlatExp lyt glyt ie)

-- Fragment
convertOpenFragmentOut :: forall t env genv.
                          Layout env  env       -- scalar environment
                       -> Layout genv genv      -- array environment
                       -> H.FragmentOut t               -- expression to be converted
                       -> Exp
convertOpenFragmentOut lyt glyt = cvt
  where
    cvt :: H.FragmentOut t' -> Exp
    cvt (H.FragmentOut fe)          = FragmentOut (convertOpenFlatExp lyt glyt fe)
    cvt (H.FragmentOutDepth e fe)   = FragmentOutDepth (convertOpenExp lyt glyt e) (convertOpenFlatExp lyt glyt fe)
    cvt (H.FragmentOutRastDepth fe) = FragmentOutRastDepth (convertOpenFlatExp lyt glyt fe)

convertFragmentFilter :: GPU a
                      => Layout genv genv
                      -> H.FragmentFilter a
                      -> FragmentFilter
convertFragmentFilter = cvt
  where
    cvt :: GPU a => Layout genv genv -> H.FragmentFilter a -> FragmentFilter
    cvt glyt H.PassAll      = PassAll
    cvt glyt (H.Filter f)   = Filter (convertFun1Exp glyt f)

-- Geometry
convertOpenGeometryOut :: forall t env genv.
                          Layout env  env       -- scalar environment
                       -> Layout genv genv      -- array environment
                       -> H.GeometryOut t               -- expression to be converted
                       -> Exp
convertOpenGeometryOut lyt glyt = cvt
  where
    cvt :: H.GeometryOut t' -> Exp
    cvt (H.GeometryOut e1 e2 e3 e4 e5 ie) = GeometryOut (convertOpenExp lyt glyt e1)
                                                        (convertOpenExp lyt glyt e2)
                                                        (convertOpenExp lyt glyt e3)
                                                        (convertOpenExp lyt glyt e4)
                                                        (convertOpenExp lyt glyt e5)
                                                        (convertOpenInterpolatedFlatExp lyt glyt ie)

convertGeometryShader :: Layout genv genv
                      -> H.GeometryShader primIn primOut layerNum a b
                      -> GeometryShader
convertGeometryShader = cvt
  where
    cvt ::  Layout genv genv -> H.GeometryShader primIn primOut layerNum a b -> GeometryShader
    cvt glyt H.NoGeometryShader                 = NoGeometryShader
    cvt glyt (H.GeometryShader a b c e1 e2 e3)  = GeometryShader a b c (convertFun1Exp glyt e1) (convertFun1Exp glyt e2) (convertFun1Geom glyt e3)

-- Common
convertOpenInterpolatedFlatExp :: forall stage t env genv.
                              Layout env  env       -- scalar environment
                           -> Layout genv genv      -- array environment
                           -> H.InterpolatedFlatExp stage t               -- expression to be converted
                           -> [Interpolated Exp]
convertOpenInterpolatedFlatExp lyt glyt = cvt
  where
    cvt :: H.InterpolatedFlatExp stage t' -> [Interpolated Exp]
    cvt (ZT)    = []
    cvt (e:.xs) = cvt' e : cvt xs

    cvt' :: Interpolated (H.Exp stage) t' -> Interpolated Exp
    cvt' (Flat e)           = Flat          (convertOpenExp lyt glyt e)
    cvt' (Smooth e)         = Smooth        (convertOpenExp lyt glyt e)
    cvt' (NoPerspective e)  = NoPerspective (convertOpenExp lyt glyt e)

convertOpenFlatExp :: forall stage t env genv.
                      Layout env  env       -- scalar environment
                   -> Layout genv genv      -- array environment
                   -> H.FlatExp stage t               -- expression to be converted
                   -> [Exp]
convertOpenFlatExp lyt glyt = cvt
  where
    cvt :: H.FlatExp stage t' -> [Exp]
    cvt (ZT)    = []
    cvt (e:.xs) = convertOpenExp lyt glyt e : cvt xs

convertOpenExp :: forall stage t env genv.
                  Layout env  env       -- scalar environment
               -> Layout genv genv      -- array environment
               -> H.Exp stage t               -- expression to be converted
               -> Exp
convertOpenExp lyt glyt = cvt
  where
    cvt :: H.Exp stage t' -> Exp
    cvt (H.Tag i)           = Var (prjIdx i lyt)
    cvt (H.Const v)         = Const v
    cvt (H.PrimVar v)       = PrimVar v
    cvt (H.Uni v)           = Uni v
    cvt (H.Tup tup)         = Tup (convertTuple lyt glyt tup)
    cvt (H.Prj idx e)       = Prj idx (cvt e)
    cvt (H.Cond e1 e2 e3)   = Cond (cvt e1) (cvt e2) (cvt e3)
    cvt (H.PrimApp p e)     = PrimApp p (cvt e)
    cvt (H.Sampler f em t)  = Sampler f em (convertTexture t)

convertFun1Vert :: forall a b genv. GPU a
                => Layout genv genv 
                -> (H.Exp V a -> H.VertexOut b) 
                -> ExpFun
convertFun1Vert = convertFun1 convertOpenVertexOut

convertFun1Geom :: forall a b genv. GPU a
                => Layout genv genv 
                -> (H.Exp G a -> H.GeometryOut b) 
                -> ExpFun
convertFun1Geom = convertFun1 convertOpenGeometryOut

convertFun1Frag :: forall a b genv. GPU a
                => Layout genv genv 
                -> (H.Exp F a -> H.FragmentOut b) 
                -> ExpFun
convertFun1Frag = convertFun1 convertOpenFragmentOut

convertFun1Exp :: forall stage a b genv. GPU a
               => Layout genv genv 
               -> (H.Exp stage a -> H.Exp stage b) 
               -> ExpFun
convertFun1Exp = convertFun1 convertOpenExp

{-
convertFun1 :: (GPU a, Typeable a)
            => (Layout (env, a) ((), a) -> Layout genv genv -> he b -> de (env, a) genv b)
            -> Layout genv genv
            -> (H.Exp a -> he b)
            -> D.OpenFun de env genv (a -> b)
-}
convertFun1 cvt glyt f = Lam (Body openF)
  where
    a     = H.Tag 0
    lyt   = EmptyLayout 
            `PushLayout` 
            ZeroIdx
            --(ZeroIdx :: Idx ((), EltRepr a) (EltRepr a))
    openF = cvt lyt glyt (f a)

convertTexture :: T.Texture (H.GP) dim arr t ar
               -> Texture GP
convertTexture (T.TextureSlot n t) = TextureSlot n t
convertTexture (T.Texture t m d) = Texture t m (map convertGP d)

convertTuple :: Layout env env 
             -> Layout genv genv 
             -> Tuple (H.Exp stage) t 
             -> [Exp]
convertTuple _lyt _glyt NilTup          = []
convertTuple lyt  glyt (es `SnocTup` e) = convertTuple lyt glyt es ++ [convertOpenExp lyt glyt e]

convertExp :: Layout genv genv      -- array environment
           -> H.Exp stage t          -- expression to be converted
           -> Exp
convertExp glyt = convertOpenExp EmptyLayout glyt
