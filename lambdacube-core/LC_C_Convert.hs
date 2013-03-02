module LC_C_Convert (convertGPOutput) where

import Data.Typeable
import Debug.Trace

import TypeLevel.Number.Nat
import TypeLevel.Number.Nat.Num

import LC_T_APIType (FlatTuple(..),V,G,F)
import LC_T_DSLType (GPU,Tuple(..),TupleType(..),TupleIdx(..))
import qualified LC_T_APIType as T
import qualified LC_T_DSLType as T
import qualified LC_T_PrimFun as T
import qualified LC_T_HOAS as H
import LC_U_DeBruijn
import LC_U_APIType
import LC_G_APIType
import LC_C_PrimFun

prjIdx _ _ = 0
prjToInt :: TupleIdx t e -> Int
prjToInt ZeroTupIdx     = 0
prjToInt (SuccTupIdx i) = 1 + prjToInt i

type Layout = [Ty]

genTy :: GPU a => a -> Ty
genTy a = case cvt $ T.tupleType a of
    [e] -> {-trace (show e)-} e
    e   -> {-trace (show (Tuple e)) $-} Tuple $ e
  where
    cvt :: TupleType a -> [Ty]
    cvt UnitTuple         = []--[Tuple []]
    cvt (SingleTuple a)   = [Single (T.toType a)]
--    cvt (PairTuple UnitTuple b ) = cvt b
    cvt (PairTuple a b )  = cvt a ++ cvt b

convertGPOutput :: ExpC exp => H.GPOutput -> exp
convertGPOutput (H.ImageOut a b)    = imageOut a $ convertGP b
convertGPOutput (H.ScreenOut a)   = screenOut $ convertGP a

-- GP
convertGP :: ExpC exp => H.Exp T.Obj t -> exp
convertGP = convertOpenGP []

convertOpenGP :: ExpC exp => Layout -> H.Exp T.Obj t -> exp
convertOpenGP = cvt
  where
    cvt :: ExpC exp => Layout -> H.Exp T.Obj t -> exp
    --cvt glyt (H.GPtag i)                    = error "GPtag is not implemented yet!"-- var (genTy (undefined :: t')) (prjIdx i glyt) <$> get
    cvt glyt (H.Fetch n p i)                = fetch n (T.toPrimitive p) (T.toInputList i)
    cvt glyt (H.Transform vs ps)            = transform  (convertFun1Vert glyt vs) (cvt glyt ps)
    cvt glyt (H.Reassemble sh ps)           = reassemble (convertGeometryShader glyt sh) (cvt glyt ps)
    cvt glyt (H.Rasterize ctx ps)           = rasterize (convertRasterContext ctx) $ cvt glyt ps
    cvt glyt (H.FrameBuffer fb)             = frameBuffer (convertFrameBuffer fb)
    cvt glyt (H.Accumulate ctx f sh fs fb)  = accumulate (convertAccumulationContext ctx) (convertFragmentFilter glyt f)
                                                                                          (convertFun1Frag glyt sh)
                                                                                          (cvt glyt fs)
                                                                                          (cvt glyt fb)
    cvt glyt (H.PrjFrameBuffer n idx fb)    = prjFrameBuffer n (prjToInt idx) $ convertGP fb
    cvt glyt (H.PrjImage n idx img)         = prjImage n (toInt idx) $ convertGP img

-- Vertex
convertOpenVertexOut :: ExpC exp => forall t.
                        Layout       -- scalar environment
                     -> Layout      -- array environment
                     -> H.VertexOut t               -- expression to be converted
                     -> exp
convertOpenVertexOut lyt glyt = cvt
  where
    cvt :: ExpC exp => H.VertexOut t' -> exp
    cvt (H.VertexOut e1 e2 ie :: H.VertexOut t')  = vertexOut (convertOpenExp lyt glyt e1) (convertOpenExp lyt glyt e2) (convertOpenInterpolatedFlatExp lyt glyt ie)

-- Fragment
convertOpenFragmentOut :: ExpC exp => forall t.
                          Layout       -- scalar environment
                       -> Layout      -- array environment
                       -> H.FragmentOut t               -- expression to be converted
                       -> exp
convertOpenFragmentOut lyt glyt = cvt
  where
    cvt :: ExpC exp => H.FragmentOut t' -> exp
    cvt (H.FragmentOut fe :: H.FragmentOut t')          = fragmentOut $ convertOpenFlatExp lyt glyt fe
    cvt (H.FragmentOutDepth e fe :: H.FragmentOut t')   = fragmentOutDepth (convertOpenExp lyt glyt e) (convertOpenFlatExp lyt glyt fe)
    cvt (H.FragmentOutRastDepth fe :: H.FragmentOut t') = fragmentOutRastDepth $ convertOpenFlatExp lyt glyt fe

convertFragmentFilter :: (ExpC exp, GPU a)
                      => Layout
                      -> H.FragmentFilter a
                      -> exp
convertFragmentFilter = cvt
  where
    cvt :: (ExpC exp, GPU a) => Layout -> H.FragmentFilter a -> exp
    cvt glyt H.PassAll      = passAll
    cvt glyt (H.Filter f)   = filter_ $ convertFun1Exp glyt f

-- Geometry
convertOpenGeometryOut :: ExpC exp => forall t.
                          Layout       -- scalar environment
                       -> Layout      -- array environment
                       -> H.GeometryOut t               -- expression to be converted
                       -> exp
convertOpenGeometryOut lyt glyt = cvt
  where
    cvt :: ExpC exp => H.GeometryOut t' -> exp
    cvt (H.GeometryOut e1 e2 e3 e4 e5 ie :: H.GeometryOut t') = geometryOut (convertOpenExp lyt glyt e1)
                                                                            (convertOpenExp lyt glyt e2)
                                                                            (convertOpenExp lyt glyt e3)
                                                                            (convertOpenExp lyt glyt e4)
                                                                            (convertOpenExp lyt glyt e5)
                                                                            (convertOpenInterpolatedFlatExp lyt glyt ie)

convertGeometryShader :: ExpC exp
                      => Layout
                      -> H.GeometryShader primIn primOut layerNum a b
                      -> exp
convertGeometryShader = cvt
  where
    cvt :: ExpC exp => Layout -> H.GeometryShader primIn primOut layerNum a b -> exp
    cvt glyt (H.GeometryShader a b c e1 e2 e3)  = geometryShader (toInt a) (T.toPrimitive b) c (convertFun1Exp glyt e1)
                                                                                               (convertFun1Exp glyt e2)
                                                                                               (convertFun1Geom glyt e3)

-- Common
convertOpenInterpolatedFlatExp :: ExpC exp => forall stage t.
                                  Layout       -- scalar environment
                               -> Layout      -- array environment
                               -> H.InterpolatedFlatExp stage t               -- expression to be converted
                               -> [exp]
convertOpenInterpolatedFlatExp lyt glyt = cvt
  where
    cvt :: ExpC exp => H.InterpolatedFlatExp stage t' -> [exp]
    cvt (ZT)    = []
    cvt (e:.xs) = cvt' e : cvt xs

    cvt' :: ExpC exp => T.Interpolated (H.Exp stage) t' -> exp
    cvt' (T.Flat e)           = flat          $ convertOpenExp lyt glyt e
    cvt' (T.Smooth e)         = smooth        $ convertOpenExp lyt glyt e
    cvt' (T.NoPerspective e)  = noPerspective $ convertOpenExp lyt glyt e

convertOpenFlatExp :: ExpC exp => forall stage t.
                      Layout       -- scalar environment
                   -> Layout      -- array environment
                   -> H.FlatExp stage t               -- expression to be converted
                   -> [exp]
convertOpenFlatExp lyt glyt = cvt
  where
    cvt :: ExpC exp => H.FlatExp stage t' -> [exp]
    cvt (ZT)    = []
    cvt (e:.xs) = convertOpenExp lyt glyt e : cvt xs

convertOpenExp :: ExpC exp => forall stage t.
                  Layout       -- scalar environment
               -> Layout      -- array environment
               -> H.Exp stage t               -- expression to be converted
               -> exp
convertOpenExp lyt glyt = cvt
  where
    cvt :: ExpC exp => H.Exp stage t' -> exp
    cvt (H.Tag i li :: H.Exp stage t')        = var (genTy (undefined :: t')) (prjIdx i lyt) li
    cvt (H.Const v :: H.Exp stage t')         = const_ (genTy (undefined :: t')) (T.toValue v)
    cvt (H.PrimVar v :: H.Exp stage t')       = primVar (genTy (undefined :: t')) (fst $ T.toInput v)
    cvt (H.Uni v :: H.Exp stage t')           = uni (genTy (undefined :: t')) (fst $ T.toInput v)
    cvt (H.Tup tupl :: H.Exp stage t')        = tup (genTy (undefined :: t')) $ convertTuple lyt glyt tupl
    cvt (H.Prj idx e :: H.Exp stage t')       = prj (genTy (undefined :: t')) (prjToInt idx) $ cvt e
    cvt (H.Cond e1 e2 e3 :: H.Exp stage t')   = cond (genTy (undefined :: t')) (cvt e1) (cvt e2) (cvt e3)
    cvt (H.PrimApp p e :: H.Exp stage t')     = primApp (genTy (undefined :: t')) (convertPrimFun p) $ cvt e
    cvt (H.Sampler f em t :: H.Exp stage t')  = sampler (genTy (undefined :: t')) f em $ convertTexture t
    cvt (H.Loop e1 e2 e3 s :: H.Exp stage t') = loop (genTy (undefined :: t')) (convertFun1Exp glyt e1) (convertFun1Exp glyt e2) (convertFun1Exp glyt e3) (cvt s)

convertFun1Vert :: ExpC exp => forall a b. GPU a =>
                   Layout
                -> (H.Exp V a -> H.VertexOut b) 
                -> exp
convertFun1Vert = convertFun1 convertOpenVertexOut

convertFun1Geom :: ExpC exp => forall a b. GPU a =>
                   Layout
                -> (H.Exp G a -> H.GeometryOut b) 
                -> exp
convertFun1Geom = convertFun1 convertOpenGeometryOut

convertFun1Frag :: ExpC exp => forall a b. GPU a =>
                   Layout 
                -> (H.Exp F a -> H.FragmentOut b) 
                -> exp
convertFun1Frag = convertFun1 convertOpenFragmentOut

convertFun1Exp :: ExpC exp => forall stage a b. GPU a
               => Layout 
               -> (H.Exp stage a -> H.Exp stage b) 
               -> exp
convertFun1Exp = convertFun1 convertOpenExp

{-
convertFun1 :: (GPU a, Typeable a)
            => (Layout (env, a) ((), a) -> Layout genv genv -> he b -> de (env, a) genv b)
            -> Layout genv genv
            -> (H.Exp a -> he b)
            -> D.OpenFun de env genv (a -> b)
-}
convertFun1 :: (GPU t, ExpC b) => ([t3] -> t1 -> t2 -> b) -> t1 -> (H.Exp stage t -> t2) -> b
convertFun1 cvt glyt f = lam $ body $ cvt lyt glyt (f a)
  where
    a     = case f of
              (fv :: H.Exp stage t -> t2) -> H.Tag 0 (typeOf (undefined :: t))
    lyt   = [undefined]{-EmptyLayout 
            `PushLayout` 
            ZeroIdx-}
            --(ZeroIdx :: Idx ((), EltRepr a) (EltRepr a))
    

convertExp :: ExpC exp
           => Layout      -- array environment
           -> H.Exp stage t          -- expression to be converted
           -> exp
convertExp glyt = convertOpenExp [] glyt

convertTuple :: ExpC exp
             => Layout
             -> Layout
             -> Tuple (H.Exp stage) t 
             -> [exp]
convertTuple _lyt _glyt NilTup          = []
convertTuple lyt  glyt (es `SnocTup` e) = convertTuple lyt glyt es ++ [convertOpenExp lyt glyt e]

-- data type conversion

convertTexture :: ExpC exp
               => T.Texture (H.Exp T.Obj) dim arr t ar
               -> exp
convertTexture (T.TextureSlot n t) = textureSlot n (convertTextureType t)
convertTexture (T.Texture t s m d) = texture (convertTextureType t) (T.toValue s) (convertMipMap m) (map convertGP d)

convertTextureDataType :: T.TextureDataType t ar -> TextureDataType
convertTextureDataType (T.Float a)  = FloatT (T.toColorArity a)
convertTextureDataType (T.Int a)    = IntT (T.toColorArity a)
convertTextureDataType (T.Word a)   = WordT (T.toColorArity a)
convertTextureDataType T.Shadow     = ShadowT

convertTextureType :: T.TextureType dim mip arr layerCount t ar -> TextureType
convertTextureType (T.Texture1D a b)    = Texture1D (convertTextureDataType a) (toInt b)
convertTextureType (T.Texture2D a b)    = Texture2D (convertTextureDataType a) (toInt b)
convertTextureType (T.Texture3D a)      = Texture3D (convertTextureDataType a)
convertTextureType (T.TextureCube a)    = TextureCube (convertTextureDataType a)
convertTextureType (T.TextureRect a)    = TextureRect (convertTextureDataType a)
convertTextureType (T.Texture2DMS a b)  = Texture2DMS (convertTextureDataType a) (toInt b)
convertTextureType (T.TextureBuffer a)  = TextureBuffer (convertTextureDataType a)

convertMipMap :: T.MipMap t -> MipMap
convertMipMap (T.NoMip)         = NoMip
convertMipMap (T.Mip a b)       = Mip a b
convertMipMap (T.AutoMip a b)   = AutoMip a b

convertRasterContext :: T.RasterContext p -> RasterContext
convertRasterContext (T.PointCtx a b c)         = PointCtx a b c
convertRasterContext (T.LineCtx a b)            = LineCtx a b
convertRasterContext (T.TriangleCtx a b c d)    = TriangleCtx a b c d

convertBlending :: T.Blending c -> Blending
convertBlending T.NoBlending        = NoBlending
convertBlending (T.BlendLogicOp a)  = BlendLogicOp a
convertBlending (T.Blend a b c)     = Blend a b c

convertAccumulationContext :: T.AccumulationContext b -> AccumulationContext
convertAccumulationContext (T.AccumulationContext n ops) = AccumulationContext n $ cvt ops
  where
    cvt :: FlatTuple Typeable T.FragmentOperation b -> [FragmentOperation]
    cvt ZT                          = []
    cvt (T.DepthOp a b:.xs)         = DepthOp a b : cvt xs
    cvt (T.StencilOp a b c :. xs)   = StencilOp a b c : cvt xs
    cvt (T.ColorOp a b :. xs)       = ColorOp (convertBlending a) (T.toValue b) : cvt xs

convertFrameBuffer :: T.FrameBuffer layerCount t -> [Image]
convertFrameBuffer = cvt
  where
    cvt :: T.FrameBuffer layerCount t -> [Image]
    cvt ZT                          = []
    cvt (T.DepthImage a b:.xs)      = DepthImage (toInt a) b : cvt xs
    cvt (T.StencilImage a b:.xs)    = StencilImage (toInt a) b : cvt xs
    cvt (T.ColorImage a b:.xs)      = ColorImage (toInt a) (T.toValue b) : cvt xs
