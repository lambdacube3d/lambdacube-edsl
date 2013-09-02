module LC_C_Convert (convertGPOutput) where

import GHC.TypeLits

import Debug.Trace

import LC_T_APIType (FlatTuple(..),Frequency(..))
import LC_T_DSLType (GPU,Tuple(..),TupleIdx(..))
import qualified LC_T_APIType as T
import qualified LC_T_DSLType as T hiding (Shadow)
import qualified LC_T_PrimFun as T
import qualified LC_T_HOAS as H
import LC_U_DeBruijn
import LC_U_APIType
import LC_G_APIType
import LC_C_PrimFun
import LC_G_Type as G
import LC_B2_IR (SamplerDescriptor(..))

toInt :: SingI n => T.NatNum n -> Int
toInt (a :: T.NatNum n) = fromInteger $ fromSing (sing :: Sing (n :: Nat))

prjIdx i lyt = i--length lyt - i - 1


prjToInt :: TupleIdx t e -> Int
prjToInt ZeroTupIdx     = 0
prjToInt (SuccTupIdx i) = 1 + prjToInt i

genTupLen :: GPU a => Int -> a -> Int
genTupLen i a = sum $ map tySize $ take i rt
  where
    rt = reverse t
    Tuple t = genTy a

type Layout = [[Ty]]

genTy :: GPU a => a -> Ty
genTy = T.tupleType

convertGPOutput :: ExpC exp => H.GPOutput o -> exp
convertGPOutput (H.ImageOut a b c)  = imageOut a b $ convertGP c
convertGPOutput (H.ScreenOut a)     = screenOut $ convertGP a
convertGPOutput (H.MultiOut a)      = multiOut $ map convertGPOutput a

-- GP
convertGP :: ExpC exp => H.Exp T.Obj t -> exp
convertGP = convertOpenGP []

convertOpenGP :: ExpC exp => Layout -> H.Exp T.Obj t -> exp
convertOpenGP = cvt
  where
    cvt :: ExpC exp => Layout -> H.Exp T.Obj t -> exp
    cvt lyt (H.Fetch n p i)                = fetch n (convertFetchPrimitive p) (T.toInputList i)
    cvt lyt (H.Transform vs ps)            = transform  (convertFun1Vert lyt vs) (cvt lyt ps)
    cvt lyt (H.Reassemble sh ps)           = reassemble (convertGeometryShader lyt sh) (cvt lyt ps)
    cvt lyt (H.Rasterize ctx ps)           = rasterize (convertRasterContext ctx) $ cvt lyt ps
    cvt lyt (H.FrameBuffer fb)             = frameBuffer (convertFrameBuffer fb)
    cvt lyt (H.Accumulate ctx f sh fs fb)  = accumulate (convertAccumulationContext ctx) (convertFragmentFilter lyt f)
                                                                                          (convertFun1Frag lyt sh)
                                                                                          (cvt lyt fs)
                                                                                          (cvt lyt fb)
    cvt lyt (H.PrjFrameBuffer n idx fb)    = prjFrameBuffer n (prjToInt idx) $ convertGP fb
    cvt lyt (H.PrjImage n idx img)         = prjImage n (toInt idx) $ convertGP img

-- Vertex
convertOpenVertexOut :: ExpC exp => forall t.
                        Layout       -- environment
                     -> H.VertexOut clipDistances t               -- expression to be converted
                     -> exp
convertOpenVertexOut lyt = cvt
  where
    cvt :: ExpC exp => H.VertexOut clipDistances t' -> exp
    cvt (H.VertexOut e1 e2 e3 ie :: H.VertexOut clipDistances t')  = vertexOut (convertOpenExp lyt e1) (convertOpenExp lyt e2) (convertOpenFlatExp lyt e3) (convertOpenInterpolatedFlatExp lyt ie)

-- Fragment
convertOpenFragmentOut :: ExpC exp => forall t.
                          Layout       -- environment
                       -> H.FragmentOut t               -- expression to be converted
                       -> exp
convertOpenFragmentOut lyt = cvt
  where
    cvt :: ExpC exp => H.FragmentOut t' -> exp
    cvt (H.FragmentOut fe :: H.FragmentOut t')          = fragmentOut $ convertOpenFlatExp lyt fe
    cvt (H.FragmentOutDepth e fe :: H.FragmentOut t')   = fragmentOutDepth (convertOpenExp lyt e) (convertOpenFlatExp lyt fe)
    cvt (H.FragmentOutRastDepth fe :: H.FragmentOut t') = fragmentOutRastDepth $ convertOpenFlatExp lyt fe

convertFragmentFilter :: (ExpC exp, GPU a)
                      => Layout
                      -> H.FragmentFilter a
                      -> exp
convertFragmentFilter = cvt
  where
    cvt :: (ExpC exp, GPU a) => Layout -> H.FragmentFilter a -> exp
    cvt lyt H.PassAll      = passAll
    cvt lyt (H.Filter f)   = filter_ $ convertFun1Exp lyt f

-- Geometry
convertOpenGeometryOut :: ExpC exp => forall i clipDistances t.
                          Layout       -- environment
                       -> H.GeometryOut i clipDistances t               -- expression to be converted
                       -> exp
convertOpenGeometryOut lyt = cvt
  where
    cvt :: ExpC exp => H.GeometryOut i clipDistances t' -> exp
    cvt (H.GeometryOut e1 e2 e3 e4 ie :: H.GeometryOut i clipDistances t') = geometryOut (convertOpenExp lyt e1)
                                                                            (convertOpenExp lyt e2)
                                                                            (convertOpenExp lyt e3)
                                                                            (convertOpenFlatExp lyt e4)
                                                                            (convertOpenInterpolatedFlatExp lyt ie)

convertGeometryShader :: ExpC exp
                      => Layout
                      -> H.GeometryShader inputPrimitive outputPrimitive inputClipDistances outputClipDistances layerCount a b
                      -> exp
convertGeometryShader = cvt
  where
    cvt :: ExpC exp => Layout -> H.GeometryShader inputPrimitive outputPrimitive inputClipDistances outputClipDistances layerCount a b -> exp
    cvt lyt (H.GeometryShader a b c e1 e2 e3)  = geometryShader (toInt a) (convertOutputPrimitive b) c (convertFun1Exp lyt e1)
                                                                                              (convertFun1Exp lyt e2)
                                                                                              (convertFun1Geom lyt e3)

-- Common
convertOpenInterpolatedFlatExp :: ExpC exp => forall stage t.
                                  Layout       -- environment
                               -> H.InterpolatedFlatExp stage t               -- expression to be converted
                               -> [exp]
convertOpenInterpolatedFlatExp lyt = cvt
  where
    cvt :: ExpC exp => H.InterpolatedFlatExp stage t' -> [exp]
    cvt (ZT)    = []
    cvt (e:.xs) = cvt' e : cvt xs

    cvt' :: ExpC exp => T.Interpolated (H.Exp stage) t' -> exp
    cvt' (T.Flat e)           = flat          $ convertOpenExp lyt e
    cvt' (T.Smooth e)         = smooth        $ convertOpenExp lyt e
    cvt' (T.NoPerspective e)  = noPerspective $ convertOpenExp lyt e

convertOpenFlatExp :: ExpC exp => forall stage t.
                      Layout       -- environment
                   -> H.FlatExp stage t               -- expression to be converted
                   -> [exp]
convertOpenFlatExp lyt = cvt
  where
    cvt :: ExpC exp => H.FlatExp stage t' -> [exp]
    cvt (ZT)    = []
    cvt (e:.xs) = convertOpenExp lyt e : cvt xs

convertOpenExp :: ExpC exp => forall stage t.
                  Layout       -- environment
               -> H.Exp stage t               -- expression to be converted
               -> exp
convertOpenExp lyt = cvt
  where
    cvt :: ExpC exp => H.Exp stage t' -> exp
    cvt (H.Tag i li :: H.Exp stage t')        = var (genTy (undefined :: t')) (prjIdx i lyt) li
    cvt (H.Const v :: H.Exp stage t')         = const_ (genTy (undefined :: t')) (T.toValue v)
    cvt (H.PrimVar v :: H.Exp stage t')       = primVar (genTy (undefined :: t')) (fst $ T.toInput v)
    cvt (H.Uni v :: H.Exp stage t')           = uni (genTy (undefined :: t')) (fst $ T.toInput v)
    cvt (H.Tup tupl :: H.Exp stage t')        = tup (genTy (undefined :: t')) $ convertTuple lyt tupl
    cvt (H.Prj idx (e :: H.Exp stage e') :: H.Exp stage' t')       = prj (genTy (undefined :: t')) (genTupLen (prjToInt idx) (undefined :: e')) $ cvt e
    cvt (H.Cond e1 e2 e3 :: H.Exp stage t')   = cond (genTy (undefined :: t')) (cvt e1) (cvt e2) (cvt e3)
    cvt (H.PrimApp p e :: H.Exp stage t')     = primApp (genTy (undefined :: t')) (convertPrimFun p) $ cvt e
    cvt s@(H.Sampler {H.smpTexture = t} :: H.Exp stage t') = sampler (genTy (undefined :: t')) (convertSampler s) $ convertTexture t
    cvt (H.Loop e1 e2 e3 s :: H.Exp stage t') = loop (genTy (undefined :: t')) (convertFun1Exp lyt e1) (convertFun1Exp lyt e2) (convertFun1Exp lyt e3) (cvt s)

convertFun1Vert :: ExpC exp => forall a b clipDistances. GPU a
                => Layout
                -> (H.Exp V a -> H.VertexOut clipDistances b) 
                -> exp
convertFun1Vert = convertFun1 convertOpenVertexOut

convertFun1Geom :: ExpC exp => (GPU a, GPU i, GPU b, GPU clipDistances)
                => Layout
                -> (H.Exp G a -> H.GeometryOut i clipDistances b) 
                -> exp
convertFun1Geom = convertFun1 convertOpenGeometryOut

convertFun1Frag :: ExpC exp => forall a b. GPU a
                => Layout
                -> (H.Exp F a -> H.FragmentOut b) 
                -> exp
convertFun1Frag = convertFun1 convertOpenFragmentOut

convertFun1Exp :: ExpC exp => forall stage a b. GPU a
               => Layout
               -> (H.Exp stage a -> H.Exp stage b) 
               -> exp
convertFun1Exp = convertFun1 convertOpenExp

convertFun1 :: (GPU a, ExpC exp)
            => (Layout -> b -> exp) -> Layout -> (H.Exp stage a -> b) -> exp
convertFun1 cvt lyt (f :: H.Exp stage t' -> b) = lam (genTy (undefined :: t')) $ body $ cvt lyt' (f a)
  where
    lyt'    = []:lyt
    a       = case f of
              (fv :: H.Exp stage t -> t2) -> H.Tag (length lyt) (show $ genTy (undefined :: t))

convertExp :: ExpC exp
           => Layout      -- array environment
           -> H.Exp stage t          -- expression to be converted
           -> exp
convertExp lyt = convertOpenExp lyt

convertTuple :: ExpC exp
             => Layout
             -> Tuple (H.Exp stage) t 
             -> [exp]
convertTuple _lyt NilTup          = []
convertTuple lyt (es `SnocTup` e) = convertTuple lyt es ++ [convertOpenExp lyt e]

-- data type conversion
convertSampler :: H.Exp stage (T.Sampler dim arr t ar) -> SamplerDescriptor
convertSampler smp@(H.Sampler minF magF eMod bCol minL maxL lBias cmpF _) =
      SamplerDescriptor
        { samplerWrapS          = wrapS
        , samplerWrapT          = wrapT
        , samplerWrapR          = wrapR
        , samplerMinFilter      = cvtFilter minF
        , samplerMagFilter      = cvtFilter magF
        , samplerBorderColor    = T.toValue bCol
        , samplerMinLod         = minL
        , samplerMaxLod         = maxL
        , samplerLodBias        = lBias
        , samplerCompareFunc    = case cmpF of
                                    T.NoCompare -> Nothing
                                    T.Compare a -> Just a
        }
      where
        (wrapS,wrapT,wrapR) = T.convertEdgeMode eMod
        cvtFilter f = case f of
            T.Nearest               -> Nearest
            T.Linear                -> Linear
            T.NearestMipmapNearest  -> NearestMipmapNearest
            T.NearestMipmapLinear   -> NearestMipmapLinear
            T.LinearMipmapNearest   -> LinearMipmapNearest
            T.LinearMipmapLinear    -> LinearMipmapLinear

convertTexture :: ExpC exp
               => T.Texture (H.Exp T.Obj) dim arr t ar mip
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
convertTextureType (T.Texture2DMS a b c d)  = Texture2DMS (convertTextureDataType a) (toInt b) c d
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

convertFetchPrimitive :: T.FetchPrimitive a -> FetchPrimitive
convertFetchPrimitive v = case v of
    T.Points                    -> Points
    T.Lines                     -> Lines
    T.Triangles                 -> Triangles
    T.LinesAdjacency            -> LinesAdjacency
    T.TrianglesAdjacency        -> TrianglesAdjacency

convertOutputPrimitive :: T.OutputPrimitive a -> OutputPrimitive
convertOutputPrimitive v = case v of
    T.TrianglesOutput   -> TrianglesOutput
    T.LinesOutput       -> LinesOutput
    T.PointsOutput      -> PointsOutput

convertAccumulationContext :: T.AccumulationContext b -> AccumulationContext
convertAccumulationContext (T.AccumulationContext n ops) = AccumulationContext n $ cvt ops
  where
    cvt :: FlatTuple T.NoConstraint T.FragmentOperation b -> [FragmentOperation]
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

convertFilter :: T.Filter mip -> Filter
convertFilter f = case f of
    T.Nearest               -> Nearest
    T.Linear                -> Linear
    T.NearestMipmapNearest  -> NearestMipmapNearest
    T.NearestMipmapLinear   -> NearestMipmapLinear
    T.LinearMipmapNearest   -> LinearMipmapNearest
    T.LinearMipmapLinear    -> LinearMipmapLinear
