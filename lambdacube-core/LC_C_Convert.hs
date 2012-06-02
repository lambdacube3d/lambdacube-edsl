module LC_C_Convert (convertGPOutput) where

import Debug.Trace
import Data.Typeable

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
{-
-- Typed de Bruijn indices
-- -----------------------

-- De Bruijn variable index projecting a specific type from a type
-- environment.  Type environments are nested pairs (..((), t1), t2, ..., tn).
--
data Idx env t where
    ZeroIdx ::              Idx (env, t) t
    SuccIdx :: Idx env t -> Idx (env, s) t


-- Environments
-- ------------

-- Valuation for an environment
--
data Val env where
    Empty :: Val ()
    Push  :: Val env -> t -> Val (env, t)

deriving instance Typeable1 Val


-- Projection of a value from a valuation using a de Bruijn index
--
prj :: Idx env t -> Val env -> t
prj ZeroIdx       (Push _   v) = v
prj (SuccIdx idx) (Push val _) = prj idx val
prj _             _            = error "prj" "inconsistent valuation"

-- Convert a typed de Bruijn index to the corresponding integer
--
idxToInt :: Idx env t -> Int
idxToInt = go 0
  where go :: Int -> Idx env t -> Int
        go !n ZeroIdx       = n
        go !n (SuccIdx idx) = go (n+1) idx

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
-}
prjIdx _ _ = 0
{-
size :: Layout env env' -> Int
size EmptyLayout        = 0
size (PushLayout lyt _) = size lyt + 1

-- Add an entry to a layout, incrementing all indices
--
incLayout :: Layout env env' -> Layout (env, t) env'
incLayout EmptyLayout         = EmptyLayout
incLayout (PushLayout lyt ix) = PushLayout (incLayout lyt) (SuccIdx ix)
-}
--prjToInt :: TupleIdx t e -> TupleType a -> Int
--prjToInt = undefined
--prjToInt _ = 0
prjToInt :: TupleIdx t e -> Int
prjToInt ZeroTupIdx     = 0
prjToInt (SuccTupIdx i) = 1 + prjToInt i
{-
prjToInt ZeroTupIdx     _                 = 0
prjToInt (SuccTupIdx i) (b `PairTuple` a) = length (codeGenTupleType a) + prjToInt i b
prjToInt _ _ = error "prjToInt" "inconsistent valuation"
-}
type Layout = [Ty]

{-
data TupleType a where
  UnitTuple   ::                               TupleType ()
  SingleTuple :: IsScalar a =>            a -> TupleType a
  PairTuple   :: TupleType a -> TupleType b -> TupleType (a, b)
-}
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

convertGPOutput :: H.GPOutput -> GPOutput
convertGPOutput (H.ImageOut a b)    = ImageOut a (convertGP b)
convertGPOutput (H.ScreenOut a)     = ScreenOut (convertGP a)

-- GP
convertGP :: H.GP t -> GP
convertGP = convertOpenGP []

convertOpenGP :: Layout -> H.GP t -> GP
convertOpenGP = cvt
  where
    cvt :: Layout -> H.GP t -> GP
    cvt glyt (H.GPtag i)                    = GPVar (prjIdx i glyt)
    cvt glyt (H.Fetch n p i)                = Fetch n (T.toPrimitive p) (T.toInputList i)
    cvt glyt (H.Transform vs ps)            = Transform (convertFun1Vert glyt vs) (cvt glyt ps)
    cvt glyt (H.Reassemble sh ps)           = Reassemble (convertGeometryShader glyt sh) (cvt glyt ps)
    cvt glyt (H.Rasterize ctx ps)           = Rasterize (convertRasterContext ctx) (cvt glyt ps)
    cvt glyt (H.FrameBuffer fb)             = FrameBuffer (convertFrameBuffer fb)
    cvt glyt (H.Accumulate ctx f sh fs fb)  = Accumulate (convertAccumulationContext ctx) (convertFragmentFilter glyt f) (convertFun1Frag glyt sh) (cvt glyt fs) (cvt glyt fb)
    cvt glyt (H.PrjFrameBuffer n idx fb)    = PrjFrameBuffer n (prjToInt idx) (convertGP fb)
    cvt glyt (H.PrjImage n idx img)         = PrjImage n (toInt idx) (convertGP img)

-- Vertex
convertOpenVertexOut :: forall t.
                        Layout       -- scalar environment
                     -> Layout      -- array environment
                     -> H.VertexOut t               -- expression to be converted
                     -> Exp
convertOpenVertexOut lyt glyt = cvt
  where
    cvt :: H.VertexOut t' -> Exp
    cvt (H.VertexOut e1 e2 ie :: H.VertexOut t')  = VertexOut {-(genTy (undefined :: t'))-} (convertOpenExp lyt glyt e1) (convertOpenExp lyt glyt e2) (convertOpenInterpolatedFlatExp lyt glyt ie)

-- Fragment
convertOpenFragmentOut :: forall t.
                          Layout       -- scalar environment
                       -> Layout      -- array environment
                       -> H.FragmentOut t               -- expression to be converted
                       -> Exp
convertOpenFragmentOut lyt glyt = cvt
  where
    cvt :: H.FragmentOut t' -> Exp
    cvt (H.FragmentOut fe :: H.FragmentOut t')          = FragmentOut {-(genTy (undefined :: t'))-} (convertOpenFlatExp lyt glyt fe)
    cvt (H.FragmentOutDepth e fe :: H.FragmentOut t')   = FragmentOutDepth {-(genTy (undefined :: t'))-} (convertOpenExp lyt glyt e) (convertOpenFlatExp lyt glyt fe)
    cvt (H.FragmentOutRastDepth fe :: H.FragmentOut t') = FragmentOutRastDepth {-(genTy (undefined :: t'))-} (convertOpenFlatExp lyt glyt fe)

convertFragmentFilter :: GPU a
                      => Layout
                      -> H.FragmentFilter a
                      -> FragmentFilter
convertFragmentFilter = cvt
  where
    cvt :: GPU a => Layout -> H.FragmentFilter a -> FragmentFilter
    cvt glyt H.PassAll      = PassAll
    cvt glyt (H.Filter f)   = Filter (convertFun1Exp glyt f)

-- Geometry
convertOpenGeometryOut :: forall t.
                          Layout       -- scalar environment
                       -> Layout      -- array environment
                       -> H.GeometryOut t               -- expression to be converted
                       -> Exp
convertOpenGeometryOut lyt glyt = cvt
  where
    cvt :: H.GeometryOut t' -> Exp
    cvt (H.GeometryOut e1 e2 e3 e4 e5 ie :: H.GeometryOut t') = GeometryOut {-(genTy (undefined :: t'))-} (convertOpenExp lyt glyt e1)
                                                        (convertOpenExp lyt glyt e2)
                                                        (convertOpenExp lyt glyt e3)
                                                        (convertOpenExp lyt glyt e4)
                                                        (convertOpenExp lyt glyt e5)
                                                        (convertOpenInterpolatedFlatExp lyt glyt ie)

convertGeometryShader :: Layout
                      -> H.GeometryShader primIn primOut layerNum a b
                      -> GeometryShader
convertGeometryShader = cvt
  where
    cvt ::  Layout -> H.GeometryShader primIn primOut layerNum a b -> GeometryShader
    cvt glyt (H.GeometryShader a b c e1 e2 e3)  = GeometryShader (toInt a) (T.toPrimitive b) c (convertFun1Exp glyt e1) (convertFun1Exp glyt e2) (convertFun1Geom glyt e3)

-- Common
convertOpenInterpolatedFlatExp :: forall stage t.
                              Layout       -- scalar environment
                           -> Layout      -- array environment
                           -> H.InterpolatedFlatExp stage t               -- expression to be converted
                           -> [Interpolated Exp]
convertOpenInterpolatedFlatExp lyt glyt = cvt
  where
    cvt :: H.InterpolatedFlatExp stage t' -> [Interpolated Exp]
    cvt (ZT)    = []
    cvt (e:.xs) = cvt' e : cvt xs

    cvt' :: T.Interpolated (H.Exp stage) t' -> Interpolated Exp
    cvt' (T.Flat e)           = Flat          (convertOpenExp lyt glyt e)
    cvt' (T.Smooth e)         = Smooth        (convertOpenExp lyt glyt e)
    cvt' (T.NoPerspective e)  = NoPerspective (convertOpenExp lyt glyt e)

convertOpenFlatExp :: forall stage t.
                      Layout       -- scalar environment
                   -> Layout      -- array environment
                   -> H.FlatExp stage t               -- expression to be converted
                   -> [Exp]
convertOpenFlatExp lyt glyt = cvt
  where
    cvt :: H.FlatExp stage t' -> [Exp]
    cvt (ZT)    = []
    cvt (e:.xs) = convertOpenExp lyt glyt e : cvt xs

convertOpenExp :: forall stage t.
                  Layout       -- scalar environment
               -> Layout      -- array environment
               -> H.Exp stage t               -- expression to be converted
               -> Exp
convertOpenExp lyt glyt = cvt
  where
    cvt :: H.Exp stage t' -> Exp
    cvt (H.Tag i :: H.Exp stage t')           = Var (genTy (undefined :: t')) (prjIdx i lyt)
    cvt (H.Const v :: H.Exp stage t')         = Const (genTy (undefined :: t')) (T.toValue v)
    cvt (H.PrimVar v :: H.Exp stage t')       = PrimVar (genTy (undefined :: t')) (fst $ T.toInput v)
    cvt (H.Uni v :: H.Exp stage t')           = Uni (genTy (undefined :: t')) (fst $ T.toInput v)
    cvt (H.Tup tup :: H.Exp stage t')         = Tup (genTy (undefined :: t')) (convertTuple lyt glyt tup)
    cvt (H.Prj idx e :: H.Exp stage t')       = Prj (genTy (undefined :: t')) (prjToInt idx) (cvt e)
    cvt (H.Cond e1 e2 e3 :: H.Exp stage t')   = Cond (genTy (undefined :: t')) (cvt e1) (cvt e2) (cvt e3)
    cvt (H.PrimApp p e :: H.Exp stage t')     = PrimApp (genTy (undefined :: t')) (convertPrimFun p) (cvt e)
    cvt (H.Sampler f em t :: H.Exp stage t')  = Sampler (genTy (undefined :: t')) f em (convertTexture t)

convertFun1Vert :: forall a b. GPU a
                => Layout
                -> (H.Exp V a -> H.VertexOut b) 
                -> ExpFun
convertFun1Vert = convertFun1 convertOpenVertexOut

convertFun1Geom :: forall a b. GPU a
                => Layout
                -> (H.Exp G a -> H.GeometryOut b) 
                -> ExpFun
convertFun1Geom = convertFun1 convertOpenGeometryOut

convertFun1Frag :: forall a b. GPU a
                => Layout 
                -> (H.Exp F a -> H.FragmentOut b) 
                -> ExpFun
convertFun1Frag = convertFun1 convertOpenFragmentOut

convertFun1Exp :: forall stage a b. GPU a
               => Layout 
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
    lyt   = [undefined]{-EmptyLayout 
            `PushLayout` 
            ZeroIdx-}
            --(ZeroIdx :: Idx ((), EltRepr a) (EltRepr a))
    openF = cvt lyt glyt (f a)

convertExp :: Layout      -- array environment
           -> H.Exp stage t          -- expression to be converted
           -> Exp
convertExp glyt = convertOpenExp [] glyt

convertTuple :: Layout
             -> Layout
             -> Tuple (H.Exp stage) t 
             -> [Exp]
convertTuple _lyt _glyt NilTup          = []
convertTuple lyt  glyt (es `SnocTup` e) = convertTuple lyt glyt es ++ [convertOpenExp lyt glyt e]

-- data type conversion

convertTexture :: T.Texture (H.GP) dim arr t ar
               -> Texture GP
convertTexture (T.TextureSlot n t) = TextureSlot n (convertTextureType t)
convertTexture (T.Texture t s m d) = Texture (convertTextureType t) (T.toValue s) (convertMipMap m) (map convertGP d)

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
convertRasterContext T.PointCtx                 = PointCtx
convertRasterContext (T.LineCtx a b)            = LineCtx a b
convertRasterContext (T.TriangleCtx a b c d)    = TriangleCtx a b c d

convertBlending :: T.Blending c -> Blending
convertBlending T.NoBlending        = NoBlending
convertBlending (T.BlendLogicOp a)  = BlendLogicOp a
convertBlending (T.Blend a b c)     = Blend a b c

convertAccumulationContext :: T.AccumulationContext b -> [FragmentOperation]
convertAccumulationContext = cvt
  where
    cvt :: T.AccumulationContext b -> [FragmentOperation]
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
