module LC_C_Convert {-(convertOutput)-} where

import Data.Typeable
import Debug.Trace

import TypeLevel.Number.Nat
import TypeLevel.Number.Nat.Num

import LC_T_APIType (Tuple(..),V,G,F)
import qualified LC_T_APIType as T
import qualified LC_T_PrimFun as T
import qualified LC_T_HOAS as H
import LC_U_DeBruijn
import LC_U_APIType
import LC_G_APIType
import LC_C_PrimFun


type Layout = [Ty]
{-
genTy :: GPU a => a -> Ty
genTy a = Ty

convertOutput :: ExpC exp => H.Output -> exp

-- GP
convertGP :: ExpC exp => H.Exp T.Obj t -> exp
convertGP = convertOpenGP []


-- Common
convertOpenInterpolatedFlatExp :: ExpC exp => forall freq t.
                                  Layout       -- scalar environment
                               -> Layout      -- array environment
                               -> H.InterpolatedFlatExp freq t               -- expression to be converted
                               -> [exp]
convertOpenInterpolatedFlatExp lyt glyt = cvt
  where
    cvt :: ExpC exp => H.InterpolatedFlatExp freq t' -> [exp]
    cvt (ZT)    = []
    cvt (e:.xs) = cvt' e : cvt xs

    cvt' :: ExpC exp => T.Interpolated (H.Exp freq) t' -> exp
    cvt' (T.Flat e)           = flat          $ convertOpenExp lyt glyt e
    cvt' (T.Smooth e)         = smooth        $ convertOpenExp lyt glyt e
    cvt' (T.NoPerspective e)  = noPerspective $ convertOpenExp lyt glyt e

convertOpenFlatExp :: ExpC exp => forall freq t.
                      Layout       -- scalar environment
                   -> Layout      -- array environment
                   -> H.FlatExp freq t               -- expression to be converted
                   -> [exp]
convertOpenFlatExp lyt glyt = cvt
  where
    cvt :: ExpC exp => H.FlatExp freq t' -> [exp]
    cvt (ZT)    = []
    cvt (e:.xs) = convertOpenExp lyt glyt e : cvt xs

convertOpenExp :: ExpC exp => forall freq t.
                  Layout
               -> H.Exp freq t               -- expression to be converted
               -> exp
convertOpenExp lyt expr = cvtExpt
  where
    ty = genTy expr
    cvtExpr = case expr of
        H.Tag i li                      -> var          ty (prjIdx i lyt) li
        H.Const v                       -> const_       ty (T.toValue v)
        H.PrimVar v                     -> primVar      ty (fst $ T.toInput v)
        H.Uni v                         -> uni          ty (fst $ T.toInput v)
        H.Tup tupl                      -> tup          ty $ convertTuple lyt glyt tupl
        H.Prj idx e                     -> prj          ty (prjToInt idx) $ cvt e
        H.Cond e1 e2 e3                 -> cond         ty (cvt e1) (cvt e2) (cvt e3)
        H.PrimApp p e                   -> primApp      ty (convertPrimFun p) $ cvt e
        H.Loop e1 e2 e3 s               -> loop         ty (convertFun1Exp glyt e1) (convertFun1Exp glyt e2) (convertFun1Exp glyt e3) (cvt s)
        H.Fetch n p i                   -> fetch        ty n (T.toPrimitive p) (T.toInputList i)
        H.Transform vs ps               -> transform    ty (convertFun1Vert glyt vs) (cvt glyt ps)
        H.Reassemble sh ps              -> reassemble   ty (convertGeometryShader glyt sh) (cvt glyt ps)
        H.Rasterize ctx ps              -> rasterize    ty (convertRasterContext ctx) $ cvt glyt ps
        H.FrameBuffer fb                -> frameBuffer  ty (convertFrameBuffer fb)
        H.Accumulate ctx f sh fs fb     -> accumulate   ty (convertAccumulationContext ctx) (convertFragmentFilter glyt f)
                                                                                     (convertFun1Frag glyt sh)
                                                                                     (cvt glyt fs)
                                                                                     (cvt glyt fb)
        H.PrjFrameBuffer n idx fb       -> prjFrameBuffer       ty n (prjToInt idx) $ convertGP fb
        H.PrjImage n idx img            -> prjImage             ty n (toInt idx) $ convertGP img

        H.ImageOut a b                  -> imageOut             ty a $ convertGP b
        H.ScreenOut a                   -> screenOut            ty $ convertGP a

        H.VertexOut e1 e2 ie            -> vertexOut            ty (convertOpenExp lyt glyt e1) (convertOpenExp lyt glyt e2) (convertOpenInterpolatedFlatExp lyt glyt ie)
        H.FragmentOut fe                -> fragmentOut          ty $ convertOpenFlatExp lyt glyt fe
        H.FragmentOutDepth e fe         -> fragmentOutDepth     ty (convertOpenExp lyt glyt e) (convertOpenFlatExp lyt glyt fe)
        H.FragmentOutRastDepth fe       -> fragmentOutRastDepth ty $ convertOpenFlatExp lyt glyt fe
        H.GeometryOut e1 e2 e3 e4 e5 ie -> geometryOut          ty (convertOpenExp lyt glyt e1)
                                                                            (convertOpenExp lyt glyt e2)
                                                                            (convertOpenExp lyt glyt e3)
                                                                            (convertOpenExp lyt glyt e4)
                                                                            (convertOpenExp lyt glyt e5)
                                                                            (convertOpenInterpolatedFlatExp lyt glyt ie)

        H.PassAll                       -> passAll  ty
        H.Filter f                      -> filter_  ty $ convertFun1Exp glyt f

        H.GeometryShader a b c e1 e2 e3 -> geometryShader   ty (toInt a) (T.toPrimitive b) c (convertFun1Exp glyt e1)
                                                                                               (convertFun1Exp glyt e2)
                                                                                               (convertFun1Geom glyt e3)
-}
{-
convertFun1 :: (GPU a, Typeable a)
            => (Layout (env, a) ((), a) -> Layout genv genv -> he b -> de (env, a) genv b)
            -> Layout genv genv
            -> (H.Exp a -> he b)
            -> D.OpenFun de env genv (a -> b)
-}
{-
convertFun1 :: (GPU t, ExpC b) => ([t3] -> t1 -> t2 -> b) -> t1 -> (H.Exp freq t -> t2) -> b
convertFun1 cvt glyt f = lam $ body $ cvt lyt glyt (f a)
  where
    a     = case f of
              (fv :: H.Exp freq t -> t2) -> H.Tag 0 (typeOf (undefined :: t))
    lyt   = [undefined]


convertExp :: ExpC exp
           => Layout      -- array environment
           -> H.Exp freq t          -- expression to be converted
           -> exp
convertExp glyt = convertOpenExp [] glyt

convertTuple :: ExpC exp
             => Layout
             -> Layout
             -> Tuple (H.Exp freq) t 
             -> [exp]
convertTuple _lyt _glyt NilTup          = []
convertTuple lyt  glyt (es `SnocTup` e) = convertTuple lyt glyt es ++ [convertOpenExp lyt glyt e]
-}

-- data type conversion

convertColorArity :: T.ColorArity a -> ColorArity
convertColorArity v = case v of
    T.Red   -> Red
    T.RG    -> RG
    T.RGB   -> RGB
    T.RGBA  -> RGBA

convertTextureDataType :: T.TextureDataType t ar -> TextureDataType
convertTextureDataType v = case v of
    T.FloatTexel a  -> FloatTexel   (convertColorArity a)
    T.IntTexel a    -> IntTexel     (convertColorArity a)
    T.WordTexel a   -> WordTexel    (convertColorArity a)
    T.ShadowTexel   -> ShadowTexel

convertTextureType :: T.TextureType dim mip arr layerCount t ar -> TextureType
convertTextureType v = case v of
    T.Texture1D a b     -> Texture1D     (convertTextureDataType a) (toInt b)
    T.Texture2D a b     -> Texture2D     (convertTextureDataType a) (toInt b)
    T.Texture3D a       -> Texture3D     (convertTextureDataType a)
    T.TextureCube a     -> TextureCube   (convertTextureDataType a)
    T.TextureRect a     -> TextureRect   (convertTextureDataType a)
    T.Texture2DMS a b   -> Texture2DMS   (convertTextureDataType a) (toInt b)
    T.TextureBuffer a   -> TextureBuffer (convertTextureDataType a)

convertMipMap :: T.MipMap t -> MipMap
convertMipMap v = case v of
    T.NoMip         -> NoMip
    T.Mip a b       -> Mip a b
    T.AutoMip a b   -> AutoMip a b

convertRasterContext :: T.RasterContext p -> RasterContext
convertRasterContext v = case v of
    T.PointCtx              -> PointCtx
    T.LineCtx a b           -> LineCtx a b
    T.TriangleCtx a b c d   -> TriangleCtx a b c d

convertBlending :: T.Blending c -> Blending
convertBlending v = case v of 
    T.NoBlending        -> NoBlending
    T.BlendLogicOp a    -> BlendLogicOp a
    T.Blend a b c       -> Blend a b c
{-
convertAccumulationContext :: T.AccumulationContext b -> AccumulationContext
convertAccumulationContext (T.AccumulationContext n ops) = AccumulationContext n $ cvt ops
  where
    cvt :: FlatTuple Typeable T.FragmentOperation b -> [FragmentOperation]
    cvt ZT                          = []
    cvt (T.DepthOp a b :. xs)       = DepthOp a b : cvt xs
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
-}