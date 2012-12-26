module LC_C_Convert (convertExp) where

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

genTy :: T.LCType t => H.Exp freq t -> Ty
genTy a = Ty

convertInterpolatedExpTuple :: ExpC exp => forall freq t.
                               Layout       -- scalar environment
                            -> H.InterpolatedExpTuple freq t               -- expression to be converted
                            -> [exp]
convertInterpolatedExpTuple lyt expr = cvtExpr
  where
    cvtExpr = case expr of
        ZT      -> []
        e :. xs -> cvt' e : convertInterpolatedExpTuple lyt xs

    cvt' :: (T.LCType t', ExpC exp) => H.Interpolated (H.Exp freq) t' -> exp
    cvt' v = case v of
        H.Flat e            -> flat          ty (convertExp lyt e)
        H.Smooth e          -> smooth        ty (convertExp lyt e)
        H.NoPerspective e   -> noPerspective ty (convertExp lyt e)
      where
        ty = undefined--genTy v

convertExpTuple :: ExpC exp => forall freq t.
                   Layout       -- scalar environment
                -> H.ExpTuple freq t               -- expression to be converted
                -> [exp]
convertExpTuple lyt expr = case expr of
    ZT      -> []
    e :. xs -> convertExp lyt e : convertExpTuple lyt xs

convertExp :: (T.LCType t, ExpC exp) => forall freq.
                  Layout
               -> H.Exp freq t               -- expression to be converted
               -> exp
convertExp lyt expr = cvtExpr
  where
    ty = genTy expr
    cvtExpr = case expr of
{-
        H.Tag i li                      -> var              ty (prjIdx i lyt) li
        H.Const v                       -> const_           ty (T.toValue v)
        H.Input v                       -> input            ty (fst $ T.toInput v)
-}
        H.Use a                         -> use              ty (convertExp lyt a)
        H.Cond a b c                    -> cond             ty (convertExp lyt a) (convertExp lyt b) (convertExp lyt c)
        H.PrimApp a b                   -> primApp          ty (convertPrimFun a) (convertExp lyt b)
        H.Tup a                         -> tup              ty (convertExpTuple lyt a)
        H.Prj a b                       -> prj              ty (toInt a) (convertExp lyt b)
        H.Loop a b c d                  -> loop             ty (convertFun1 lyt a) (convertFun1 lyt b) (convertFun1 lyt c) (convertExp lyt d)

        H.ArrayFromList a               -> arrayFromList    ty (map (convertExp lyt) a)
        H.ArrayReplicate a b            -> arrayReplicate   ty (convertExp lyt a) (convertExp lyt b)
        H.ArrayGenerate a b             -> arrayGenerate    ty (convertExp lyt a) (convertFun1 lyt b)
        H.ArrayIterateN a b c           -> arrayIterateN    ty (convertExp lyt a) (convertFun1 lyt b) (convertExp lyt c)
        H.ArrayIndex a b                -> arrayIndex       ty (convertExp lyt a) (convertExp lyt b)
        H.ArrayFilter a b               -> arrayFilter      ty (convertFun1 lyt a) (convertExp lyt b)
        H.ArrayMap a b                  -> arrayMap         ty (convertFun1 lyt a) (convertExp lyt b)
        H.ArrayZipWith a b c            -> arrayZipWith     ty (convertFun2 lyt a) (convertExp lyt b) (convertExp lyt c)
        H.ArrayAccumulate a b c         -> arrayAccumulate  ty (convertFun2 lyt a) (convertExp lyt b) (convertExp lyt c)

        H.Fetch a b c                   -> fetch            ty (convertFetchPrimitive a) (convertExp lyt b) (maybe Nothing (\v -> Just (convertExp lyt v)) c)
        H.Transform a b                 -> transform        ty (convertFun1 lyt a) (convertExp lyt b)
        H.Reassemble a b                -> reassemble       ty (convertExp lyt a) (convertExp lyt b)
        H.Rasterize a b                 -> rasterize        ty (convertRasterContext a) (convertExp lyt b)
{-
        H.FrameBuffer a                 -> frameBuffer      ty (convertFrameBuffer a)

        H.Accumulate a b c d e          -> accumulate       ty (convertAccumulationContext a) (convertExp lyt b)
                                                                                     (convertFun1 lyt c)
                                                                                     (convertExp lyt d)
                                                                                     (convertExp lyt e)

-}
        H.ArrayFromStream a             -> arrayFromStream  ty (convertExp lyt a)

        H.PrjFrameBuffer a b            -> prjFrameBuffer   ty (toInt a) (convertExp lyt b)
        H.PrjImage a b                  -> prjImage         ty (toInt a) (convertExp lyt b)

        H.VertexOut a b c               -> vertexOut            ty (convertExp lyt a) (convertExp lyt b) (convertInterpolatedExpTuple lyt c)
        H.GeometryShader a b c d e f    -> geometryShader       ty (toInt a) (convertOutputPrimitive b) c (convertFun1 lyt d) (convertFun1 lyt e) (convertFun1 lyt f)
        H.GeometryOut a b c d e f       -> geometryOut          ty (convertExp lyt a) (convertExp lyt b) (convertExp lyt c) (convertExp lyt d) (convertExp lyt e) (convertInterpolatedExpTuple lyt f)
        H.FragmentOut a                 -> fragmentOut          ty (convertExpTuple lyt a)
        H.FragmentOutDepth a b          -> fragmentOutDepth     ty (convertExp lyt a) (convertExpTuple lyt b)
        H.FragmentOutRastDepth a        -> fragmentOutRastDepth ty (convertExpTuple lyt a)

        H.PassAll                       -> passAll              ty
        H.Filter f                      -> filter_              ty (convertFun1 lyt f)

        H.ImageOut n a                  -> imageOut             ty n (convertExp lyt a)
        H.ScreenOut a                   -> screenOut            ty (convertExp lyt a)

-- TODO
convertFun1 :: (T.LCType a, T.LCType b, ExpC exp)
            => [Ty] -> (H.Exp freq a -> H.Exp freq b) -> exp
convertFun1 lyt f = lam ty1 $ body ty2 $ convertExp lyt' (f a)
  where
    a       = case f of
              (fv :: H.Exp freq a -> H.Exp freq b) -> H.Tag (length lyt) undefined--(typeOf (undefined :: a))
    lyt'    = undefined:lyt -- TODO
    ty1     = undefined
    ty2     = undefined

convertFun2 :: (T.LCType a, T.LCType b, T.LCType c, ExpC exp)
            => [Ty] -> (H.Exp freq a -> H.Exp freq b -> H.Exp freq c) -> exp
convertFun2 lyt f = lam ty1 $ lam ty2 $ body ty3 $ convertExp lyt' (f a b)
  where
    a       = case f of
              (fv :: H.Exp freq a -> H.Exp freq b -> H.Exp freq c) -> H.Tag (length lyt+1) undefined--(typeOf (undefined :: a))
    b       = case f of
              (fv :: H.Exp freq a -> H.Exp freq b -> H.Exp freq c) -> H.Tag (length lyt) undefined--(typeOf (undefined :: a))
    lyt'    = undefined:undefined:lyt -- TODO
    ty1     = undefined
    ty2     = undefined
    ty3     = undefined


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

convertFetchPrimitive :: T.FetchPrimitive a b -> FetchPrimitive
convertFetchPrimitive v = case v of
    T.Points                    -> Points
    T.LineStrip                 -> LineStrip
    T.LineLoop                  -> LineLoop
    T.Lines                     -> Lines
    T.TriangleStrip             -> TriangleStrip
    T.TriangleFan               -> TriangleFan
    T.Triangles                 -> Triangles
    T.LinesAdjacency            -> LinesAdjacency
    T.LineStripAdjacency        -> LineStripAdjacency
    T.TrianglesAdjacency        -> TrianglesAdjacency
    T.TriangleStripAdjacency    -> TriangleStripAdjacency

convertOutputPrimitive :: T.OutputPrimitive a -> OutputPrimitive
convertOutputPrimitive v = case v of
    T.TrianglesOutput   -> TrianglesOutput
    T.LinesOutput       -> LinesOutput
    T.PointsOutput      -> PointsOutput

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