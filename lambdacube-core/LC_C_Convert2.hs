{-# LANGUAGE        BangPatterns #-}
{-# LANGUAGE        ConstraintKinds #-}
{-# LANGUAGE        DataKinds #-}
{-# LANGUAGE        DeriveDataTypeable #-}
{-# LANGUAGE        EmptyDataDecls #-}
{-# LANGUAGE        FlexibleContexts #-}
{-# LANGUAGE        FlexibleInstances #-}
{-# LANGUAGE        FunctionalDependencies #-}
{-# LANGUAGE        GADTs #-}
{-# LANGUAGE        ImpredicativeTypes #-}
{-# LANGUAGE        KindSignatures #-}
{-# LANGUAGE        MultiParamTypeClasses #-}
{-# LANGUAGE        OverloadedStrings #-}
{-# LANGUAGE        ParallelListComp #-}
{-# LANGUAGE        Rank2Types #-}
{-# LANGUAGE        ScopedTypeVariables #-}
{-# LANGUAGE        StandaloneDeriving #-}
{-# LANGUAGE        TupleSections #-}
{-# LANGUAGE        TypeFamilies #-}
{-# LANGUAGE        TypeOperators #-}
{-# LANGUAGE        TypeSynonymInstances #-}
{-# LANGUAGE        UndecidableInstances #-}
module LC_C_Convert2 {-(convertGPOutput)-} where

import GHC.TypeLits

import Debug.Trace

import LC_T_APIType (FlatTuple(..),Frequency(..))
import LC_T_DSLType (GPU,Tuple(..),TupleIdx(..))
import qualified LC_T_APIType as T
import qualified LC_T_DSLType as T hiding (Shadow)
import qualified LC_T_PrimFun as T
import qualified LC_T_HOAS as H
import LC_U_DeBruijn2
import LC_U_APIType
import LC_G_APIType
import LC_C_PrimFun
import LC_G_Type as G

import Data.Reify
import Data.Typeable
import Data.Dynamic
import Control.Applicative hiding (Const)

deriving instance Typeable H.VertexOut
deriving instance Typeable H.GeometryOut
deriving instance Typeable H.FragmentOut
deriving instance Typeable H.Exp
deriving instance Typeable H.GPOutput

instance GPU b => NewVar (H.Exp a b) where
  mkVar = H.Var

class NewVar a where
  mkVar :: Dynamic -> a

instance Eq (H.Exp a b) where
    _ == _ = False

instance (MuRef a,Typeable a, NewVar a, Typeable b, MuRef b, DeRef a ~ DeRef (a -> b),DeRef b ~ DeRef (a -> b)) => MuRef (a -> b) where
  type DeRef (a -> b) = Exp

  mapDeRef f fn = let v = mkVar $ toDyn fn 
                  in Lam <$> f v <*> f (fn v)

instance MuRef (H.Exp a b) where
  type DeRef (H.Exp a b) = Exp

  mapDeRef f (H.Var t)        = pure Var
  mapDeRef f (H.Const t)      = pure (Const $ T.toValue t)
  mapDeRef f (H.PrimVar i)    = pure (PrimVar $ fst $ T.toInput i)
  mapDeRef f (H.Uni i)        = pure (Uni $ fst $ T.toInput i)
  mapDeRef f (H.Cond c t e)   = Cond <$> f c <*> f t <*> f e
  mapDeRef f (H.PrimApp p a)  = PrimApp (convertPrimFun p) <$> f a
  mapDeRef f (H.Tup t)        = Tup <$> f t
  mapDeRef f (H.Prj idx (e :: H.Exp stage e') :: H.Exp stage' t') = Prj (genTupLen (prjToInt idx) (undefined :: e')) <$> f e
  mapDeRef f (H.Sampler a b c)  = Sampler a b <$> f c
  mapDeRef f (H.Loop a b c d)   = Loop <$> f a <*> f b <*> f c <*> f d
  mapDeRef f (H.Fetch a b c)    = pure $ Fetch a (convertFetchPrimitive b) (T.toInputList c)
  mapDeRef f (H.Transform a b)  = Transform <$> f a <*> f b
  mapDeRef f (H.Reassemble a b) = Reassemble <$> f a <*> f b
  mapDeRef f (H.Rasterize a b)  = Rasterize (convertRasterContext a) <$> f b
  mapDeRef f (H.FrameBuffer a)  = pure $ FrameBuffer $ convertFrameBuffer a
  mapDeRef f (H.Accumulate a b c d e) = Accumulate (convertAccumulationContext a) <$> f b <*> f c <*> f d <*> f e
  mapDeRef f (H.PrjFrameBuffer a b c) = PrjFrameBuffer a (prjToInt b) <$> f c
  mapDeRef f (H.PrjImage a b c) = PrjImage a (toInt b) <$> f c

instance (Typeable a, MuRef a,DeRef [a] ~ DeRef a) => MuRef [a] where
  type DeRef [a] = Exp

  mapDeRef f (x:xs) = liftA2 Cons (f x) (f xs)
  mapDeRef f []     = pure Nil

instance MuRef (T.Texture (H.Exp T.Obj) dim arr t ar) where
  type DeRef (T.Texture (H.Exp T.Obj) dim arr t ar) = Exp

  mapDeRef f (T.TextureSlot n t) = pure $ TextureSlot n $ convertTextureType t
  mapDeRef f (T.Texture t s m d) = Texture (convertTextureType t) (T.toValue s) (convertMipMap m) <$> f d

instance MuRef (Tuple (H.Exp stage) t) where
  type DeRef (Tuple (H.Exp stage) t) = Exp

  mapDeRef f NilTup             = pure Nil
  mapDeRef f (es `SnocTup` e)   = liftA2 Cons (f es) (f e) -- inverted list

instance MuRef (T.Interpolated (H.Exp f) a) where
  type DeRef (T.Interpolated (H.Exp f) a) = Exp

  mapDeRef f (T.Flat e)          = Flat <$> f e
  mapDeRef f (T.Smooth e)        = Smooth <$> f e
  mapDeRef f (T.NoPerspective e) = NoPerspective <$> f e

instance MuRef (H.FlatExp f a) where
  type DeRef (H.FlatExp f a) = Exp

  mapDeRef f (e:.xs) = liftA2 Cons (f e) (f xs)
  mapDeRef f ZT = pure Nil

instance MuRef (H.InterpolatedFlatExp f a) where
  type DeRef (H.InterpolatedFlatExp f a) = Exp

  mapDeRef f (e:.xs) = liftA2 Cons (f e) (f xs)
  mapDeRef f ZT = pure Nil

instance GPU a => MuRef (H.FragmentFilter a) where
  type DeRef (H.FragmentFilter a) = Exp

  mapDeRef f H.PassAll    = pure PassAll
  mapDeRef f (H.Filter a) = Filter <$> f a

instance MuRef (H.FragmentOut a) where
  type DeRef (H.FragmentOut a) = Exp

  mapDeRef f (H.FragmentOut a) = FragmentOut <$> f a
  mapDeRef f (H.FragmentOutDepth a b) = FragmentOutDepth <$> f a <*> f b
  mapDeRef f (H.FragmentOutRastDepth a) = FragmentOutRastDepth <$> f a

instance MuRef (H.VertexOut a b) where
  type DeRef (H.VertexOut a b) = Exp

  mapDeRef f (H.VertexOut a b c d) = VertexOut <$> f a <*> f b <*> f c <*> f d

instance MuRef (H.GeometryOut a b c) where
  type DeRef (H.GeometryOut a b c) = Exp

  mapDeRef f (H.GeometryOut a b c d e) = GeometryOut <$> f a <*> f b <*> f c <*> f d <*> f e

instance MuRef (H.GeometryShader a b c d e f g) where
  type DeRef (H.GeometryShader a b c d e f g) = Exp

  mapDeRef f (H.GeometryShader a b c d e g) = GeometryShader (toInt a) (convertOutputPrimitive b) c <$> f d <*> f e <*> f g

instance MuRef (H.GPOutput a) where
  type DeRef (H.GPOutput a) = Exp

  mapDeRef f (H.ImageOut a b c) = ImageOut a b <$> f c
  mapDeRef f (H.ScreenOut a) = ScreenOut <$> f a
  mapDeRef f (H.MultiOut a) = MultiOut <$> f a

main = do
        let g1 :: H.Exp V Float
            g1 = H.Const 13
        reifyGraph g1 >>= print

toInt :: KnownNat n => T.NatNum n -> Int
toInt (a :: T.NatNum n) = fromInteger $ natVal a

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
