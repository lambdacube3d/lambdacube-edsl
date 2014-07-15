module LC_C_Convert2 {-(convertGPOutput)-} (convert) where

import GHC.TypeLits

import Debug.Trace

import LC_T_APIType (FlatTuple(..),Frequency(..))
import LC_T_DSLType (GPU,Tuple(..),TupleIdx(..))
import qualified LC_T_APIType as T
import qualified LC_T_DSLType as T hiding (Shadow)
import qualified LC_T_PrimFun as T
import qualified LC_T_HOAS as H
import qualified LC_U_DeBruijn as U
import LC_U_DeBruijn2
import LC_U_APIType
import LC_G_APIType
import LC_C_PrimFun
import LC_G_Type as G

import Data.Reify
import Data.Typeable
import Data.Dynamic
import Control.Applicative hiding (Const)
import Data.Map (Map,(!))
import qualified Data.Map as Map
import qualified Data.IntMap as IM
import Control.Monad.State
import Data.List (elemIndex)
import BiMap
import qualified Data.Vector as V

data ConvertState
    = ConvertState
    { lambdas   :: [Unique]
    , expMap    :: Map Unique (TypedExp Unique, Maybe Unique)
    , cvtMap    :: Map Unique (Ty,U.Exp)
    }

type C = State ConvertState

cvt :: Unique -> C Unique
cvt idx = do
    s <- get
    case expMap s ! idx of
        (_,Just eId)    -> return eId
        (e,Nothing)     -> convertExp idx e

cvtList :: Unique -> C [Unique]
cvtList idx = do
    s <- get
    case expMap s ! idx of
        ((E ty e),_)  -> case e of
            Nil         -> return []
            Cons x xs   -> (:) <$> cvt x <*> cvtList xs
            _           -> error "internal error (cvtList)"

cvtTuple :: Unique -> C [Unique]
cvtTuple idx = do
    s <- get
    case expMap s ! idx of
        ((E ty e),_)  -> case e of
            Nil         -> return []
            Cons x xs   -> (:) <$> cvt x <*> cvtTuple xs
            _           -> error "internal error (cvtTuple)"

emit :: Unique -> Ty -> U.Exp -> C Unique
emit idx ty e = do
    ConvertState{..} <- get
    let nextIdx = Map.size cvtMap
        cvtMap' = Map.insert nextIdx (ty,e) cvtMap
        expMap' = Map.adjust (\(e,Nothing) -> (e,Just nextIdx)) idx expMap
    put $ ConvertState lambdas expMap' cvtMap'
    return nextIdx

convertExp :: Unique -> TypedExp Unique -> C Unique
convertExp eIdx (E ty exp) = case exp of
    Lam a b -> do
        let add e = do
                cm <- gets cvtMap
                let idx = Map.size cm
                modify (\s -> s {cvtMap = Map.insert idx (Unknown "Body",e) cm})
                return idx
        ls <- gets lambdas
        modify (\s -> s {lambdas = a : ls})
        v <- cvt a
        f <- cvt b
        modify (\s -> s {lambdas = ls})
        emit eIdx ty =<< U.Lam <$> add (U.Body f)

    Var                     -> do
        ls <- gets lambdas
        let Just i = elemIndex eIdx ls
        emit eIdx ty $ U.Var i ""

    -- Exp
    Const a                 -> emit eIdx ty $ U.Const a
    PrimVar a               -> emit eIdx ty $ U.PrimVar a
    Uni a                   -> emit eIdx ty $ U.Uni a

    Tup a                   -> emit eIdx ty =<< (\t -> U.Tup (reverse t)) <$> cvtTuple a
    Prj a b                 -> emit eIdx ty =<< U.Prj a <$> cvt b
    Cond a b c              -> emit eIdx ty =<< U.Cond <$> cvt a <*> cvt b <*> cvt c
    PrimApp a b             -> emit eIdx ty =<< U.PrimApp a <$> cvt b
    Sampler a b c           -> emit eIdx ty =<< U.Sampler a b <$> cvt c
    Loop a b c d            -> emit eIdx ty =<< U.Loop <$> cvt a <*> cvt b <*> cvt c <*> cvt d

    -- special tuple expressions
    VertexOut a b c d       -> emit eIdx ty =<< U.VertexOut <$> cvt a <*> cvt b <*> cvtList c <*> cvtList d
    GeometryOut a b c d e   -> emit eIdx ty =<< U.GeometryOut <$> cvt a <*> cvt b <*> cvt c <*> cvtList d <*> cvtList e
    FragmentOut a           -> emit eIdx ty =<< U.FragmentOut <$> cvtList a
    FragmentOutDepth a b    -> emit eIdx ty =<< U.FragmentOutDepth <$> cvt a <*> cvtList b
    FragmentOutRastDepth a  -> emit eIdx ty =<< U.FragmentOutRastDepth <$> cvtList a

    -- GP
    Fetch a b c             -> emit eIdx ty $ U.Fetch a b c
    Transform a b           -> emit eIdx ty =<< U.Transform <$> cvt a <*> cvt b
    Reassemble a b          -> emit eIdx ty =<< U.Reassemble <$> cvt a <*> cvt b
    Rasterize a b           -> emit eIdx ty =<< U.Rasterize a <$> cvt b
    FrameBuffer a           -> emit eIdx ty $ U.FrameBuffer a
    Accumulate a b c d e    -> emit eIdx ty =<< U.Accumulate a <$> cvt b <*> cvt c <*> cvt d <*> cvt e
    PrjFrameBuffer a b c    -> emit eIdx ty =<< U.PrjFrameBuffer a b <$> cvt c
    PrjImage a b c          -> emit eIdx ty =<< U.PrjImage a b <$> cvt c

    -- Texture
    TextureSlot a b         -> emit eIdx ty $ U.TextureSlot a b
    Texture a b c d         -> emit eIdx ty =<< U.Texture a b c <$> cvtList d

    -- Interpolated
    Flat a                  -> emit eIdx ty =<< U.Flat <$> cvt a
    Smooth a                -> emit eIdx ty =<< U.Smooth <$> cvt a
    NoPerspective a         -> emit eIdx ty =<< U.NoPerspective <$> cvt a

    GeometryShader a b c d e f  -> emit eIdx ty =<< U.GeometryShader a b c <$> cvt d <*> cvt e <*> cvt f

    -- FragmentFilter
    PassAll                 -> emit eIdx ty $ U.PassAll
    Filter a                -> emit eIdx ty =<< U.Filter <$> cvt a

    -- GPOutput
    ImageOut a b c          -> emit eIdx ty =<< U.ImageOut a b <$> cvt c
    ScreenOut a             -> emit eIdx ty =<< U.ScreenOut <$> cvt a
    MultiOut a              -> emit eIdx ty =<< U.MultiOut <$> cvtList a
    _ -> error $ "internal error (convertExp) " ++ show exp

convert :: H.GPOutput a -> IO (U.Exp,U.DAG)
convert e = do
    g@(Graph l r) <- reifyGraph e
    let s = ConvertState [] (Map.fromList [(i,(e,Nothing)) | (i,e) <- l]) Map.empty
        (r',s') = runState (cvt r) s
        tel = Map.toList $ cvtMap s'
        (tl,el) = unzip [((i,t),(i,e)) | (i,(t,e)) <- tel]
        emap  = IM.fromList el
        bimap = BiMap (Map.fromList [(e,i) | (i,e) <- el]) emap
        tmap  = IM.fromList tl
        cmap  = IM.fromList [(i,2) | (i,_) <- tl] -- HINT: hack
    print r >> mapM print l
    print "----"
    print r' >> mapM print tel
    return (emap IM.! r',U.DAG bimap tmap cmap V.empty V.empty)

saveCore :: String -> H.GPOutput a -> IO ()
saveCore name e = convert e >>= writeFile (name ++ ".lcore") . show

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

data TypedExp a = E Ty (Exp a) deriving (Eq,Ord,Show,Typeable)

addType (e :: H.Exp a t) f = E (genTy (undefined :: t)) <$> f
noType s f = E (Unknown s) <$> f

instance (GPU a,MuRef (H.Exp f a),Typeable (H.Exp f a), NewVar (H.Exp f a), Typeable b, MuRef b, DeRef (H.Exp f a) ~ DeRef ((H.Exp f a) -> b),DeRef b ~ DeRef ((H.Exp f a) -> b)) => MuRef ((H.Exp f a) -> b) where
  type DeRef ((H.Exp f a) -> b) = TypedExp

  mapDeRef f (fn :: (H.Exp f a) -> b) = let v = mkVar $ toDyn fn 
                  in addType (undefined :: H.Exp f a) $ Lam <$> f v <*> f (fn v)


instance MuRef (H.Exp a b) where
  type DeRef (H.Exp a b) = TypedExp

  mapDeRef f x@(H.Var t)        = addType x $ pure Var
  mapDeRef f x@(H.Const t)      = addType x $ pure (Const $ T.toValue t)
  mapDeRef f x@(H.PrimVar i)    = addType x $ pure (PrimVar $ fst $ T.toInput i)
  mapDeRef f x@(H.Uni i)        = addType x $ pure (Uni $ fst $ T.toInput i)
  mapDeRef f x@(H.Cond c t e)   = addType x $ Cond <$> f c <*> f t <*> f e
  mapDeRef f x@(H.PrimApp p a)  = addType x $ PrimApp (convertPrimFun p) <$> f a
  mapDeRef f x@(H.Tup t)        = addType x $ Tup <$> f t
  mapDeRef f x@(H.Prj idx (e :: H.Exp stage e') :: H.Exp stage' t') = addType x $ Prj (genTupLen (prjToInt idx) (undefined :: e')) <$> f e
  mapDeRef f x@(H.Sampler a b c)   = addType x $ Sampler a b <$> f c
  mapDeRef f x@(H.TextureSlot n t) = noType "TextureSlot" $ pure $ TextureSlot n $ convertTextureType t
  mapDeRef f x@(H.Texture t s m d) = noType "Texture" $ Texture (convertTextureType t) (T.toValue s) (convertMipMap m) <$> f d
  mapDeRef f x@(H.Loop a b c d)   = addType x $ Loop <$> f a <*> f b <*> f c <*> f d
  mapDeRef f x@(H.Fetch a b c)    = noType "Fetch" $ pure $ Fetch a (convertFetchPrimitive b) (T.toInputList c)
  mapDeRef f x@(H.Transform a b)  = noType "Transform" $ Transform <$> f a <*> f b
  mapDeRef f x@(H.Reassemble a b) = noType "Reassemble" $ Reassemble <$> f a <*> f b
  mapDeRef f x@(H.Rasterize a b)  = noType "Rasterize" $ Rasterize (convertRasterContext a) <$> f b
  mapDeRef f x@(H.FrameBuffer a)  = noType "FrameBuffer" $ pure $ FrameBuffer $ convertFrameBuffer a
  mapDeRef f x@(H.Accumulate a b c d e) = noType "Accumulate" $ Accumulate (convertAccumulationContext a) <$> f b <*> f c <*> f d <*> f e
  mapDeRef f x@(H.PrjFrameBuffer a b c) = noType "PrjFrameBuffer" $ PrjFrameBuffer a (prjToInt b) <$> f c
  mapDeRef f x@(H.PrjImage a b c) = noType "PrjImage" $ PrjImage a (toInt b) <$> f c

instance (Typeable a, MuRef a,DeRef [a] ~ DeRef a) => MuRef [a] where
  type DeRef [a] = TypedExp

  mapDeRef f (x:xs) = noType "ConsL" $ liftA2 Cons (f x) (f xs)
  mapDeRef f []     = noType "NilL" $ pure Nil

instance MuRef (Tuple (H.Exp stage) t) where
  type DeRef (Tuple (H.Exp stage) t) = TypedExp

  mapDeRef f NilTup             = noType "NilT" $ pure Nil
  mapDeRef f (es `SnocTup` e)   = noType "ConsT" $ liftA2 Cons (f e) (f es) -- inverted list

instance MuRef (T.Interpolated (H.Exp f) a) where
  type DeRef (T.Interpolated (H.Exp f) a) = TypedExp

  mapDeRef f (T.Flat e)          = noType "Flat" $ Flat <$> f e
  mapDeRef f (T.Smooth e)        = noType "Smooth" $ Smooth <$> f e
  mapDeRef f (T.NoPerspective e) = noType "NoPerspective" $ NoPerspective <$> f e

instance MuRef (H.FlatExp f a) where
  type DeRef (H.FlatExp f a) = TypedExp

  mapDeRef f (e:.xs) = noType "ConsFE" $ liftA2 Cons (f e) (f xs)
  mapDeRef f ZT = noType "NilFE" $ pure Nil

instance MuRef (H.InterpolatedFlatExp f a) where
  type DeRef (H.InterpolatedFlatExp f a) = TypedExp

  mapDeRef f (e:.xs) = noType "ConsIFE" $ liftA2 Cons (f e) (f xs)
  mapDeRef f ZT = noType "NilIFE" $ pure Nil

instance GPU a => MuRef (H.FragmentFilter a) where
  type DeRef (H.FragmentFilter a) = TypedExp

  mapDeRef f H.PassAll    = noType "PassAll" $ pure PassAll
  mapDeRef f (H.Filter a) = noType "Filter" $ Filter <$> f a

instance MuRef (H.FragmentOut a) where
  type DeRef (H.FragmentOut a) = TypedExp

  mapDeRef f (H.FragmentOut a) = noType "FragmentOut" $ FragmentOut <$> f a
  mapDeRef f (H.FragmentOutDepth a b) = noType "FragmentOutDepth" $ FragmentOutDepth <$> f a <*> f b
  mapDeRef f (H.FragmentOutRastDepth a) = noType "FragmentOutRastDepth" $ FragmentOutRastDepth <$> f a

instance MuRef (H.VertexOut a b) where
  type DeRef (H.VertexOut a b) = TypedExp

  mapDeRef f (H.VertexOut a b c d) = noType "VertexOut" $ VertexOut <$> f a <*> f b <*> f c <*> f d

instance MuRef (H.GeometryOut a b c) where
  type DeRef (H.GeometryOut a b c) = TypedExp

  mapDeRef f (H.GeometryOut a b c d e) = noType "GeometryOut" $ GeometryOut <$> f a <*> f b <*> f c <*> f d <*> f e

instance MuRef (H.GeometryShader a b c d e f g) where
  type DeRef (H.GeometryShader a b c d e f g) = TypedExp

  mapDeRef f (H.GeometryShader a b c d e g) = noType "GeometryShader" $ GeometryShader (toInt a) (convertOutputPrimitive b) c <$> f d <*> f e <*> f g

instance MuRef (H.GPOutput a) where
  type DeRef (H.GPOutput a) = TypedExp

  mapDeRef f (H.ImageOut a b c) = noType "ImageOut" $ ImageOut a b <$> f c
  mapDeRef f (H.ScreenOut a) = noType "ScreenOut" $ ScreenOut <$> f a
  mapDeRef f (H.MultiOut a) = noType "MultiOut" $ MultiOut <$> f a

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
