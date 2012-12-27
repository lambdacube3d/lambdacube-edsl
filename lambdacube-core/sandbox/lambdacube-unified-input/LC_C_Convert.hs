module LC_C_Convert (convertExp) where

import Control.Applicative hiding (empty,Const)
import Control.Monad.State
import Data.ByteString.Char8 (ByteString)
import Debug.Trace
import TypeLevel.Number.Nat
import qualified Data.IntMap as IM

import BiMap

import LC_T_APIType (Tuple(..),LCType)
import qualified LC_T_HOAS as H
import LC_U_DeBruijn
import LC_C_Data

-- Hint: This module does closure conversion (HOAS -> DeBruijn) and hash consing (CSE)

-- Hash consing

-- Fun
lam                     !t !a               = mkN t (Lam    <$> unN a)
body                    !t !a               = mkN t (Body   <$> unN a)
let_                    !t !a !b            = mkN t (Let    <$> unN a <*> unN b)
var                     !t !a !b            = mkN t $ return $ Var a b
apply                   !t !a !b            = mkN t (Apply  <$> unN a <*> unN b)

-- Exp
const_                  !t !a               = mkN t $ return $ Const a
input                   !t !a               = mkN t $ return $ Input a

frameBuffer             !t !a               = mkN t (return $ FrameBuffer a)
accumulate              !t !a !b !c !d !e   = mkN t (Accumulate a   <$> unN b <*> unN c <*> unN d <*> unN e)

newtype N = N {unN :: State DAG ExpId}

--mkN :: Ty -> StateT DAG Data.Functor.Identity.Identity Exp -> N
mkN t m = N (hashcons t =<< m)

hashcons :: Ty -> Exp -> State DAG ExpId
hashcons !t !e = do
  DAG !m !tm <- get
  case lookup_key e m of
    Nothing -> let (!k,!m') = insert e m
                   !tm'    = IM.insert k t tm
               in put (DAG m' tm') >> return k
    Just !k  -> {-trace ("sharing : " ++ show k ++ " :: " ++ show (tm IM.! k)) $ -} return k

-- Closure conversion

type Layout = [Ty]

genTy :: LCType t => H.Exp freq t -> Ty
genTy a = Ty

convertInterpolatedExpTuple :: forall freq t.
                               Layout       -- scalar environment
                            -> H.InterpolatedExpTuple freq t               -- expression to be converted
                            -> [N]
convertInterpolatedExpTuple lyt expr = cvtExpr
  where
    unE :: LCType a => forall freq. H.Exp freq a -> State DAG ExpId
    unE = unN . convertExp lyt

    cvtExpr = case expr of
        ZT      -> []
        e :. xs -> cvt' e : convertInterpolatedExpTuple lyt xs

    cvt' :: LCType t' => H.Interpolated (H.Exp freq) t' -> N
    cvt' v = mkN ty $ case v of
        H.Flat e            -> Flat <$> unE e
        H.Smooth e          -> Smooth <$> unE e
        H.NoPerspective e   -> NoPerspective <$> unE e
      where
        ty = undefined--genTy v

convertExpTuple :: forall freq t.
                   Layout       -- scalar environment
                -> H.ExpTuple freq t               -- expression to be converted
                -> [N]
convertExpTuple lyt expr = case expr of
    ZT      -> []
    e :. xs -> convertExp lyt e : convertExpTuple lyt xs


convertExp :: LCType t => forall freq.
                  Layout
               -> H.Exp freq t               -- expression to be converted
               -> N
convertExp lyt expr = mkN (genTy expr) cvtExpr
  where
    unE :: LCType t => forall freq. H.Exp freq t -> State DAG ExpId
    unE     = unN . convertExp lyt
    unF1 :: (LCType a, LCType b) => forall freq. (H.Exp freq a -> H.Exp freq b) -> State DAG ExpId
    unF1    = unN . convertFun1 lyt
    unF2 :: (LCType a, LCType b, LCType c) => forall freq. (H.Exp freq a -> H.Exp freq b -> H.Exp freq c) -> State DAG ExpId
    unF2    = unN . convertFun2 lyt
    unET :: forall freq t. H.ExpTuple freq t -> State DAG [ExpId]
    unET a  = mapM unN (convertExpTuple lyt a)
    unIET :: forall freq t. H.InterpolatedExpTuple freq t -> State DAG [ExpId]
    unIET a = mapM unN (convertInterpolatedExpTuple lyt a)
    cvtExpr = case expr of
{-
        H.Tag i li                      -> var              ty (prjIdx i lyt) li
-}
        -- Exp
{-
        H.Const v                       -> const_           ty (T.toValue v)
        H.Input v                       -> input            ty (fst $ T.toInput v)
-}
        H.Use a                         -> Use      <$> unE a
        H.Cond a b c                    -> Cond     <$> unE a <*> unE b <*> unE c
        H.PrimApp a b                   -> PrimApp  (convertPrimFun a) <$> unE b
        H.Tup a                         -> Tup      <$> unET a
        H.Prj a b                       -> Prj      (toInt a) <$> unE b
        H.Loop a b c d                  -> Loop     <$> unF1 a <*> unF1 b <*> unF1 c <*> unE d

        -- Array operations
        H.ArrayFromList a               -> ArrayFromList      <$> mapM unE a
        H.ArrayReplicate a b            -> ArrayReplicate     <$> unE a <*> unE b
        H.ArrayGenerate a b             -> ArrayGenerate      <$> unE a <*> unF1 b
        H.ArrayIterateN a b c           -> ArrayIterateN      <$> unE a <*> unF1 b <*> unE c
        H.ArrayIndex a b                -> ArrayIndex         <$> unE a <*> unE b
        H.ArrayFilter a b               -> ArrayFilter        <$> unF1 a <*> unE b
        H.ArrayMap a b                  -> ArrayMap           <$> unF1 a <*> unE b
        H.ArrayZipWith a b c            -> ArrayZipWith       <$> unF2 a <*> unE b <*> unE c
        H.ArrayAccumulate a b c         -> ArrayAccumulate    <$> unF2 a <*> unE b <*> unE c

        -- GPU pipeline model
        H.Fetch a b c                   -> Fetch        (convertFetchPrimitive a) <$> unE b <*> maybe (return Nothing) (\v -> return . Just =<< unE v) c
        H.Transform a b                 -> Transform    <$> unF1 a <*> unE b
        H.Reassemble a b                -> Reassemble   <$> unE a <*> unE b
        H.Rasterize a b                 -> Rasterize    (convertRasterContext a) <$> unE b
{-
        H.FrameBuffer a                 -> frameBuffer      ty (convertFrameBuffer a)

        H.Accumulate a b c d e          -> accumulate       ty (convertAccumulationContext a) (cvtE b)
                                                                                     (cvtF1 c)
                                                                                     (cvtE d)
                                                                                     (cvtE e)

-}
        -- Transform feedback support
        H.ArrayFromStream a             -> ArrayFromStream  <$> unE a

        -- FrameBuffer and Image helpers
        H.PrjFrameBuffer a b            -> PrjFrameBuffer   (toInt a) <$> unE b
        H.PrjImage a b                  -> PrjImage         (toInt a) <$> unE b

        -- Special tuple expressions
        H.VertexOut a b c               -> VertexOut              <$> unE a <*> unE b <*> unIET c
        H.GeometryOut a b c d e f       -> GeometryOut            <$> unE a <*> unE b <*> unE c <*> unE d <*> unE e <*> unIET f
        H.FragmentOut a                 -> FragmentOut            <$> unET a
        H.FragmentOutDepth a b          -> FragmentOutDepth       <$> unE a <*> unET b
        H.FragmentOutRastDepth a        -> FragmentOutRastDepth   <$> unET a

        -- GeometryShader constructors
        H.GeometryShader a b c d e f    -> GeometryShader (toInt a) (convertOutputPrimitive b) c <$> unF1 d <*> unF1 e <*> unF1 f

        -- FragmentFilter constructors
        H.PassAll                       -> pure PassAll
        H.Filter a                      -> Filter     <$> unF1 a

        -- Output constructors
        H.ImageOut a b                  -> ImageOut a <$> unE b
        H.ScreenOut a                   -> ScreenOut  <$> unE a

-- TODO
convertFun1 :: (LCType a, LCType b)
            => [Ty] -> (H.Exp freq a -> H.Exp freq b) -> N
convertFun1 lyt f = lam ty1 $ body ty2 $ convertExp lyt' (f a)
  where
    a       = case f of
              (fv :: H.Exp freq a -> H.Exp freq b) -> H.Tag (length lyt) undefined--(typeOf (undefined :: a))
    lyt'    = undefined:lyt -- TODO
    ty1     = undefined
    ty2     = undefined

convertFun2 :: (LCType a, LCType b, LCType c)
            => [Ty] -> (H.Exp freq a -> H.Exp freq b -> H.Exp freq c) -> N
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
