module LC_C_Convert (convertExp) where

import qualified Data.Map as Map
import Control.Monad.State

import Data.ByteString.Char8 (ByteString)
import Debug.Trace
import TypeLevel.Number.Nat

import Data.Generics.Fixplate

import LC_T_APIType (Tuple(..),LCType)
import qualified LC_T_HOAS as H
import LC_U_APIType
import LC_U_DeBruijn
import LC_C_Data
import LC_T_Instances

import Data.Foldable
import Data.Traversable as T
import Control.Applicative

-- Hint: This module does closure conversion (HOAS -> DeBruijn) and hash consing (CSE)

-- Hash consing

-- Fun
lam                     !t                  = Fix . Ann t . Lam
body                    !t                  = Fix . Ann t . Body
{-
let_                    !t !a !b            = mkN t (Let    <$> unN a <*> unN b)
var                     !t !a !b            = mkN t $ return $ Var a b
apply                   !t !a !b            = mkN t (Apply  <$> unN a <*> unN b)

-- Exp
const_                  !t !a               = mkN t $ return $ Const a
input                   !t !a               = mkN t $ return $ Input a

frameBuffer             !t !a               = mkN t (return $ FrameBuffer a)
accumulate              !t !a !b !c !d !e   = mkN t (Accumulate a   <$> unN b <*> unN c <*> unN d <*> unN e)
-}

newtype DAG = DAG (Map.Map (Exp Int) Int) deriving Show

hashcons :: Exp Int -> State DAG Int
hashcons !e = do
    DAG !m <- get
    case Map.lookup e m of
        Nothing -> let k    = Map.size m
                       !m'  = Map.insert e k m
                   in put (DAG m') >> return k
        Just !k  -> {-trace ("sharing : " ++ show k ++ " :: " ++ show (tm IM.! k)) $ -} return k

cse :: Mu Exp -> State DAG (Attr Exp Int)
cse f = synthetiseM hashcons f

-- Closure conversion

type Layout = [Ty]

genTy :: LCType freq t => H.Exp freq t -> Ty
genTy a = undefined

convertInterpolatedExpTuple :: forall freq t.
                               Layout       -- scalar environment
                            -> H.InterpolatedExpTuple freq t               -- expression to be converted
                            -> [Attr Exp Ty]
convertInterpolatedExpTuple lyt expr = cvtExpr
  where
    unE :: LCType freq a => H.Exp freq a -> Attr Exp Ty
    unE = convertExp lyt

    cvtExpr = case expr of
        ZT      -> []
        e :. xs -> cvt' e : convertInterpolatedExpTuple lyt xs

    cvt' :: LCType freq t' => H.Interpolated (H.Exp freq) t' -> Attr Exp Ty
    cvt' v = Fix $ Ann ty $ case v of
        H.Flat e            -> Flat (unE e)
        H.Smooth e          -> Smooth (unE e)
        H.NoPerspective e   -> NoPerspective (unE e)
      where
        ty = undefined--genTy v

convertExpTuple :: forall freq t.
                   Layout       -- scalar environment
                -> H.ExpTuple freq t               -- expression to be converted
                -> [Attr Exp Ty]
convertExpTuple lyt expr = case expr of
    ZT      -> []
    e :. xs -> convertExp lyt e : convertExpTuple lyt xs


convertExp  :: LCType freq t
            => Layout
            -> H.Exp freq t               -- expression to be converted
            -> Attr Exp Ty
convertExp lyt expr = Fix $ Ann (genTy expr) cvtExpr
  where
    cE      :: LCType freq t => H.Exp freq t -> Attr Exp Ty
    cE      = convertExp lyt
    cF1     :: (LCType freq a, LCType freq b) => (H.Exp freq a -> H.Exp freq b) -> Attr Exp Ty
    cF1     = convertFun1 lyt
    cF2     :: (LCType freq a, LCType freq b, LCType freq c) => (H.Exp freq a -> H.Exp freq b -> H.Exp freq c) -> Attr Exp Ty
    cF2     = convertFun2 lyt
    cET     :: forall freq t. H.ExpTuple freq t -> [Attr Exp Ty]
    cET     = convertExpTuple lyt
    cIET    :: forall freq t. H.InterpolatedExpTuple freq t -> [Attr Exp Ty]
    cIET    = convertInterpolatedExpTuple lyt

    cvtExpr = case expr of
{-
        H.Tag i li                      -> var              ty (prjIdx i lyt) li
-}
        -- Exp
{-
        H.Const v                       -> const_           ty (T.toValue v)
        H.Input v                       -> input            ty (fst $ T.toInput v)
-}
        H.Use a                         -> Use (cE a)
        H.Cond a b c                    -> Cond (cE a) (cE b) (cE c)
        H.PrimApp a b                   -> PrimApp (convertPrimFun a) (cE b)
        H.Tup a                         -> Tup (cET a)
        H.Prj a b                       -> Prj (toInt a) (cE b)
        H.Loop a b c d                  -> Loop (cF1 a) (cF1 b) (cF1 c) (cE d)

        -- Array operations
        H.ArrayFromList a               -> ArrayFromList (map cE a)
        H.ArrayReplicate a b            -> ArrayReplicate (cE a) (cE b)
        H.ArrayGenerate a b             -> ArrayGenerate (cE a) (cF1 b)
        H.ArrayIterateN a b c           -> ArrayIterateN (cE a) (cF1 b) (cE c)
        H.ArrayIndex a b                -> ArrayIndex (cE a) (cE b)
        H.ArrayFilter a b               -> ArrayFilter (cF1 a) (cE b)
        H.ArrayMap a b                  -> ArrayMap (cF1 a) (cE b)
        H.ArrayZipWith a b c            -> ArrayZipWith (cF2 a) (cE b) (cE c)
        H.ArrayAccumulate a b c         -> ArrayAccumulate (cF2 a) (cE b) (cE c)

        -- GPU pipeline model
        H.Fetch a b c                   -> Fetch (convertFetchPrimitive a) (cE b) (fmap cE c)
        H.Transform a b                 -> Transform (cF1 a) (cE b)
        H.Reassemble a b                -> Reassemble (cE a) (cE b)
        H.Rasterize a b c d             -> Rasterize (convertRasterContext a) (cF1 b) (fmap cE c) (cE d)
{-
        H.FrameBuffer a                 -> frameBuffer      ty (convertFrameBuffer a)
        H.Accumulate a b c d e          -> accumulate       ty (convertAccumulationContext a) (fmap cE b) (cE c) (cF1 d) (cE e) (cE f)

-}
        -- Transform feedback support
        H.ArrayFromStream a             -> ArrayFromStream (cE a)

        -- FrameBuffer and Image helpers
        H.PrjFrameBuffer a b            -> PrjFrameBuffer (toInt a) (cE b)
        H.PrjImage a b                  -> PrjImage (toInt a) (cE b)

        -- Special tuple expressions
        H.Vertex a b c d                -> Vertex (cE a) (cE b) (cET c) (cIET d)
        H.Fragment a                    -> Fragment (cET a)
        H.FragmentDepth a b             -> FragmentDepth (cE a) (cET b)
        H.FragmentRastDepth a           -> FragmentRastDepth (cET a)

        -- GeometryShader constructors
        H.GeometryShader a b c d e f    -> GeometryShader (toInt a) (convertOutputPrimitive b) c (cF1 d) (cF1 e) (cF1 f)

        -- Output constructors
        H.Output a b                    -> Output a (cE b)
        H.ScreenOutput a                -> ScreenOutput (cE a)

-- TODO
convertFun1 :: (LCType freq a, LCType freq b)
            => [Ty] -> (H.Exp freq a -> H.Exp freq b) -> Attr Exp Ty
convertFun1 lyt f = lam ty1 $ body ty2 $ convertExp lyt' (f a)
  where
    a       = case f of
              (fv :: H.Exp freq a -> H.Exp freq b) -> H.Tag (length lyt)
    lyt'    = undefined:lyt -- TODO
    ty1     = undefined
    ty2     = undefined

convertFun2 :: (LCType freq a, LCType freq b, LCType freq c)
            => [Ty] -> (H.Exp freq a -> H.Exp freq b -> H.Exp freq c) -> Attr Exp Ty
convertFun2 lyt f = lam ty1 $ lam ty2 $ body ty3 $ convertExp lyt' (f a b)
  where
    a       = case f of
              (fv :: H.Exp freq a -> H.Exp freq b -> H.Exp freq c) -> H.Tag (length lyt+1)
    b       = case f of
              (fv :: H.Exp freq a -> H.Exp freq b -> H.Exp freq c) -> H.Tag (length lyt)
    lyt'    = undefined:undefined:lyt -- TODO
    ty1     = undefined
    ty2     = undefined
    ty3     = undefined
