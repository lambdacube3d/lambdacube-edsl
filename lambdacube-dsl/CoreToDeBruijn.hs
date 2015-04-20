{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module CoreToDeBruijn where

import Data.ByteString.Char8 (pack)
import Text.Show.Pretty

import Core
import LambdaCube.Core.DeBruijn hiding (Exp,N)
import LambdaCube.Core.DeBruijn (N())
import LambdaCube.Core.Type hiding (Ty)
import qualified LambdaCube.Core.PrimFun as C
import qualified LambdaCube.Core.Type as C
import ToDeBruijn hiding (compile)

import Data.Monoid
import qualified Type as AST
import Type hiding (ELet, EApp, ELam, EVar, ELit, ETuple, Exp, Exp_ (..), Pat, PVar, PLit, PTuple)

-- Core to DeBruijn
compile :: Exp -> Either String LambdaCube.Core.DeBruijn.N
compile x = case comp . reduce mempty mempty $ x of
    [n] -> Right n
    ns -> Left $ show ns

noType = Unknown ""

tyOf :: Exp -> Ty
tyOf (EVar (VarE _ t)) = t
tyOf (EApp (tyOf -> TArr _ t) _) = t
tyOf e = error $ "tyOf " ++ ppShow e

tyOf' :: Exp -> C.Ty
tyOf' = toTy . tyOf

comp :: Exp -> [N]
comp x = case x of
    A1 "ScreenOut" (f -> [n]) -> [screenOut $ prjFrameBuffer mempty 0 $ n]
    A5 "Accumulate" (f -> [a]) (f -> [b]) (f -> [c]) (f -> [d]) (f -> [e]) -> [accumulate a b c d e]
    A1 "AccumulationContext" (ETuple [a,b]) -> [accumulationContext Nothing [compFrag a, compFrag b]]
    A0 "PassAll" -> [passAll]
    A1 "FragmentOutRastDepth" x -> [fragmentOutRastDepth (f x)] -- or const_
    A2 "Rasterize" x (f -> [y]) -> [rasterize (compRC x) y]
    A2 "Transform" (f -> [a]) (f -> [b]) -> [transform a b]
    A4 "VertexOut" (f -> [a]) b (f -> c) (f -> d) -> [vertexOut a (const_ (Single C.Float) (VFloat $ realToFrac $ compLF b)) c d]
    A2 "PrimMul" av@(f -> [a]) bv@(f -> [b]) -> [primApp t C.PrimMul  $ tup (C.Tuple [t,v]) [a, b]]
        where t = tyOf' av; v = tyOf' bv
    A2 "PrimMulMatVec" (f -> [a]) bv@(f -> [b]) -> [primApp (tyOf' bv) C.PrimMulMatVec $ tup noType [a, b]]
    A3 "Fetch" (ELit (LString a)) (compFetchPrimitive -> b) (compInput -> (c,t{-!-})) -> [fetch (pack a) b [(c,V4F)]]
    A1 "FrameBuffer" (compImg -> i) -> [frameBuffer i]
    A1 "Const" v -> [const_ t $ compValue v]
        where t = tyOf' v
    A1 "Smooth" (f -> [a]) -> [smooth a]
    A1 "Uni" (compInput -> (n, t)) -> [uni t n]
    ETuple [] -> []
    ELam (VarE _ t) (f -> [x]) -> [lam (toTy t) $ body x]
    A0t "v" t -> [var ty 0 ""] where ty = toTy t
    x -> error $ "comp " ++ ppShow x
  where
    f :: Exp -> [N]
    f = comp

compValue x = case x of
    A4 "V4F" (ELit (LFloat x)) (ELit (LFloat y)) (ELit (LFloat z)) (ELit (LFloat w)) -> VV4F (V4 (realToFrac x) (realToFrac y) (realToFrac z) (realToFrac w))
    A4 "V4B" _ _ _ _ -> VV4B (V4 True True True True)
    x -> error $ "compValue " ++ show x

compRC x = case x of
    A4 "TriangleCtx" a b c d -> TriangleCtx (compCM a) (compPM b) (compPO c) (compPV d)
    x -> error $ "compRC " ++ show x

compLF x = case x of
    ELit (LFloat x) -> x
    x -> error $ "compLF " ++ show x

compFetchPrimitive x = case x of
    A0 "Triangles" -> Triangles
    x -> error $ "compFetchPr " ++ show x

compInput x = case x of
    A1 "IV4F" (ELit (LString s)) -> (pack s, Single C.V3F)
    A1 "IM44F" (ELit (LString s)) -> (pack s, Single C.M44F)
    x -> error $ "compInput " ++ show x

compImg x = case x of
    ETuple a -> concatMap compImg a
    A2 "DepthImage" (ELit (LNat i)) (ELit (LFloat a)) -> [DepthImage i (realToFrac a)]
    A2 "ColorImage" (ELit (LNat i)) _ -> [ColorImage i (VV4F (V4 0.5 0.0 0.4 1.0))]
    x -> error $ "compImg " ++ ppShow x

compCM x = case x of
    A0 "CullNone" -> CullNone
    x -> error $ "compCM " ++ show x

compPM x = case x of
    A0 "PolygonFill" -> PolygonFill
    x -> error $ "compPM " ++ show x

compPO x = case x of
    A0 "NoOffset" -> NoOffset
    x -> error $ "compPO " ++ show x

compPV x = case x of
    A0 "FirstVertex" -> FirstVertex
    x -> error $ "compPV " ++ show x

compBlending x = case x of
    A3 "Blend" a b c--(comp1 -> a) (comp2 -> b) (comp3 -> c)
{-
        Tuple' _ [[BlendEquation a],[BlendEquation b]]:
        Tuple' _ [[Tuple' _ [[BlendingFactor c],[BlendingFactor d]]],[Tuple' _ [[BlendingFactor e],[BlendingFactor f]]]]
        Val (Single C.V4F) (VV4F g):xs) =
-}
        -> NoBlending -- a b c --(a,b) ((c,d),(e,f)) g
    x -> error $ "compBlending " ++ show x

compComparisonFunction x = case x of
    A0 "Less" -> Less
    x -> error $ "compComparisonFunction " ++ show x

compBool x = case x of
    A0 "False" -> False
    x -> error $ "compBool " ++ show x

compFrag x = case x of
    A2 "DepthOp" (compComparisonFunction -> a) (compBool -> b) -> DepthOp a b
    A2 "ColorOp" (compBlending -> b) (compValue -> v) -> ColorOp b v
    x -> error $ "compFrag " ++ ppShow x
