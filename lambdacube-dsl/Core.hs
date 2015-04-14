{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Core where

import Control.DeepSeq
import Data.ByteString.Char8 (pack)
import Debug.Trace
import Data.Monoid
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Applicative
import CompositionalLC hiding (Exp(..))
import qualified CompositionalLC as AST
import qualified Type as AST
import Type hiding (ELet, EApp, ELam, EVar, ELit, ETuple, Exp)
import Text.Trifecta (Result(..))

import LambdaCube.Core.DeBruijn hiding (Exp,N)
import LambdaCube.Core.DeBruijn (N())
import LambdaCube.Core.Type hiding (Ty)
import qualified LambdaCube.Core.PrimFun as C
import qualified LambdaCube.Core.Type as C

import Parser (parseLC_)
import ToDeBruijn hiding (compile)
import Text.Show.Pretty

data Kind
  = Star
  deriving (Show,Eq,Ord)

type Type = Ty

data Var
  = VarE EName Type
  | VarT EName -- Kind
  | VarC (Constraint Ty)               -- constraint var
  deriving (Show,Eq,Ord)

data Exp
  = ELit     Lit
  | EVar     Var
  | EApp     Exp Exp
  | ELam     Var Exp
  | ELet     Var Exp Exp -- VarE only!
  | ETuple   [Exp]
  | EType    Ty
  | EConstraint (Constraint Ty)  -- TODO
  deriving (Show,Eq,Ord)


reduce :: Subst -> Map EName Exp -> Exp -> Exp
reduce s m e = case e of
    EVar (VarE v t) -> maybe (EVar $ VarE v $ subst s t) r $ Map.lookup v m
    ELet (VarE v _) e f -> reduce s (Map.insert v e m) f
    EApp f x -> case r f of
        (ELam (VarE v _) e) -> reduce s (Map.insert v x m) e
        (ELam (VarT v) e) -> case r x of
            EType x -> reduce (s `composeSubst` Map.singleton v x) m e
        e -> case r x of
            EType x -> e --reduce (s `composeSubst` Map.singleton v x) m e
--            EType t ->
            EConstraint _ -> e
            x -> EApp e x
    ETuple es -> ETuple $ map r es
    ELam v e -> ELam v $ r e
    ELit{} -> e
    EType t -> EType $ subst s t
    EConstraint{} -> e
  where
    r = reduce s m

toCore :: Subst -> AST.Exp (Subst, Typing) -> Exp
toCore sub e = case e of
  AST.ELit _ a      -> ELit a
  AST.ETuple _ a    -> ETuple $ fmap toCore' a
  AST.EVar t n      -> foldl EApp (foldl EApp (EVar $ VarE n $ toType $ snd t) pv) cs
    where
      cs = map EConstraint $ subst sub' $ constraints $ snd t
      pv = map EType $ subst sub' $ map (\n -> TVar C n) $ Map.keys $ fst t
  AST.EApp t f a    -> EApp (toCore' f) (toCore' a)
  AST.ELet _ (PVar _ n) a b  -> ELet (VarE n $ toType' $ getTag a) (pv --> ctr --> toCore' a) (toCore' b)
    where
      ctr = map VarC $ constraints $ snd $ getTag a
      pv = map VarT $ Set.toList $ polyVars $ snd $ getTag a
  AST.ELam t (PVar tn n) a -> ELam (VarE n $ toType' tn) $ toCore' a
  _ -> error $ "toCore: " ++ show e
 where
    toCore' = toCore sub'
    s = fst $ getTag e
    sub' = s `composeSubst` sub
    toType' (_, t) = toType $ subst sub' t
    infixr 9 -->
    pv --> x = foldr eLam x pv

    toType :: Typing -> Type
    toType ty = foldr Forall (foldr TConstraintArg (typingType ty) $ constraints ty) $ Set.toList $ polyVars ty

eLam (VarT n) (EApp e (EType (TVar C m))) | n == m = e  -- optimization
eLam (VarC c) (EApp e (EConstraint c')) | c == c' = e  -- optimization
eLam vt x = ELam vt x

compile :: AST.Exp (Subst, Typing) -> Either String LambdaCube.Core.DeBruijn.N
compile x = case comp . reduce mempty mempty . toCore mempty $ x of
    [n] -> Right n
    ns -> Left $ show ns

pattern Va x <- VarE x _
pattern A0 x <- EVar (Va x)
pattern A1 f x <- EApp (A0 f) x
pattern A2 f x y <- EApp (A1 f x) y
pattern A3 f x y z <- EApp (A2 f x y) z
pattern A4 f x y z v <- EApp (A3 f x y z) v
pattern A5 f x y z v w <-  EApp (A4 f x y z v) w

noType = Unknown ""

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
    A2 "PrimMul" (f -> [a]) (f -> [b]) -> [primApp t C.PrimMul  $ tup (C.Tuple [t,v]) [a, b]]
        where t = Unknown "1"; v = Unknown "2" {-!-}
    A2 "PrimMulMatVec" (f -> [a]) (f -> [b]) -> [primApp (Unknown "3"){-!-} C.PrimMulMatVec $ tup (Unknown "4") [a, b]]
    A3 "Fetch" (ELit (LString a)) (compFetchPrimitive -> b) (compInput -> (c,t{-!-})) -> [fetch (pack a) b [(c,V4F)]]
    A1 "FrameBuffer" (compImg -> i) -> [frameBuffer i]
    A1 "Const" v -> [const_ t $ compValue v]
        where t = (Unknown "5")
    A1 "Smooth" (f -> [a]) -> [smooth a]
    A1 "Uni" (compInput -> (n, t)) -> [uni t n]
    ETuple [] -> []
    ELam (VarE _ t) (f -> [x]) -> [lam (toTy t) $ body x]
--    A0 "v" -> [var ty 0 ""] where ty = (Unknown "6")
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
    ETuple [a, b] -> [head $ compImg a, head $ compImg b] {-!!-} 
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


test = test_ $ putStrLn . ppShow
test' = test_ $ \x -> head (comp x) `seq` print "ok"

test_ f = do
  (src, r) <- parseLC_ "gfx03.lc" -- "gfx03.lc" -- "example01.lc"
--  (src, r) <- parseLC_ "tests/accept/instantiate.lc" -- "gfx03.lc" -- "example01.lc"
--  (src, r) <- parseLC_ "tests/accept/id.lc" -- "gfx03.lc" -- "example01.lc"
  putStrLn "====================="
  case r of
    Success e -> do
        (f $ reduce mempty mempty $ toCore mempty $ either (error . ($ src)) id $ inference e) 
    Failure m -> do
      error $ show m

