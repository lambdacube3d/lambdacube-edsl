{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Core where

import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.Traversable
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
import Type hiding (ELet, EApp, ELam, EVar, ELit, ETuple, Exp, Exp_ (..), Pat, PVar, PLit, PTuple)
import Text.Trifecta (Result(..))

import LambdaCube.Core.DeBruijn hiding (Exp,N)
import LambdaCube.Core.DeBruijn (N())
import LambdaCube.Core.Type hiding (Ty)
import qualified LambdaCube.Core.PrimFun as C
import qualified LambdaCube.Core.Type as C

import Parser (parseLC)
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

data Pat
  = PLit     Lit
  | PVar     Var
  | PCon EName Type [Pat]
  | PTuple   [Pat]
  deriving (Show,Eq,Ord)

newtype Exp = Exp (Exp_ Exp)
  deriving (Show,Eq,Ord)

dummyType = TVar ""

stripTypes :: Exp -> Exp
stripTypes e = case e of
    EVar (VarE n _) -> EVar $ VarE n dummyType
    Exp e -> Exp $ stripTypes <$> e

data Exp_ a
  = ELit_     Lit
  | EVar_     Var
  | EApp_     a a
  | ELam_     Var a
  | ELet_     Var a a -- VarE only!
  | ETuple_   [a]
  | EType_    Ty
  | EConstraint_ (Constraint Ty)  -- TODO: wittnesses here if needed
  | ECase_    a [(Pat, a)]
  deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

pattern ELit a = Exp (ELit_ a)
pattern EVar a = Exp (EVar_ a)
pattern EApp a b = Exp (EApp_ a b)
pattern ELam a b = Exp (ELam_ a b)
pattern ELet a b c = Exp (ELet_ a b c)
pattern ECase a b = Exp (ECase_ a b)
pattern ETuple a = Exp (ETuple_ a)
--pattern ERecord a b = Exp (ERecord_ a b)
--pattern EFieldProj a c = Exp (EFieldProj_ a c)
pattern EType a = Exp (EType_ a)
pattern EConstraint a = Exp (EConstraint_ a)

reduce :: Subst -> Map EName Exp -> Exp -> Exp
reduce s m e = case e of
    EVar (VarE v t) -> maybe (EVar $ VarE v $ subst s t) r $ Map.lookup v m
    ELet (VarE v _) e f -> reduce s (Map.insert v e m) f
    EApp f x -> case r f of
        (ELam (VarE v _) e) -> reduce s (Map.insert v x m) e
        (ELam (VarT v) e) -> case r x of
            EType x -> reduce (s `composeSubst` Map.singleton v x) m e
        (EVar (VarE v (Forall tv t))) -> case r x of
            EType t' -> EVar $ VarE v $ subst (Map.singleton tv t') t
        e -> case r x of
--            EType x -> e --reduce (s `composeSubst` Map.singleton v x) m e
--            EType t ->
            EConstraint _ -> e
            x -> EApp e x
    ETuple es -> ETuple $ map r es
--    ELam v@(VarE n t) e -> ELam v $ reduce (s `composeSubst` Map.singleton n t) m e
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
  AST.EVar t n      -> foldl EApp (foldl EApp (EVar $ VarE n $ toType $ subst sub' $ snd t) pv) cs
    where
      cs = map EConstraint $ subst sub' $ constraints $ snd t
      pv = map EType $ subst sub' $ map (\n -> TVar n) $ Map.keys $ fst t
  AST.EApp t f a    -> EApp (toCore' f) (toCore' a)
  AST.ELet _ (AST.PVar _ n) a b  -> ELet (VarE n $ toType' $ getTag a) (pv --> ctr --> toCore' a) (toCore' b)
    where
      ctr = map VarC $ constraints $ snd $ getTag a
      pv = map VarT $ Set.toList $ polyVars $ snd $ getTag a
  AST.ELam t (AST.PVar tn n) a -> ELam (VarE n $ toType' tn) $ toCore' a
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

eLam (VarT n) (EApp e (EType (TVar m))) | n == m = e  -- optimization
eLam (VarC c) (EApp e (EConstraint c')) | c == c' = e  -- optimization
eLam vt x = ELam vt x

compile :: Exp -> Either String LambdaCube.Core.DeBruijn.N
compile x = case comp . reduce mempty mempty $ x of
    [n] -> Right n
    ns -> Left $ show ns

pattern Va x <- VarE x _
pattern A0 x <- EVar (Va x)
pattern A0t x t <- EVar (VarE x t)
pattern A1 f x <- EApp (A0 f) x
pattern A2 f x y <- EApp (A1 f x) y
pattern A3 f x y z <- EApp (A2 f x y) z
pattern A4 f x y z v <- EApp (A3 f x y z) v
pattern A5 f x y z v w <-  EApp (A4 f x y z v) w

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

showStrippedReducedTest = test'' (stripTypes . reduce mempty mempty) >>= writeFile "testStripped.tmp"
showReducedTest = test'' (reduce mempty mempty) >>= writeFile "test.tmp"
showUnreducedTest = test'' id >>= writeFile "testUnreduced.tmp"

test = test'' (reduce mempty mempty) >>= putStrLn
test'' f = test_ $ return . ppShow . f
test' = test_ $ \x -> head (comp $ reduce mempty mempty x) `seq` print "ok"

test_ f = do
  r <- parseAndToCoreMain "gfx03.lc" -- "gfx03.lc" -- "example01.lc"
--  (src, r) <- parseLC_ "tests/accept/instantiate.lc" -- "gfx03.lc" -- "example01.lc"
--  (src, r) <- parseLC_ "tests/accept/id.lc" -- "gfx03.lc" -- "example01.lc"
  putStrLn "====================="
  case r of
    Right e -> f e
    Left m -> do
      error $ show m

parseAndToCoreMain :: String -> IO (Either String Exp)
parseAndToCoreMain fname = do
  res <- parseLC fname
  case res of
    Left m -> do
      return (Left $ show m)
    Right (src, e) -> do
      case inference e of
        Right t   -> do
          return $ Right $ toCore mempty $ getMain t
        Left m    -> do
          return $ Left $ m src

getMain m = case [e | ("main", e) <- definitions m] of
    [e] -> e
    [] -> error "main not found"
    _ -> error "multiple main found"

