{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Core where

import Control.Monad.State
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.Traversable
import Control.DeepSeq
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
import System.Directory
import System.FilePath

import Text.Show.Pretty
import Parser (parseLC)
import Typing (primFunMap)

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
        ELam (VarE v _) e -> reduce s (Map.insert v x m) e
        ELam (VarT v) e -> case r x of
            EType x -> reduce (s `composeSubst` Map.singleton v x) m e
        EVar (VarE v (Forall tv t)) -> case r x of
            EType t' -> EVar $ VarE v $ subst (Map.singleton tv t') t
        EVar (VarE v (TConstraintArg t ty)) -> case r x of
            EConstraint t'
               | t == t' -> EVar $ VarE v ty
               | otherwise -> error "unification of constraints is not yet implemented"
            e -> error $ "reduce constr: " ++ show e
        e -> EApp e $ r x
    ETuple es -> ETuple $ map r es
--    ELam v@(VarE n t) e -> ELam v $ reduce (s `composeSubst` Map.singleton n t) m e
    ELam v e -> ELam v $ r e
    ELit{} -> e
    EType t -> EType $ subst s t
    EConstraint c -> EConstraint $ subst s c
  where
    r = reduce s m

toCore :: Subst -> AST.Exp (Subst, Typing) -> Exp
toCore sub e = case e of
  AST.ELit _ a      -> ELit a
  AST.ETuple _ a    -> ETuple $ fmap toCore' a
  AST.EVar t n      -> foldl EApp (foldl EApp (EVar $ VarE n $ toType $ subst sub' $ snd t) pv) cs
    where
      cs = map EConstraint $ subst sub' $ constraints $ snd t
      pv = map EType $ subst sub' $ map TVar $ Map.keys $ fst t
  AST.EApp t f a    -> EApp (toCore' f) (toCore' a)
  AST.ELet _ (AST.PVar _ n) a b  -> ELet (VarE n $ toType' $ getTag a) (pv --> ctr --> toCore' a) (toCore' b)
    where
      ctr = map VarC $ constraints $ snd $ getTag a
      pv = map VarT $ Set.toList $ polyVars $ snd $ getTag a
  AST.ELam t (AST.PVar tn n) a -> ELam (VarE n $ toType' tn) $ toCore' a
--  AST.ERecord 
  _ -> error $ "toCore: " ++ ppShow e
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

tyOf :: Exp -> Ty
tyOf (EVar (VarE _ t)) = t
tyOf (EApp (tyOf -> TArr _ t) _) = t
tyOf e = error $ "tyOf " ++ ppShow e

pattern Va x <- VarE x _
pattern A0 x <- EVar (Va x)
pattern A0t x t <- EVar (VarE x t)
pattern A1 f x <- EApp (A0 f) x
pattern A2 f x y <- EApp (A1 f x) y
pattern A3 f x y z <- EApp (A2 f x y) z
pattern A4 f x y z v <- EApp (A3 f x y z) v
pattern A5 f x y z v w <-  EApp (A4 f x y z v) w


--test' = test_ $ \x -> head (comp $ reduce mempty mempty x) `seq` print "ok"
----

showStrippedReducedTest = test'' (stripTypes . reduce mempty mempty) >>= writeFile "testStripped.tmp"
showReducedTest = test'' (reduce mempty mempty) >>= writeFile "test.tmp"
showUnreducedTest = test'' id >>= writeFile "testUnreduced.tmp"

test = test'' (reduce mempty mempty) >>= putStrLn
test'' f = test_ $ ppShow . f

test_ f = either error f <$> parseAndToCoreMain "./tests/accept" "gfx03" -- "example01"

reducedMain :: FilePath -> String -> IO (Either String Exp)
reducedMain path fname = fmap (reduce mempty mempty) <$> parseAndToCoreMain path fname

parseAndToCoreMain :: FilePath -> String -> IO (Either String Exp)
parseAndToCoreMain path fname =
  either Left (either Left (Right . toCore mempty) . getMain) <$> typeCheckLC path fname

typeCheckLC :: FilePath -> String -> IO (Either String (Module (Subst, Typing)))
typeCheckLC path mname = do
 let fname = lcModuleFile path mname
 b <- doesFileExist fname
 if not b then return $ Left $ "can't find module " ++ fname
 else do
  res <- parseLC fname
  case res of
    Left m -> return $ Left m
    Right (src, e) -> do
      ms <- mapM (typeCheckLC path . qData) $ moduleImports e
      return $ case joinPolyEnvs . (PolyEnv primFunMap:) . map exportEnv <$> sequence ms of
        Left m -> Left m
        Right (Left m) -> Left m
        Right (Right env) -> case inference_ env e of
            Right x   -> Right x
            Left m    -> Left $ m src

lcModuleFile path n = path </> (n ++ ".lc")

getMain :: Module (Subst, Typing) -> Either String (AST.Exp (Subst, Typing))
getMain m = case [e | (AST.PVar _ "main", e) <- definitions m] of
    [e] -> Right e
    [] -> Left "main not found"
    _ -> Left "multiple main found"

