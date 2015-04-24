{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Core where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Foldable (Foldable, toList)
import qualified Data.Foldable as F
import Data.Traversable
import Control.DeepSeq
import Debug.Trace
import Data.Monoid
import Data.Maybe
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Applicative
import Text.Trifecta (Result(..))
import System.Directory
import System.FilePath
import Text.Show.Pretty

import Type hiding (ELet, EApp, ELam, EVar, ELit, ETuple, ECase, Exp, Pat, PVar, PLit, PTuple, PCon, Wildcard)
import qualified Type as AST
import Typecheck hiding (Exp(..))

data Kind
  = Star
  deriving (Show,Eq,Ord)

type Type = Ty

data Var
  = VarE EName Type
  | VarT EName -- Kind
  | VarC (Constraint Ty)               -- constraint var
  deriving (Show,Eq,Ord)

newtype Pat = Pat (Pat_ (EName, Type) Var Pat)
  deriving (Show,Eq,Ord)

pattern PLit l = Pat (PLit_ l)
pattern PVar l = Pat (PVar_ l)
pattern PCon c l = Pat (PCon_ c l)
pattern PTuple l = Pat (PTuple_ l)
pattern Wildcard = Pat Wildcard_

newtype Exp = Exp (Exp_ Var Ty Pat Exp)
  deriving (Show,Eq,Ord)

dummyType = TVar ""

stripTypes :: Exp -> Exp
stripTypes e = case e of
    EVar (VarE n _) -> ELit $ LString n --EVar $ VarE n dummyType
    EVar (VarC _) -> ELit $ LNat 13
    EType t -> ELit $ LNat 29
    EConstraint c -> ELit $ LNat 27
    ELam p e -> ELam (stripPats p) (stripTypes e)
    ELet p e e' -> ELet (stripPats p) (stripTypes e) (stripTypes e')
    Exp e -> Exp $ stripTypes <$> e

stripPats :: Pat -> Pat
stripPats = \case
    PVar (VarE n _) -> PLit $ LString n --EVar $ VarE n dummyType
    PVar (VarC _) -> PLit $ LNat 17
    Pat e -> Pat $ stripPats <$> e

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
    ETuple es -> ETuple $ map r es
    ELam p e -> ELam p $ r e
    ELit{} -> e
    EType t -> EType $ subst s t
    EConstraint c -> EConstraint $ subst s c
    EVar (VarE v t) -> maybe (EVar $ VarE v $ subst s t) r $ Map.lookup v m
    ELet p x e' -> case defs re p of
        Just m' -> reduce s (m' <> m) e'
        _ -> ELet p re $ r e'
     where re = r x
    EApp f x -> case r f of
        ELam p e' -> case p of
            PVar (VarT v) -> case re of
                EType x -> reduce (s `composeSubst` Map.singleton v x) m e'
            PVar (VarC v) -> case re of
                EConstraint x -> case unifC (subst s v) x of
                    Right s' -> reduce (s `composeSubst` s') m e'
                    Left e -> error $ "reduce: " ++ e
            _ -> case defs re p of
                Just m' -> reduce s (m' <> m) e'
                _ -> ELam p $ r e'
         where re = r x
        EVar e' -> case e' of
--            VarE "unpack'" _ -> EVar (VarE "#const" undefined)
--            VarE "#const" _ -> r x
            VarE v (Forall tv t) -> case r x of
                EType t' -> EVar $ VarE v $ subst (Map.singleton tv t') t
            VarE v (TConstraintArg t ty) -> case r x of
                EConstraint t' -> case unifC (subst s t) t' of
                    Right s' -> EVar $ VarE v ty -- TODO: s `composeSubst` s'
                    Left e -> error $ "reduce (2): " ++ e
                e -> error $ "reduce constr: " ++ show e
            _ -> EApp (EVar e') $ r x
        e -> EApp e $ r x
  where
    r = reduce s m

    unifC (CEq t f) (CEq t' f') = runExcept $ unifyTypes_ throwError True $ [t, t']: zipWith (\x y->[x,y]) (toList f) (toList f')
    unifC (CClass c t) (CClass c' t') | c == c' = runExcept $ unifyTypes_ throwError True $ [t, t']: []
    unifC a b = error $ "unifC: " ++ ppShow a ++ "\n ~ \n" ++ ppShow b

    defs e = \case
        Wildcard -> mempty
        PVar (VarE v _) -> Just $ Map.singleton v e
        PCon (c, _) ps     -> case getApp (c, ps) (length ps) e of
            Just (EVar (VarE c' _), xs)
                | c == c' -> mconcat <$> sequence (zipWith defs xs ps)
                | otherwise -> error "defs"
            _ -> Nothing
        PTuple ps -> case e of
            ETuple xs ->  mconcat <$> sequence (zipWith defs xs ps)
            _ -> Nothing
        p -> error $ "defs: " ++ ppShow p

getApp c n x = f [] n x where
    f acc 0 = \e -> Just (e, acc)
    f acc n = \case
        EApp a b -> f (b: acc) (n-1) a
        e -> Nothing -- error $ "getApp: " ++ ppShow c ++ "\n" ++ ppShow e

toCorePat :: Subst -> AST.Pat (Subst, Typing) -> Pat
toCorePat sub p = case p of
  AST.PLit _ l      -> PLit l
  AST.PVar t n    -> PVar $ VarE n $ toType' t
  AST.PCon t n ps -> PCon (n, toType' t) $ map toCorePat' ps
  AST.Wildcard _  -> Wildcard
  AST.PTuple t ps -> PTuple $ map toCorePat' ps
  p -> error $ "toCorePat: " ++ ppShow p
 where
    toCorePat' = toCorePat sub'
    s = fst $ getTag p
    sub' = s `composeSubst` sub
    toType' (_, t) = toType $ subst sub' t

toCore :: Subst -> AST.Exp (Subst, Typing) -> Exp
toCore sub e = case e of
  AST.ELit _ a      -> ELit a
  AST.ETuple _ a    -> ETuple $ fmap toCore' a
  AST.EVar t n      -> foldl EApp (foldl EApp (EVar $ VarE n $ toType $ subst sub' $ snd t) pv) cs
    where
      cs = map EConstraint $ subst sub' $ constraints $ snd t
      pv = map EType $ subst sub' $ map TVar $ Map.keys $ fst t
  AST.EApp t f a    -> EApp (toCore' f) (toCore' a)
  AST.ELet _ p a b  -> ELet (toCorePat' p) (pv --> ctr --> toCore' a) (toCore' b)
    where
      ctr = map VarC $ subst sub' $ constraints $ snd $ getTag a
      pv = map VarT $ Set.toList $ polyVars $ snd $ getTag a
  AST.ELam t p a -> ELam (toCorePat' p) $ toCore' a
  AST.ECase t e ps -> ECase (toCore' e) [(toCorePat' p, toCore' x) | (p, x) <- ps]
--  AST.ERecord
  _ -> error $ "toCore: " ++ ppShow e
 where
    toCore' = toCore sub'
    toCorePat' = toCorePat sub'
    s = fst $ getTag e
    sub' = s `composeSubst` sub
    toType' (_, t) = toType $ subst sub' t
    infixr 9 -->
    pv --> x = foldr eLam x pv

toType :: Typing -> Type
toType ty = foldr Forall (foldr TConstraintArg (typingType ty) $ constraints ty) $ Set.toList $ polyVars ty

eLam (VarT n) (EApp e (EType (TVar m))) | n == m = e  -- optimization
eLam (VarC c) (EApp e (EConstraint c')) | c == c' = e  -- optimization
eLam vt x = ELam (PVar vt) x

tyOf :: Exp -> Ty
tyOf = \case
    ETuple es -> TTuple $ map tyOf es
    EVar (VarE _ t) -> t
    EApp (tyOf -> TArr _ t) _ -> t
    e -> error $ "tyOf " ++ ppShow e

pattern Va x <- VarE x _
pattern A0 x <- EVar (Va x)
pattern A0t x t <- EVar (VarE x t)
pattern A1 f x <- EApp (A0 f) x
pattern A2 f x y <- EApp (A1 f x) y
pattern A3 f x y z <- EApp (A2 f x y) z
pattern A4 f x y z v <- EApp (A3 f x y z) v
pattern A5 f x y z v w <-  EApp (A4 f x y z v) w

buildLet :: [(AST.Pat (Subst, Typing), AST.Exp (Subst, Typing))] -> AST.Exp (Subst, Typing) -> AST.Exp (Subst, Typing)
buildLet es e = foldr (\(p, e) x -> AST.ELet (getTag e) p e x) e es

