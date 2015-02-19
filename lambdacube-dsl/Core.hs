module Core where

import Data.Map
import CompositionalLC
import Type

{-

data Var
  = TyVar TName Kind
  | Id    EName Type
  deriving (Show,Eq,Ord)
-}
{-
data Ty -- star kind
  = TVar    Frequency TName
  | TArr    Ty Ty
  -- composit
  | TTuple  Frequency [Ty]
  | TArray  Frequency Ty
  -- primitive types
  | TInt    Frequency
-}
{-
data Type
  = TyVarTy   Var
  | AppTy     Type  Type
  | ForAllTy  Var   Type
  | LitTy     String
  | Arr Type Type
  | Int
  deriving (Show,Eq,Ord)
-}

data Kind
  = Star
  deriving (Show,Eq,Ord)

data Type = ForAll [(TName,Kind)] Ty
  deriving (Show,Eq,Ord)

data Var
  = VarE EName Type
  | VarT TName Kind
  deriving (Show,Eq,Ord)

data Expr
  = Var   Var
  | Lit   Lit
  | App   Expr Expr
  | Lam   Var Expr
  | Let   Var Expr Expr -- VarE only!
  | Type  Type
  deriving (Show,Eq,Ord)

toCore :: Exp Typing -> Expr
toCore = undefined

{-
  = ELit      a Lit
  | EPrimFun  a PrimFun
  | EVar      a EName
  | EApp      a (Exp a) (Exp a)
  | ELam      a EName (Exp a)
  | ELet      a EName (Exp a) (Exp a)
  | ETuple    a [Exp a]
-}

idAST = ELet (fromList [],[],TInt C) "id" (
        ELam (fromList [],[],TArr (TVar C "t0") (TVar C "t0")) "x"
          (EVar (fromList [("x",TVar C "t0")],[],TVar C "t0") "x"))
        (EApp (fromList [],[],TInt C) (EVar (fromList [],[],TArr (TVar C "t2") (TVar C "t2")) "id") (ELit (fromList [],[],TInt C) (LInt 1)))
{-
idCore = Let "id" (
          Lam "a" (Lam "x" (Var $ Id "x" (TyVarTy $ TyVar "a" Star))))
         (App (App (Var $ Id "id" (ForAllTy (TyVar "a" Star) $ Arr (TyVarTy (TyVar "a" Star)) (TyVarTy $ TyVar "a" Star))) (Type Int)) (Lit $ LInt 1))
-}
{-
idCore = Let "id" (
          Lam "a" (Lam "x" (Var $ Id "x" (TyVarTy $ TyVar "a" Star))))
         (App (App (Var $ Id "id" (ForAllTy (TyVar "a" Star) $ Arr (TyVarTy (TyVar "a" Star)) (TyVarTy $ TyVar "a" Star))) (Type Int)) (Lit $ LInt 1))
-}