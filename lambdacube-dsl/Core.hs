module Core where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import CompositionalLC hiding (Exp(..))
import qualified CompositionalLC as AST
import Type

import ParseTrifectaLC (parseLC)
import Text.Show.Pretty

data Kind
  = Star
  deriving (Show,Eq,Ord)

data Type = ForAll [(TName,Kind)] Ty
  deriving (Show,Eq,Ord)

data Var
  = VarE EName Type
  | VarT EName Kind
  deriving (Show,Eq,Ord)

data Exp
  = ELit     Lit      -- done
  | EPrimFun PrimFun  -- done
  | EVar     Var      -- done insert from Core Env or use mangling for type var names
  | EApp     Exp Exp  -- TODO: insert EType if necessary
  | ELam     Var Exp  -- done
  | ELet     Var Exp Exp -- VarE only! -- done insert new variable to Core Env
  | ETuple   [Exp]    -- done
  | EType    Type     -- TODO: to be inserted :)
  deriving (Show,Eq,Ord)

toType :: Typing -> Type
toType (m,i,t) = ForAll [(n,Star) | n <- Set.toList $ freeVarsTy t] t

toCore :: AST.Exp Typing -> Exp
toCore e = case e of
  AST.ELit t a      -> ELit a
  AST.EPrimFun t a  -> EPrimFun a
  AST.ETuple t a    -> ETuple $ fmap toCore a
  AST.EVar (_,_,t) n-> EVar $ VarE n $ ForAll [] t
  AST.ELet t n a b  -> ELet (VarE n $ toType $ getTag a) (toCore a) (toCore b)
  AST.ELam t n a    -> let ForAll tv _ = toType t
                           lam = ELam (VarE n $ ForAll [] (TVar C "TODO"){-TODO-}) (toCore a)
                           tyLam (tv,k) a = ELam (VarT tv k) a
                       in foldr tyLam lam tv -- introduce additional type lambda parameter for polymorphic functions; insert new variable to Core Env
  AST.EApp t s f a  -> let ForAll tv _ = toType $ getTag f
                           tyApp ((tv,k):xs) = EApp (tyApp xs) $ EType $ ForAll [] $ Map.findWithDefault (TVar C tv) tv s
                           tyApp [] = toCore f
                       in EApp (tyApp tv) (toCore a) -- insert type applications
{-
  = ELit      a Lit
  | EPrimFun  a PrimFun
  | EVar      a EName
  | EApp      a (Exp a) (Exp a)
  | ELam      a EName (Exp a)
  | ELet      a EName (Exp a) (Exp a)
  | ETuple    a [Exp a]
-}
{-
let id x = x
in id 1
-}
idAST = AST.ELet (Map.fromList [],[],TInt C) "id" (
        AST.ELam (Map.fromList [],[],TArr (TVar C "t0") (TVar C "t0")) "x"
          (AST.EVar (Map.fromList [("x",TVar C "t0")],[],TVar C "t0") "x"))
        (AST.EApp (Map.fromList [],[],TInt C) Map.empty (AST.EVar (Map.fromList [],[],TArr (TVar C "t2") (TVar C "t2")) "id") (AST.ELit (Map.fromList [],[],TInt C) (LInt 1)))
{-
idCore = Let "id" (
          Lam "a" (Lam "x" (Var $ Id "x" (TyVarTy $ TyVar "a" Star))))
         (App (App (Var $ Id "id" (ForAllTy (TyVar "a" Star) $ Arr (TyVarTy (TyVar "a" Star)) (TyVarTy $ TyVar "a" Star))) (Type Int)) (Lit $ LInt 1))
-}

idCoreLam = ELam (VarT "a" Star) $ ELam (VarE "x" $ ForAll [] $ TVar C "a") $ EVar $ VarE "x" $ ForAll [] $ TVar C "a"
idCore = ELet (VarE "id" $ ForAll [("t",Star)] $ TVar C "t" ~> TVar C "t") idCoreLam $
          EApp (EApp (EVar $ VarE "id" $ ForAll [("t",Star)] $ TVar C "t" ~> TVar C "t") (EType $ ForAll [] $ TInt C)) (ELit $ LInt 1)
{-
idCore = Let "id" (
          Lam "a" (Lam "x" (Var $ Id "x" (TyVarTy $ TyVar "a" Star))))
         (App (App (Var $ Id "id" (ForAllTy (TyVar "a" Star) $ Arr (TyVarTy (TyVar "a" Star)) (TyVarTy $ TyVar "a" Star))) (Type Int)) (Lit $ LInt 1))
-}
test = do
  Right e <- parseLC "gfx02.lc" -- "example01.lc"
  putStrLn "====================="
  putStrLn $ ppShow $ toCore e
