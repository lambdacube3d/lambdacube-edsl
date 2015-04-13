{-# LANGUAGE PatternSynonyms #-}
module Core where

import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import CompositionalLC hiding (Exp(..))
import qualified CompositionalLC as AST
import qualified Type as AST
import Type hiding (ELet, EApp, ELam, EVar, ELit, ETuple, Exp)
import Text.Trifecta (Result(..))

import Parser (parseLC_)
import Text.Show.Pretty

data Kind
  = Star
  deriving (Show,Eq,Ord)

data Type = ForAll [(TName,Kind)] Ty    -- TODO
  deriving (Show,Eq,Ord)

data Var
  = VarE EName Type
  | VarT EName Kind
  deriving (Show,Eq,Ord)

data Exp
  = ELit     Lit      -- done
  | EVar     Var      -- done insert from Core Env or use mangling for type var names
  | EApp     Exp Exp  -- TODO: insert EType if necessary
  | ELam     Var Exp  -- done
  | ELet     Var Exp Exp -- VarE only! -- done insert new variable to Core Env
  | ETuple   [Exp]    -- done
  | EType    Type     -- TODO: to be inserted :)
  deriving (Show,Eq,Ord)

toType :: Typing -> Type
toType (Typing m i t) = ForAll [(n,Star) | n <- Set.toList $ freeVars t] t

toCore :: AST.Exp Typing -> Exp
toCore e = case e of
  AST.ELit t a      -> ELit a
  AST.ETuple t a    -> ETuple $ fmap toCore a
  AST.EVar (Typing _ _ t) n -> EVar $ VarE n $ ForAll [] t
  AST.ELet t (PVar _ n) a b  -> ELet (VarE n $ toType $ getTag a) (toCore a) (toCore b)
  AST.ELam t (PVar _ n) a
                    -> let ForAll tv _ = toType t
                           lam = ELam (VarE n $ ForAll [] (TVar C "TODO"){-TODO-}) (toCore a)
                           tyLam (tv,k) a = ELam (VarT tv k) a
                       in foldr tyLam lam tv -- introduce additional type lambda parameter for polymorphic functions; insert new variable to Core Env
  AST.ESubst _ s (AST.EApp t   f a)
                    -> let ForAll tv _ = toType $ getTag f
                           tyApp ((tv,k):xs) = EApp (tyApp xs) $ EType $ ForAll [] $ Map.findWithDefault (TVar C tv) tv s
                           tyApp [] = toCore f
                       in EApp (tyApp tv) (toCore a) -- insert type applications
  AST.ESubst _ _ e  -> toCore e
  _ -> error $ "toCore: " ++ show e


-- test case
idCoreLam = ELam (VarT "a" Star) $ ELam (VarE "x" $ ForAll [] $ TVar C "a") $ EVar $ VarE "x" $ ForAll [] $ TVar C "a"
idCore = ELet (VarE "id" $ ForAll [("t",Star)] $ TVar C "t" ~> TVar C "t") idCoreLam $
          EApp (EApp (EVar $ VarE "id" $ ForAll [("t",Star)] $ TVar C "t" ~> TVar C "t") (EType $ ForAll [] $ TInt C)) (ELit $ LInt 1)

test = do
  (src, Success e) <- parseLC_ "tests/accept/id.lc" -- "gfx03.lc" -- "example01.lc"
  putStrLn "====================="
  putStrLn $ ppShow $ toCore $ either (error . ($ src)) id $ inference e

