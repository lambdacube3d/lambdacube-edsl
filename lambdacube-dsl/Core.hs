{-# LANGUAGE PatternSynonyms #-}
module Core where

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

import Parser (parseLC_)
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

toCore :: Subst -> AST.Exp (Subst, Typing) -> Exp
toCore sub e = case e of
  AST.ELit _ a      -> ELit a
  AST.ETuple _ a    -> ETuple $ fmap toCore' a
  AST.EVar t n      -> foldl EApp (foldl EApp (EVar $ VarE n $ toType $ snd t) pv) cs
    where
      cs = map EConstraint $ subst sub' $ constraints $ snd t
      pv = map EType $ subst sub' $ map (\n -> TVar C n) $ Map.keys $ fst t
  AST.EApp t f a    -> EApp (toCore' f) (toCore' a)
  AST.ELet _ (PVar _ n) a b  -> traceShow (n, pv) $ ELet (VarE n $ toType' $ getTag a) (pv --> ctr --> toCore' a) (toCore' b)
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

test = do
  (src, r) <- parseLC_ "tests/accept/instantiate.lc" -- "gfx03.lc" -- "example01.lc"
--  (src, r) <- parseLC_ "tests/accept/id.lc" -- "gfx03.lc" -- "example01.lc"
  putStrLn "====================="
  case r of
    Success e -> putStrLn $ ppShow $ toCore mempty $ either (error . ($ src)) id $ inference e
    Failure m -> do
      print m

