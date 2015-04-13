{-# LANGUAGE PatternSynonyms #-}
module Core where

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
  deriving (Show,Eq,Ord)

data Exp
  = ELit     Lit
  | EVar     Var
  | EApp     Exp Exp
  | ELam     Var Exp
  | ELet     Var Exp Exp -- VarE only!
  | ETuple   [Exp]
  | EType    Ty
  deriving (Show,Eq,Ord)

toCore :: Set TName -> Subst -> AST.Exp (Subst, Typing) -> Exp
toCore tvs sub e = case e of
  AST.ELit _ a      -> ELit a
  AST.ETuple _ a    -> ETuple $ fmap toCore' a
  AST.EVar t n      -> foldl EApp (EVar $ VarE n $ toType $ snd t) $ map (EType {- . ([] ==>)-}) pv'
    where
      pv' = subst sub' $ map (\n -> TVar C n) $ Map.keys $ fst t
  AST.EApp t f a    -> EApp (toCore' f) (toCore' a)
  AST.ELet _ (PVar _ n) a b  -> ELet (VarE n $ toType' $ getTag a) (pv --> toCore'' (pv <> tvs) a) (toCore'' tvs b)
    where
      pv = polyVars $ snd $ getTag a
  AST.ELam t (PVar tn n) a -> ELam (VarE n $ toType' tn) $ toCore'' tvs a
  _ -> error $ "toCore: " ++ show e
 where
    toCore' = toCore'' tvs
    toCore'' tvs = toCore tvs sub'
    s = fst $ getTag e
    sub' = s `composeSubst` sub
    toType' (_, t) = toType $ subst sub' t
    varT t = VarT t -- Star
    pv --> x = foldr ELam x $ map varT $ Set.toList pv

    toType :: Typing -> Type
    toType ty@(Typing me cs t) = foldr Forall t $ Set.toList $ polyVars ty Set.\\ tvs


-- test case
{-
idCoreLam = ELam (VarT "a" Star) $ ELam (VarE "x" $ ForAll [] $ TVar C "a") $ EVar $ VarE "x" $ ForAll [] $ TVar C "a"
idCore = ELet (VarE "id" $ ForAll [("t",Star)] $ TVar C "t" ~> TVar C "t") idCoreLam $
          EApp (EApp (EVar $ VarE "id" $ ForAll [("t",Star)] $ TVar C "t" ~> TVar C "t") (EType $ ForAll [] $ TInt C)) (ELit $ LInt 1)
-}
test = do
  (src, Success e) <- parseLC_ "tests/accept/id.lc" -- "gfx03.lc" -- "example01.lc"
  putStrLn "====================="
  putStrLn $ ppShow $ toCore mempty mempty $ either (error . ($ src)) id $ inference e

