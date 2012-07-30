{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Typing.Infer
       ( inferDefs, inferExpr
       , runInfer -- XXX
       , TyEnv(..), PolyEnv(..) -- XXX
       , Typing
       ) where

import Typing.Repr
import Typing.Fresh
import Typing.Unify
import Typing.Subst
import Typing.MonoEnv

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Set.Unicode

import Control.Applicative
import Control.Monad hiding (mapM, mapM_, forM_)
import Data.Monoid
import Prelude hiding (mapM, mapM_)
import Data.Traversable (mapM)
import Data.Foldable (mapM_, forM_)
import qualified Data.Stream as Stream

import Control.Monad.Trans
import Control.Monad.Reader hiding (mapM, mapM_, forM_)
import Control.Arrow

runFresh_ :: Fresh Tv a -> a
runFresh_ m = runFresh m $ Stream.map (('v':) . show) $ Stream.iterate succ 0

newtype TyEnv = TyEnv{ tyEnvCons :: Map Con Ty }
              deriving Monoid

type Typing = (MonoEnv, Ty)

type Typings = (MonoEnv, [Ty])

newtype PolyEnv = PolyEnv{ polyEnvMap :: Map Var Typing }
                deriving Monoid

newtype Infer a = Infer{ unInfer :: ReaderT (TyEnv, PolyEnv) (Fresh Tv) a }
                deriving (Functor, Applicative, Monad)

runInfer m tyEnv polyEnv = runFresh_ $ runReaderT (unInfer m) (tyEnv, polyEnv)

instance MonadFresh Tv Infer where
    fresh = Infer . lift $ fresh

generalize :: Set Tv -> SubstT (Fresh Tv) ()
generalize = mapM_ $ \α -> do
    β <- TyVar <$> lift fresh
    addSubst α β

instantiateTy :: Ty -> Infer Ty
instantiateTy τ = Infer . lift . runSubstT $ do
    generalize (tvs τ)
    subst τ

instantiateTyping :: Typing -> Infer Typing
instantiateTyping (m, τ) = Infer . lift . runSubstT $ do
    generalize (tvs τ)
    (,) <$> mapMono subst m <*> subst τ

lookupCon :: Con -> Infer Ty
lookupCon con = do
    mτ <- Infer . asks $ Map.lookup con . tyEnvCons . fst
    case mτ of
        Nothing -> fail $ unwords ["Undefined constructor", show con]
        Just τ -> return τ

lookupPolyVar :: Var -> Infer (Maybe Typing)
lookupPolyVar x = Infer . asks $ Map.lookup x . polyEnvMap . snd

shadow :: MonoEnv -> Infer a -> Infer a
shadow m = Infer . local (second fixup) . unInfer
  where
    fixup env@PolyEnv{..} = env{ polyEnvMap = foldr Map.delete polyEnvMap . Set.toList $ vars }
    vars = monoVars m

withPolyVars :: Map Var Typing -> Infer a -> Infer a
withPolyVars vars = Infer . local (second fixup) . unInfer
  where
    fixup env@PolyEnv{..} = env{ polyEnvMap = vars <> polyEnvMap }

un :: Unify a -> Infer a
un u = Infer . lift $ runUnify u

inferExpr :: Expr -> Infer Typing
inferExpr e = case e of
    EVar x -> do
        mtyping <- lookupPolyVar x
        case mtyping of
            Nothing -> do
                α <- TyVar <$> fresh
                return (monoVar x α, α)
            Just typing -> instantiateTyping typing
    ECon c -> do
        τ <- instantiateTy =<< lookupCon c
        return (mempty, τ)
    EApp f e -> do
        (m1, τ1) <- inferExpr f
        (m2, τ2) <- inferExpr e
        α <- TyVar <$> fresh
        (m, TyApp (TyApp TyFun _) τ) <- un $ unify [m1, m2] [τ1, TyApp (TyApp TyFun τ2) α]
        return (m, τ)
    ELam pat e -> inferMatch $ Match [pat] e

inferPat :: Pat -> Infer Typing
inferPat pat = case pat of
    PVar x -> do
        α <- TyVar <$> fresh
        return (monoVar x α, α)
    PWildcard -> do
        α <- TyVar <$> fresh
        return (mempty, α)
    PCon con pats -> do
        τ0 <- instantiateTy =<< lookupCon con
        (m, τs) <- inferPats pats
        α <- TyVar <$> fresh
        (m', τ) <- un $ unify [m] [τ0, foldr (~>) α τs]
        return (m', tyFunResult τ)

inferPats :: [Pat] -> Infer Typings
inferPats pats = do
    (ms, τs) <- unzip <$> mapM inferPat pats
    checkConflicts . map monoVars $ ms
    return (mconcat ms, τs)

inferDefs :: Defs -> Infer (Map Var Typing)
inferDefs (Defs defss) = foldM inferGroup mempty defss
  where
    inferGroup :: Map Var Typing -> [Def] -> Infer (Map Var Typing)
    inferGroup varMap defs = do
        varMap' <- Map.fromList <$> (withPolyVars varMap $ mapM inferDef defs)
        let newVars = Map.keysSet varMap'
            generalize (m, τ) = (removeMonoVars newVars m, τ)
            varMap'' = fmap generalize varMap'
        return $ varMap'' <> varMap

    inferDef :: Def -> Infer (Var, Typing)
    inferDef def = case def of
        DefVar x e -> do
            (m, τ) <- inferExpr e
            return (x, (m, τ))
        DefFun f matches -> do
            (ms, τs) <- unzip <$> mapM inferMatch matches
            (m, τ) <- un $ unify ms τs
            return (f, (m, τ))

inferMatch :: Match -> Infer Typing
inferMatch (Match pats body) = do
    (mPats, τPats) <- inferPats pats
    (mBody, τBody) <- shadow mPats $ inferExpr body
    let τ0 = foldr (~>) τBody τPats
    (m, τ) <- un $ unify [mBody, mPats] [foldr (~>) τBody τPats]
    let m' = removeMonoVars (monoVars mPats) m
    return (m', τ)

checkConflicts :: [Set Var] -> Infer ()
checkConflicts = foldM_ check mempty
  where
    check :: Set Var -> Set Var -> Infer (Set Var)
    check vars vars' = do
        forM_ vars' $ \x -> do
            unless (x ∉ vars) $
              fail $ unwords ["Conflicting variable name", show x]
        return $ vars ∪ vars'
