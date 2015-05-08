{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Core where

import qualified Data.Foldable as F
import Data.Char
import Data.Traversable
import Data.Monoid
import Data.Maybe
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Applicative
import Control.Arrow
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Foldable (Foldable, toList)
import Debug.Trace

import Pretty
import Type
import Typecheck

--trace' = trace
trace' _ x = x


{-
-- TODO
unifC = undefined
unifC (CEq t f) (CEq t' f') = runExcept $ unifyTypes_ throwError True $ [t, t']: zipWith (\x y->[x,y]) (toList f) (toList f')
unifC (CClass c t) (CClass c' t') | c == c' = runExcept $ unifyTypes_ throwError True $ [t, t']: []
unifC a b = error $ "unifC: " ++ ppShow a ++ "\n ~ \n" ++ ppShow b
-}
-------------------------------------------------------------------------------- System-F conversion
{-
toCore :: Subst -> AST.ExpT -> Exp
toCore sub (AST.Exp (s, t) e) = case e of
    EVar_ n -> foldl EApp (EVar $ VarE n t')
        $  map EType (subst sub' $ map (TVar Star{-TODO-}) []) -- TODO! $ toList $ polyVars t)
--        ++ map EConstraint (subst sub' $ constraints t)
    ELet_ p a@(snd . getTag -> ty) b -> ELet (toCorePat sub' p) (foldr eLam (toCore sub' a) pv) (toCore sub' b)
      where
        pv = map VarT (toList []) -- TODO! $ polyVars ty)
--          ++ map VarC (subst sub' $ constraints ty)
    _ -> Exp $ setTag_ (error "toCore 1") (subst sub' . typingToTy) (toCorePat sub') $ toCore sub' <$> e
 where
    sub' = sub `composeSubst` s
    t' = typingToTy $ subst sub' t

toCorePat :: Subst -> AST.PatT -> Pat
toCorePat sub (AST.Pat (s, t) p) = Pat $ mapPat (flip (,) t') (`VarE` t') $ toCorePat sub' <$> p
 where
    sub' = sub `composeSubst` s
    t' = typingToTy $ subst sub' t

--eLam (VarT n) (EApp e (EType (TVar _ m))) | n == m = e  -- optimization
--eLam (VarC c) (EApp e (EConstraint c')) | c == c' = e  -- optimization
eLam vt x = ELam (PVar vt) x
-}

-------------------------------------------------------------------------------- reduce to Head Normal Form

reduceHNF :: Thunk -> Either String Thunk'       -- Left: pattern match failure
reduceHNF th@(peelThunk -> exp) = case exp of
    ENext_ -> Left "? err"
    EAlts_ 0 (map reduceHNF -> es) -> case [e | Right e <- es] of
        (thu:_) -> Right thu
        [] -> error $ "pattern match failure " ++ show [err | Left err <- es]
    EVar_ v -> case v of
        VarE v _t
          | isConstr v -> keep
          | otherwise -> maybe keep reduceHNF $ join $ Map.lookup v $ envMap th
    ELet_ p x e -> case matchPattern (recEnv p x) p of
        Left err -> Left err
        Right (Just m') -> reduceHNF $ applyEnvBefore m' e
        Right _ -> keep

    EApp_ f x_@(peelThunk -> x) -> reduceHNF' f $ \f -> case f of

        EAlts_ i es | i > 0 -> reduceHNF $ thunk $ EAlts_ (i-1) $ thunk . (`EApp_` x_) <$> es
        EFieldProj_ fi -> reduceHNF' x_ $ \x -> case x of
            ERecord_ fs -> case [e | (fi', e) <- fs, fi' == fi] of
                [e] -> reduceHNF e
            _ -> keep

        ELam_ p e_@(peelThunk -> e) -> case p of
            PVar (VarT v _k) -> case x of
                EType_ x -> reduceHNF $ applySubst (Map.singleton v x) e_
                x -> error $ "reduce varT: " ++ ppShow (x, e)
            _ -> case matchPattern x_ p of
                Left err -> Left err
                Right (Just m') -> reduceHNF $ applyEnvBefore m' e_
                Right _ -> keep

        -- TODO
        EVar_ (VarE (ExpN "fromInt") (TArr _ TFloat)) -> case x of
            ELit_ (LInt i) -> Right $ ELit_ $ LFloat $ fromIntegral i

        EVar_ (VarE v ty) -> case ty of
            Forall tv _ t -> case x of
                EType_ x -> Right $ EVar_ $ VarE v $ subst (Map.singleton tv x) t
                x -> error $ "reduce forall: " ++ ppShow x
            _ -> keep
        _ -> keep
    _ -> keep
  where
    keep = Right exp

reduceHNF' x f = case reduceHNF x of
    Left e -> Left e
    Right t -> f t

-- TODO: make this more efficient (memoize reduced expressions)
matchPattern :: Thunk -> Pat -> Either String (Maybe TEnv)       -- Left: pattern match failure; Right Nothing: can't reduce
matchPattern e = \case
    Wildcard -> Right $ Just mempty
    PVar (VarE v _) -> Right $ Just $ TEnv mempty $ Map.singleton v (Just e)
    PTuple ps -> reduceHNF' e $ \e -> case e of
        ETuple_ xs -> fmap mconcat . sequence <$> sequence (zipWith matchPattern xs ps)
        _ -> Right Nothing
    PCon (c, _) ps -> case getApp [] e of
        Left err -> Left err
        Right Nothing -> Right Nothing
        Right (Just (xx, xs)) -> case xx of
          EVar_ (VarE c' _)
            | c == c' -> fmap mconcat . sequence <$> sequence (zipWith matchPattern xs ps)
            | otherwise -> Left $ "constructors doesn't match: " ++ ppShow (c, c')
          q -> error $ "match rj: " ++ ppShow q
    p -> error $ "matchPattern: " ++ ppShow p
  where
    getApp acc e = reduceHNF' e $ \e -> case e of
        EApp_ a b -> getApp (b: acc) a
        EVar_ (VarE n _) | isConstr n -> Right $ Just (e, acc)
        _ -> Right Nothing

-------------------------------------------------------------------------------- full reduction

mkReduce :: Exp -> Exp
mkReduce = reduce . mkThunk

reduce = either (error "pattern match failure.") id . reduceEither
reduce' p = reduce . applyEnvBefore (TEnv mempty $ Map.fromList [(v, Nothing) | v <- patternEVars p])

reduceEither :: Thunk -> Either String Exp
reduceEither e = reduceHNF' e $ \e -> Right $ case e of
    ELam_ p e -> ELam p $ reduce' p e
    ELet_ p x e' -> ELet p (reduce' p x) $ reduce' p e'
    EAlts_ i es -> case [e | Right e <- reduceEither <$> es] of
        [e] -> e
        es -> EAlts i es
    e -> Exp'' $ reduce <$> e

