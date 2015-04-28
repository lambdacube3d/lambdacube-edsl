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
import Control.DeepSeq
import Data.Foldable (Foldable, toList)
import Text.Show.Pretty
import Debug.Trace

import Type hiding (ELet, EApp, ELam, EVar, ELit, ETuple, ECase, ERecord, EFieldProj, EAlts, ENext, Exp, Pat, PAt, PVar, PLit, PTuple, PCon, Wildcard)
import qualified Type as AST
import Typecheck

--trace' = trace
trace' _ x = x

-------------------------------------------------------------------------------- data types

data Var
  = VarE EName Ty
  | VarT EName -- Ty
  | VarC {-Witness-} (Constraint Ty)      -- constraint var
  deriving (Show,Eq,Ord)

newtype Pat = Pat (Pat_ (EName, Ty) Var Pat)
  deriving (Show,Eq,Ord)

instance Substitute Var where
    subst s = \case
        VarE n t -> VarE n $ subst s t
        VarC c -> VarC $ subst s c
        VarT n -> VarT n

instance Substitute Pat where
    subst s = \case
        PVar v -> PVar $ subst s v
        PCon (n, ty) l -> PCon (n, subst s ty) $ subst s l
        Pat p -> Pat $ fmap (subst s) p

pattern PAt v l = Pat (PAt_ v l)
pattern PLit l = Pat (PLit_ l)
pattern PVar l = Pat (PVar_ l)
pattern PCon c l = Pat (PCon_ c l)
pattern PTuple l = Pat (PTuple_ l)
pattern Wildcard = Pat Wildcard_

newtype Exp = Exp (Exp_ Var Ty Pat Exp)
  deriving (Show,Eq,Ord)

pattern ELit a = Exp (ELit_ a)
pattern EVar a = Exp (EVar_ a)
pattern EApp a b = Exp (EApp_ a b)
pattern ELam a b = Exp (ELam_ a b)
pattern ELet a b c = Exp (ELet_ a b c)
pattern ECase a b = Exp (ECase_ a b)
pattern ETuple a = Exp (ETuple_ a)
pattern ERecord a b = Exp (ERecord_ a b)
pattern EFieldProj a = Exp (EFieldProj_ a)
pattern EType a = Exp (EType_ a)
pattern EConstraint a = Exp (EConstraint_ a)
pattern EAlts i b = Exp (EAlts_ i b)
pattern ENext = Exp ENext_

pattern Va x <- VarE x _
pattern A0 x <- EVar (Va x)
pattern A0t x t <- EVar (VarE x t)
pattern A1 f x <- EApp (A0 f) x
pattern A1t f t x <- EApp (A0t f t) x
pattern A2 f x y <- EApp (A1 f x) y
pattern A3 f x y z <- EApp (A2 f x y) z
pattern A4 f x y z v <- EApp (A3 f x y z) v
pattern A5 f x y z v w <-  EApp (A4 f x y z v) w

-------------------------------------------------------------------------------- utility

tyOf :: Exp -> Ty
tyOf = \case
    ETuple es -> TTuple $ map tyOf es
    EVar (VarE _ t) -> t
    EApp (tyOf -> TArr _ t) _ -> t
    ELam (tyOfPat -> a) (tyOf -> b) -> TArr a b
    e -> error $ "tyOf " ++ ppShow e

tyOfPat :: Pat -> Ty
tyOfPat = \case
    PCon (_, t) ps -> stripArgs (length ps) t
    e -> error $ "tyOfPat " ++ ppShow e
  where
    stripArgs 0 t = t
    stripArgs n (TArr _ t) = stripArgs (n-1) t

-- TODO: make pretty print instead
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

patternEVars (Pat p) = case p of
    PVar_ (VarE v _) -> [v]
    p -> foldMap patternEVars p

-- TODO
unifC (CEq t f) (CEq t' f') = runExcept $ unifyTypes_ throwError True $ [t, t']: zipWith (\x y->[x,y]) (toList f) (toList f')
unifC (CClass c t) (CClass c' t') | c == c' = runExcept $ unifyTypes_ throwError True $ [t, t']: []
unifC a b = error $ "unifC: " ++ ppShow a ++ "\n ~ \n" ++ ppShow b

-------------------------------------------------------------------------------- System-F conversion

toCore :: Subst -> AST.Exp (Subst, Typing) -> Exp
toCore sub (AST.Exp (s, t) e) = case e of
    EVar_ n -> foldl EApp (EVar $ VarE n t')
        $  map EType (subst sub' $ map (TVar Star{-TODO-}) $ toList $ polyVars t)
        ++ map EConstraint (subst sub' $ constraints t)
    ELet_ p a@(snd . getTag -> ty) b -> ELet (toCorePat sub' p) (foldr eLam (toCore sub' a) pv) (toCore sub' b)
      where
        pv = map VarT (toList $ polyVars ty)
          ++ map VarC (subst sub' $ constraints ty)
    _ -> Exp $ setTag_ (error "toCore") (error "toCore") (toCorePat sub') $ toCore sub' <$> e
 where
    sub' = sub `composeSubst` s
    t' = typingToTy $ subst sub' t

toCorePat :: Subst -> AST.Pat (Subst, Typing) -> Pat
toCorePat sub (AST.Pat (s, t) p) = Pat $ mapPat (flip (,) t') (`VarE` t') $ toCorePat sub' <$> p
 where
    sub' = sub `composeSubst` s
    t' = typingToTy $ subst sub' t

eLam (VarT n) (EApp e (EType (TVar _ m))) | n == m = e  -- optimization
eLam (VarC c) (EApp e (EConstraint c')) | c == c' = e  -- optimization
eLam vt x = ELam (PVar vt) x

-------------------------------------------------------------------------------- thunks

data Thunk = Thunk Env Thunk'
  deriving (Show)

type Thunk' = Exp_ Var Ty Pat Thunk

data Env = Env Subst EnvMap
  deriving (Show)

type EnvMap = Map EName (Maybe Thunk)   -- Nothing: statically unknown but defined

instance Monoid Env where
    mempty = Env mempty mempty
    -- semantics: apply (m1 <> m2) = apply m1 . apply m2;  see 'composeSubst'
    m1@(Env x1 y1) `mappend` Env x2 y2 = Env (x1 `composeSubst` x2) $ ((applyEnv m1 <$>) <$> y2) <> y1

envMap :: Thunk -> EnvMap
envMap (Thunk (Env _ m) _) = m

subst' :: Substitute a => Env -> a -> a
subst' (Env s _) = subst s

applyEnv :: Env -> Thunk -> Thunk
applyEnv m1 (Thunk m exp) = Thunk (m1 <> m) exp

applyEnvBefore :: Env -> Thunk -> Thunk
applyEnvBefore m1 (Thunk m exp) = Thunk (m <> m1) exp

--   applySubst s  ===  applyEnv (Env s mempty)
-- but the following is more efficient
applySubst :: Subst -> Thunk -> Thunk
applySubst s' (Thunk (Env s m) exp) = Thunk (Env (s' `composeSubst` s) m) exp

-- build recursive environment  TODO: generalize
recEnv :: Pat -> Thunk -> Thunk
recEnv (PVar (VarE v _)) th_ = th where th = applyEnvBefore (Env mempty (Map.singleton v (Just th))) th_
recEnv _ th = th

mkThunk :: Exp -> Thunk
mkThunk (Exp e) = thunk $ mkThunk <$> e

thunk :: Thunk' -> Thunk
thunk = Thunk mempty

peelThunk :: Thunk -> Thunk'
peelThunk (Thunk env e) = setTag_ vf (subst' env) (subst' env) $ applyEnv env <$> e
  where
    vf = \case
        VarE v t -> VarE v $ subst' env t

-------------------------------------------------------------------------------- reduce to Head Normal Form

reduceHNF :: Thunk -> Either String Thunk'       -- Left: pattern match failure
reduceHNF th@(peelThunk -> exp) = case exp of
    ENext_ -> Left "? err"
    EAlts_ 0 (map reduceHNF -> es) -> case [e | Right e <- es] of
        (thu:_) -> Right thu
        [] -> error $ "pattern match failure " ++ show [err | Left err <- es]
    EVar_ v -> case v of
        VarE v t
          | isConstr v -> keep
          | otherwise -> maybe keep reduceHNF $ join $ Map.lookup v $ envMap th
    ELet_ p x e -> case matchPattern (recEnv p x) p of
        Left err -> Left err
        Right (Just m') -> reduceHNF $ applyEnvBefore m' e
        Right _ -> keep

    EApp_ f x_@(peelThunk -> x) -> reduceHNF' f $ \f -> case f of

        EAlts_ i es | i > 0 -> reduceHNF $ thunk $ EAlts_ (i-1) $ thunk . (`EApp_` x_) <$> es
        EFieldProj_ fi -> reduceHNF' x_ $ \x -> case x of
            ERecord_ Nothing fs -> case [e | (fi', e) <- fs, fi' == fi] of
                [e] -> reduceHNF e
            _ -> keep

        ELam_ p e_@(peelThunk -> e) -> case p of
            PVar (VarT v) -> case x of
                EType_ x -> reduceHNF $ applySubst (Map.singleton v x) e_
                x -> error $ "reduce varT: " ++ ppShow (x, e)
            PVar (VarC v) -> case x of
                EConstraint_ x -> case unifC v x of
                    Right s' -> reduceHNF $ applySubst s' e_
                    Left e -> error $ "reduce_c: " ++ e
            _ -> case matchPattern x_ p of
                Left err -> Left err
                Right (Just m') -> reduceHNF $ applyEnvBefore m' e_
                Right _ -> keep

        -- TODO
        EVar_ (VarE "fromInt" (TArr _ TFloat)) -> case x of
            ELit_ (LInt i) -> Right $ ELit_ $ LFloat $ fromIntegral i

        EVar_ (VarE v ty) -> case ty of
            Forall tv _ t -> case x of
                EType_ x -> Right $ EVar_ $ VarE v $ subst (Map.singleton tv x) t
            TConstraintArg t ty -> case x of
                EConstraint_ t' -> case unifC t t' of
                    Right s' -> Right $ EVar_ $ VarE v $ subst s' ty
                    Left e -> error $ "reduce (2): " ++ e
                e -> error $ "reduce constr: " ++ show e
            _ -> keep
        _ -> keep
    _ -> keep
  where
    keep = Right exp

reduceHNF' x f = case reduceHNF x of
    Left e -> Left e
    Right t -> f t

-- TODO: make this more efficient (memoize reduced expressions)
matchPattern :: Thunk -> Pat -> Either String (Maybe Env)       -- Left: pattern match failure; Right Nothing: can't reduce
matchPattern e = \case
    Wildcard -> Right $ Just mempty
    PVar (VarE v _) -> Right $ Just $ Env mempty $ Map.singleton v (Just e)
    PTuple ps -> reduceHNF' e $ \e -> case e of
        ETuple_ xs -> fmap mconcat . sequence <$> sequence (zipWith matchPattern xs ps)
        _ -> Right Nothing
    PCon (c, _) ps -> case getApp [] e of
        Left err -> Left err
        Right Nothing -> Right Nothing
        Right (Just (xx, xs)) -> case xx of
          EVar_ (VarE c' _)
            | c == c' -> fmap mconcat . sequence <$> sequence (zipWith matchPattern xs ps)
            | otherwise -> Left $ "constructors doesn't match: " ++ show (c, c')
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
reduce' p = reduce . applyEnvBefore (Env mempty $ Map.fromList [(v, Nothing) | v <- patternEVars p])

reduceEither :: Thunk -> Either String Exp
reduceEither e = reduceHNF' e $ \e -> Right $ case e of
    ELam_ p e -> ELam p $ reduce' p e
    ELet_ p x e' -> ELet p (reduce' p x) $ reduce' p e'
    EAlts_ i es -> case [e | Right e <- reduceEither <$> es] of
        [e] -> e
        es -> EAlts i es
    e -> Exp $ reduce <$> e

