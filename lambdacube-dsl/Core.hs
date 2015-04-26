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

patternVars (Pat p) = case p of
    PVar_ (VarE v _) -> [v]
    p -> foldMap patternVars p

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
    sub' = s `composeSubst` sub
    t' = typingToTy $ subst sub' t

toCorePat :: Subst -> AST.Pat (Subst, Typing) -> Pat
toCorePat sub (AST.Pat (s, t) p) = Pat $ mapPat (flip (,) t') (`VarE` t') $ toCorePat sub' <$> p
 where
    sub' = s `composeSubst` sub
    t' = typingToTy $ subst sub' t

eLam (VarT n) (EApp e (EType (TVar _ m))) | n == m = e  -- optimization
eLam (VarC c) (EApp e (EConstraint c')) | c == c' = e  -- optimization
eLam vt x = ELam (PVar vt) x

-------------------------------------------------------------------------------- reduction

data Thunk = Thunk Env Exp

data Env = Env {envSubst :: Subst, envMap :: EnvMap}

type EnvMap = Map EName (Maybe Thunk)   -- Nothing: statically unknown but defined

instance Monoid Env where
    mempty = Env mempty mempty
    e@(Env x y) `mappend` Env x' y' = Env (x' `composeSubst` x) (y <> (fmap (\(Thunk e' x) -> Thunk (e <> e') x) <$> y'))

subst' :: Substitute a => Env -> a -> a
subst' (Env s _) = subst s

addSubst s' (Thunk (Env s m) e) = Thunk (Env (s `composeSubst` s') m) e

deleteEnvVars vs (Env s m) = Env s $ foldr (\i m -> Map.insert i Nothing m) m vs

-- build recursive environment  TODO: generalize
recEnv (PVar (VarE v _)) env x = th where
    th = Thunk (Env mempty (Map.singleton v (Just th)) <> env) x
recEnv _ env x = Thunk env x

-------------------------------------------------------------------------------- reduce to Head Normal Form

reduceHNF :: Thunk -> Either String Thunk       -- Left: pattern match failure
reduceHNF th@(Thunk env exp) = case exp of
    ENext -> Left "? err"
    EAlts 0 (map (reduceHNF . Thunk env) -> es) -> case [e | Right e <- es] of
        (thu:_) -> Right thu
        [] -> error $ "pattern match failure " ++ show [err | Left err <- es]
    EVar v -> case v of
        VarE v t
          | isConstr v -> keep
          | otherwise -> maybe keep reduceHNF $ join $ Map.lookup v $ envMap env
    ELet p x e' -> case matchPattern (recEnv p env x) p of
        Left err -> Left err
        Right (Just m') -> reduceHNF $ Thunk (m' <> env) e'
        Right _ -> keep
    EApp f@(Thunk env -> tf) x@(Thunk env -> tx) -> reduceHNF' tf $ \(Thunk env' exp') -> case exp' of

        EAlts i es | i > 0 -> reduceHNF $ Thunk env' $ EAlts (i-1) $ (`EApp` x) <$> es
        EFieldProj fi -> reduceHNF' tx $ \(Thunk env'' exp'') -> case exp'' of
            ERecord Nothing fs -> case [e | (fi', e) <- fs, fi' == fi] of
                [e] -> reduceHNF $ Thunk env'' e
            _ -> keep

        ELam p e'@(Thunk env' -> te) -> case p of
            PVar (VarT v) -> case x of
                EType x -> reduceHNF $ addSubst (Map.singleton v (subst' env x)) te
                x -> error $ "reduce varT: " ++ ppShow (x, e')
            PVar (VarC v) -> case x of
                EConstraint x -> case unifC (subst' env' v) (subst' env x) of
                    Right s' -> reduceHNF $ addSubst s' te
                    Left e -> error $ "reduce_c: " ++ e
            _ -> case matchPattern tx p of
                Left err -> Left err
                Right (Just m') -> reduceHNF $ Thunk (m' <> env') e'
                Right _ -> keep

        -- TODO
        EVar (VarE "fromInt" (TArr _ TFloat)) -> case x of
            ELit (LInt i) -> Right $ Thunk mempty $ ELit $ LFloat $ fromIntegral i
        EVar (VarE v ty) -> case ty of
            Forall tv t -> case x of
                EType x -> Right $ addSubst (Map.singleton tv (subst' env x)) $ Thunk env' $ EVar $ VarE v t
            TConstraintArg t ty -> case x of
                EConstraint t' -> case unifC (subst' env' t) (subst' env t') of
                    Right s' -> Right $ addSubst s' $ Thunk env' $ EVar $ VarE v ty
                    Left e -> error $ "reduce (2): " ++ e
                e -> error $ "reduce constr: " ++ show e
            _ -> keep
        _ -> keep
    _ -> keep
  where
    keep = Right th

reduceHNF' x f = case reduceHNF x of
    Left e -> Left e
    Right t -> f t

-- TODO: make this more efficient (memoize reduced expressions)
matchPattern :: Thunk -> Pat -> Either String (Maybe Env)       -- Left: pattern match failure; Right Nothing: can't reduce
matchPattern e@(Thunk env _) = \case
    Wildcard -> Right $ Just mempty
    PVar (VarE v _) -> Right $ Just $ Env mempty $ Map.singleton v (Just e)
    PTuple ps -> reduceHNF' e $ \(Thunk env' exp') -> case exp' of
        ETuple (map (Thunk env) -> xs) -> fmap mconcat . sequence <$> sequence (zipWith matchPattern xs ps)
        _ -> Right Nothing
    PCon (c, _) ps -> case getApp [] e of
        Left err -> Left err
        Right Nothing -> Right Nothing
        Right (Just (xx, xs)) -> case xx of
          EVar (VarE c' _)
            | c == c' -> fmap mconcat . sequence <$> sequence (zipWith matchPattern xs ps)
            | otherwise -> Left $ "constructors doesn't match: " ++ show (c, c')
          q -> error $ "match rj: " ++ ppShow q
    p -> error $ "matchPattern: " ++ ppShow p
  where
    getApp acc e = reduceHNF' e $ \(Thunk env' exp') -> case exp' of
        EApp a b -> getApp (Thunk env' b: acc) $ Thunk env' a
        EVar (VarE n _) | isConstr n -> Right $ Just (exp', acc)
        e -> Right Nothing

-------------------------------------------------------------------------------- full reduction

mkReduce :: Exp -> Exp
mkReduce = reduce . Thunk mempty

reduce = either (error "pattern match failure.") id . reduceEither

reduceDel env vs = reduce . Thunk (deleteEnvVars vs env)

reduceEither :: Thunk -> Either String Exp
reduceEither t = reduceHNF' t $ \(Thunk env exp) -> Right $ case exp of
    ELam p e -> ELam (subst' env p) $ reduceDel env (patternVars p) e
    ELet p x e' -> ELet (subst' env p) (reduceDel env (patternVars p) x) $ reduceDel env (patternVars p) e'
    EAlts i es -> case [e | Right e <- reduceEither . Thunk env <$> es] of
        [e] -> e
        es -> EAlts i es
    Exp e -> Exp $ setTag_ vf (subst' env) id $ reduce . Thunk env <$> e
      where
        vf = \case
            VarE v t -> VarE v $ subst' env t

