{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Applicative
import Control.Monad hiding (mapM)
import Data.Monoid
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Prelude hiding (mapM)
import Data.Traversable (mapM)
import Data.Stream (Stream(..))
import qualified Data.Stream as Stream

import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Arrow

type Id = String

type Tv = Id
type TyCon = Id

data Ty = TyCon TyCon
        | TyVar Tv
        | TyApp Ty Ty
        | TyFun
        deriving Show

tyFunResult :: Ty -> Ty
tyFunResult (TyApp (TyApp TyFun _) t) = tyFunResult t
tyFunResult t = t

data TyEq = Ty :~: Ty
          deriving Show

data Unification = Skip
                 | Substitute Tv Ty
                 | Recurse [TyEq]
                 | Flip
                 | Incongruent

unify1 :: TyEq -> Unification
unify1 tyEq = case tyEq of
    TyVar x     :~: t -> Substitute x t
    t           :~: TyVar x -> Flip
    TyFun       :~: TyFun -> Skip
    TyApp t u   :~: TyApp t' u' -> Recurse [t :~: t', u :~: u']
    TyCon c     :~: TyCon c' | c == c' -> Skip
    _ -> Incongruent

occurs :: Tv -> Ty -> Bool
occurs x (TyVar y) | x == y = True
occurs x (TyApp t u) = occurs x t || occurs x u
occurs x _ = False

class (Monad m) => MonadFresh a m | m -> a where
    fresh :: m a

newtype Fresh m a = Fresh{ unFresh :: StateT (Stream Id) m a }
                  deriving (Functor, Applicative, Monad)

instance (Monad m) => MonadFresh Tv (Fresh m) where
    fresh = do
        Cons x xs <- Fresh get
        Fresh $ put xs
        return x

runFresh :: Fresh Identity a -> a
runFresh m = evalState (unFresh m) (Stream.map (('v':) . show) $ Stream.iterate succ 0)

newtype Unify m a = Unify{ unUnify :: StateT Subst (Fresh m) a }
                deriving (Functor, Applicative, Monad)

instance (Monad m) => MonadFresh Tv (Unify m) where
    fresh = Unify $ lift fresh

type Subst = Map Tv Ty

substTv :: (Monad m) => Tv -> Unify m Ty
substTv x = do
    mt <- Unify $ gets $ Map.lookup x
    case mt of
        Nothing -> return $ TyVar x
        Just t -> subst t

subst :: (Monad m) => Ty -> Unify m Ty
subst t = case t of
    TyVar x -> substTv x
    TyApp t u -> liftM2 TyApp (subst t) (subst u)
    _ -> return t

addSubst :: (Monad m) => Tv -> Ty -> Unify m ()
addSubst x (TyVar y) | x == y = return ()
addSubst x t | occurs x t = fail $ unwords ["infinite type:", show x, "=", show t]
             | otherwise = Unify $ modify $ Map.insert x t

unifyEqs :: (Monad m) => [TyEq] -> Unify m ()
unifyEqs tyEqs = case tyEqs of
    [] -> return ()
    (e:es) -> case unify1 e of
        Skip -> unifyEqs es
        Substitute x t -> do
            addSubst x t
            es' <- mapM (\(t :~: t') -> liftM2 (:~:) (subst t) (subst t')) es
            unifyEqs es'
        Recurse es' -> unifyEqs $ es' ++ es
        Flip -> unifyEqs $ (flip e):es
        Incongruent -> fail $ unwords ["cannot unify", show e]
  where
    flip (t :~: t') = t' :~: t

data MonoEnv = MonoEnv{ monoVarMap :: Map Var Ty }
             deriving Show

instance Monoid MonoEnv where
    mempty = MonoEnv{ monoVarMap = mempty }
    MonoEnv{ monoVarMap = mvs } `mappend` MonoEnv{ monoVarMap = mvs' } = MonoEnv{ monoVarMap = mvs `mappend` mvs' }

monoVars :: MonoEnv -> [Var]
monoVars = Map.keys . monoVarMap

removeMonoVars :: [Var] -> MonoEnv -> MonoEnv
removeMonoVars vars m@MonoEnv{..} = m{ monoVarMap = foldr Map.delete monoVarMap vars }

substMonoEnv :: (Monad m) => MonoEnv -> Unify m MonoEnv
substMonoEnv m = do
    monoVarMap' <- mapM subst $ monoVarMap m
    return m{ monoVarMap = monoVarMap' }

unify :: (Monad m) => [MonoEnv] -> [Ty] -> Unify m (MonoEnv, Ty)
unify ms τs = do
    α <- fresh
    let eqs = map (TyVar α :~:) τs
        eqs' = concatMap toEqs vars
    unifyEqs $ eqs ++ eqs'
    ms' <- mapM substMonoEnv ms
    τ <- substTv α
    return (mconcat ms', τ)
  where
    vars = nub $ concatMap monoVars ms
    toEqs v = case mapMaybe (Map.lookup v . monoVarMap) ms of
        [] -> []
        t:ts -> map (t :~:) ts

test = testInfer $ runUnify (unify ms ts)
  where
    ms = [ MonoEnv $ Map.fromList [ ("f", c) ]
         , MonoEnv $ Map.fromList [ ("f", a ~> b), ("x", a), ("xs", tyList a) ]
         , MonoEnv $ Map.fromList [ ("f", TyCon "Int" ~> tyList (TyCon "Bool")) ]
         ]
    ts = [ tyList c ]

    tyList t = TyApp (TyCon "[]") t
    a = TyVar "a"
    b = TyVar "b"
    c = TyVar "c"
    d = TyVar "d"

infixr ~>
t ~> u = TyApp (TyApp TyFun t) u

type Var = Id

type Con = Id

data Def = DefVar Var Expr
         | DefFun Var [Match]

data Match = Match [Pat] Expr

data Pat = PVar Var
         | PCon Con [Pat]
         | PWildcard

data Expr = EVar Var
          | ECon Con
          | ELam Pat Expr
          | EApp Expr Expr
          | ELet [Def] Expr

newtype TyEnv = TyEnv{ tyEnvCons :: Map Con Ty }
type Typing = (MonoEnv, Ty)
type Typings = (MonoEnv, [Ty])
newtype PolyEnv = PolyEnv{ polyEnvMap :: Map Var Typing }

newtype Infer m a = Infer{ unInfer :: ReaderT (TyEnv, PolyEnv) (Fresh m) a }
                  deriving (Functor, Applicative, Monad)

runInfer m = curry $ runReaderT $ unInfer m

instance (Monad m) => MonadFresh Tv (Infer m) where
    fresh = Infer . lift $ fresh

lookupCon :: (Monad m) => Con -> Infer m Ty
lookupCon con = do
    mτ <- Infer . asks $ Map.lookup con . tyEnvCons . fst
    case mτ of
        Nothing -> fail $ unwords ["Undefined constructor", show con]
        Just τ -> return τ

lookupPolyVar :: (Monad m) => Var -> Infer m (Maybe Typing)
lookupPolyVar x = Infer . asks $ Map.lookup x . polyEnvMap . snd

shadow :: (Monad m) => MonoEnv -> Infer m a -> Infer m a
shadow m = Infer . local (second fixup) . unInfer
  where
    fixup env@PolyEnv{..} = env{ polyEnvMap = foldr Map.delete polyEnvMap vars }
    vars = monoVars m

runUnify :: (Monad m) => Unify m a -> Infer m a
runUnify u = Infer . lift $ evalStateT (unUnify u) mempty

inferExpr :: (Monad m) => Expr -> Infer m Typing
inferExpr e = case e of
    EVar x -> do
        mtyping <- lookupPolyVar x
        case mtyping of
            Nothing -> do
                α <- liftM TyVar fresh
                return (MonoEnv $ Map.singleton x α, α)
            Just typing -> return typing
    ECon c -> do
        τ <- lookupCon c
        return (mempty, τ)
    EApp f e -> do
        (m1, τ1) <- inferExpr f
        (m2, τ2) <- inferExpr e
        α <- liftM TyVar fresh
        (m, TyApp (TyApp TyFun _) τ) <- runUnify $ unify [m1, m2] [τ1, TyApp (TyApp TyFun τ2) α]
        return (m, τ)
    ELam pat e -> do
        (mPat, τPat) <- inferPat pat
        (mBody, τBody) <- shadow mPat $ inferExpr e
        (m, τ) <- runUnify $ unify [mPat, mBody] [τPat ~> τBody]
        let m' = removeMonoVars (monoVars mPat) m
        return (m', τ)

inferPat :: (Monad m) => Pat -> Infer m Typing
inferPat pat = case pat of
    PVar x -> do
        α <- liftM TyVar fresh
        return (MonoEnv $ Map.singleton x α, α)
    PWildcard -> do
        α <- liftM TyVar fresh
        return (mempty, α)
    PCon con pats -> do
        τ0 <- lookupCon con
        (m, τs) <- inferPats pats
        α <- liftM TyVar fresh
        (m', τ) <- runUnify $ unify [m] [τ0, foldr (~>) α τs]
        return (m', tyFunResult τ)

inferPats :: (Monad m) => [Pat] -> Infer m Typings
inferPats pats = do
    (ms, τs) <- liftM unzip $ mapM inferPat pats
    -- TODO: check that the vars of each m are pairwise disjunct
    return (mconcat ms, τs)

testInfer m = runFresh $ runInfer m (TyEnv tyEnv) (PolyEnv mempty)
  where
    tyEnv = Map.fromList [ ("Nil", tyList a)
                         , ("Con", a ~> tyList a ~> tyList a)
                         ]

    tyList t = TyApp (TyCon "[]") t
    a = TyVar "a"

-- (inactivate-input-method)
-- (activate-input-method "Agda")

foo = testInfer $ inferExpr $
      ELam (PCon "Con" [PVar "x", PVar "xs"]) $
      EApp (EApp (ECon "Con") (EApp (EVar "f") (EVar "x"))) (EVar "xs")
