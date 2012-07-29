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

import Control.Monad.Trans
import Control.Monad.State hiding (mapM)
import Control.Monad.Reader hiding (mapM)
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

newtype Fresh a = Fresh{ unFresh :: State (Stream Id) a }
                deriving (Functor, Applicative, Monad)

instance MonadFresh Tv Fresh where
    fresh = do
        Cons x xs <- Fresh get
        Fresh $ put xs
        return x

runFresh :: Fresh a -> a
runFresh m = evalState (unFresh m) (Stream.map (('v':) . show) $ Stream.iterate succ 0)

newtype Unify a = Unify{ unUnify :: StateT Subst Fresh a }
                deriving (Functor, Applicative, Monad)

instance MonadFresh Tv Unify where
    fresh = Unify $ lift fresh

type Subst = Map Tv Ty

substTv :: Tv -> Unify Ty
substTv x = do
    mt <- Unify $ gets $ Map.lookup x
    case mt of
        Nothing -> return $ TyVar x
        Just t -> subst t

subst :: Ty -> Unify Ty
subst t = case t of
    TyVar x -> substTv x
    TyApp t u -> TyApp <$> subst t <*> subst u
    _ -> return t

addSubst :: Tv -> Ty -> Unify ()
addSubst x (TyVar y) | x == y = return ()
addSubst x t | occurs x t = fail $ unwords ["infinite type:", show x, "=", show t]
             | otherwise = Unify $ modify $ Map.insert x t

unifyEqs :: [TyEq] -> Unify ()
unifyEqs tyEqs = case tyEqs of
    [] -> return ()
    (e:es) -> case unify1 e of
        Skip -> unifyEqs es
        Substitute x t -> do
            addSubst x t
            es' <- mapM (\(t :~: t') -> (:~:) <$> subst t <*> subst t') es
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

substMonoEnv :: MonoEnv -> Unify MonoEnv
substMonoEnv m = do
    monoVarMap' <- mapM subst $ monoVarMap m
    return m{ monoVarMap = monoVarMap' }

unify :: [MonoEnv] -> [Ty] -> Unify (MonoEnv, Ty)
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

newtype Infer a = Infer{ unInfer :: ReaderT (TyEnv, PolyEnv) Fresh a }
                deriving (Functor, Applicative, Monad)

runInfer m = curry $ runReaderT $ unInfer m

instance MonadFresh Tv Infer where
    fresh = Infer . lift $ fresh

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
    fixup env@PolyEnv{..} = env{ polyEnvMap = foldr Map.delete polyEnvMap vars }
    vars = monoVars m

runUnify :: Unify a -> Infer a
runUnify u = Infer . lift $ evalStateT (unUnify u) mempty

inferExpr :: Expr -> Infer Typing
inferExpr e = case e of
    EVar x -> do
        mtyping <- lookupPolyVar x
        case mtyping of
            Nothing -> do
                α <- TyVar <$> fresh
                return (MonoEnv $ Map.singleton x α, α)
            Just typing -> return typing
    ECon c -> do
        τ <- lookupCon c
        return (mempty, τ)
    EApp f e -> do
        (m1, τ1) <- inferExpr f
        (m2, τ2) <- inferExpr e
        α <- TyVar <$> fresh
        (m, TyApp (TyApp TyFun _) τ) <- runUnify $ unify [m1, m2] [τ1, TyApp (TyApp TyFun τ2) α]
        return (m, τ)
    ELam pat e -> do
        (mPat, τPat) <- inferPat pat
        (mBody, τBody) <- shadow mPat $ inferExpr e
        (m, τ) <- runUnify $ unify [mPat, mBody] [τPat ~> τBody]
        let m' = removeMonoVars (monoVars mPat) m
        return (m', τ)

inferPat :: Pat -> Infer Typing
inferPat pat = case pat of
    PVar x -> do
        α <- TyVar <$> fresh
        return (MonoEnv $ Map.singleton x α, α)
    PWildcard -> do
        α <- TyVar <$> fresh
        return (mempty, α)
    PCon con pats -> do
        τ0 <- lookupCon con
        (m, τs) <- inferPats pats
        α <- TyVar <$> fresh
        (m', τ) <- runUnify $ unify [m] [τ0, foldr (~>) α τs]
        return (m', tyFunResult τ)

inferPats :: [Pat] -> Infer Typings
inferPats pats = do
    (ms, τs) <- unzip <$> mapM inferPat pats
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
