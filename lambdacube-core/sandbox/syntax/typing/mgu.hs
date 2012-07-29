{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Set.Unicode

import Data.Graph

import Control.Applicative
import Control.Monad hiding (mapM, forM_)
import Data.Monoid
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Prelude hiding (mapM)
import Data.Traversable (mapM)
import Data.Foldable (forM_)
import Data.Stream (Stream(..))
import qualified Data.Stream as Stream

import Control.Monad.Trans
import Control.Monad.State hiding (mapM, forM_)
import Control.Monad.Reader hiding (mapM, forM_)
import Control.Monad.Trans.Writer
import Control.Arrow

type Id = String

type Tv = Id
type TyCon = Id

data Ty = TyCon TyCon
        | TyVar Tv
        | TyApp Ty Ty
        | TyFun

instance Show Ty where
    showsPrec prec ty = case ty of
        TyApp (TyCon "[]") t -> showString "[" . showsPrec 0 t . showString "]"
        TyCon con -> showString con
        TyVar α -> showString α
        TyApp (TyApp TyFun t) u -> paren $ showsPrec 10 t . showString " -> " . showsPrec 0 u
        TyApp t u -> paren $ showsPrec 0 t . showString " " . showsPrec 10 u
      where
        paren = showParen (prec >= 10)


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
    MonoEnv{ monoVarMap = mvs } `mappend` MonoEnv{ monoVarMap = mvs' } = MonoEnv{ monoVarMap = mvs <> mvs' }

monoVars :: MonoEnv -> Set Var
monoVars = Map.keysSet . monoVarMap

removeMonoVars :: Set Var -> MonoEnv -> MonoEnv
removeMonoVars vars m@MonoEnv{..} = m{ monoVarMap = removeFrom monoVarMap }
  where
    removeFrom = Map.filterWithKey (\var _ -> var ∉ vars)

substMonoEnv :: MonoEnv -> Unify MonoEnv
substMonoEnv m = do
    monoVarMap' <- mapM subst $ monoVarMap m
    return m{ monoVarMap = monoVarMap' }

unify :: [MonoEnv] -> [Ty] -> Unify (MonoEnv, Ty)
unify ms τs = do
    α <- fresh
    let eqs = map (TyVar α :~:) τs
        eqs' = concatMap toEqs . Set.toList $ vars
    unifyEqs $ eqs ++ eqs'
    ms' <- mapM substMonoEnv ms
    τ <- substTv α
    return (mconcat ms', τ)
  where
    vars = Set.unions $ map monoVars ms
    toEqs v = case mapMaybe (Map.lookup v . monoVarMap) ms of
        [] -> []
        t:ts -> map (t :~:) ts

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
          | ELet Defs Expr

data Defs = Defs [[Def]] -- NOTE: this assumes the defs are already grouped into SCC's

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
    fixup env@PolyEnv{..} = env{ polyEnvMap = foldr Map.delete polyEnvMap . Set.toList $ vars }
    vars = monoVars m

withPolyVars :: Map Var Typing -> Infer a -> Infer a
withPolyVars vars = Infer . local (second fixup) . unInfer
  where
    fixup env@PolyEnv{..} = env{ polyEnvMap = vars <> polyEnvMap }

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
    ELam pat e -> inferMatch $ Match [pat] e

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
    checkConflicts . map monoVars $ ms
    return (mconcat ms, τs)

inferDefs :: Defs -> Infer (Map Var Typing)
inferDefs (Defs defs) = foldM inferGroup mempty defs
  where
    inferGroup :: Map Var Typing -> [Def] -> Infer (Map Var Typing)
    inferGroup varMap defs = do
        varMap' <- Map.fromList <$> (withPolyVars varMap $ mapM inferDef defs)
        let newVars = Map.keysSet varMap'
            generalize (m, τ) = (removeMonoVars newVars m, τ)
            varMap'' = fmap generalize varMap'
        return varMap''

    inferDef :: Def -> Infer (Var, Typing)
    inferDef def = case def of
        DefVar x e -> do
            (m, τ) <- inferExpr e
            return (x, (m, τ))
        DefFun f matches -> do
            (ms, τs) <- unzip <$> mapM inferMatch matches
            (m, τ) <- runUnify $ unify ms τs
            return (f, (m, τ))

inferMatch :: Match -> Infer Typing
inferMatch (Match pats body) = do
    (mPats, τPats) <- inferPats pats
    (mBody, τBody) <- shadow mPats $ inferExpr body
    (m, τ) <- runUnify $ unify [mBody, mPats] [foldr (~>) τBody τPats]
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

testInfer m = runFresh $ runInfer m (TyEnv tyEnv) (PolyEnv mempty)
  where
    tyEnv = Map.fromList [ ("Nil", tyList a)
                         , ("Con", a ~> tyList a ~> tyList a)
                         ]

    tyList t = TyApp (TyCon "[]") t
    a = TyVar "a"

-- (inactivate-input-method)
-- (activate-input-method "Agda")

foo = testInfer $ inferExpr $ case map2 of
    Match [pat] e -> ELam pat e
      -- inferDefs (Defs [[defMap]])
  where
    defMap = DefFun "map" [map1, map2]

    -- Con x xs -> Con (f x) (map f xs)
    map1 = Match [PCon "Con" [PVar "x", PVar "xs"]] $
           EApp (EApp (ECon "Con") (EApp (EVar "f") (EVar "x")))
                (EApp (EApp (EVar "map") (EVar "f")) (EVar "xs"))
    -- Nil -> Nil
    map2 = Match [PCon "Nil" []] $
           ECon "Nil"
