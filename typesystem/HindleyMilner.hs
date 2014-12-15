import Data.Functor.Identity
import Control.Monad.Except
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Monoid
import Data.Functor

data Lit
  = LInt
  | LBool
  | LFloat
  deriving (Show,Eq,Ord)

data PrimFun
  = PAddI
  | PAnd
  | PMulF
  deriving (Show,Eq,Ord)

data Exp
  = ELit Lit
  | EPrimFun PrimFun
  | EVar EName
  | EApp Exp Exp
  | ELam EName Exp
  | ELet EName Exp Exp
  deriving (Show,Eq,Ord)

infixr 7 :->
data Ty
  = TVar TName
  | Ty :-> Ty
  -- primitive types
  | TInt
  | TBool
  | TFloat
  deriving (Show,Eq,Ord)

data Scheme = Scheme (Set TName) Ty

type EName = String
type TName = String
type Env = Map EName Scheme
type Subst = Map TName Ty

inferPrimFun :: PrimFun -> Ty
inferPrimFun a = case a of
  PAddI   -> TInt :-> TInt :-> TInt
  PAnd    -> TBool :-> TBool :-> TBool
  PMulF   -> TFloat :-> TFloat :-> TFloat

inferLit :: Lit -> Ty
inferLit a = case a of
  LInt    -> TInt
  LBool   -> TBool
  LFloat  -> TFloat

type Unique a = StateT Int (Except String) a

newVar :: Unique Ty
newVar = do
  n <- get
  put (n+1)
  return $ TVar $ 't':show n

applyEnv :: Env -> Subst -> Env
applyEnv e s = fmap (flip applyScheme s) e

applyScheme :: Scheme -> Subst -> Scheme
applyScheme (Scheme vars t) s = Scheme vars (apply t (Map.filterWithKey (\k _ -> Set.notMember k vars) s))

freeVarsScheme :: Scheme -> Set TName
freeVarsScheme (Scheme vars t) = freeVars t `Set.difference` vars

freeVarsEnv :: Env -> Set TName
freeVarsEnv env = Set.unions . Map.elems . fmap freeVarsScheme $ env

generalize :: Env -> Ty -> Scheme
generalize env t = Scheme (freeVars t `Set.difference` freeVarsEnv env) t

instantiate :: Scheme -> Unique Ty
instantiate (Scheme vars t) = do
  let l = Set.toList vars
  s <- mapM (\_ -> newVar) l
  return $ apply t $ Map.fromList $ zip l s

apply :: Ty -> Subst -> Ty
apply (TVar a) st = case Map.lookup a st of
  Nothing -> TVar a
  Just t  -> t
apply (a :-> b) st = (apply a st) :-> (apply b st)
apply t _ = t

unify :: Ty -> Ty -> Unique Subst
unify (TVar u) t = bindVar u t
unify t (TVar u) = bindVar u t
unify (a1 :-> b1) (a2 :-> b2) = do
  s1 <- unify a1 a2
  s2 <- unify (apply b1 s1) (apply b2 s1)
  return $ s1 `compose` s2
unify a b
  | a == b = return mempty
  | otherwise = throwError $ "can not unify " ++ show a ++ " with " ++ show b

freeVars :: Ty -> Set TName
freeVars (TVar a) = Set.singleton a
freeVars (a :-> b) = freeVars a `mappend` freeVars b
freeVars _ = mempty

bindVar :: TName -> Ty -> Unique Subst
bindVar n t
  | n `Set.member` freeVars t = throwError $ "Infinite type, type variable " ++ n ++ " occurs in " ++ show t
  | TVar n == t = return mempty
  | otherwise = return $ Map.singleton n t

compose :: Subst -> Subst -> Subst
compose a b = mappend a $ (flip apply) a <$> b

remove :: EName -> Env -> Env
remove n e = Map.delete n e

infer :: Env -> Exp -> Unique (Subst,Ty)
infer env (EPrimFun f) = return (mempty,inferPrimFun f)
infer env (ELit l) = return (mempty,inferLit l)
infer env (EVar n) = case Map.lookup n env of
  Nothing -> throwError $ "unbounded variable: " ++ n
  Just t  -> do
    t' <- instantiate t
    return (mempty,t')
infer env (EApp f a) = do
  (s1,t1) <- infer env f
  (s2,t2) <- infer env a
  tv <- newVar
  s3 <- unify (apply t1 s2) (t2 :-> tv)
  return (s1 `compose` s2 `compose` s3, apply tv s3)
infer env (ELam n e) = do
  tv <- newVar
  (s1,tbody) <- infer (Map.insert n (Scheme mempty tv) env) e
  return (s1,(apply tv s1) :-> tbody)
infer env (ELet n e1 e2) = do
  (s1,t1) <- infer env e1
  let env' = applyEnv (Map.insert n (generalize env t1) env) s1
  (s2,t2) <- infer env' e2
  return (s1 `compose` s2,t2)

inference :: Exp -> Either String Ty
inference e = runIdentity $ runExceptT $ (flip evalStateT) 0 act
 where
  act = do
    (s,t) <- infer mempty e
    return (apply t s)

-- test
ok =
  [ ELit LInt
  , ELam "x" $ EVar "x"
  , ELam "x" $ ELam "y" $ ELit LFloat
  , ELam "x" $ EApp (EVar "x") (ELit LBool)
  , ELam "x" $ EApp (EApp (EPrimFun PAddI) (ELit LInt)) (EVar "x")
  , ELet "id" (ELam "x" $ EVar "x") (ELet "a" (EApp (EVar "id") (ELit LBool)) (EApp (EVar "id") (ELit LBool)))
  ]
err =
  [ ELam "x" $ EApp (EVar "x") (EVar "x")
  , EApp (ELit LInt) (ELit LInt)
  , ELet "id" (ELam "x" $ EVar "x") (ELet "a" (EApp (EVar "id") (ELit LBool)) (EApp (EVar "id") (ELit LFloat)))
  ]

test = do
  putStrLn "Ok:"
  mapM_ (\e -> print e >> (print . inference $ e)) ok
  putStrLn "Error:"
  mapM_ (\e -> print e >> (print . inference $ e)) err

