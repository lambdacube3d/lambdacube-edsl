import Data.Functor.Identity
import Control.Monad.Except
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Monoid
import Data.Functor
import qualified Data.Traversable as T
import qualified Data.Foldable as F

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
--  | EFix EName Exp
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

type EName = String
type TName = String
type Subst = Map TName Ty

type MonoEnv = Map EName Ty
type PolyEnv = Map EName Typing
type Typing = (MonoEnv,Ty)
type Env = (PolyEnv,MonoEnv)

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

applyTy :: Subst -> Ty -> Ty
applyTy st (TVar a) = case Map.lookup a st of
  Nothing -> TVar a
  Just t  -> t
applyTy st (a :-> b) = (applyTy st a) :-> (applyTy st b)
applyTy _ t = t

applyMonoEnv :: Subst -> MonoEnv -> MonoEnv
applyMonoEnv s e = fmap (applyTy s) e

freeVarsTy :: Ty -> Set TName
freeVarsTy (TVar a) = Set.singleton a
freeVarsTy (a :-> b) = freeVarsTy a `mappend` freeVarsTy b
freeVarsTy _ = mempty

freeVarsMonoEnv :: MonoEnv -> Set TName
freeVarsMonoEnv m = F.foldMap freeVarsTy m

-- union mono envs matching on intersection
joinMonoEnv :: MonoEnv -> MonoEnv -> Unique MonoEnv
joinMonoEnv a b = do
  let merge k ml mr = do
        l <- ml
        r <- mr
        if l == r then ml else throwError $ k ++ " mismatch " ++ show l ++ " with " ++ show r
  T.sequence $ Map.unionWithKey merge (fmap return a) (fmap return b)

instTyping :: Typing -> Unique Typing
instTyping (m,t) = do
  let fv = freeVarsTy t `mappend` freeVarsMonoEnv m
  newVars <- replicateM (Set.size fv) newVar
  let s = Map.fromList $ zip (Set.toList fv) newVars
  return (applyMonoEnv s m,applyTy s t)

bindVar :: TName -> Ty -> Unique Subst
bindVar n t
  | TVar n == t = return mempty
  | n `Set.member` freeVarsTy t = throwError $ "Infinite type, type variable " ++ n ++ " occurs in " ++ show t
  | otherwise = return $ Map.singleton n t

compose :: Subst -> Subst -> Subst
compose a b = mappend a $ applyTy a <$> b

unifyTy :: Ty -> Ty -> Unique Subst
unifyTy (TVar u) t = bindVar u t
unifyTy t (TVar u) = bindVar u t
unifyTy (a1 :-> b1) (a2 :-> b2) = do
  s1 <- unifyTy a1 a2
  s2 <- unifyTy (applyTy s1 b1) (applyTy s1 b2)
  return $ s1 `compose` s2
unifyTy a b
  | a == b = return mempty
  | otherwise = throwError $ "can not unify " ++ show a ++ " with " ++ show b

unify :: [MonoEnv] -> [(Ty,Ty)] -> Unique Subst
unify ml tl = do
  s1 <- foldM (\s (a,b) -> unifyTy (applyTy s a) (applyTy s b)) mempty tl
  let ks = Set.unions $ map Map.keysSet ml
  vars <- T.sequence $ Map.fromSet (const newVar) ks
  let uni s (k,v) = do
        let vars' = applyMonoEnv s vars
        a <- case Map.lookup k vars' of
          Nothing -> throwError $ "internal error - unknown key: " ++ k
          Just t  -> return t
        s' <- unifyTy a v
        return $ s `compose` s'
  foldM uni s1 $ concatMap Map.toList ml

infer :: PolyEnv -> Exp -> Unique Typing
infer penv (ELit l) = return (mempty,inferLit l)
infer penv (EPrimFun f) = return (mempty,inferPrimFun f)
infer penv (EVar n) = case Map.lookup n penv of
  Nothing -> do
    t <- newVar
    return (Map.singleton n t,t)
  Just t -> instTyping t
infer penv (ELam n f) = do
  (m,t) <- infer penv f
  case Map.lookup n m of
    Nothing -> do
      a <- newVar
      return (m,a :-> t)
    Just a -> return (Map.delete n m,a :-> t)
infer penv (EApp f a) = do
  (m1,t1) <- infer penv f
  (m2,t2) <- infer penv a
  a <- newVar
  s <- unify [m1,m2] [(t1,t2 :-> a)]
  m3 <- joinMonoEnv (applyMonoEnv s m1) (applyMonoEnv s m2)
  return (m3,applyTy s a)
infer penv (ELet n x e) = do
  (m1,t1) <- infer penv x
  a <- newVar
  s0 <- unify [m1] [(t1,a)] -- TODO
  let m0 = Map.delete n $ applyMonoEnv s0 m1
      penv' = Map.insert n (m0,applyTy s0 a) penv
  (m',t') <- infer penv' e
  s <- unify [m0,m'] []
  m <- joinMonoEnv (applyMonoEnv s m') (applyMonoEnv s m0)
  return (m,applyTy s t')

inference :: Exp -> Either String Ty
inference e = case scopeChk e of
  Left m  -> Left m
  Right () -> runIdentity $ runExceptT $ (flip evalStateT) 0 act
   where
    act = do
      (m,t) <- infer mempty e
      return t

-- scope checking
scopeCheck :: Set EName -> Exp -> Either String ()
scopeCheck vars (EVar n) = if Set.member n vars then return () else throwError $ "Variable " ++ n ++ " is not in scope."
scopeCheck vars (EApp f a) = scopeCheck vars f >> scopeCheck vars a
scopeCheck vars (ELam n f) = if Set.notMember n vars then scopeCheck (Set.insert n vars) f else throwError $ "Variable name clash: " ++ n
scopeCheck vars (ELet n x e) = do
  let vars' = Set.insert n vars
  if Set.notMember n vars then scopeCheck vars' x >> scopeCheck vars' e else throwError $ "Variable name clash: " ++ n
scopeCheck vars _ = return ()

scopeChk :: Exp -> Either String ()
scopeChk e = scopeCheck mempty e

-- test
ok =
  [ ELit LInt
  , ELam "x" $ EVar "x"
  , ELam "x" $ ELam "y" $ ELit LFloat
  , ELam "x" $ EApp (EVar "x") (ELit LBool)
  , ELam "x" $ EApp (EApp (EPrimFun PAddI) (ELit LInt)) (EVar "x")
  , ELet "id" (ELam "x" $ EVar "x") (ELet "a" (EApp (EVar "id") (ELit LBool)) (EApp (EVar "id") (ELit LBool)))
  , ELet "id" (ELam "x" $ EVar "x") (ELet "a" (EApp (EVar "id") (ELit LBool)) (EApp (EVar "id") (ELit LFloat)))
  , ELet "f" (ELam "x" $ EApp (EApp (EPrimFun PAddI) (ELit LInt)) (EVar "x")) (EVar "f")
--  , EFix "f" $ ELam "x" $ EApp (EVar "f") $ ELit LFloat
  ]
err =
  [ ELam "x" $ EApp (EVar "x") (EVar "x")
  , EApp (ELit LInt) (ELit LInt)
  , ELet "f" (ELam "x" $ EApp (EApp (EPrimFun PAddI) (ELit LInt)) (EVar "x")) (EApp (EVar "f") (ELit LBool))
  , ELet "f1" (ELam "x" $ EApp (EApp (EPrimFun PAddI) (ELit LInt)) (EVar "x")) (EVar "f")
--  , EFix "f" $ ELam "x" $ EApp (EVar "f") $ EVar "f"
  ]

test = do
  putStrLn "Ok:"
  mapM_ (\e -> print e >> (print . inference $ e)) ok
  putStrLn "Error:"
  mapM_ (\e -> print e >> (print . inference $ e)) err
