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
--  | EPrimFun PrimFun
  | EVar EName
  | EApp Exp Exp
  | ELam EName Exp
--  | ELet EName Exp Exp
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

-- TODO
joinMonoEnv :: MonoEnv -> MonoEnv -> Unique MonoEnv
joinMonoEnv = undefined

instTyping :: Typing -> Unique Typing
instTyping = undefined

unify :: [MonoEnv] -> [(Ty,Ty)] -> Unique Subst
unify = undefined
{-
unify (TVar u) t = bindVar u t
unify t (TVar u) = bindVar u t
unify (a1 :-> b1) (a2 :-> b2) = do
  s1 <- unify a1 a2
  s2 <- unify (apply b1 s1) (apply b2 s1)
  return $ s1 `compose` s2
unify a b
  | a == b = return mempty
  | otherwise = throwError $ "can not unify " ++ show a ++ " with " ++ show b
-}
-- END TODO

infer :: PolyEnv -> Exp -> Unique Typing
infer penv (ELit l) = return (mempty,inferLit l)
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

inference :: Exp -> Either String Ty
inference e = case scopeChk e of
  Just m  -> Left m
  Nothing -> runIdentity $ runExceptT $ (flip evalStateT) 0 act
   where
    act = do
      (m,t) <- infer mempty e
      return t

-- scope checking
scopeCheck :: Set EName -> Exp -> Maybe String
scopeCheck vars (EVar n) = if Set.member n vars then Nothing else Just $ "Variable " ++ n ++ " is not in scope."
scopeCheck vars (EApp f a) = scopeCheck vars f >> scopeCheck vars a
scopeCheck vars (ELam n f) = if Set.notMember n vars then scopeCheck (Set.insert n vars) f else Just $ "Variable name clash: " ++ n
scopeCheck vars _ = Nothing

scopeChk :: Exp -> Maybe String
scopeChk e = scopeCheck mempty e

-- test
ok =
  [ ELit LInt
  , ELam "x" $ EVar "x"
  , ELam "x" $ ELam "y" $ ELit LFloat
  , ELam "x" $ EApp (EVar "x") (ELit LBool)
--  , ELam "x" $ EApp (EApp (EPrimFun PAddI) (ELit LInt)) (EVar "x")
--  , ELet "id" (ELam "x" $ EVar "x") (ELet "a" (EApp (EVar "id") (ELit LBool)) (EApp (EVar "id") (ELit LBool)))
--  , ELet "id" (ELam "x" $ EVar "x") (ELet "a" (EApp (EVar "id") (ELit LBool)) (EApp (EVar "id") (ELit LFloat)))
--  , EFix "f" $ ELam "x" $ EApp (EVar "f") $ ELit LFloat
  ]
err =
  [ ELam "x" $ EApp (EVar "x") (EVar "x")
  , EApp (ELit LInt) (ELit LInt)
--  , EFix "f" $ ELam "x" $ EApp (EVar "f") $ EVar "f"
  ]

test = do
  putStrLn "Ok:"
  mapM_ (\e -> print e >> (print . inference $ e)) ok
  putStrLn "Error:"
  mapM_ (\e -> print e >> (print . inference $ e)) err

