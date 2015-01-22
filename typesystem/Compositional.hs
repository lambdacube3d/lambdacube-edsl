module Compositional where

import Text.PrettyPrint.ANSI.Leijen (pretty)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
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
import Text.Trifecta hiding (err)
import Text.Trifecta.Delta

data Lit
  = LInt
  | LChar
  | LFloat
  deriving (Show,Eq,Ord)

data PrimFun
  = PAddI
  | PUpper
  | PMulF
  deriving (Show,Eq,Ord)

type Range = (Delta,Delta)
data Exp
  = ELit      Range Lit
  | EPrimFun  Range PrimFun
  | EVar      Range EName
  | EApp      Range Exp Exp
  | ELam      Range EName Exp
  | ELet      Range EName Exp Exp
--  | EFix EName Exp
  deriving (Show,Eq,Ord)

infixr 7 :->
data Ty
  = TVar TName
  | Ty :-> Ty
  -- primitive types
  | TInt
  | TChar
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
  PUpper  -> TChar :-> TChar
  PMulF   -> TFloat :-> TFloat :-> TFloat

inferLit :: Lit -> Ty
inferLit a = case a of
  LInt    -> TInt
  LChar   -> TChar
  LFloat  -> TFloat

type Unique a = StateT (Int,ByteString,[Range]) (Except String) a

withRanges :: [Range] -> Unique a -> Unique a
withRanges rl a = do
  (x,y,rl0) <- get
  put (x,y,rl)
  res <- a
  (z,q,_) <- get
  put (z,q,rl0)
  return res

throwErrorUnique :: String -> Unique a
throwErrorUnique s = do
  (_,src,rl) <- get
  throwErrorSrc src rl s

throwErrorSrc src rl s = do
  let sl = map mkSpan rl
      mkSpan (s,e) = unlines [show $ pretty s, {-BS.unpack str-}show $ pretty r]
        where
          r = render spn
          str = x <> BS.takeWhile (\a -> notElem a ['\n','\r']) y
          spn = Span s e str
          (x,y) = BS.splitAt (se - sb) $ BS.drop sb src
          b = rewind s
          sb = fromIntegral $ bytes b
          se = fromIntegral $ bytes e
  throwError $ concat sl ++ s

newVar :: Unique Ty
newVar = do
  (n,s,r) <- get
  put (n+1,s,r)
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
        if l == r then ml else throwErrorUnique $ k ++ " mismatch " ++ show l ++ " with " ++ show r
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
  | n `Set.member` freeVarsTy t = throwErrorUnique $ "Infinite type, type variable " ++ n ++ " occurs in " ++ show t
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
  | otherwise = throwErrorUnique $ "can not unify " ++ show a ++ " with " ++ show b

unify :: [MonoEnv] -> [(Ty,Ty)] -> Unique Subst
unify ml tl = do
  s1 <- foldM (\s (a,b) -> unifyTy (applyTy s a) (applyTy s b)) mempty tl
  let ks = Set.unions $ map Map.keysSet ml
  vars <- T.sequence $ Map.fromSet (const newVar) ks
  let uni s (k,v) = do
        let vars' = applyMonoEnv s vars
        a <- case Map.lookup k vars' of
          Nothing -> throwErrorUnique $ "internal error - unknown key: " ++ k
          Just t  -> return t
        s' <- unifyTy a v
        return $ s `compose` s'
  foldM uni s1 $ concatMap Map.toList ml

infer :: PolyEnv -> Exp -> Unique Typing
infer penv (ELit r l) = withRanges [r] $ return (mempty,inferLit l)
infer penv (EPrimFun r f) = withRanges [r] $ return (mempty,inferPrimFun f)
infer penv (EVar r n) = withRanges [r] $ case Map.lookup n penv of
  Nothing -> do
    t <- newVar
    return (Map.singleton n t,t)
  Just t -> instTyping t
infer penv (ELam r n f) = withRanges [r] $ do
  (m,t) <- infer penv f
  case Map.lookup n m of
    Nothing -> do
      a <- newVar
      return (m,a :-> t)
    Just a -> return (Map.delete n m,a :-> t)
infer penv (EApp r f a) = withRanges [r] $ do
  (m1,t1) <- infer penv f
  (m2,t2) <- infer penv a
  a <- newVar
  s <- unify [m1,m2] [(t1,t2 :-> a)]
  m3 <- joinMonoEnv (applyMonoEnv s m1) (applyMonoEnv s m2)
  return (m3,applyTy s a)
infer penv (ELet r n x e) = withRanges [r] $ do
  (m1,t1) <- infer penv x
  a <- newVar
  s0 <- unify [m1] [(t1,a)] -- TODO
  let m0 = Map.delete n $ applyMonoEnv s0 m1
      penv' = Map.insert n (m0,applyTy s0 a) penv
  (m',t') <- infer penv' e
  s <- unify [m0,m'] []
  m <- joinMonoEnv (applyMonoEnv s m') (applyMonoEnv s m0)
  return (m,applyTy s t')

inference :: ByteString -> Exp -> Either String Ty
inference src e = case scopeChk src e of
  Left m  -> Left m
  Right () -> runIdentity $ runExceptT $ (flip evalStateT) (0,src,[]) act
   where
    act = do
      (m,t) <- infer mempty e
      return t

-- scope checking
scopeCheck :: ByteString -> Set EName -> Exp -> Either String ()
scopeCheck src vars (EVar r n) = if Set.member n vars then return () else throwErrorSrc src [r] $ "Variable " ++ n ++ " is not in scope."
scopeCheck src vars (EApp r f a) = scopeCheck src vars f >> scopeCheck src vars a
scopeCheck src vars (ELam r n f) = if Set.notMember n vars then scopeCheck src (Set.insert n vars) f else throwErrorSrc src [r] $ "Variable name clash: " ++ n
scopeCheck src vars (ELet r n x e) = do
  let vars' = Set.insert n vars
  if Set.notMember n vars then scopeCheck src vars' x >> scopeCheck src vars' e else throwErrorSrc src [r] $ "Variable name clash: " ++ n
scopeCheck src vars _ = return ()

scopeChk :: ByteString -> Exp -> Either String ()
scopeChk src e = scopeCheck src mempty e

-- test
spn = (mempty,mempty)

ok =
  [ ELit spn LInt
  , ELam spn "x" $ EVar spn "x"
  , ELam spn "x" $ ELam spn "y" $ ELit spn LFloat
  , ELam spn "x" $ EApp spn (EVar spn "x") (ELit spn LChar)
  , ELam spn "x" $ EApp spn (EApp spn (EPrimFun spn PAddI) (ELit spn LInt)) (EVar spn "x")
  , ELet spn "id" (ELam spn "x" $ EVar spn "x") (ELet spn "a" (EApp spn (EVar spn "id") (ELit spn LChar)) (EApp spn (EVar spn "id") (ELit spn LChar)))
  , ELet spn "id" (ELam spn "x" $ EVar spn "x") (ELet spn "a" (EApp spn (EVar spn "id") (ELit spn LChar)) (EApp spn (EVar spn "id") (ELit spn LFloat)))
  , ELet spn "f" (ELam spn "x" $ EApp spn (EApp spn (EPrimFun spn PAddI) (ELit spn LInt)) (EVar spn "x")) (EVar spn "f")
--  , EFix "f" $ ELam "x" $ EApp (EVar "f") $ ELit LFloat
  ]
err =
  [ ELam spn "x" $ EApp spn (EVar spn "x") (EVar spn "x")
  , EApp spn (ELit spn LInt) (ELit spn LInt)
  , ELet spn "f" (ELam spn "x" $ EApp spn (EApp spn (EPrimFun spn PAddI) (ELit spn LInt)) (EVar spn "x")) (EApp spn (EVar spn "f") (ELit spn LChar))
  , ELet spn "f1" (ELam spn "x" $ EApp spn (EApp spn (EPrimFun spn PAddI) (ELit spn LInt)) (EVar spn "x")) (EVar spn "f")
--  , EFix "f" $ ELam "x" $ EApp (EVar "f") $ EVar "f"
  ]

test = do
  putStrLn "Ok:"
  mapM_ (\e -> print e >> (print . inference mempty $ e)) ok
  putStrLn "Error:"
  mapM_ (\e -> print e >> (print . inference mempty $ e)) err
