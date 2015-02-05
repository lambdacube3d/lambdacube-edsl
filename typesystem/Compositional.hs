module Compositional where

--import Debug.Trace
import Data.Maybe
import Text.PrettyPrint.ANSI.Leijen (pretty)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Functor.Identity
import Control.Applicative
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

trace_ _ = id
trace = trace_

data Lit
  = LInt
  | LChar
  | LFloat
  deriving (Show,Eq,Ord)

data PrimFun
  = PAddI
  | PUpper
  | PMulF
  | PShow
  | PRead
  deriving (Show,Eq,Ord)

type Range = (Delta,Delta)
data Exp
  = ELit      Range Lit
  | EPrimFun  Range PrimFun
  | EVar      Range EName
  | EApp      Range Exp Exp
  | ELam      Range EName Exp
  | ELet      Range EName Exp Exp
  | ETuple    Range [Exp]
--  | EFix EName Exp
  deriving (Show,Eq,Ord)

infixr 7 :->
data Ty
  = TVar TName
  | Ty :-> Ty
  -- composit
  | TTuple [Ty]
  | TArray Ty
  -- primitive types
  | TInt
  | TChar
  | TFloat
  | TString
  deriving (Show,Eq,Ord)

data Constraint
  = CNum
  | CTextual
  deriving (Show,Eq,Ord)

instances :: Map Constraint (Set Ty)
instances = Map.fromList [(CNum,Set.fromList [TInt,TFloat])]

type EName = String
type TName = String
type Subst = Map TName Ty

type MonoEnv = Map EName Ty
type PolyEnv = Map EName Typing
type InstEnv = [(Constraint,Ty)]
type Typing = (MonoEnv,InstEnv,Ty)
type Env = (PolyEnv,MonoEnv,InstEnv)

{-
  ->>
  perdicate resolution operator

  O
  instance environment

  +
  e.g. O + O'
  substitution-resolution-combinator

  typing = type + mono env + instance env
-}


inferPrimFun :: PrimFun -> Unique Typing
inferPrimFun a = case a of
  PAddI -> do
    t <- newVar
    return (mempty,[(CNum,t)],t :-> t :-> t)
  PUpper -> return (mempty,mempty,TChar :-> TChar)
  PMulF -> return (mempty,mempty,TFloat :-> TFloat :-> TFloat)
  PShow -> do
    t <- newVar
    return (mempty,[(CTextual,t)],t :-> TString)
  PRead -> do
    t <- newVar
    return (mempty,[(CTextual,t)],TString :-> t)

inferLit :: Lit -> Unique Typing
inferLit a = case a of
  LInt    -> do
    t <- newVar
    return (mempty,[(CNum,t)],t)
  LChar   -> return (mempty,mempty,TChar)
  LFloat  -> return (mempty,mempty,TFloat)

type Unique a = StateT (Int,ByteString,[Range]) (Except String) a

getRange :: Exp -> Range
getRange (ELit      r _) = r
getRange (EPrimFun  r _) = r
getRange (EVar      r _) = r
getRange (EApp      r _ _) = r
getRange (ELam      r _ _) = r
getRange (ELet      r _ _ _) = r

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
      fullCode = True
      mkSpan (s,e) = unlines [show $ pretty (s,e), if fullCode then BS.unpack str else show $ pretty r]
        where
          r = render spn
          str = x -- <> BS.takeWhile (\a -> notElem a ['\n','\r']) y
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

applyInstEnv :: Subst -> InstEnv -> Unique InstEnv -- TODO: check constraints, remove redundant superclass
applyInstEnv s e = return $ (trace_ (show (s,e,"->",e'))) e'
 where
  e' = fmap (\(c,t) -> (c,applyTy s t)) e

joinInstEnv :: [InstEnv] -> InstEnv
joinInstEnv e = Set.toList . Set.unions . map Set.fromList $ e

freeVarsTy :: Ty -> Set TName
freeVarsTy (TVar a) = Set.singleton a
freeVarsTy (a :-> b) = freeVarsTy a `mappend` freeVarsTy b
freeVarsTy (TTuple a) = foldl mappend mempty $ map freeVarsTy a
freeVarsTy (TArray a) = freeVarsTy a
freeVarsTy _ = mempty

freeVarsMonoEnv :: MonoEnv -> Set TName
freeVarsMonoEnv m = F.foldMap freeVarsTy m

freeVarsInstEnv :: InstEnv -> Set TName
freeVarsInstEnv i = F.foldMap (freeVarsTy . snd) i

-- union mono envs matching on intersection
joinMonoEnv :: MonoEnv -> MonoEnv -> Unique MonoEnv
joinMonoEnv a b = do
  let merge k ml mr = do
        l <- ml
        r <- mr
        if l == r then ml else throwErrorUnique $ k ++ " mismatch " ++ show l ++ " with " ++ show r
  T.sequence $ Map.unionWithKey merge (fmap return a) (fmap return b)

instTyping :: Typing -> Unique Typing
instTyping (m,i,t) = do
  let fv = freeVarsTy t `mappend` freeVarsMonoEnv m -- `mappend` freeVarsInstEnv i
  newVars <- replicateM (Set.size fv) newVar
  let s = Map.fromList $ zip (Set.toList fv) newVars
  i' <- applyInstEnv s i
  return (applyMonoEnv s m,i',applyTy s t)

bindVar :: TName -> Ty -> Unique Subst
bindVar n t
  | TVar n == t = return mempty
  | n `Set.member` freeVarsTy t = throwErrorUnique $ "Infinite type, type variable " ++ n ++ " occurs in " ++ show t
  | otherwise = return $ Map.singleton n t

compose :: Subst -> Subst -> Subst
compose b a = mappend a $ applyTy a <$> b

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

unifyEqs :: [(Ty,Ty)] -> Unique Subst
unifyEqs eqs = do
  let uniTy s (a,b) = do
        s' <- unifyTy (applyTy s a) (applyTy s b)
        return $ s `compose` s'
  foldM uniTy mempty eqs

unify :: [MonoEnv] -> [Ty] -> Unique Subst
unify ml tl = do
  a <- newVar
  let toEqs :: EName -> [(Ty,Ty)]
      toEqs v = case mapMaybe (Map.lookup v) ml of
        [] -> []
        x:xs -> map ((,) x) xs

      vars :: Set EName
      vars = mconcat . map Map.keysSet $ ml

      varEqs :: [(Ty,Ty)]
      varEqs = concatMap toEqs . Set.toList $ vars

      tyEqs :: [(Ty,Ty)]
      tyEqs = map ((,) a) tl

  unifyEqs $ tyEqs ++ varEqs

prune :: Typing -> Typing
prune (m,i,t) = (m,i',t)
 where
  v = Set.map TVar $ freeVarsTy t `mappend` freeVarsMonoEnv m
  i' = filter (\(_,a) -> Set.member a v) i

unamb :: PolyEnv -> Typing -> Unique ()
unamb env (m,i,t) = do
  let v = Set.map TVar $ freeVarsTy t `mappend` freeVarsMonoEnv m
  return ()
  forM_ i $ \(_,a) -> if Set.member a v then return () else throwErrorUnique $ unlines ["ambiguous type: " ++ show (i,t),"env: " ++ show m, "free vars: " ++ show v, "poly env: " ++ show env]

infer :: PolyEnv -> Exp -> Unique Typing
infer penv (ETuple r t) = withRanges [r] $ do
  (ml,il,tl) <- unzip3 <$> mapM (infer penv) t
  s <- unify ml []
  m <- foldM (\a b -> joinMonoEnv (applyMonoEnv s a) (applyMonoEnv s b)) mempty ml
  i <- joinInstEnv <$> mapM (applyInstEnv s) il
  return (m,i,TTuple $ map (applyTy s) tl)
infer penv (ELit r l) = withRanges [r] $ inferLit l
infer penv (EPrimFun r f) = withRanges [r] $ inferPrimFun f
infer penv (EVar r n) = withRanges [r] $ case Map.lookup n penv of
  Nothing -> do
    t <- trace "mono var" <$> newVar
    return (Map.singleton n t,mempty,t)
  Just t -> trace "poly var" <$> instTyping t
infer penv (ELam r n f) = withRanges [r] $ do
  (m,i,t) <- infer penv f
  case Map.lookup n m of
    Nothing -> do
      a <- newVar
      return (m,i,a :-> t)
    Just a -> return (Map.delete n m,i,a :-> t)
infer penv (EApp r f a) = withRanges [r] $ do
  (m1,i1,t1) <- infer penv f
  (m2,i2,t2) <- infer penv a
  a <- newVar
  s <- unify [m1,m2] [t1,t2 :-> a]
  m3 <- joinMonoEnv (applyMonoEnv s m1) (applyMonoEnv s m2)
  i3 <- (\a1 a2 -> joinInstEnv [a1,a2]) <$> applyInstEnv s i1 <*> applyInstEnv s i2
  unamb penv (m3,i3,applyTy s a)
  return (m3,i3,applyTy s a)

--- bugfix begin
infer penv (ELet r n x e) = withRanges [r] $ do
  d1@(m1,i1,t1) <- infer penv x
  s0 <- unify [m1] [t1]
  let m0 = Map.delete n $ applyMonoEnv s0 m1
      t0 = applyTy s0 t1
  i0 <- trace_ "1" <$> applyInstEnv s0 i1
  trace (show ("m1",m1,"let1",d1,"let2",(m0,i0,t0))) $ unamb penv (m0,i0,t0)
  let penv' = Map.insert n (m0,i0,t0) penv
  (m',i',t') <- infer penv' e
  s_ <- unify [m0,m'] []
  let s = s0 `compose` s_
  m <- joinMonoEnv (applyMonoEnv s m') (applyMonoEnv s m0)
  a1 <- trace_ "2" <$> applyInstEnv s i'
  a2 <- trace_ "3" <$> applyInstEnv s i0
  let i = joinInstEnv [a1,a2]
  return $ prune $ trace (show ("s",s,"penv",penv',"in",(m',i',t'))) $ trace_ (show $ ("s0",s0,m1,"s",s,m0,m')) $ (m,i,applyTy s t')
--- bugfix end

inference :: ByteString -> Exp -> Either String (InstEnv,Ty)
inference src e = case scopeChk src e of
  Left m  -> Left m
  Right () -> runIdentity $ runExceptT $ (flip evalStateT) (0,src,[]) act
   where
    act = do
      a@(m,i,t) <- infer mempty e
      unamb mempty a
      return (i,t)

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
  , ELet spn "a" (ELit spn LInt) (EVar spn "a")
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
