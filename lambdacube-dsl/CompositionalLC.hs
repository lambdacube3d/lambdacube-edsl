module CompositionalLC where

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
  = LInt    Integer
  | LChar   Char
  | LString String
  | LFloat  Double
  deriving (Show,Eq,Ord)

data PrimFun
  -- temp
  = PAddI
  | PUpper
  | PMulF
  | PShow
  | PRead
  -- lc prims
  | PAccumulate
  | PAccumulationContext
  | PColorImage
  | PColorOp
  | PCullNone
  | PFetch
  | PFragmentOutRastDepth
  | PFrameBuffer
  | PIV4F
  | PLastVertex
  | PNoBlending
  | PNoOffset
  | PPassAll
  | PPolygonFill
  | PRasterize
  | PScreenOut
  | PSmooth
  | PTransform
  | PTriangleCtx
  | PTriangles
  | PV4
  | PVertexOut
  | Pone
  | PMulMV
  | PUni
  | PIM44F
  deriving (Show,Eq,Ord)

type Range = (Delta,Delta)
data Exp a
  = ELit      a Lit
  | EPrimFun  a PrimFun
  | EVar      a EName
  | EApp      a (Exp a) (Exp a)
  | ELam      a EName (Exp a)
  | ELet      a EName (Exp a) (Exp a)
  | ETuple    a [Exp a]
--  | EFix EName Exp
  deriving (Show,Eq,Ord)

data Frequency -- frequency kind
  -- frequency values
  = C
  | O
  | V
  | F
  -- type family representation
  | FVar TName
  | FMax [Frequency]
  deriving (Show,Eq,Ord)

infixr 7 ~>
a ~> b = TArr C a b

data Ty -- star kind
  = TVar    Frequency TName
  | TArr    Frequency Ty Ty -- ????
  -- composit
  | TTuple  Frequency [Ty]
  | TArray  Frequency Ty
  -- primitive types
  | TInt    Frequency
  | TChar   Frequency
  | TFloat  Frequency
  | TString Frequency
  -- lambdacube types
  | TM44F   Frequency
  | TV4F    Frequency
  | TImage  Frequency
  | TFrameBuffer  Frequency
  | TOutput Frequency
  | TRasterContext Frequency
  | TCullMode Frequency
  | TPolygonMode Frequency
  | TPolygonOffset Frequency
  | TProvokingVertex Frequency
  | TAccumulationContext Frequency
  | TFragmentOperation Frequency
  | TBlending Frequency
  | TVertexOut Frequency
  | TInterpolated Frequency
  | TFetchPrimitive Frequency
  | TInput Frequency
  | TVertexStream Frequency
  | TPrimitiveStream Frequency
  | TFragmentStream Frequency
  | TFragmentOut Frequency
  | TFragmentFilter Frequency
  deriving (Show,Eq,Ord)

data Constraint
  = CNum
  | CTextual
  deriving (Show,Eq,Ord)

instances :: Map Constraint (Set Ty)
instances = Map.fromList [(CNum,Set.fromList [TInt C,TFloat C])]

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
    t <- newVar C
    return (mempty,[(CNum,t)],t ~> t ~> t)
  PUpper -> return (mempty,mempty,TChar C ~> TChar C)
  PMulF -> return (mempty,mempty,TFloat C ~> TFloat C ~> TFloat C)
  PShow -> do
    t <- newVar C
    return (mempty,[(CTextual,t)],t ~> TString C)
  PRead -> do
    t <- newVar C
    return (mempty,[(CTextual,t)],TString C ~> t)
  PMulMV -> do
    return (mempty,mempty,TM44F C ~> TV4F C ~> TV4F C)
  PV4 -> do
    return (mempty,mempty,TFloat C ~> TFloat C ~> TFloat C ~> TFloat C ~> TV4F C)
  PColorImage -> do
    return (mempty,mempty,TInt C ~> TV4F C ~> TImage C)
  PFrameBuffer -> do
    return (mempty,mempty,TImage C ~> TFrameBuffer C)
  PScreenOut -> do
    return (mempty,mempty,TFrameBuffer C ~> TOutput C)
  PTriangleCtx -> do
    return (mempty,mempty,TCullMode C ~> TPolygonMode C ~> TPolygonOffset C ~> TProvokingVertex C ~> TRasterContext C)
  PCullNone -> do
    return (mempty,mempty,TCullMode C)
  PPolygonFill -> do
    return (mempty,mempty,TPolygonMode C)
  PNoOffset -> do
    return (mempty,mempty,TPolygonOffset C)
  PLastVertex -> do
    return (mempty,mempty,TProvokingVertex C)
  PAccumulationContext -> do
    return (mempty,mempty,TFragmentOperation C ~> TAccumulationContext C)
  PColorOp -> do
    return (mempty,mempty,TBlending C ~> TFragmentOperation C)
  PNoBlending -> do
    return (mempty,mempty,TBlending C)
  PVertexOut -> do
    return (mempty,mempty,TV4F C ~> TFloat C ~> TTuple C [] ~> TInterpolated C ~> TVertexOut C)
  PSmooth -> do
    return (mempty,mempty,TV4F C ~> TInterpolated C)
  PFetch -> do
    return (mempty,mempty,TString C ~> TFetchPrimitive C ~> TInput C ~> TVertexStream C)
  PTriangles -> do
    return (mempty,mempty,TFetchPrimitive C)
  PIV4F -> do
    return (mempty,mempty,TString C ~> TInput C)
  PIM44F -> do
    return (mempty,mempty,TString C ~> TInput C)
  PUni -> do
    return (mempty,mempty,TInput C ~> TM44F C)
  PTransform -> do
    return (mempty,mempty,(TV4F C ~> TVertexOut C) ~> TVertexStream C ~> TPrimitiveStream C)
  PRasterize -> do
    return (mempty,mempty,TRasterContext C ~> TPrimitiveStream C ~> TFragmentStream C)
  PFragmentOutRastDepth -> do
    return (mempty,mempty,TV4F C ~> TFragmentOut C)
  PAccumulate -> do
    return (mempty,mempty,TAccumulationContext C ~> TFragmentFilter C ~> (TV4F C ~> TFragmentOut C) ~> TFragmentStream C ~> TFrameBuffer C ~> TFrameBuffer C)
  PPassAll -> do
    return (mempty,mempty, TFragmentFilter C)
  a -> throwErrorUnique $ "unknown primitive: " ++ show a

inferLit :: Lit -> Unique Typing
inferLit a = case a of
  LInt _ -> do
    t <- newVar C
    return (mempty,[(CNum,t)],t) -- ????
  LChar   _ -> return (mempty,mempty,TChar C)
  LFloat  _ -> return (mempty,mempty,TFloat C)
  LString _ -> return (mempty,mempty,TString C)

type Unique a = StateT (Int,ByteString,[Range]) (Except String) a

getTag :: Show a => Exp a -> a
getTag (ELit      r _) = r
getTag (EPrimFun  r _) = r
getTag (EVar      r _) = r
getTag (EApp      r _ _) = r
getTag (ELam      r _ _) = r
getTag (ELet      r _ _ _) = r
getTag (ETuple    r _) = r
getTag x = error $ "getTag error: " ++ show x

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

newVar :: Frequency -> Unique Ty
newVar f = do
  (n,s,r) <- get
  put (n+1,s,r)
  return $ TVar f $ 't':show n

applyTy :: Subst -> Ty -> Ty
applyTy st tv@(TVar _ a) = case Map.lookup a st of
  Nothing -> tv
  Just t  -> t
applyTy st (TArr f a b) = TArr f (applyTy st a) (applyTy st b)
applyTy _ t = t

applyMonoEnv :: Subst -> MonoEnv -> MonoEnv
applyMonoEnv s e = fmap (applyTy s) e

applyInstEnv :: Subst -> InstEnv -> Unique InstEnv
applyInstEnv s e = filterM tyInst $ (trace_ (show (s,e,"->",e'))) e'
 where
  e' = fmap (\(c,t) -> (c,applyTy s t)) e
  tyInst (c,TVar{}) = return True
  tyInst (c,t) = case Map.lookup c instances of
    Nothing -> err
    Just ts -> if Set.member t ts then return False else err
   where err = throwErrorUnique $ "no " ++ show c ++ " instance for " ++ show t

joinInstEnv :: [InstEnv] -> InstEnv
joinInstEnv e = Set.toList . Set.unions . map Set.fromList $ e

freeVarsTy :: Ty -> Set TName
freeVarsTy (TVar _ a) = Set.singleton a
freeVarsTy (TArr f a b) = freeVarsTy a `mappend` freeVarsTy b
freeVarsTy (TTuple _ a) = foldl mappend mempty $ map freeVarsTy a
freeVarsTy (TArray _ a) = freeVarsTy a
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
  newVars <- replicateM (Set.size fv) (newVar C)
  let s = Map.fromList $ zip (Set.toList fv) newVars
  i' <- applyInstEnv s i
  return (applyMonoEnv s m,i',applyTy s t)

bindVar :: TName -> Ty -> Unique Subst
bindVar n t
  | tvarEq t = return mempty
  | n `Set.member` freeVarsTy t = throwErrorUnique $ "Infinite type, type variable " ++ n ++ " occurs in " ++ show t
  | otherwise = return $ Map.singleton n t
 where
  tvarEq (TVar _ m) = m == n
  tvarEq _ = False

compose :: Subst -> Subst -> Subst
compose b a = mappend a $ applyTy a <$> b

unifyTy :: Ty -> Ty -> Unique Subst
unifyTy (TVar _ u) t = bindVar u t
unifyTy t (TVar _ u) = bindVar u t
unifyTy (TArr f1 a1 b1) (TArr f2 a2 b2) = do
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
  a <- newVar C
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
  v = Set.map (TVar C) $ freeVarsTy t `mappend` freeVarsMonoEnv m
  i' = filter (\(_,a) -> Set.member a v) i

unamb :: PolyEnv -> Typing -> Unique ()
unamb env (m,i,t) = do
  let v = Set.map (TVar C) $ freeVarsTy t `mappend` freeVarsMonoEnv m
  return ()
  forM_ i $ \(_,a) -> if Set.member a v then return () else throwErrorUnique $ unlines ["ambiguous type: " ++ show (i,t),"env: " ++ show m, "free vars: " ++ show v, "poly env: " ++ show env]

infer :: PolyEnv -> Exp Range -> Unique (Exp Typing)
infer penv (ETuple r t) = withRanges [r] $ do
  te <- mapM (infer penv) t
  let (ml,il,tl) = unzip3 $ map getTag te
  s <- unify ml []
  m <- foldM (\a b -> joinMonoEnv (applyMonoEnv s a) (applyMonoEnv s b)) mempty ml
  i <- joinInstEnv <$> mapM (applyInstEnv s) il
  let ty = (m,i,TTuple C $ map (applyTy s) tl)
  return (ETuple ty te)
infer penv (ELit r l) = withRanges [r] $ ELit <$> inferLit l <*> pure l
infer penv (EPrimFun r f) = withRanges [r] $ EPrimFun <$> inferPrimFun f <*> pure f
infer penv (EVar r n) = withRanges [r] $ case Map.lookup n penv of
  Nothing -> do
    t <- trace "mono var" <$> newVar C
    return $ EVar (Map.singleton n t,mempty,t) n
  Just t -> trace "poly var" <$> EVar <$> instTyping t <*> pure n
infer penv (ELam r n f) = withRanges [r] $ do
  tf <- infer penv f
  let (m,i,t) = getTag tf
  case Map.lookup n m of
    Nothing -> do
      a <- newVar C
      return $ ELam (m,i,a ~> t) n tf
    Just a -> return $ ELam (Map.delete n m,i,a ~> t) n tf
infer penv (EApp r f a) = withRanges [r] $ do
  tf <- infer penv f
  ta <- infer penv a
  let (m1,i1,t1) = getTag tf
      (m2,i2,t2) = getTag ta
  a <- newVar C
  s <- unify [m1,m2] [t1,t2 ~> a]
  m3 <- joinMonoEnv (applyMonoEnv s m1) (applyMonoEnv s m2)
  i3 <- (\a1 a2 -> joinInstEnv [a1,a2]) <$> applyInstEnv s i1 <*> applyInstEnv s i2
  unamb penv (m3,i3,applyTy s a)
  return $ EApp (m3,i3,applyTy s a) tf ta
infer penv (ELet r n x e) = withRanges [r] $ do
  tx <- infer penv x
  let d1@(m1,i1,t1) = getTag tx
  s0 <- unify [m1] [t1]
  let m0 = Map.delete n $ applyMonoEnv s0 m1
      t0 = applyTy s0 t1
  i0 <- trace_ "1" <$> applyInstEnv s0 i1
  trace (show ("m1",m1,"let1",d1,"let2",(m0,i0,t0))) $ unamb penv (m0,i0,t0)
  let penv' = Map.insert n (m0,i0,t0) penv
  te <- infer penv' e
  let (m',i',t') = getTag te
  s_ <- unify [m0,m'] []
  let s = s0 `compose` s_
  m <- joinMonoEnv (applyMonoEnv s m') (applyMonoEnv s m0)
  a1 <- trace_ "2" <$> applyInstEnv s i'
  a2 <- trace_ "3" <$> applyInstEnv s i0
  let i = joinInstEnv [a1,a2]
      ty = prune $ trace (show ("s",s,"penv",penv',"in",(m',i',t'))) $ trace_ (show $ ("s0",s0,m1,"s",s,m0,m')) $ (m,i,applyTy s t')
  return $ ELet ty n tx te

inference :: ByteString -> Exp Range -> Either String (Exp Typing)
inference src e = case scopeChk src e of
  Left m  -> Left m
  Right () -> runIdentity $ runExceptT $ (flip evalStateT) (0,src,[]) act
   where
    act = do
      a <- infer mempty e
      unamb mempty $ getTag a
      return a

-- scope checking
scopeCheck :: ByteString -> Set EName -> Exp Range -> Either String ()
scopeCheck src vars (EVar r n) = if Set.member n vars then return () else throwErrorSrc src [r] $ "Variable " ++ n ++ " is not in scope."
scopeCheck src vars (EApp r f a) = scopeCheck src vars f >> scopeCheck src vars a
scopeCheck src vars (ELam r n f) = if Set.notMember n vars then scopeCheck src (Set.insert n vars) f else throwErrorSrc src [r] $ "Variable name clash: " ++ n
scopeCheck src vars (ELet r n x e) = do
  let vars' = Set.insert n vars
  if Set.notMember n vars then scopeCheck src vars' x >> scopeCheck src vars' e else throwErrorSrc src [r] $ "Variable name clash: " ++ n
scopeCheck src vars _ = return ()

scopeChk :: ByteString -> Exp Range -> Either String ()
scopeChk src e = scopeCheck src mempty e
