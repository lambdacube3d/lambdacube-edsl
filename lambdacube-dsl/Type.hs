module Type where

import Data.Monoid
import Text.PrettyPrint.ANSI.Leijen (pretty)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)
import Control.Monad.Except
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Trifecta.Delta
import Text.Trifecta hiding (err)

type Unique a = StateT (Int,ByteString,[Range]) (Except String) a

type Range = (Delta,Delta)

type TName = String
type EName = String
type MonoEnv = Map EName Ty
type PolyEnv = Map EName Typing
type InstEnv = [(Constraint,Ty)]
type Typing = (MonoEnv,InstEnv,Ty)
type Env = (PolyEnv,MonoEnv,InstEnv)

data Lit
  = LInt    Integer
  | LChar   Char
  | LString String
  | LFloat  Double
  | LNat    Int
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
  | PFragmentOut
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
a ~> b = TArr a b

type Semantic = Ty
type PrimitiveType = Ty
type Nat = Ty

data Ty -- star kind
  = TVar    Frequency TName
  | TArr    Ty Ty
  -- composit
  | TTuple  Frequency [Ty]
  | TArray  Frequency Ty
  -- primitive types
  | TInt    Frequency
  | TChar   Frequency
  | TFloat  Frequency
  | TString Frequency

  -- lambdacube types
  | TNat    Int

  -- Semantic
  | Depth   Ty
  | Stencil Ty
  | Color   Ty

  -- PrimitiveType
  | TTriangle
  | TLine
  | TPoint
  | TTriangleAdjacency
  | TLineAdjacency

  | TM44F   Frequency
  | TV4F    Frequency
  | TAccumulationContext  Frequency Ty
  | TBlending             Frequency Ty
  | TCullMode             Frequency
  | TFetchPrimitive       Frequency PrimitiveType
  | TFragmentFilter       Frequency Ty
  | TFragmentOperation    Frequency Semantic
  | TFragmentOut          Frequency Semantic
  | TFragmentStream       Frequency Nat Ty
  | TFrameBuffer          Frequency -- ???
  | TImage                Frequency Nat Semantic
  | TInput                Frequency Ty
  | TInterpolated         Frequency Ty -- ???
  | TOutput               Frequency
  | TPolygonMode          Frequency
  | TPolygonOffset        Frequency
  | TPrimitiveStream      Frequency PrimitiveType Nat Frequency Ty -- ???
  | TProvokingVertex      Frequency
  | TRasterContext        Frequency PrimitiveType
  | TVertexOut            Frequency Ty -- ???
  | TVertexStream         Frequency PrimitiveType Ty
  deriving (Show,Eq,Ord)

data Constraint
  = CNum
  | CTextual
  deriving (Show,Eq,Ord)

instances :: Map Constraint (Set Ty)
instances = Map.fromList [(CNum,Set.fromList [TInt C,TFloat C])]

ty :: Ty -> Unique Typing
ty t = return (mempty,mempty,t)

inferPrimFun :: PrimFun -> Unique Typing
inferPrimFun a = case a of
  PFragmentOut  -> do t <- newVar C ; ty $ t ~> TFragmentOut C t
  PAccumulationContext  -> do t <- newVar C ; ty $ TFragmentOperation C t ~> TAccumulationContext C t
  PMulMV        -> ty $ TM44F C ~> TV4F C ~> TV4F C
  PV4           -> ty $ TFloat C ~> TFloat C ~> TFloat C ~> TFloat C ~> TV4F C
  PColorImage   -> do [a,b] <- newVars 2 C ; ty $ a ~> b ~> TImage C a b
  PFrameBuffer  -> do [a,b] <- newVars 2 C ; ty $ TImage C a b ~> TFrameBuffer C
  PScreenOut    -> ty $ TFrameBuffer C ~> TOutput C
  PCullNone     -> ty $ TCullMode C
  PPolygonFill  -> ty $ TPolygonMode C
  PNoOffset     -> ty $ TPolygonOffset C
  PLastVertex   -> ty $ TProvokingVertex C
  PIV4F         -> ty $ TString C ~> TInput C (TV4F C)
  PIM44F        -> ty $ TString C ~> TInput C (TM44F C)
  PUni          -> do t <- newVar C ; ty $ TInput C t ~> t
  PTriangles    -> ty $ TFetchPrimitive C TTriangle
  PNoBlending   -> do t <- newVar C ; ty $ TBlending C t
  PTriangleCtx  -> ty $ TCullMode C ~> TPolygonMode C ~> TPolygonOffset C ~> TProvokingVertex C ~> TRasterContext C TTriangle
  PSmooth       -> do t <- newVar C ; ty $ t ~> TInterpolated C t
  PFetch        -> do [a,b] <- newVars 2 C ; ty $ TString C ~> TFetchPrimitive C a ~> TInput C b ~> TVertexStream C a b
  PRasterize    -> do [a,b,c] <- newVars 3 C ; ty $ TRasterContext C a ~> TPrimitiveStream C a b C c ~> TFragmentStream C b c
  PTransform    -> do [a,b,p] <- newVars 3 C ; ty $ (a ~> TVertexOut C b) ~> TVertexStream C p a ~> TPrimitiveStream C p (TNat 1) C b
  PVertexOut    -> do a <- newVar C ; ty $ TV4F C ~> TFloat C ~> TTuple C [] ~> TInterpolated C a ~> TVertexOut C a
  PAccumulate   -> do [a,b,n] <- newVars 3 C ; ty $ TAccumulationContext C b ~> TFragmentFilter C a ~> (a ~> TFragmentOut C b) ~> TFragmentStream C n a ~> TFrameBuffer C ~> TFrameBuffer C
  PColorOp      -> do a <- newVar C ; ty $ TBlending C a ~> TFragmentOperation C ({-Color-} a) -- TODO: type family needed
  PPassAll      -> do t <- newVar C ; ty $ TFragmentFilter C t
  a -> throwErrorUnique $ "unknown primitive: " ++ show a

inferLit :: Lit -> Unique Typing
inferLit a = case a of
  LInt _ -> do
    t <- newVar C
    return (mempty,[(CNum,t)],t) -- ????
  LChar   _ -> ty $ TChar C
  LFloat  _ -> ty $ TFloat C
  LString _ -> ty $ TString C
  LNat i    -> ty $ TNat i

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

newVars :: Int -> Frequency -> Unique [Ty]
newVars n f = replicateM n $ newVar f

newVar :: Frequency -> Unique Ty
newVar f = do
  (n,s,r) <- get
  put (n+1,s,r)
  return $ TVar f $ 't':show n
