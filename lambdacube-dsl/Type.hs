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
  | TM44F   Frequency
  | TV4F    Frequency
  | TImage  Frequency
  | TFrameBuffer          Frequency
  | TOutput               Frequency
  | TRasterContext        Frequency
  | TCullMode             Frequency
  | TPolygonMode          Frequency
  | TPolygonOffset        Frequency
  | TProvokingVertex      Frequency
  | TAccumulationContext  Frequency
  | TFragmentOperation    Frequency
  | TBlending             Frequency
  | TVertexOut            Frequency
  | TInterpolated         Frequency
  | TFetchPrimitive       Frequency
  | TInput                Frequency Ty
  | TVertexStream         Frequency
  | TPrimitiveStream      Frequency
  | TFragmentStream       Frequency
  | TFragmentOut          Frequency
  | TFragmentFilter       Frequency
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
  PFragmentOutRastDepth -> ty $ TV4F C ~> TFragmentOut C
  PAccumulationContext  -> ty $ TFragmentOperation C ~> TAccumulationContext C
  PMulMV        -> ty $ TM44F C ~> TV4F C ~> TV4F C
  PV4           -> ty $ TFloat C ~> TFloat C ~> TFloat C ~> TFloat C ~> TV4F C
  PColorImage   -> ty $ TInt C ~> TV4F C ~> TImage C
  PFrameBuffer  -> ty $ TImage C ~> TFrameBuffer C
  PScreenOut    -> ty $ TFrameBuffer C ~> TOutput C
  PTriangleCtx  -> ty $ TCullMode C ~> TPolygonMode C ~> TPolygonOffset C ~> TProvokingVertex C ~> TRasterContext C
  PCullNone     -> ty $ TCullMode C
  PPolygonFill  -> ty $ TPolygonMode C
  PNoOffset     -> ty $ TPolygonOffset C
  PLastVertex   -> ty $ TProvokingVertex C
  PColorOp      -> ty $ TBlending C ~> TFragmentOperation C
  PNoBlending   -> ty $ TBlending C
  PVertexOut    -> ty $ TV4F C ~> TFloat C ~> TTuple C [] ~> TInterpolated C ~> TVertexOut C
  PSmooth       -> ty $ TV4F C ~> TInterpolated C
  PFetch        -> ty $ TString C ~> TFetchPrimitive C ~> TInput C (TV4F C) ~> TVertexStream C
  PTriangles    -> ty $ TFetchPrimitive C
  PIV4F         -> ty $ TString C ~> TInput C (TV4F C)
  PIM44F        -> ty $ TString C ~> TInput C (TM44F C)
  PUni          -> do t <- newVar C ; ty $ TInput C t ~> t
  PTransform    -> ty $ (TV4F C ~> TVertexOut C) ~> TVertexStream C ~> TPrimitiveStream C
  PRasterize    -> ty $ TRasterContext C ~> TPrimitiveStream C ~> TFragmentStream C
  PAccumulate   -> ty $ TAccumulationContext C ~> TFragmentFilter C ~> (TV4F C ~> TFragmentOut C) ~> TFragmentStream C ~> TFrameBuffer C ~> TFrameBuffer C
  PPassAll      -> ty $ TFragmentFilter C
  a -> throwErrorUnique $ "unknown primitive: " ++ show a

inferLit :: Lit -> Unique Typing
inferLit a = case a of
  LInt _ -> do
    t <- newVar C
    return (mempty,[(CNum,t)],t) -- ????
  LChar   _ -> ty $ TChar C
  LFloat  _ -> ty $ TFloat C
  LString _ -> ty $ TString C

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
