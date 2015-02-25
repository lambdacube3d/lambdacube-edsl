module ToDeBruijn where

import Data.ByteString.Char8
import Data.Map as Map
import Data.Monoid
import CompositionalLC
import Type
import LambdaCube.Core.DeBruijn hiding (Exp,N)
import LambdaCube.Core.DeBruijn (N())
import LambdaCube.Core.Type hiding (Ty)
import qualified LambdaCube.Core.PrimFun as C
import qualified LambdaCube.Core.Type as C
{-
expV4F :: Exp Typing
expV4F =
  EApp (fromList [],[],TV4F C)
  (EApp (fromList [],[],TFloat C ~> TV4F C)
  (EApp (fromList [],[],TFloat C ~> (TFloat C ~> TV4F C))
  (EApp (fromList [],[],TFloat C ~> (TFloat C ~> (TFloat C ~> TV4F C)))
    (EPrimFun (fromList [],[],TFloat C ~> (TFloat C ~> (TFloat C ~> (TFloat C ~> TV4F C)))) PV4)
    (ELit (fromList [],[],TFloat C) (LFloat 0.0)))
    (ELit (fromList [],[],TFloat C) (LFloat 0.0)))
    (ELit (fromList [],[],TFloat C) (LFloat 0.4)))
    (ELit (fromList [],[],TFloat C) (LFloat 1.0))

expImage :: Exp Typing
expImage =
  EApp (fromList [],[],TImage C)
    (EApp (fromList [],[],TV4F C ~> TImage C)
      (EPrimFun (fromList [],[],TInt C ~> (TV4F C ~> TImage C)) PColorImage)
      (ELit (fromList [],[(CNum,TVar C "t0")],TVar C "t0") (LInt 1)))
    expV4F

expFrameBuffer :: Exp Typing
expFrameBuffer =
  EApp (fromList [],[],TFrameBuffer C)
    (EPrimFun (fromList [],[],TImage C ~> TFrameBuffer C) PFrameBuffer)
    expImage

expOutput :: Exp Typing
expOutput =
  EApp (fromList [],[],TOutput C)
    (EPrimFun (fromList [],[],TFrameBuffer C ~> TOutput C) PScreenOut)
    expFrameBuffer
-}
data NF
  = Arg Lit
  | Fun PrimFun
  | Val C.Ty Value
  | Img Image
  | N   N
  | Blending Blending
  | PolygonOffset PolygonOffset
  | CullMode CullMode
  | PolygonMode PolygonMode
  | ProvokingVertex ProvokingVertex
  | FetchPrimitive FetchPrimitive
  | FragmentOperation FragmentOperation
  | RasterContext RasterContext
  | IV4F ByteString
  | IM44F ByteString
  deriving (Show)

instance Show N where
  show _ = "N"

data EnvVar
  = LetVar (Exp Typing)
  | LamVar

type NFEnv = Map EName EnvVar
-- TODO: add let
toNF :: Subst -> NFEnv -> Exp Typing -> [NF]
toNF sub env (ELit t l) = [Arg l]
toNF sub env (EPrimFun t f) = [Fun f]
toNF sub env (EApp t s a b) = let sub' = sub `compose` s in eval $ toNF sub' env a `mappend` toNF sub' env b
toNF sub env (ELet t n a b) = --case toNF env a of -- TODO
  -- x@(N _:_) -> error $ "let is not fully supported: " ++ show x
  --x -> toNF (Map.insert n x env) b
  toNF sub (Map.insert n (LetVar a) env) b
toNF sub env (EVar (m,i,t) s n) = case Map.lookup n env of
  Nothing -> error $ "unknown variable: " ++ n
  Just (LetVar x) -> toNF (s `compose` sub) env x
  Just LamVar     -> [N $ var (toTy (m,i,applyTy (s `compose` sub) t)) 0 ""]
toNF sub env (ELam t n e) =
  let (tm,ti,tt@(TArr ta tb)) = t
  in case toNF sub (Map.insert n LamVar env) e of
    [N x] -> [N $ lam (toTy (tm,ti,applyTy sub tt)) $ body x]
    x -> error $ "lam is not fully supported: " ++ show x
toNF sub env (ETuple (m,i,t) []) = [N $ tup (toTy (m,i,applyTy sub t)) []]
toNF _ _ x = error $ "toNF error: " ++ show x

eval :: [NF] -> [NF]
eval (Fun PV4:Arg (LFloat x):Arg (LFloat y):Arg (LFloat z):Arg (LFloat w):xs) = Val (Single C.V4F) (VV4F (V4 (realToFrac x) (realToFrac y) (realToFrac z) (realToFrac w))) : eval xs
eval (Fun PColorImage:Arg (LNat i):Val _ v:xs) = Img (ColorImage i v) : eval xs
eval (Fun PFrameBuffer:Img i:xs) = N (frameBuffer [i]) : eval xs
eval (Fun PScreenOut:N n:xs) = N (screenOut $ prjFrameBuffer mempty 0 n) : eval xs
eval (Fun PFragmentOut:Val t v:xs) = N (fragmentOut [const_ t v]) : eval xs
eval (Fun PFragmentOut:N a:xs) = N (fragmentOut [a]) : eval xs
eval (Fun PSmooth:N a:xs) = N (smooth a) : eval xs
eval (Fun PVertexOut:N a:Arg (LFloat b):N c:N d:xs) = N (vertexOut a (const_ (Single C.Float) (VFloat $ realToFrac b)) [c] [d]) : eval xs
eval (Fun PScreenOut:N a:xs) = N (screenOut a) : eval xs
eval (Fun PNoBlending:xs) = Blending NoBlending : eval xs
eval (Fun PNoOffset:xs) = PolygonOffset NoOffset : eval xs
eval (Fun PCullNone:xs) = CullMode CullNone : eval xs
eval (Fun PPolygonFill:xs) = PolygonMode PolygonFill : eval xs
eval (Fun PLastVertex:xs) = ProvokingVertex LastVertex : eval xs
eval (Fun PTriangles:xs) = FetchPrimitive Triangles : eval xs
eval (Fun PColorOp:Blending b:xs) = FragmentOperation (ColorOp b (VV4B $ V4 True True True True)) : eval xs
eval (Fun PTriangleCtx:CullMode a:PolygonMode b:PolygonOffset c:ProvokingVertex d:xs) = RasterContext (TriangleCtx a b c d) : eval xs
eval (Fun PIV4F:Arg (LString s):xs) = IV4F (pack s) : eval xs
eval (Fun PIM44F:Arg (LString s):xs) = IM44F (pack s) : eval xs
eval (Fun PPassAll:xs) = N passAll : eval xs
eval (Fun PFetch:Arg (LString a):FetchPrimitive b:IV4F c:xs) = N (fetch (pack a) b [(c,V4F)]) : eval xs
eval (Fun PTransform:N a:N b:xs) = N (transform a b) : eval xs
eval (Fun PRasterize:RasterContext a:N b:xs) = N (rasterize a b) : eval xs
eval (Fun PAccumulationContext:FragmentOperation a:xs) = N (accumulationContext Nothing [a]) : eval xs
eval (Fun PAccumulate:N a:N b:N c:N d:N e:xs) = N (accumulate a b c d e) : eval xs
eval (Fun PMulMV:N a:N b:xs) = N (primApp (Single V4F) C.PrimMulMatVec $ tup (Unknown "") [a,b]) : eval xs
eval (Fun PUni:IM44F a:xs) = N (uni (Single M44F) a) : eval xs
eval (a:xs) = a : eval xs
eval l = l

toTy :: Typing -> C.Ty
toTy (_,_,TV4F _) = Single C.V4F
toTy t = Unknown (show t)
{-
data InputType
    = Bool
    | V2B
    | V3B
    | V4B
    | Word
    | V2U
    | V3U
    | V4U
    | Int
    | V2I
    | V3I
    | V4I
    | Float
    | V2F
    | V3F
    | V4F
    | M22F
    | M23F
    | M24F
    | M32F
    | M33F
    | M34F
    | M42F
    | M43F
    | M44F

data Ty
    = Single !InputType
    | Tuple [Ty]
    | Unknown String
    deriving (Read,Typeable,Show,Eq,Ord)
-}
{-
class ExpC exp where
    -- exp constructors
    let_        :: exp -> (exp -> exp) -> exp
    lam         :: Ty -> exp -> exp
    body        :: exp -> exp
    var         :: Ty -> Int -> String -> exp -- type, index, layout counter (this needed for proper sharing)
    apply       :: Ty -> exp -> exp -> exp
    const_      :: Ty -> Value -> exp
    primVar     :: Ty -> ByteString -> exp
    uni         :: Ty -> ByteString -> exp
    tup         :: Ty -> [exp] -> exp
    prj         :: Ty -> Int -> exp -> exp
    cond        :: Ty -> exp -> exp -> exp -> exp
    primApp     :: Ty -> PrimFun -> exp -> exp
    sampler     :: Ty -> Filter -> EdgeMode -> exp -> exp
    loop        :: Ty -> exp -> exp -> exp -> exp -> exp
    -- special tuple expressions
    vertexOut               :: exp -> exp -> [exp] -> [exp] -> exp
    geometryOut             :: exp -> exp -> exp -> [exp] -> [exp] -> exp
    fragmentOut             :: [exp] -> exp
    fragmentOutDepth        :: exp -> [exp] -> exp
    fragmentOutRastDepth    :: [exp] -> exp
    -- gp constructors
    fetch           :: ByteString -> FetchPrimitive -> [(ByteString,InputType)] -> exp
    transform       :: exp -> exp -> exp
    reassemble      :: exp -> exp -> exp
    rasterize       :: RasterContext -> exp -> exp
    frameBuffer     :: [Image] -> exp
    accumulationContext :: Maybe exp -> [FragmentOperation] -> exp
    accumulate      :: exp -> exp -> exp -> exp -> exp -> exp
    prjFrameBuffer  :: ByteString -> Int -> exp -> exp
    prjImage        :: ByteString -> Int -> exp -> exp
    -- texture constructors
    textureSlot     :: ByteString -> TextureType -> exp
    texture         :: TextureType -> Value -> MipMap -> [exp] -> exp -- hint: type, size, mip, data
    -- Interpolated constructors
    flat            :: exp -> exp
    smooth          :: exp -> exp
    noPerspective   :: exp -> exp
    -- GeometryShader constructors
    geometryShader  :: Int -> OutputPrimitive -> Int -> exp -> exp -> exp -> exp
    -- FragmentFilter constructors
    passAll         :: exp
    filter_         :: exp -> exp
    -- GPOutput constructors
    samplerOut      :: ByteString -> exp -> exp
    screenOut       :: exp -> exp
    multiOut        :: [exp] -> exp
-}