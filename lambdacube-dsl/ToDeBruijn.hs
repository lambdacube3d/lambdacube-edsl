module ToDeBruijn where

import Data.ByteString.Char8
import Data.Map as Map
import Data.Monoid
import CompositionalLC
import LambdaCube.Core.DeBruijn hiding (Exp,N)
import LambdaCube.Core.DeBruijn (N())
import LambdaCube.Core.Type hiding (Ty)
import qualified LambdaCube.Core.Type as C

{-
data Exp a
  = ELit      a Lit
  | EPrimFun  a PrimFun
  | EVar      a EName
  | EApp      a (Exp a) (Exp a)
  | ELam      a EName (Exp a)
  | ELet      a EName (Exp a) (Exp a)
  | ETuple    a [Exp a]
-}

{-
let frame = FrameBuffer (ColorImage 1 (V4 0.0 0.0 0.4 1.0))
in ScreenOut frame

ELet (fromList [],[],TOutput)
     "frame" (EApp (fromList [],[],TFrameBuffer)
                (EPrimFun (fromList [],[],TImage :-> TFrameBuffer) PFrameBuffer) 
                (EApp (fromList [],[],TImage)
                  (EApp (fromList [],[],TV4F :-> TImage)
                    (EPrimFun (fromList [],[],TInt :-> (TV4F :-> TImage)) PColorImage)
                    (ELit (fromList [],[(CNum,TVar "t0")],TVar "t0") (LInt 1)))
                  (EApp (fromList [],[],TV4F)
                  (EApp (fromList [],[],TFloat :-> TV4F)
                  (EApp (fromList [],[],TFloat :-> (TFloat :-> TV4F))
                  (EApp (fromList [],[],TFloat :-> (TFloat :-> (TFloat :-> TV4F)))
                    (EPrimFun (fromList [],[],TFloat :-> (TFloat :-> (TFloat :-> (TFloat :-> TV4F)))) PV4)
                    (ELit (fromList [],[],TFloat) (LFloat 0.0)))
                    (ELit (fromList [],[],TFloat) (LFloat 0.0)))
                    (ELit (fromList [],[],TFloat) (LFloat 0.4)))
                    (ELit (fromList [],[],TFloat) (LFloat 1.0)))))
      (EApp (fromList [],[],TOutput)
        (EPrimFun (fromList [],[],TFrameBuffer :-> TOutput) PScreenOut)
        (EVar (fromList [],[],TFrameBuffer) "frame"))

with frequency:
  ELet (fromList [],[],TOutput C)
     "frame" (EApp (fromList [],[],TFrameBuffer C)
                (EPrimFun (fromList [],[],TImage a :-> TFrameBuffer a) PFrameBuffer) 
                (EApp (fromList [],[],TImage C)
                  (EApp (fromList [],[],TV4F a :-> TImage a)
                    (EPrimFun (fromList [],[],TInt C :-> (TV4F a :-> TImage a)) PColorImage)
                    (ELit (fromList [],[(CNum,TVar "t0")],TVar "t0") (LInt 1)))
                  (EApp (fromList [],[],TV4F C)
                  (EApp (fromList [],[],TFloat a :-> TV4F a)
                  (EApp (fromList [],[],TFloat a :-> (TFloat b :-> TV4F (max [a,b])))
                  (EApp (fromList [],[],TFloat a :-> (TFloat b :-> (TFloat c :-> TV4F (max [a,b,c]))))
                    (EPrimFun (fromList [],[],TFloat a :-> (TFloat b :-> (TFloat c :-> (TFloat d :-> TV4F (max [a,b,c,d]))))) PV4)
                    (ELit (fromList [],[],TFloat C) (LFloat 0.0)))
                    (ELit (fromList [],[],TFloat C) (LFloat 0.0)))
                    (ELit (fromList [],[],TFloat C) (LFloat 0.4)))
                    (ELit (fromList [],[],TFloat C) (LFloat 1.0)))))
      (EApp (fromList [],[],TOutput C)
        (EPrimFun (fromList [],[],TFrameBuffer a :-> TOutput a) PScreenOut)
        (EVar (fromList [],[],TFrameBuffer C) "frame"))

screenOut $ prjFrameBuffer "" 1 $ frameBuffer [ColorImage 1 (VV4F $ V4 0 0 0.4 1)]
-}
{-
compile time representation:
 gfx01
  done: Value   - TV4F C
  done: [Image] - TImage C

 gfx02
  done: Filter
  done: EdgeMode
  done: FetchPrimitive
  done: RasterContext
  done: [FragmentOperation]
  done: TextureType
  done: MipMap
  done: OutputPrimitive
-}

{-
  EApp (fromList [],[],TV4F C)
  (EApp (fromList [],[],TFloat a :-> TV4F a)
  (EApp (fromList [],[],TFloat a :-> (TFloat b :-> TV4F (max [a,b])))
  (EApp (fromList [],[],TFloat a :-> (TFloat b :-> (TFloat c :-> TV4F (max [a,b,c]))))
    (EPrimFun (fromList [],[],TFloat a :-> (TFloat b :-> (TFloat c :-> (TFloat d :-> TV4F (max [a,b,c,d]))))) PV4)
    (ELit (fromList [],[],TFloat C) (LFloat 0.0)))
    (ELit (fromList [],[],TFloat C) (LFloat 0.0)))
    (ELit (fromList [],[],TFloat C) (LFloat 0.4)))
    (ELit (fromList [],[],TFloat C) (LFloat 1.0))
----------------------------------------------------------------------
  Value (VV4F (V4 0 0 0.4 1))
-}

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
  deriving (Show)

instance Show N where
  show _ = "N"

type NFEnv = Map EName [NF]
-- TODO: add let
toNF :: NFEnv -> Exp Typing -> [NF]
toNF env (ELit t l) = [Arg l]
toNF env (EPrimFun t f) = [Fun f]
toNF env (EApp t a b) = eval $ toNF env a `mappend` toNF env b
toNF env (ELet t n a b) = case toNF env a of -- TODO
  -- x@(N _:_) -> error $ "let is not fully supported: " ++ show x
  x -> toNF (Map.insert n x env) b
toNF env (EVar t n) = case Map.lookup n env of
  Nothing -> error $ "unknown variable: " ++ n
  Just x -> x
toNF env (ELam t n e) =
  let (tm,ti,TArr _ ta tb) = t
  in case toNF (Map.insert n ([N $ var (toTy (tm,ti,ta)) 0 ""]) env) e of
    [N x] -> [N $ lam (toTy t) $ body x]
    x -> error $ "lam is not fully supported: " ++ show x
toNF env (ETuple t []) = [N $ tup (toTy t) []]
toNF _ x = error $ "toNF error: " ++ show x

eval :: [NF] -> [NF]
eval (Fun PV4:Arg (LFloat x):Arg (LFloat y):Arg (LFloat z):Arg (LFloat w):xs) = Val (Single C.V4F) (VV4F (V4 (realToFrac x) (realToFrac y) (realToFrac z) (realToFrac w))) : eval xs
eval (Fun PColorImage:Arg (LInt i):Val _ v:xs) = Img (ColorImage (fromIntegral i) v) : eval xs
eval (Fun PFrameBuffer:Img i:xs) = N (frameBuffer [i]) : eval xs
eval (Fun PScreenOut:N n:xs) = N (screenOut $ prjFrameBuffer mempty 0 n) : eval xs
eval (Fun PFragmentOutRastDepth:Val t v:xs) = N (fragmentOutRastDepth [const_ t v]) : eval xs
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
eval (Fun PPassAll:xs) = N passAll : eval xs
eval (Fun PFetch:Arg (LString a):FetchPrimitive b:IV4F c:xs) = N (fetch (pack a) b [(c,V4F)]) : eval xs
eval (Fun PTransform:N a:N b:xs) = N (transform a b) : eval xs
eval (Fun PRasterize:RasterContext a:N b:xs) = N (rasterize a b) : eval xs
eval (Fun PAccumulationContext:FragmentOperation a:xs) = N (accumulationContext Nothing [a]) : eval xs
eval (Fun PAccumulate:N a:N b:N c:N d:N e:xs) = N (accumulate a b c d e) : eval xs
eval (a:xs) = a : eval xs
eval l = l

{-
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
-}
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