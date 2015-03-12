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
  | Fun EName
  | Val C.Ty Value
  | Img Image
  | N   N
  | Blending Blending
  | PolygonOffset PolygonOffset
  | CullMode CullMode
  | PolygonMode PolygonMode
  | PointSize' PointSize
  | ProvokingVertex ProvokingVertex
  | FetchPrimitive FetchPrimitive
  | FragmentOperation FragmentOperation
  | RasterContext RasterContext
  | Input ByteString C.Ty
  | BlendingFactor BlendingFactor
  | BlendEquation BlendEquation
  | LogicOperation LogicOperation
  | StencilOperation StencilOperation
  | ComparisonFunction ComparisonFunction
  | FrontFace FrontFace
  | PointSpriteCoordOrigin PointSpriteCoordOrigin
  | Bool' Bool
  | Tuple' C.Ty [[NF]]
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
toNF sub env (EApp t s a b) = let sub' = sub `compose` s in eval $ toNF sub' env a `mappend` toNF sub' env b
toNF sub env (ELet t n a b) = --case toNF env a of -- TODO
  -- x@(N _:_) -> error $ "let is not fully supported: " ++ show x
  --x -> toNF (Map.insert n x env) b
  toNF sub (Map.insert n (LetVar a) env) b
--toNF sub env (EPrimFun t f) = 
toNF sub env (EVar (m,i,t) s n)
  | isPrimFun n = [Fun n]
  | otherwise = case Map.lookup n env of
      Nothing -> error $ "unknown variable: " ++ n
      Just (LetVar x) -> toNF (s `compose` sub) env x
      Just LamVar     -> [N $ var (toTy (m,i,applyTy (s `compose` sub) t)) 0 ""]
toNF sub env (ELam t n e) =
  let (tm,ti,tt@(TArr ta tb)) = t
  in case toNF sub (Map.insert n LamVar env) e of
    [N x] -> [N $ lam (toTy (tm,ti,applyTy sub tt)) $ body x]
    x -> error $ "lam is not fully supported: " ++ show x
toNF sub env (ETuple (m,i,t) []) = [N $ tup (toTy (m,i,applyTy sub t)) []]
toNF sub env (ETuple (m,i,t) a) = [Tuple' (toTy (m,i,applyTy sub t)) (fmap (eval . toNF sub env) a)]
toNF _ _ x = error $ "toNF error: " ++ show x

eval :: [NF] -> [NF]
-- Vector/Matrix
eval (Fun "True":xs) = Bool' True: eval xs
eval (Fun "False":xs) = Bool' False: eval xs
eval (Fun "V2B":Bool' x:Bool' y:xs) = Val (Single C.V2B) (VV2B (V2 x y)) : eval xs
eval (Fun "V3B":Bool' x:Bool' y:Bool' z:xs) = Val (Single C.V3B) (VV3B (V3 x y z)) : eval xs
eval (Fun "V4B":Bool' x:Bool' y:Bool' z:Bool' w:xs) = Val (Single C.V4B) (VV4B (V4 x y z w)) : eval xs
eval (Fun "V2F":Arg (LFloat x):Arg (LFloat y):xs) = Val (Single C.V2F) (VV2F (V2 (realToFrac x) (realToFrac y))) : eval xs
eval (Fun "V3F":Arg (LFloat x):Arg (LFloat y):Arg (LFloat z):xs) = Val (Single C.V3F) (VV3F (V3 (realToFrac x) (realToFrac y) (realToFrac z))) : eval xs
eval (Fun "V4F":Arg (LFloat x):Arg (LFloat y):Arg (LFloat z):Arg (LFloat w):xs) = Val (Single C.V4F) (VV4F (V4 (realToFrac x) (realToFrac y) (realToFrac z) (realToFrac w))) : eval xs
eval (Fun "V2I":Arg (LInt x):Arg (LInt y):xs) = Val (Single C.V2I) (VV2I (V2 (fromIntegral x) (fromIntegral y))) : eval xs
eval (Fun "V3I":Arg (LInt x):Arg (LInt y):Arg (LInt z):xs) = Val (Single C.V3I) (VV3I (V3 (fromIntegral x) (fromIntegral y) (fromIntegral z))) : eval xs
eval (Fun "V4I":Arg (LInt x):Arg (LInt y):Arg (LInt z):Arg (LInt w):xs) = Val (Single C.V4I) (VV4I (V4 (fromIntegral x) (fromIntegral y) (fromIntegral z) (fromIntegral w))) : eval xs
eval (Fun "V2U":Arg (LInt x):Arg (LInt y):xs) = Val (Single C.V2U) (VV2U (V2 (fromIntegral x) (fromIntegral y))) : eval xs
eval (Fun "V3U":Arg (LInt x):Arg (LInt y):Arg (LInt z):xs) = Val (Single C.V3U) (VV3U (V3 (fromIntegral x) (fromIntegral y) (fromIntegral z))) : eval xs
eval (Fun "V4U":Arg (LInt x):Arg (LInt y):Arg (LInt z):Arg (LInt w):xs) = Val (Single C.V4U) (VV4U (V4 (fromIntegral x) (fromIntegral y) (fromIntegral z) (fromIntegral w))) : eval xs
-- Input declaration
eval (Fun "Uni":Input n t:xs) = N (uni t n) : eval xs
eval (Fun "IWord":Arg (LString s):xs) = Input (pack s) (Single C.Word) : eval xs
eval (Fun "IV2U":Arg (LString s):xs) = Input (pack s) (Single C.V2U) : eval xs
eval (Fun "IV3U":Arg (LString s):xs) = Input (pack s) (Single C.V3U) : eval xs
eval (Fun "IV4U":Arg (LString s):xs) = Input (pack s) (Single C.V4U) : eval xs
eval (Fun "IInt":Arg (LString s):xs) = Input (pack s) (Single C.Int) : eval xs
eval (Fun "IV2I":Arg (LString s):xs) = Input (pack s) (Single C.V2I) : eval xs
eval (Fun "IV3I":Arg (LString s):xs) = Input (pack s) (Single C.V3I) : eval xs
eval (Fun "IV4I":Arg (LString s):xs) = Input (pack s) (Single C.V4I) : eval xs
eval (Fun "IBool":Arg (LString s):xs) = Input (pack s) (Single C.Bool) : eval xs
eval (Fun "IV2B":Arg (LString s):xs) = Input (pack s) (Single C.V2B) : eval xs
eval (Fun "IV3B":Arg (LString s):xs) = Input (pack s) (Single C.V3B) : eval xs
eval (Fun "IV4B":Arg (LString s):xs) = Input (pack s) (Single C.V4B) : eval xs
eval (Fun "IFloat":Arg (LString s):xs) = Input (pack s) (Single C.Float) : eval xs
eval (Fun "IV2F":Arg (LString s):xs) = Input (pack s) (Single C.V2F) : eval xs
eval (Fun "IV3F":Arg (LString s):xs) = Input (pack s) (Single C.V3F) : eval xs
eval (Fun "IV4F":Arg (LString s):xs) = Input (pack s) (Single C.V4F) : eval xs
eval (Fun "IM22F":Arg (LString s):xs) = Input (pack s) (Single C.M22F) : eval xs
eval (Fun "IM23F":Arg (LString s):xs) = Input (pack s) (Single C.M23F) : eval xs
eval (Fun "IM24F":Arg (LString s):xs) = Input (pack s) (Single C.M44F) : eval xs
eval (Fun "IM32F":Arg (LString s):xs) = Input (pack s) (Single C.M32F) : eval xs
eval (Fun "IM33F":Arg (LString s):xs) = Input (pack s) (Single C.M33F) : eval xs
eval (Fun "IM34F":Arg (LString s):xs) = Input (pack s) (Single C.M34F) : eval xs
eval (Fun "IM42F":Arg (LString s):xs) = Input (pack s) (Single C.M42F) : eval xs
eval (Fun "IM43F":Arg (LString s):xs) = Input (pack s) (Single C.M43F) : eval xs
eval (Fun "IM44F":Arg (LString s):xs) = Input (pack s) (Single C.M44F) : eval xs
-- BlendingFactor
eval (Fun "Zero":xs) = BlendingFactor Zero: eval xs
eval (Fun "One":xs) = BlendingFactor One: eval xs
eval (Fun "SrcColor":xs) = BlendingFactor SrcColor: eval xs
eval (Fun "OneMinusSrcColor":xs) = BlendingFactor OneMinusSrcColor: eval xs
eval (Fun "DstColor":xs) = BlendingFactor DstColor: eval xs
eval (Fun "OneMinusDstColor":xs) = BlendingFactor OneMinusDstColor: eval xs
eval (Fun "SrcAlpha":xs) = BlendingFactor SrcAlpha: eval xs
eval (Fun "OneMinusSrcAlpha":xs) = BlendingFactor OneMinusSrcAlpha: eval xs
eval (Fun "DstAlpha":xs) = BlendingFactor DstAlpha: eval xs
eval (Fun "OneMinusDstAlpha":xs) = BlendingFactor OneMinusDstAlpha: eval xs
eval (Fun "ConstantColor":xs) = BlendingFactor ConstantColor: eval xs
eval (Fun "OneMinusConstantColor":xs) = BlendingFactor OneMinusConstantColor: eval xs
eval (Fun "ConstantAlpha":xs) = BlendingFactor ConstantAlpha: eval xs
eval (Fun "OneMinusConstantAlpha":xs) = BlendingFactor OneMinusConstantAlpha: eval xs
eval (Fun "SrcAlphaSaturate":xs) = BlendingFactor SrcAlphaSaturate: eval xs
-- BlendEquation
eval (Fun "FuncAdd":xs) = BlendEquation FuncAdd: eval xs
eval (Fun "FuncSubtract":xs) = BlendEquation FuncSubtract: eval xs
eval (Fun "FuncReverseSubtract":xs) = BlendEquation FuncReverseSubtract: eval xs
eval (Fun "Min":xs) = BlendEquation Min: eval xs
eval (Fun "Max":xs) = BlendEquation Max: eval xs
-- LogicOperation
eval (Fun "Clear":xs) = LogicOperation Clear: eval xs
eval (Fun "And":xs) = LogicOperation And: eval xs
eval (Fun "AndReverse":xs) = LogicOperation AndReverse: eval xs
eval (Fun "Copy":xs) = LogicOperation Copy: eval xs
eval (Fun "AndInverted":xs) = LogicOperation AndInverted: eval xs
eval (Fun "Noop":xs) = LogicOperation Noop: eval xs
eval (Fun "Xor":xs) = LogicOperation Xor: eval xs
eval (Fun "Or":xs) = LogicOperation Or: eval xs
eval (Fun "Nor":xs) = LogicOperation Nor: eval xs
eval (Fun "Equiv":xs) = LogicOperation Equiv: eval xs
eval (Fun "Invert":xs) = LogicOperation Invert: eval xs
eval (Fun "OrReverse":xs) = LogicOperation OrReverse: eval xs
eval (Fun "CopyInverted":xs) = LogicOperation CopyInverted: eval xs
eval (Fun "OrInverted":xs) = LogicOperation OrInverted: eval xs
eval (Fun "Nand":xs) = LogicOperation Nand: eval xs
eval (Fun "Set":xs) = LogicOperation Set: eval xs
-- StencilOperation
eval (Fun "OpZero":xs) = StencilOperation OpZero: eval xs
eval (Fun "OpKeep":xs) = StencilOperation OpKeep: eval xs
eval (Fun "OpReplace":xs) = StencilOperation OpReplace: eval xs
eval (Fun "OpIncr":xs) = StencilOperation OpIncr: eval xs
eval (Fun "OpIncrWrap":xs) = StencilOperation OpIncrWrap: eval xs
eval (Fun "OpDecr":xs) = StencilOperation OpDecr: eval xs
eval (Fun "OpDecrWrap":xs) = StencilOperation OpDecrWrap: eval xs
eval (Fun "OpInvert":xs) = StencilOperation OpInvert: eval xs
-- ComparisonFunction
eval (Fun "Never":xs) = ComparisonFunction Never: eval xs
eval (Fun "Less":xs) = ComparisonFunction Less: eval xs
eval (Fun "Equal":xs) = ComparisonFunction Equal: eval xs
eval (Fun "Lequal":xs) = ComparisonFunction Lequal: eval xs
eval (Fun "Greater":xs) = ComparisonFunction Greater: eval xs
eval (Fun "Notequal":xs) = ComparisonFunction Notequal: eval xs
eval (Fun "Gequal":xs) = ComparisonFunction Gequal: eval xs
eval (Fun "Always":xs) = ComparisonFunction Always: eval xs
-- ProvokingVertex
eval (Fun "LastVertex":xs) = ProvokingVertex LastVertex : eval xs
eval (Fun "FirstVertex":xs) = ProvokingVertex FirstVertex : eval xs
-- CullMode
eval (Fun "CullNone":xs) = CullMode CullNone : eval xs
eval (Fun "CullFront":FrontFace f:xs) = CullMode (CullFront f) : eval xs
eval (Fun "CullBack":FrontFace f:xs) = CullMode (CullBack f) : eval xs
-- FrontFace
eval (Fun "CW":xs) = FrontFace CW: eval xs
eval (Fun "CCW":xs) = FrontFace CCW: eval xs
-- PolygonMode
eval (Fun "PolygonFill":xs) = PolygonMode PolygonFill : eval xs
eval (Fun "PolygonLine":Arg (LFloat s):xs) = PolygonMode (PolygonLine $ realToFrac s) : eval xs
eval (Fun "PolygonPoint":PointSize' s:xs) = PolygonMode (PolygonPoint s) : eval xs
-- PolygonOffset
eval (Fun "NoOffset":xs) = PolygonOffset NoOffset : eval xs
eval (Fun "Offset":Arg (LFloat a):Arg (LFloat b):xs) = PolygonOffset (Offset (realToFrac a) (realToFrac b)): eval xs
-- PointSize
eval (Fun "PointSize":Arg (LFloat s):xs) = PointSize' (PointSize $ realToFrac s) : eval xs
eval (Fun "ProgramPointSize":xs) = PointSize' ProgramPointSize : eval xs
-- Fragment Out
eval (Fun "FragmentOut":Val t v:xs) = N (fragmentOut [const_ t v]) : eval xs
eval (Fun "FragmentOut":N a:xs) = N (fragmentOut [a]) : eval xs
eval (Fun "FragmentOutDepth":N a:N b:xs) = N (fragmentOutDepth a [b]) : eval xs
eval (Fun "FragmentOutRastDepth":N a:xs) = N (fragmentOutRastDepth [a]) : eval xs
-- Vertex Out
eval (Fun "VertexOut":N a:Arg (LFloat b):N c:N d:xs) = N (vertexOut a (const_ (Single C.Float) (VFloat $ realToFrac b)) [c] [d]) : eval xs
-- PointSpriteCoordOrigin
eval (Fun "LowerLeft":xs) = PointSpriteCoordOrigin LowerLeft: eval xs
eval (Fun "UpperLeft":xs) = PointSpriteCoordOrigin UpperLeft: eval xs
-- Raster Context
eval (Fun "TriangleCtx":CullMode a:PolygonMode b:PolygonOffset c:ProvokingVertex d:xs) = RasterContext (TriangleCtx a b c d) : eval xs
eval (Fun "PointCtx":PointSize' a: Arg (LFloat b):PointSpriteCoordOrigin c:xs) = RasterContext (PointCtx a (realToFrac b) c) : eval xs
eval (Fun "LineCtx":Arg (LFloat a):ProvokingVertex b:xs) = RasterContext (LineCtx (realToFrac a) b) : eval xs
-- Fetch Primitive
eval (Fun "Points":xs) = FetchPrimitive Points: eval xs
eval (Fun "Lines":xs) = FetchPrimitive Lines: eval xs
eval (Fun "Triangles":xs) = FetchPrimitive Triangles : eval xs
eval (Fun "LinesAdjacency":xs) = FetchPrimitive LinesAdjacency: eval xs
eval (Fun "TrianglesAdjacency":xs) = FetchPrimitive TrianglesAdjacency: eval xs
-- Accumulation Context
eval (Fun "AccumulationContext":FragmentOperation a:xs) = N (accumulationContext Nothing [a]) : eval xs
eval (Fun "AccumulationContext":Tuple' _ a:xs) = N (accumulationContext Nothing [o | [FragmentOperation o] <- a]) : eval xs
-- Image
eval (Fun "ColorImage":Arg (LNat i):Val _ v:xs) = Img (ColorImage i v) : eval xs
eval (Fun "DepthImage":Arg (LNat i):Arg (LFloat a):xs) = Img (DepthImage i (realToFrac a)) : eval xs
eval (Fun "StencilImage":Arg (LNat i):Arg (LInt a):xs) = Img (StencilImage i (fromIntegral a)) : eval xs
-- Interpolation
eval (Fun "Smooth":N a:xs) = N (smooth a) : eval xs
eval (Fun "NoPerspective":N a:xs) = N (noPerspective a) : eval xs
eval (Fun "Flat":N a:xs) = N (flat a) : eval xs
-- Fragment Operation
eval (Fun "ColorOp":Blending b:xs) = FragmentOperation (ColorOp b (VV4B $ V4 True True True True)) : eval xs
eval (Fun "DepthOp":ComparisonFunction a:Bool' b:xs) = FragmentOperation (DepthOp a b) : eval xs
-- Blending
eval (Fun "NoBlending":xs) = Blending NoBlending : eval xs
eval (Fun "BlendLogicOp":LogicOperation a:xs) = Blending (BlendLogicOp a) : eval xs
eval (Fun "Blend":
  Tuple' _ [[BlendEquation a],[BlendEquation b]]:
  Tuple' _ [[Tuple' _ [[BlendingFactor c],[BlendingFactor d]]],[Tuple' _ [[BlendingFactor e],[BlendingFactor f]]]]:Val (Single C.V4F) (VV4F g):xs) =
    Blending (Blend (a,b) ((c,d),(e,f)) g) : eval xs
-- Fragment Filter
eval (Fun "PassAll":xs) = N passAll : eval xs
eval (Fun "Filter":N f:xs) = N (filter_ f) : eval xs
-- Render Operations
eval (Fun "Fetch":Arg (LString a):FetchPrimitive b:Input c (Single C.V4F):xs) = N (fetch (pack a) b [(c,V4F)]) : eval xs
eval (Fun "Transform":N a:N b:xs) = N (transform a b) : eval xs
eval (Fun "Rasterize":RasterContext a:N b:xs) = N (rasterize a b) : eval xs
eval (Fun "Accumulate":N a:N b:N c:N d:N e:xs) = N (accumulate a b c d e) : eval xs
eval (Fun "FrameBuffer":Img i:xs) = N (frameBuffer [i]) : eval xs
eval (Fun "FrameBuffer":Tuple' _ i:xs) = N (frameBuffer [a | [Img a] <- i]) : eval xs
eval (Fun "ScreenOut":N n:xs) = N (screenOut $ prjFrameBuffer mempty 0 n) : eval xs
-- Primitive Functions
eval (Fun "MulMV":N a:N b:xs) = N (primApp (Single V4F) C.PrimMulMatVec $ tup (Unknown "") [a,b]) : eval xs
eval (a:xs) = a : eval xs
eval l = l

toTy :: Typing -> C.Ty
toTy (_,_,TBool  _) = Single C.Bool 
toTy (_,_,TV2B   _) = Single C.V2B  
toTy (_,_,TV3B   _) = Single C.V3B  
toTy (_,_,TV4B   _) = Single C.V4B  
toTy (_,_,TWord  _) = Single C.Word 
toTy (_,_,TV2U   _) = Single C.V2U  
toTy (_,_,TV3U   _) = Single C.V3U  
toTy (_,_,TV4U   _) = Single C.V4U  
toTy (_,_,TInt   _) = Single C.Int  
toTy (_,_,TV2I   _) = Single C.V2I  
toTy (_,_,TV3I   _) = Single C.V3I  
toTy (_,_,TV4I   _) = Single C.V4I  
toTy (_,_,TFloat _) = Single C.Float
toTy (_,_,TV2F   _) = Single C.V2F  
toTy (_,_,TV3F   _) = Single C.V3F  
toTy (_,_,TV4F   _) = Single C.V4F  
toTy (_,_,TM22F  _) = Single C.M22F 
toTy (_,_,TM23F  _) = Single C.M23F 
toTy (_,_,TM24F  _) = Single C.M24F 
toTy (_,_,TM32F  _) = Single C.M32F 
toTy (_,_,TM33F  _) = Single C.M33F 
toTy (_,_,TM34F  _) = Single C.M34F 
toTy (_,_,TM42F  _) = Single C.M42F 
toTy (_,_,TM43F  _) = Single C.M43F 
toTy (_,_,TM44F  _) = Single C.M44F 
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