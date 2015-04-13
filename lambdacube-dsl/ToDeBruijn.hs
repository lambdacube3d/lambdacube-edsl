{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module ToDeBruijn
    ( toNF
    , NF (N)
    ) where

import Data.ByteString.Char8 hiding (take, drop)
import Data.Map as Map
import Data.Monoid
import LambdaCube.Core.DeBruijn hiding (Exp,N)
import LambdaCube.Core.DeBruijn (N())
import LambdaCube.Core.Type hiding (Ty)
import qualified LambdaCube.Core.PrimFun as C
import qualified LambdaCube.Core.Type as C

import CompositionalLC
import Type
import Typing (isPrimFun)

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
  | App C.Ty
  | Val C.Ty Value
  | Img Image
  | N   C.Ty N
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
  = LetVar (Exp (Subst, Typing))
  | LamVar

type NFEnv = Map EName EnvVar
-- TODO: add let
toNF :: Subst -> NFEnv -> Exp (Subst, Typing) -> [NF]
toNF sub env (ELit t l) = [Arg l]
toNF sub env (EApp t a b) = eval $ {-[App $ toTy (m,i,subst sub t)] `mappend` -} toNF sub' env a `mappend` toNF sub' env b
  where sub' = addSubst t sub
toNF sub env (ELet t (PVar _ n) a b) = --case toNF env a of -- TODO
  -- x@(N _:_) -> error $ "let is not fully supported: " ++ show x
  --x -> toNF (Map.insert n x env) b
  toNF sub' (Map.insert n (LetVar a) env) b
  where sub' = addSubst t sub
--toNF sub env (EPrimFun t f) = 
toNF sub env (EVar t n)
  | isPrimFun n = [Fun n]
  | otherwise = case Map.lookup n env of
      Nothing -> error $ "unknown variable: " ++ n
      Just (LetVar x) -> eval $ toNF sub' env x
      Just LamVar     -> let ty = toTy' sub t in eval [N ty $ var ty 0 ""]
  where sub' = addSubst t sub
toNF sub env (ELam t (PVar _ n) e) =
  case eval $ toNF sub' (Map.insert n LamVar env) e of
    [N _ x] -> let ty = toTy' sub t in [N ty $ lam ty $ body x]
    x -> error $ "lam is not fully supported: " ++ show x
  where sub' = addSubst t sub
toNF sub env (ETuple t []) = let ty = toTy' sub t in [N ty $ tup ty []]
  where sub' = addSubst t sub
toNF sub env (ETuple t a) = [Tuple' (toTy' sub' t) (fmap (eval . toNF sub' env) a)]
  where sub' = addSubst t sub
--toNF _ _ x = error $ "toNF error: " ++ show x

toTy' s (_, t) = toTy $ subst s t
addSubst (s, _) sub = s `composeSubst` sub

eval :: [NF] -> [NF]
-- Const and other temp construction
eval (Fun "Tup":N t v:xs) = N t v : eval xs
eval (Fun "Tup":Tuple' t v:xs) = N (Unknown $ "Tup " ++ show t) (tup t [o | [N _ o] <- v]) : eval xs
eval (Fun "Const":Val t v:xs) = N (Unknown $ "Const " ++ show t) (const_ t v) : eval xs
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
eval (Fun "Uni":Input n t:xs) = N t (uni t n) : eval xs
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
eval (Fun "FragmentOut":Val t v:xs) = N (Unknown $ "fragmentOut" ++ show t) (fragmentOut [const_ t v]) : eval xs
eval (Fun "FragmentOut":N t a:xs) = N (Unknown $ "fragmentOut" ++ show t) (fragmentOut [a]) : eval xs
eval (Fun "FragmentOutDepth":N _ a:N t b:xs) = N (Unknown $ "fragmentOutDepth" ++ show t) (fragmentOutDepth a [b]) : eval xs
eval (Fun "FragmentOutRastDepth":N t a:xs) = N (Unknown $ "fragmentOutRastDepth" ++ show t) (fragmentOutRastDepth [a]) : eval xs
eval (Fun "FragmentOutRastDepth":Val t v:xs) = N (Unknown $ "fragmentOutRastDepth" ++ show t) (fragmentOutRastDepth [const_ t v]) : eval xs
-- Vertex Out
eval (Fun "VertexOut":N _ a:Arg (LFloat b):N _ c:N t d:xs) = N (Unknown $ "vertexOut" ++ show t) (vertexOut a (const_ (Single C.Float) (VFloat $ realToFrac b)) [c] [d]) : eval xs
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
eval (Fun "AccumulationContext":FragmentOperation a:xs) = N noType (accumulationContext Nothing [a]) : eval xs
eval (Fun "AccumulationContext":Tuple' _ a:xs) = N noType (accumulationContext Nothing [o | [FragmentOperation o] <- a]) : eval xs
-- Image
eval (Fun "ColorImage":Arg (LNat i):Val _ v:xs) = Img (ColorImage i v) : eval xs
eval (Fun "DepthImage":Arg (LNat i):Arg (LFloat a):xs) = Img (DepthImage i (realToFrac a)) : eval xs
eval (Fun "StencilImage":Arg (LNat i):Arg (LInt a):xs) = Img (StencilImage i (fromIntegral a)) : eval xs
-- Interpolation
eval (Fun "Smooth":N _ a:xs) = N noType (smooth a) : eval xs
eval (Fun "NoPerspective":N _ a:xs) = N noType (noPerspective a) : eval xs
eval (Fun "Flat":N _ a:xs) = N noType (flat a) : eval xs
-- Fragment Operation
eval (Fun "ColorOp":Blending b:Val t v:xs) = FragmentOperation (ColorOp b v) : eval xs
eval (Fun "DepthOp":ComparisonFunction a:Bool' b:xs) = FragmentOperation (DepthOp a b) : eval xs
-- Blending
eval (Fun "NoBlending":xs) = Blending NoBlending : eval xs
eval (Fun "BlendLogicOp":LogicOperation a:xs) = Blending (BlendLogicOp a) : eval xs
eval (Fun "Blend":
  Tuple' _ [[BlendEquation a],[BlendEquation b]]:
  Tuple' _ [[Tuple' _ [[BlendingFactor c],[BlendingFactor d]]],[Tuple' _ [[BlendingFactor e],[BlendingFactor f]]]]:Val (Single C.V4F) (VV4F g):xs) =
    Blending (Blend (a,b) ((c,d),(e,f)) g) : eval xs
-- Fragment Filter
eval (Fun "PassAll":xs) = N noType passAll : eval xs
eval (Fun "Filter":N _ f:xs) = N noType (filter_ f) : eval xs
-- Render Operations
eval (Fun "Fetch":Arg (LString a):FetchPrimitive b:Input c (Single V4F):xs) = N noType (fetch (pack a) b [(c,V4F)]) : eval xs
eval (Fun "Transform":N _ a:N _ b:xs) = N noType (transform a b) : eval xs
eval (Fun "Rasterize":RasterContext a:N _ b:xs) = N noType (rasterize a b) : eval xs
eval (Fun "Accumulate":N _ a:N _ b:N _ c:N _ d:N _ e:xs) = N noType (accumulate a b c d e) : eval xs
eval (Fun "FrameBuffer":Img i:xs) = N noType (frameBuffer [i]) : eval xs
eval (Fun "FrameBuffer":Tuple' _ i:xs) = N noType (frameBuffer [a | [Img a] <- i]) : eval xs
eval (Fun "ScreenOut":N _ n:xs) = N noType (screenOut $ prjFrameBuffer mempty 0 n) : eval xs
-- * Primitive Functions *
-- Arithmetic Functions (componentwise)
eval (Fun "PrimAdd"   :N t a:N _ b:xs) = N t (primApp t C.PrimAdd  $ tup (Unknown "") [a,b]) : eval xs
eval (Fun "PrimAddS"  :N t a:N _ b:xs) = N t (primApp t C.PrimAddS $ tup (Unknown "") [a,b]) : eval xs
eval (Fun "PrimSub"   :N t a:N _ b:xs) = N t (primApp t C.PrimSub  $ tup (Unknown "") [a,b]) : eval xs
eval (Fun "PrimSubS"  :N t a:N _ b:xs) = N t (primApp t C.PrimSubS $ tup (Unknown "") [a,b]) : eval xs
eval (Fun "PrimMul"   :N t a:N v b:xs) = N t (primApp t C.PrimMul  $ tup (Tuple [t,v]) [a,b]) : eval xs
eval (Fun "PrimMulS"  :N t a:N _ b:xs) = N t (primApp t C.PrimMulS $ tup (Unknown "") [a,b]) : eval xs
eval (Fun "PrimDiv"   :N t a:N _ b:xs) = N t (primApp t C.PrimDiv  $ tup (Unknown "") [a,b]) : eval xs
eval (Fun "PrimDivS"  :N t a:N _ b:xs) = N t (primApp t C.PrimDivS $ tup (Unknown "") [a,b]) : eval xs
eval (Fun "PrimNeg"   :N t a:xs) = N t (primApp t C.PrimNeg a) : eval xs
eval (Fun "PrimMod"   :N t a:N _ b:xs) = N t (primApp t C.PrimMod  $ tup (Unknown "") [a,b]) : eval xs
eval (Fun "PrimModS"  :N t a:N _ b:xs) = N t (primApp t C.PrimModS $ tup (Unknown "") [a,b]) : eval xs
-- Bit-wise Functions
eval (Fun "PrimBAnd"      :N _ a:N _ b:xs) = N noType (primApp noType C.PrimBAnd      $ tup (Unknown "") [a,b]) : eval xs
eval (Fun "PrimBAndS"     :N _ a:N _ b:xs) = N noType (primApp noType C.PrimBAndS     $ tup (Unknown "") [a,b]) : eval xs
eval (Fun "PrimBOr"       :N _ a:N _ b:xs) = N noType (primApp noType C.PrimBOr       $ tup (Unknown "") [a,b]) : eval xs
eval (Fun "PrimBOrS"      :N _ a:N _ b:xs) = N noType (primApp noType C.PrimBOrS      $ tup (Unknown "") [a,b]) : eval xs
eval (Fun "PrimBXor"      :N _ a:N _ b:xs) = N noType (primApp noType C.PrimBXor      $ tup (Unknown "") [a,b]) : eval xs
eval (Fun "PrimBXorS"     :N _ a:N _ b:xs) = N noType (primApp noType C.PrimBXorS     $ tup (Unknown "") [a,b]) : eval xs
eval (Fun "PrimBNot"      :N _ a:xs) = N noType (primApp noType C.PrimBNot a) : eval xs
eval (Fun "PrimBShiftL"   :N _ a:N _ b:xs) = N noType (primApp noType C.PrimBShiftL   $ tup (Unknown "") [a,b]) : eval xs
eval (Fun "PrimBShiftLS"  :N _ a:N _ b:xs) = N noType (primApp noType C.PrimBShiftLS  $ tup (Unknown "") [a,b]) : eval xs
eval (Fun "PrimBShiftR"   :N _ a:N _ b:xs) = N noType (primApp noType C.PrimBShiftR   $ tup (Unknown "") [a,b]) : eval xs
eval (Fun "PrimBShiftRS"  :N _ a:N _ b:xs) = N noType (primApp noType C.PrimBShiftRS  $ tup (Unknown "") [a,b]) : eval xs
-- Logic Functions
eval (Fun "PrimAnd" :N _ a:N _ b:xs) = N noType (primApp noType C.PrimAnd  $ tup (Unknown "") [a,b]) : eval xs
eval (Fun "PrimOr"  :N _ a:N _ b:xs) = N noType (primApp noType C.PrimOr   $ tup (Unknown "") [a,b]) : eval xs
eval (Fun "PrimXor" :N _ a:N _ b:xs) = N noType (primApp noType C.PrimXor  $ tup (Unknown "") [a,b]) : eval xs
eval (Fun "PrimNot" :N _ a:xs) = N noType (primApp noType C.PrimNot  a) : eval xs
eval (Fun "PrimAny" :N _ a:xs) = N noType (primApp noType C.PrimAny  a) : eval xs
eval (Fun "PrimAll" :N _ a:xs) = N noType (primApp noType C.PrimAll  a) : eval xs
-- Angle and Trigonometry Functions
eval (Fun "PrimACos"    :N _ a:xs) = N noType (primApp noType C.PrimACos    a) : eval xs
eval (Fun "PrimACosH"   :N _ a:xs) = N noType (primApp noType C.PrimACosH   a) : eval xs
eval (Fun "PrimASin"    :N _ a:xs) = N noType (primApp noType C.PrimASin    a) : eval xs
eval (Fun "PrimASinH"   :N _ a:xs) = N noType (primApp noType C.PrimASinH   a) : eval xs
eval (Fun "PrimATan"    :N _ a:xs) = N noType (primApp noType C.PrimATan    a) : eval xs
eval (Fun "PrimATan2"   :N _ a:N _ b:xs) = N noType (primApp noType C.PrimATan2   $ tup noType [a,b]) : eval xs
eval (Fun "PrimATanH"   :N _ a:xs) = N noType (primApp noType C.PrimATanH   a) : eval xs
eval (Fun "PrimCos"     :N _ a:xs) = N noType (primApp noType C.PrimCos     a) : eval xs
eval (Fun "PrimCosH"    :N _ a:xs) = N noType (primApp noType C.PrimCosH    a) : eval xs
eval (Fun "PrimDegrees" :N _ a:xs) = N noType (primApp noType C.PrimDegrees a) : eval xs
eval (Fun "PrimRadians" :N _ a:xs) = N noType (primApp noType C.PrimRadians a) : eval xs
eval (Fun "PrimSin"     :N _ a:xs) = N noType (primApp noType C.PrimSin     a) : eval xs
eval (Fun "PrimSinH"    :N _ a:xs) = N noType (primApp noType C.PrimSinH    a) : eval xs
eval (Fun "PrimTan"     :N _ a:xs) = N noType (primApp noType C.PrimTan     a) : eval xs
eval (Fun "PrimTanH"    :N _ a:xs) = N noType (primApp noType C.PrimTanH    a) : eval xs
-- Exponential Functions
eval (Fun "PrimPow"     :N _ a:N _ b:xs) = N noType (primApp noType C.PrimPow   $ tup noType [a,b]) : eval xs
eval (Fun "PrimExp"     :N _ a:xs) = N noType (primApp noType C.PrimExp     a) : eval xs
eval (Fun "PrimLog"     :N _ a:xs) = N noType (primApp noType C.PrimLog     a) : eval xs
eval (Fun "PrimExp2"    :N _ a:xs) = N noType (primApp noType C.PrimExp2    a) : eval xs
eval (Fun "PrimLog2"    :N _ a:xs) = N noType (primApp noType C.PrimLog2    a) : eval xs
eval (Fun "PrimSqrt"    :N _ a:xs) = N noType (primApp noType C.PrimSqrt    a) : eval xs
eval (Fun "PrimInvSqrt" :N _ a:xs) = N noType (primApp noType C.PrimInvSqrt a) : eval xs
-- Common Functions
eval (Fun "PrimIsNan"       :N _ a:xs) = N noType (primApp noType C.PrimIsNan     a) : eval xs
eval (Fun "PrimIsInf"       :N _ a:xs) = N noType (primApp noType C.PrimIsInf     a) : eval xs
eval (Fun "PrimAbs"         :N _ a:xs) = N noType (primApp noType C.PrimAbs       a) : eval xs
eval (Fun "PrimSign"        :N _ a:xs) = N noType (primApp noType C.PrimSign      a) : eval xs
eval (Fun "PrimFloor"       :N _ a:xs) = N noType (primApp noType C.PrimFloor     a) : eval xs
eval (Fun "PrimTrunc"       :N _ a:xs) = N noType (primApp noType C.PrimTrunc     a) : eval xs
eval (Fun "PrimRound"       :N _ a:xs) = N noType (primApp noType C.PrimRound     a) : eval xs
eval (Fun "PrimRoundEven"   :N _ a:xs) = N noType (primApp noType C.PrimRoundEven a) : eval xs
eval (Fun "PrimCeil"        :N _ a:xs) = N noType (primApp noType C.PrimCeil      a) : eval xs
eval (Fun "PrimFract"       :N _ a:xs) = N noType (primApp noType C.PrimFract     a) : eval xs
eval (Fun "PrimModF"        :N _ a:xs) = N noType (primApp noType C.PrimModF      a) : eval xs
eval (Fun "PrimMin"         :N _ a:N _ b:xs) = N noType (primApp noType C.PrimMin   $ tup noType [a,b]) : eval xs
eval (Fun "PrimMinS"        :N _ a:N _ b:xs) = N noType (primApp noType C.PrimMinS  $ tup noType [a,b]) : eval xs
eval (Fun "PrimMax"         :N _ a:N _ b:xs) = N noType (primApp noType C.PrimMax   $ tup noType [a,b]) : eval xs
eval (Fun "PrimMaxS"        :N _ a:N _ b:xs) = N noType (primApp noType C.PrimMaxS  $ tup noType [a,b]) : eval xs
eval (Fun "PrimClamp"       :N _ a:N _ b:N _ c:xs) = N noType (primApp noType C.PrimClamp  $ tup noType [a,b,c]) : eval xs
eval (Fun "PrimClampS"      :N _ a:N _ b:N _ c:xs) = N noType (primApp noType C.PrimClampS  $ tup noType [a,b,c]) : eval xs
eval (Fun "PrimMix"         :N _ a:N _ b:N _ c:xs) = N noType (primApp noType C.PrimMix  $ tup noType [a,b,c]) : eval xs
eval (Fun "PrimMixS"        :N _ a:N _ b:N _ c:xs) = N noType (primApp noType C.PrimMixS  $ tup noType [a,b,c]) : eval xs
eval (Fun "PrimMixB"        :N _ a:N _ b:N _ c:xs) = N noType (primApp noType C.PrimMixB  $ tup noType [a,b,c]) : eval xs
eval (Fun "PrimStep"        :N _ a:N _ b:xs) = N noType (primApp noType C.PrimStep   $ tup noType [a,b]) : eval xs
eval (Fun "PrimStepS"       :N _ a:N _ b:xs) = N noType (primApp noType C.PrimStepS  $ tup noType [a,b]) : eval xs
eval (Fun "PrimSmoothStep"  :N _ a:N _ b:N _ c:xs) = N noType (primApp noType C.PrimSmoothStep  $ tup noType [a,b,c]) : eval xs
eval (Fun "PrimSmoothStepS" :N _ a:N _ b:N _ c:xs) = N noType (primApp noType C.PrimSmoothStepS  $ tup noType [a,b,c]) : eval xs
-- Integer/Float Conversion Functions
eval (Fun "PrimFloatBitsToInt"  :N _ a:xs) = N noType (primApp noType C.PrimFloatBitsToInt  a) : eval xs
eval (Fun "PrimFloatBitsToUInt" :N _ a:xs) = N noType (primApp noType C.PrimFloatBitsToUInt a) : eval xs
eval (Fun "PrimIntBitsToFloat"  :N _ a:xs) = N noType (primApp noType C.PrimIntBitsToFloat  a) : eval xs
eval (Fun "PrimUIntBitsToFloat" :N _ a:xs) = N noType (primApp noType C.PrimUIntBitsToFloat a) : eval xs
-- Geometric Functions
eval (Fun "PrimLength"      :N _ a:xs) = N noType (primApp noType C.PrimLength  a) : eval xs
eval (Fun "PrimDistance"    :N _ a:N _ b:xs) = N noType (primApp noType C.PrimDistance $ tup noType [a,b]) : eval xs
eval (Fun "PrimDot"         :N _ a:N _ b:xs) = N noType (primApp noType C.PrimDot   $ tup noType [a,b]) : eval xs
eval (Fun "PrimCross"       :N _ a:N _ b:xs) = N noType (primApp noType C.PrimCross   $ tup noType [a,b]) : eval xs
eval (Fun "PrimNormalize"   :N _ a:xs) = N noType (primApp noType C.PrimNormalize  a) : eval xs
eval (Fun "PrimFaceForward" :N _ a:N _ b:N _ c:xs) = N noType (primApp noType C.PrimFaceForward  $ tup noType [a,b,c]) : eval xs
eval (Fun "PrimReflect"     :N _ a:N _ b:xs) = N noType (primApp noType C.PrimReflect   $ tup noType [a,b]) : eval xs
eval (Fun "PrimRefract"     :N _ a:N _ b:N _ c:xs) = N noType (primApp noType C.PrimRefract  $ tup noType [a,b,c]) : eval xs
-- Matrix Functions
eval (Fun "PrimTranspose"     :N _ a:xs) = N noType (primApp noType C.PrimTranspose a) : eval xs
eval (Fun "PrimDeterminant"   :N _ a:xs) = N noType (primApp noType C.PrimDeterminant a) : eval xs
eval (Fun "PrimInverse"       :N _ a:xs) = N noType (primApp noType C.PrimInverse a) : eval xs
eval (Fun "PrimOuterProduct"  :N _ a:N _ b:xs) = N noType (primApp noType C.PrimOuterProduct $ tup noType [a,b]) : eval xs
eval (Fun "PrimMulMatVec"     :N _ a:N t b:xs) = N t (primApp t C.PrimMulMatVec $ tup noType [a,b]) : eval xs
eval (Fun "PrimMulVecMat"     :N _ a:N _ b:xs) = N noType (primApp noType C.PrimMulVecMat $ tup noType [a,b]) : eval xs
eval (Fun "PrimMulMatMat"     :N _ a:N _ b:xs) = N noType (primApp noType C.PrimMulMatMat $ tup noType [a,b]) : eval xs
-- Vector and Scalar Relational Functions
eval (Fun "PrimLessThan"          :N _ a:N _ b:xs) = N noType (primApp noType C.PrimLessThan $ tup noType [a,b]) : eval xs
eval (Fun "PrimLessThanEqual"     :N _ a:N _ b:xs) = N noType (primApp noType C.PrimLessThanEqual $ tup noType [a,b]) : eval xs
eval (Fun "PrimGreaterThan"       :N _ a:N _ b:xs) = N noType (primApp noType C.PrimGreaterThan $ tup noType [a,b]) : eval xs
eval (Fun "PrimGreaterThanEqual"  :N _ a:N _ b:xs) = N noType (primApp noType C.PrimGreaterThanEqual $ tup noType [a,b]) : eval xs
eval (Fun "PrimEqualV"            :N _ a:N _ b:xs) = N noType (primApp noType C.PrimEqualV $ tup noType [a,b]) : eval xs
eval (Fun "PrimEqual"             :N _ a:N _ b:xs) = N noType (primApp noType C.PrimEqual $ tup noType [a,b]) : eval xs
eval (Fun "PrimNotEqualV"         :N _ a:N _ b:xs) = N noType (primApp noType C.PrimNotEqualV $ tup noType [a,b]) : eval xs
eval (Fun "PrimNotEqual"          :N _ a:N _ b:xs) = N noType (primApp noType C.PrimNotEqual $ tup noType [a,b]) : eval xs
-- Fragment Processing Functions
eval (Fun "PrimDFdx"    :N _ a:xs) = N noType (primApp noType C.PrimDFdx a) : eval xs
eval (Fun "PrimDFdy"    :N _ a:xs) = N noType (primApp noType C.PrimDFdy a) : eval xs
eval (Fun "PrimFWidth"  :N _ a:xs) = N noType (primApp noType C.PrimFWidth a) : eval xs
-- Noise Functions
eval (Fun "PrimNoise1"  :N _ a:xs) = N noType (primApp noType C.PrimNoise1 a) : eval xs
eval (Fun "PrimNoise2"  :N _ a:xs) = N noType (primApp noType C.PrimNoise2 a) : eval xs
eval (Fun "PrimNoise3"  :N _ a:xs) = N noType (primApp noType C.PrimNoise3 a) : eval xs
eval (Fun "PrimNoise4"  :N _ a:xs) = N noType (primApp noType C.PrimNoise4 a) : eval xs
eval (a:xs) = a : eval xs
eval l = l

noType = Unknown ""

-- TODO: use constraints
toTy :: Typing -> C.Ty
toTy ty@(Typing m i t) = case t of
    TBool  _ -> Single C.Bool 
    TWord  _ -> Single C.Word 
    TInt   _ -> Single C.Int  
    TFloat _ -> Single C.Float
    TVec 2 (TBool a) -> Single C.V2B
    TVec 3 (TBool a) -> Single C.V3B
    TVec 4 (TBool a) -> Single C.V4B
    TVec 2 (TWord a) -> Single C.V2U
    TVec 3 (TWord a) -> Single C.V3U
    TVec 4 (TWord a) -> Single C.V4U
    TVec 2 (TInt a) -> Single C.V2I
    TVec 3 (TInt a) -> Single C.V3I
    TVec 4 (TInt a) -> Single C.V4I
    TVec 2 (TFloat a) -> Single C.V2F
    TVec 3 (TFloat a) -> Single C.V3F
    TVec 4 (TFloat a) -> Single C.V4F
    TMat 2 2 (TFloat a) -> Single C.M22F
    TMat 2 3 (TFloat a) -> Single C.M23F
    TMat 2 4 (TFloat a) -> Single C.M24F
    TMat 3 2 (TFloat a) -> Single C.M32F
    TMat 3 3 (TFloat a) -> Single C.M33F
    TMat 3 4 (TFloat a) -> Single C.M34F
    TMat 4 2 (TFloat a) -> Single C.M42F
    TMat 4 3 (TFloat a) -> Single C.M43F
    TMat 4 4 (TFloat a) -> Single C.M44F

    TInterpolated _ t -> toTy $ Typing m i t
    t -> Unknown (show ty)
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
