{-# LANGUAGE PatternSynonyms #-}
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

isPrimFun n = Set.member n primFunSet
primFunSet = Set.fromList
  [ "Fetch"
  , "Transform"
  , "Rasterize"
  , "Accumulate"
  , "FrameBuffer"
  , "ScreenOut"
  , "Uni"
  , "IBool"
  , "IV2B"
  , "IV3B"
  , "IV4B"
  , "IWord"
  , "IV2U"
  , "IV3U"
  , "IV4U"
  , "IInt"
  , "IV2I"
  , "IV3I"
  , "IV4I"
  , "IFloat"
  , "IV2F"
  , "IV3F"
  , "IV4F"
  , "IM22F"
  , "IM23F"
  , "IM24F"
  , "IM32F"
  , "IM33F"
  , "IM34F"
  , "IM42F"
  , "IM43F"
  , "IM44F"
  , "PassAll"
  , "Filter"
  , "Smooth"
  , "Flat"
  , "NoPerspective"
  , "NoBlending"
  , "BlendLogicOp"
  , "Blend"
  , "DepthOp"
  , "ColorOp"
  , "StencilOp"
  , "DepthImage"
  , "StencilImage"
  , "ColorImage"
  , "TriangleCtx"
  , "PointCtx"
  , "LineCtx"
  , "AccumulationContext"
  , "Points"
  , "Lines"
  , "Triangles"
  , "LinesAdjacency"
  , "TrianglesAdjacency"
  , "VertexOut"
  , "FragmentOut"
  , "FragmentOutDepth"
  , "FragmentOutRastDepth"
  , "MulMV"
  , "True"
  , "False"
  , "V2B"
  , "V3B"
  , "V4B"
  , "V2U"
  , "V3U"
  , "V4U"
  , "V2I"
  , "V3I"
  , "V4I"
  , "V2F"
  , "V3F"
  , "V4F"
  , "M22F"
  , "M23F"
  , "M24F"
  , "M32F"
  , "M33F"
  , "M34F"
  , "M42F"
  , "M43F"
  , "M44F"
  , "CullNone"
  , "CullFront"
  , "CullBack"
  , "CW"
  , "CCW"
  , "PolygonFill"
  , "PolygonPoint"
  , "PolygonLine"
  , "PointSize"
  , "ProgramPointSize"
  , "NoOffset"
  , "Offset"
  , "LastVertex"
  , "FirstVertex"
  , "Never"
  , "Less"
  , "Equal"
  , "Lequal"
  , "Greater"
  , "Notequal"
  , "Gequal"
  , "Always"
  , "OpZero"
  , "OpKeep"
  , "OpReplace"
  , "OpIncr"
  , "OpIncrWrap"
  , "OpDecr"
  , "OpDecrWrap"
  , "OpInvert"
  , "Clear"
  , "And"
  , "AndReverse"
  , "Copy"
  , "AndInverted"
  , "Noop"
  , "Xor"
  , "Or"
  , "Nor"
  , "Equiv"
  , "Invert"
  , "OrReverse"
  , "CopyInverted"
  , "OrInverted"
  , "Nand"
  , "Set"
  , "FuncAdd"
  , "FuncSubtract"
  , "FuncReverseSubtract"
  , "Min"
  , "Max"
  , "Zero"
  , "One"
  , "SrcColor"
  , "OneMinusSrcColor"
  , "DstColor"
  , "OneMinusDstColor"
  , "SrcAlpha"
  , "OneMinusSrcAlpha"
  , "DstAlpha"
  , "OneMinusDstAlpha"
  , "ConstantColor"
  , "OneMinusConstantColor"
  , "ConstantAlpha"
  , "OneMinusConstantAlpha"
  , "SrcAlphaSaturate"
  , "LowerLeft"
  , "UpperLeft"
  ]

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
  | TChar   Frequency
  | TString Frequency
  | TBool   Frequency
  | TWord   Frequency
  | TInt    Frequency
  | TFloat  Frequency

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

  -- Vector/Matrix
  | TV2B    Frequency
  | TV3B    Frequency
  | TV4B    Frequency
  | TV2U    Frequency
  | TV3U    Frequency
  | TV4U    Frequency
  | TV2I    Frequency
  | TV3I    Frequency
  | TV4I    Frequency
  | TV2F    Frequency
  | TV3F    Frequency
  | TV4F    Frequency
  | TM22F   Frequency
  | TM23F   Frequency
  | TM24F   Frequency
  | TM32F   Frequency
  | TM33F   Frequency
  | TM34F   Frequency
  | TM42F   Frequency
  | TM43F   Frequency
  | TM44F   Frequency

  -- ADT
  | TCullMode               Frequency
  | TPolygonMode            Frequency
  | TPolygonOffset          Frequency
  | TProvokingVertex        Frequency
  | TFrontFace              Frequency
  | TPointSize              Frequency
  | TBlendingFactor         Frequency
  | TBlendEquation          Frequency
  | TLogicOperation         Frequency
  | TStencilOperation       Frequency
  | TComparisonFunction     Frequency
  | TPointSpriteCoordOrigin Frequency

  -- GADT
  | TAccumulationContext  Frequency Ty
  | TBlending             Frequency Ty
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
  | TPrimitiveStream      Frequency PrimitiveType Nat Frequency Ty -- ???
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

inferPrimFun :: EName -> Unique Typing
inferPrimFun a = case a of
  -- Vector/Matrix
  "True"         -> ty $ TBool C
  "False"        -> ty $ TBool C
  "V2B"          -> ty $ TBool C ~> TBool C ~> TV2B C
  "V3B"          -> ty $ TBool C ~> TBool C ~> TBool C ~> TV3B C
  "V4B"          -> ty $ TBool C ~> TBool C ~> TBool C ~> TBool C ~> TV4B C
  "V2U"          -> ty $ TWord C ~> TWord C ~> TV2U C
  "V3U"          -> ty $ TWord C ~> TWord C ~> TWord C ~> TV3U C
  "V4U"          -> ty $ TWord C ~> TWord C ~> TWord C ~> TWord C ~> TV4U C
  "V2I"          -> ty $ TInt C ~> TInt C ~> TV2I C
  "V3I"          -> ty $ TInt C ~> TInt C ~> TInt C ~> TV3I C
  "V4I"          -> ty $ TInt C ~> TInt C ~> TInt C ~> TInt C ~> TV4I C
  "V2F"          -> ty $ TFloat C ~> TFloat C ~> TV2F C
  "V3F"          -> ty $ TFloat C ~> TFloat C ~> TFloat C ~> TV3F C
  "V4F"          -> ty $ TFloat C ~> TFloat C ~> TFloat C ~> TFloat C ~> TV4F C
  "M22F"         -> ty $ TV2F C ~> TV2F C ~> TM22F C
  "M23F"         -> ty $ TV2F C ~> TV2F C ~> TV2F C ~> TM23F C
  "M24F"         -> ty $ TV2F C ~> TV2F C ~> TV2F C ~> TV2F C ~> TM24F C
  "M32F"         -> ty $ TV3F C ~> TV3F C ~> TM32F C
  "M33F"         -> ty $ TV3F C ~> TV3F C ~> TV3F C ~> TM33F C
  "M34F"         -> ty $ TV3F C ~> TV3F C ~> TV3F C ~> TV3F C ~> TM34F C
  "M42F"         -> ty $ TV4F C ~> TV4F C ~> TM42F C
  "M43F"         -> ty $ TV4F C ~> TV4F C ~> TV4F C ~> TM43F C
  "M44F"         -> ty $ TV4F C ~> TV4F C ~> TV4F C ~> TV4F C ~> TM44F C
  -- Input declaration
  "Uni"          -> do t <- newVar C ; ty $ TInput C t ~> t
  "IBool"        -> ty $ TString C ~> TInput C (TBool  C)
  "IV2B"         -> ty $ TString C ~> TInput C (TV2B   C)
  "IV3B"         -> ty $ TString C ~> TInput C (TV3B   C)
  "IV4B"         -> ty $ TString C ~> TInput C (TV4B   C)
  "IWord"        -> ty $ TString C ~> TInput C (TWord  C)
  "IV2U"         -> ty $ TString C ~> TInput C (TV2U   C)
  "IV3U"         -> ty $ TString C ~> TInput C (TV3U   C)
  "IV4U"         -> ty $ TString C ~> TInput C (TV4U   C)
  "IInt"         -> ty $ TString C ~> TInput C (TInt   C)
  "IV2I"         -> ty $ TString C ~> TInput C (TV2I   C)
  "IV3I"         -> ty $ TString C ~> TInput C (TV3I   C)
  "IV4I"         -> ty $ TString C ~> TInput C (TV4I   C)
  "IFloat"       -> ty $ TString C ~> TInput C (TFloat C)
  "IV2F"         -> ty $ TString C ~> TInput C (TV2F   C)
  "IV3F"         -> ty $ TString C ~> TInput C (TV3F   C)
  "IV4F"         -> ty $ TString C ~> TInput C (TV4F   C)
  "IM22F"        -> ty $ TString C ~> TInput C (TM22F  C)
  "IM23F"        -> ty $ TString C ~> TInput C (TM23F  C)
  "IM24F"        -> ty $ TString C ~> TInput C (TM24F  C)
  "IM32F"        -> ty $ TString C ~> TInput C (TM32F  C)
  "IM33F"        -> ty $ TString C ~> TInput C (TM33F  C)
  "IM34F"        -> ty $ TString C ~> TInput C (TM34F  C)
  "IM42F"        -> ty $ TString C ~> TInput C (TM42F  C)
  "IM43F"        -> ty $ TString C ~> TInput C (TM43F  C)
  "IM44F"        -> ty $ TString C ~> TInput C (TM44F  C)
  -- BlendingFactor
  "Zero"                   -> ty $ TBlendingFactor C
  "One"                    -> ty $ TBlendingFactor C
  "SrcColor"               -> ty $ TBlendingFactor C
  "OneMinusSrcColor"       -> ty $ TBlendingFactor C
  "DstColor"               -> ty $ TBlendingFactor C
  "OneMinusDstColor"       -> ty $ TBlendingFactor C
  "SrcAlpha"               -> ty $ TBlendingFactor C
  "OneMinusSrcAlpha"       -> ty $ TBlendingFactor C
  "DstAlpha"               -> ty $ TBlendingFactor C
  "OneMinusDstAlpha"       -> ty $ TBlendingFactor C
  "ConstantColor"          -> ty $ TBlendingFactor C
  "OneMinusConstantColor"  -> ty $ TBlendingFactor C
  "ConstantAlpha"          -> ty $ TBlendingFactor C
  "OneMinusConstantAlpha"  -> ty $ TBlendingFactor C
  "SrcAlphaSaturate"       -> ty $ TBlendingFactor C
  -- BlendEquation
  "FuncAdd"                -> ty $ TBlendEquation C
  "FuncSubtract"           -> ty $ TBlendEquation C
  "FuncReverseSubtract"    -> ty $ TBlendEquation C
  "Min"                    -> ty $ TBlendEquation C
  "Max"                    -> ty $ TBlendEquation C
  -- LogicOperation
  "Clear"        -> ty $ TLogicOperation C
  "And"          -> ty $ TLogicOperation C
  "AndReverse"   -> ty $ TLogicOperation C
  "Copy"         -> ty $ TLogicOperation C
  "AndInverted"  -> ty $ TLogicOperation C
  "Noop"         -> ty $ TLogicOperation C
  "Xor"          -> ty $ TLogicOperation C
  "Or"           -> ty $ TLogicOperation C
  "Nor"          -> ty $ TLogicOperation C
  "Equiv"        -> ty $ TLogicOperation C
  "Invert"       -> ty $ TLogicOperation C
  "OrReverse"    -> ty $ TLogicOperation C
  "CopyInverted" -> ty $ TLogicOperation C
  "OrInverted"   -> ty $ TLogicOperation C
  "Nand"         -> ty $ TLogicOperation C
  "Set"          -> ty $ TLogicOperation C
  -- StencilOperation
  "OpZero"       -> ty $ TStencilOperation C
  "OpKeep"       -> ty $ TStencilOperation C
  "OpReplace"    -> ty $ TStencilOperation C
  "OpIncr"       -> ty $ TStencilOperation C
  "OpIncrWrap"   -> ty $ TStencilOperation C
  "OpDecr"       -> ty $ TStencilOperation C
  "OpDecrWrap"   -> ty $ TStencilOperation C
  "OpInvert"     -> ty $ TStencilOperation C
  -- ComparisonFunction
  "Never"    -> ty $ TComparisonFunction C
  "Less"     -> ty $ TComparisonFunction C
  "Equal"    -> ty $ TComparisonFunction C
  "Lequal"   -> ty $ TComparisonFunction C
  "Greater"  -> ty $ TComparisonFunction C
  "Notequal" -> ty $ TComparisonFunction C
  "Gequal"   -> ty $ TComparisonFunction C
  "Always"   -> ty $ TComparisonFunction C
  -- ProvokingVertex
  "LastVertex"   -> ty $ TProvokingVertex C
  "FirstVertex"  -> ty $ TProvokingVertex C
  -- CullMode
  "CullNone"     -> ty $ TCullMode C
  "CullFront"    -> ty $ TFrontFace C ~> TCullMode C
  "CullBack"     -> ty $ TFrontFace C ~> TCullMode C
  -- FrontFace
  "CW"   -> ty $ TFrontFace C
  "CCW"  -> ty $ TFrontFace C
  -- PolygonMode
  "PolygonFill"  -> ty $ TPolygonMode C
  "PolygonPoint" -> ty $ TPointSize C ~> TPolygonMode C
  "PolygonLine"  -> ty $ TFloat C ~> TPolygonMode C
  -- PolygonOffset
  "NoOffset" -> ty $ TPolygonOffset C
  "Offset"   -> ty $ TFloat C ~> TFloat C ~> TPolygonOffset C
  -- PointSize
  "PointSize"        -> ty $ TFloat C ~> TPointSize C
  "ProgramPointSize" -> ty $ TPointSize C
  -- Fragment Out
  "FragmentOut"  -> do t <- newVar C ; ty $ (TV4F C) ~> TFragmentOut C (TV4F C) --t
    --PFragmentOutDepth
    --PFragmentOutRastDepth
  -- Vertex Out
  "VertexOut"    -> do a <- newVar C ; ty $ TV4F C ~> TFloat C ~> TTuple C [] ~> TInterpolated C a ~> TVertexOut C a
  -- PointSpriteCoordOrigin
  "LowerLeft"  -> ty $ TPointSpriteCoordOrigin C
  "UpperLeft"  -> ty $ TPointSpriteCoordOrigin C
  -- Raster Context
  "TriangleCtx"  -> ty $ TCullMode C ~> TPolygonMode C ~> TPolygonOffset C ~> TProvokingVertex C ~> TRasterContext C TTriangle
  "PointCtx"     -> ty $ TPointSize C ~> TFloat C ~> TPointSpriteCoordOrigin C ~> TRasterContext C TPoint
  "LineCtx"      -> ty $ TFloat C ~> TProvokingVertex C ~> TRasterContext C TLine
  -- Fetch Primitive
  "Points"             -> ty $ TFetchPrimitive C TPoint
  "Lines"              -> ty $ TFetchPrimitive C TLine
  "Triangles"          -> ty $ TFetchPrimitive C TTriangle
  "LinesAdjacency"     -> ty $ TFetchPrimitive C TLineAdjacency
  "TrianglesAdjacency" -> ty $ TFetchPrimitive C TTriangleAdjacency
  -- Accumulation Context
  "AccumulationContext"  -> do t <- newVar C ; ty $ TFragmentOperation C t ~> TAccumulationContext C t
  -- Image
  "ColorImage"   -> do [a,b] <- newVars 2 C ; ty $ a ~> b ~> TImage C a b
  "DepthImage"   -> do [a] <- newVars 1 C ; ty $ a ~> TFloat C ~> TImage C a (TFloat C)
  "StencilImage" -> do [a] <- newVars 1 C ; ty $ a ~> TInt C ~> TImage C a (TInt C)
  -- Interpolation
  "Smooth"         -> do t <- newVar C ; ty $ t ~> TInterpolated C t
  "Flat"           -> do t <- newVar C ; ty $ t ~> TInterpolated C t
  "NoPerspective"  -> do t <- newVar C ; ty $ t ~> TInterpolated C t
  -- Fragment Operation
  "ColorOp"    -> do a <- newVar C ; ty $ TBlending C a ~> TFragmentOperation C ({-Color-} a) -- TODO: type family needed
  "DepthOp"    -> ty $ TComparisonFunction C ~> TBool C ~> TFragmentOperation C (TFloat C)
    -- "StencilOp       :: StencilTests -> StencilOps -> StencilOps -> FragmentOperation (Stencil Int32)
  -- Blending
  "NoBlending"   -> do t <- newVar C ; ty $ TBlending C t
  "BlendLogicOp" -> do t <- newVar C ; ty $ TLogicOperation C ~> TBlending C t
  "Blend"        -> do t <- newVar C ; ty $ TTuple C [TBlendEquation C,TBlendEquation C]
                         ~> TTuple C [TTuple C [TBlendingFactor C,TBlendingFactor C],TTuple C [TBlendingFactor C,TBlendingFactor C]]
                         ~> TV4F C ~> TBlending C t --(TFloat C)
  -- Fragment Filter
  "PassAll"  -> do t <- newVar C ; ty $ TFragmentFilter C t
  "Filter"   -> do t <- newVar C ; ty $ (t ~> TBool C) ~> TFragmentFilter C t
  -- Render Operations
  "Fetch"        -> do [a,b] <- newVars 2 C ; ty $ TString C ~> TFetchPrimitive C a ~> TInput C b ~> TVertexStream C a b
  "Transform"    -> do [a,b,p] <- newVars 3 C ; ty $ (a ~> TVertexOut C b) ~> TVertexStream C p a ~> TPrimitiveStream C p (TNat 1) C b
  "Rasterize"    -> do [a,b,c] <- newVars 3 C ; ty $ TRasterContext C a ~> TPrimitiveStream C a b C c ~> TFragmentStream C b c
  "Accumulate"   -> do [a,b,n] <- newVars 3 C ; ty $ TAccumulationContext C b ~> TFragmentFilter C a ~> (a ~> TFragmentOut C b) ~> TFragmentStream C n a ~> TFrameBuffer C ~> TFrameBuffer C
  "FrameBuffer"  -> do [a,b] <- newVars 2 C ; ty $ TImage C a b ~> TFrameBuffer C
  "ScreenOut"    -> ty $ TFrameBuffer C ~> TOutput C
  -- Primitive Functions
  "MulMV"        -> ty $ TM44F C ~> TV4F C ~> TV4F C
  a -> throwErrorUnique $ "unknown primitive: " ++ show a

inferLit :: Lit -> Unique Typing
inferLit a = case a of
  LInt _ -> do
    --t <- newVar C
    --return (mempty,[(CNum,t)],t) -- ????
    ty $ TInt C
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
