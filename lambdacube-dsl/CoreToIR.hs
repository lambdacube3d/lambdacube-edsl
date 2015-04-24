{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module CoreToIR where

import Debug.Trace
import Control.Applicative
import Control.Monad.State
import Data.Monoid
import Text.Show.Pretty
import qualified Data.Vector as V
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Type as AST
import Type hiding (ELet, EApp, ELam, EVar, ELit, ETuple, Exp, Exp_ (..), Pat, PVar, PLit, PTuple)
import Core
import CoreToGLSL
import qualified IR as IR

type CG = State IR.Pipeline

test'' n f = test_ n $ ppShow . f
test_ n f = either error f <$> runMM "./tests/accept" (parseAndToCoreMain n)

emptyPipeline = IR.Pipeline mempty mempty mempty mempty mempty mempty
testCompile n = test'' n (\a -> execState (compilePipeline . reduce mempty mempty $ a) emptyPipeline)
run' n = testCompile n >>= putStrLn
run = run' "gfx03"

imageToSemantic :: IR.Image -> (IR.ImageSemantic, IR.Value)
imageToSemantic a = case a of
  IR.DepthImage _ v   -> (IR.Depth, IR.VFloat v)
  IR.StencilImage _ v -> (IR.Stencil, IR.VInt v)
  IR.ColorImage _ v   -> (IR.Color, v)

newRenderTarget :: [IR.Image] -> CG IR.RenderTargetName
newRenderTarget a = do
  tv <- gets IR.targets
  let t = IR.RenderTarget [(s,Just (IR.Framebuffer s)) | i <- a, let s = fst (imageToSemantic i)]
  modify (\s -> s {IR.targets = tv `V.snoc` t})
  return $ V.length tv

compilePipeline :: Exp -> CG ()
compilePipeline e = do
  c <- getCommands e
  modify (\s -> s {IR.commands = c})

mergeSlot a b = a
  { IR.slotUniforms = IR.slotUniforms a <> IR.slotUniforms b
  , IR.slotStreams  = IR.slotStreams a <> IR.slotStreams b
  , IR.slotPrograms = IR.slotPrograms a <> IR.slotPrograms b
  }

getSlot :: Exp -> CG IR.SlotName
getSlot (A3 "Fetch" (ELit (LString slotName)) prim attrs) = do
  let input = compInput attrs
      slot = IR.Slot
        { IR.slotName       = slotName
        , IR.slotUniforms   = mempty
        , IR.slotStreams    = Map.fromList input
        , IR.slotPrimitive  = compFetchPrimitive prim
        , IR.slotPrograms   = []
        }
  sv <- gets IR.slots
  case V.findIndex ((slotName ==) . IR.slotName) sv of
    Nothing -> do
      modify (\s -> s {IR.slots = sv `V.snoc` slot})
      return $ V.length sv
    Just i -> do
      modify (\s -> s {IR.slots = sv V.// [(i,mergeSlot (sv V.! i) slot)]})
      return i

addProgramToSlot :: IR.ProgramName -> IR.SlotName -> CG ()
addProgramToSlot prgName slotName = do
  sv <- gets IR.slots
  pv <- gets IR.programs
  let slot = sv V.! slotName
      prg = pv V.! prgName
      slot' = slot
        { IR.slotUniforms = IR.slotUniforms slot <> IR.programUniforms prg
        , IR.slotPrograms = IR.slotPrograms slot <> [prgName]
        }
  modify (\s -> s {IR.slots = sv V.// [(slotName,slot')]})

getProgram :: IR.SlotName -> Exp -> Exp -> CG IR.ProgramName
getProgram slot vert frag = do
  let (vertOut,vertSrc) = genVertexGLSL vert
      fragSrc = genFragmentGLSL vertOut frag
      prg = IR.Program
        { IR.programUniforms    = Map.fromList $ Set.toList $ getUniforms vert <> getUniforms frag
        , IR.programStreams     = Map.fromList [("v",("position",IR.V4F))] -- TODO
        , IR.programInTextures  = mempty -- TODO
        , IR.programOutput      = [("f0",IR.V4F)] -- TODO
        , IR.vertexShader       = trace vertSrc vertSrc
        , IR.geometryShader     = mempty -- TODO
        , IR.fragmentShader     = trace fragSrc fragSrc
        }
  pv <- gets IR.programs
  modify (\s -> s {IR.programs = pv `V.snoc` prg})
  let prgName = V.length pv
  addProgramToSlot prgName slot
  return prgName

getCommands :: Exp -> CG [IR.Command]
getCommands e = case e of
  A5 "Accumulate" actx ffilter frag (A2 "Rasterize" rctx (A2 "Transform" vert input)) fbuf -> do
    slot <- getSlot input
    prog <- getProgram slot vert frag
    (<>) <$> getCommands fbuf <*> pure [IR.SetRasterContext (compRC rctx), IR.SetAccumulationContext (compAC actx), IR.SetProgram prog, IR.RenderSlot slot]
  A1 "FrameBuffer" a -> do
    let i = compImg a
    rt <- newRenderTarget i
    pure [IR.SetRenderTarget rt, IR.ClearRenderTarget (map imageToSemantic i)]
  Exp e -> F.foldrM (\a b -> (<> b) <$> getCommands a) [] e

getUniforms :: Exp -> Set (String,IR.InputType)
getUniforms e = case e of
  A1 "Uni" a -> Set.fromList $ compInput a
  Exp e -> F.foldMap getUniforms e

compAC x = case x of
  A1 "AccumulationContext" (ETuple a) -> IR.AccumulationContext Nothing (map compFrag a)
  A1 "AccumulationContext" a -> IR.AccumulationContext Nothing [compFrag a]
  x -> error $ "compAC " ++ show x

compBlending x = case x of
  A0 "NoBlending" -> IR.NoBlending
  A1 "BlendLogicOp" a -> IR.BlendLogicOp (compLO a)
  A3 "Blend" (ETuple [a,b]) (ETuple [ETuple [c,d],ETuple [e,f]]) (compValue -> IR.VV4F g) -> IR.Blend (compBE a,compBE b) ((compBF c,compBF d),(compBF e,compBF f)) g
  x -> error $ "compBlending " ++ show x

compBF x = case x of
  A0 "Zero" -> IR.Zero
  A0 "One" -> IR.One
  A0 "SrcColor" -> IR.SrcColor
  A0 "OneMinusSrcColor" -> IR.OneMinusSrcColor
  A0 "DstColor" -> IR.DstColor
  A0 "OneMinusDstColor" -> IR.OneMinusDstColor
  A0 "SrcAlpha" -> IR.SrcAlpha
  A0 "OneMinusSrcAlpha" -> IR.OneMinusSrcAlpha
  A0 "DstAlpha" -> IR.DstAlpha
  A0 "OneMinusDstAlpha" -> IR.OneMinusDstAlpha
  A0 "ConstantColor" -> IR.ConstantColor
  A0 "OneMinusConstantColor" -> IR.OneMinusConstantColor
  A0 "ConstantAlpha" -> IR.ConstantAlpha
  A0 "OneMinusConstantAlpha" -> IR.OneMinusConstantAlpha
  A0 "SrcAlphaSaturate" -> IR.SrcAlphaSaturate
  x -> error $ "compBF " ++ show x

compBE x = case x of
  A0 "FuncAdd" -> IR.FuncAdd
  A0 "FuncSubtract" -> IR.FuncSubtract
  A0 "FuncReverseSubtract" -> IR.FuncReverseSubtract
  A0 "Min" -> IR.Min
  A0 "Max" -> IR.Max
  x -> error $ "compBE " ++ show x

compLO x = case x of
  A0 "Clear" -> IR.Clear
  A0 "And" -> IR.And
  A0 "AndReverse" -> IR.AndReverse
  A0 "Copy" -> IR.Copy
  A0 "AndInverted" -> IR.AndInverted
  A0 "Noop" -> IR.Noop
  A0 "Xor" -> IR.Xor
  A0 "Or" -> IR.Or
  A0 "Nor" -> IR.Nor
  A0 "Equiv" -> IR.Equiv
  A0 "Invert" -> IR.Invert
  A0 "OrReverse" -> IR.OrReverse
  A0 "CopyInverted" -> IR.CopyInverted
  A0 "OrInverted" -> IR.OrInverted
  A0 "Nand" -> IR.Nand
  A0 "Set" -> IR.Set
  x -> error $ "compLO " ++ show x

compComparisonFunction x = case x of
  A0 "Never" -> IR.Never
  A0 "Less" -> IR.Less
  A0 "Equal" -> IR.Equal
  A0 "Lequal" -> IR.Lequal
  A0 "Greater" -> IR.Greater
  A0 "Notequal" -> IR.Notequal
  A0 "Gequal" -> IR.Gequal
  A0 "Always" -> IR.Always
  x -> error $ "compComparisonFunction " ++ show x

compBool x = case x of
  A0 "True" -> True
  A0 "False" -> False
  x -> error $ "compBool " ++ show x

compFrag x = case x of
  A2 "DepthOp" (compComparisonFunction -> a) (compBool -> b) -> IR.DepthOp a b
  A2 "ColorOp" (compBlending -> b) (compValue -> v) -> IR.ColorOp b v
  x -> error $ "compFrag " ++ ppShow x

compInput x = case x of
  ETuple a -> concatMap compInput a
  A1 "IV4F" (ELit (LString s)) -> [(s, IR.V4F)]
  A1 "IM44F" (ELit (LString s)) -> [(s, IR.M44F)]
  x -> error $ "compInput " ++ show x

compFetchPrimitive x = case x of
  A0 "Points" -> IR.Points
  A0 "Lines" -> IR.Lines
  A0 "Triangles" -> IR.Triangles
  A0 "LinesAdjacency" -> IR.LinesAdjacency
  A0 "TrianglesAdjacency" -> IR.TrianglesAdjacency
  x -> error $ "compFetchPrimitive " ++ show x

compImg x = case x of
  ETuple a -> concatMap compImg a
  A2 "DepthImage" (ELit (LNat i)) (ELit (LFloat a)) -> [IR.DepthImage i (realToFrac a)]
  A2 "ColorImage" (ELit (LNat i)) a -> [IR.ColorImage i (compValue a)]
  x -> error $ "compImg " ++ ppShow x

compValue x = case x of
  ELit (LFloat a) -> IR.VFloat $ realToFrac a
  ELit (LInt a) -> IR.VInt $ fromIntegral a
  A4 "V4F" (ELit (LFloat a)) (ELit (LFloat b)) (ELit (LFloat c)) (ELit (LFloat d)) -> IR.VV4F $ IR.V4 (realToFrac a) (realToFrac b) (realToFrac c) (realToFrac d)
  A4 "V4B" (compBool -> a) (compBool -> b) (compBool -> c) (compBool -> d) -> IR.VV4B $ IR.V4 a b c d
  x -> error $ "compValue " ++ ppShow x

compRC x = case x of
  A3 "PointCtx" a (ELit (LFloat b)) c -> IR.PointCtx (compPS a) (realToFrac b) (compPSCO c)
  A2 "LineCtx" (ELit (LFloat a)) b -> IR.LineCtx (realToFrac a) (compPV b)
  A4 "TriangleCtx" a b c d -> IR.TriangleCtx (compCM a) (compPM b) (compPO c) (compPV d)
  x -> error $ "compRC " ++ show x

compPSCO x = case x of
  A0 "LowerLeft" -> IR.LowerLeft
  A0 "UpperLeft" -> IR.UpperLeft
  x -> error $ "compPSCO " ++ show x

compCM x = case x of
  A0 "CullNone" -> IR.CullNone
  A1 "CullFront" a -> IR.CullFront $ compFF a
  A1 "CullBack" a -> IR.CullBack $ compFF a
  x -> error $ "compCM " ++ show x

compFF x = case x of
  A0 "CW" -> IR.CW
  A0 "CCW" -> IR.CCW
  x -> error $ "compFF " ++ show x

compPM x = case x of
  A0 "PolygonFill" -> IR.PolygonFill
  A1 "PolygonLine" (ELit (LFloat a)) -> IR.PolygonLine $ realToFrac a
  A1 "PolygonPoint" a  -> IR.PolygonPoint $ compPS a
  x -> error $ "compPM " ++ show x

compPS x = case x of
  A1 "PointSize" (ELit (LFloat a)) -> IR.PointSize $ realToFrac a
  A0 "ProgramPointSize" -> IR.ProgramPointSize
  x -> error $ "compPS " ++ show x

compPO x = case x of
  A2 "Offset" (ELit (LFloat a)) (ELit (LFloat b)) -> IR.Offset (realToFrac a) (realToFrac b)
  A0 "NoOffset" -> IR.NoOffset
  x -> error $ "compPO " ++ show x

compPV x = case x of
    A0 "FirstVertex" -> IR.FirstVertex
    A0 "LastVertex" -> IR.LastVertex
    x -> error $ "compPV " ++ show x
