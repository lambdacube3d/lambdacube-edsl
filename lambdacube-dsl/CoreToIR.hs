{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module CoreToIR where

import Control.Applicative
import Control.Monad.State
import Data.Monoid
import Text.Show.Pretty
import qualified Data.Vector as V
import Data.Foldable (Foldable)
import qualified Data.Foldable as F

import qualified Type as AST
import Type hiding (ELet, EApp, ELam, EVar, ELit, ETuple, Exp, Exp_ (..), Pat, PVar, PLit, PTuple)
import Core
import IR

type CG = State IR.Pipeline


emptyPipeline = IR.Pipeline mempty mempty mempty mempty mempty mempty
testCompile = test'' (\a -> execState (compilePipeline . reduce mempty mempty $ a) emptyPipeline)
testCompile' = test_ $ return . (\a -> execState (compilePipeline . reduce mempty mempty $ a) emptyPipeline)


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

getSlot :: Exp -> CG IR.SlotName
getSlot (A3 "Fetch" (ELit (LString slot)) prim attrs) = return 0

getProgram :: IR.SlotName -> Exp -> Exp -> CG IR.ProgramName
getProgram slot vert frag = return 0

getCommands :: Exp -> CG [IR.Command]
getCommands e = case e of
  A5 "Accumulate" actx ffilter frag (A2 "Rasterize" rctx (A2 "Transform" vert input)) fbuf -> do
    slot <- getSlot input
    prog <- getProgram slot vert frag
    (<>) <$> getCommands fbuf <*> pure [IR.SetRasterContext (compRC' rctx), IR.SetProgram prog, IR.RenderSlot slot]-- TODO [IR.SetAccumulationContext (IR.AccumulationContext Nothing [])]
  A1 "FrameBuffer" a -> do
    let i = compImg' a
    rt <- newRenderTarget i
    pure [IR.SetRenderTarget rt, IR.ClearRenderTarget (map imageToSemantic i)]
  Exp e -> F.foldrM (\a b -> (<> b) <$> getCommands a) [] e

compFetchPrimitive' x = case x of
    A0 "Triangles" -> IR.Triangles
    x -> error $ "compFetchPrimitive' " ++ show x

compImg' x = case x of
  ETuple a -> concatMap compImg' a
  A2 "DepthImage" (ELit (LNat i)) (ELit (LFloat a)) -> [IR.DepthImage i (realToFrac a)]
  A2 "ColorImage" (ELit (LNat i)) a -> [IR.ColorImage i (compValue' a)]
  x -> error $ "compImg' " ++ ppShow x

compValue' x = case x of
  ELit (LFloat a) -> IR.VFloat $ realToFrac a
  ELit (LInt a) -> IR.VInt $ fromIntegral a
  A4 "V4F" (ELit (LFloat a)) (ELit (LFloat b)) (ELit (LFloat c)) (ELit (LFloat d)) -> IR.VV4F $ IR.V4 (realToFrac a) (realToFrac b) (realToFrac c) (realToFrac d)
  x -> error $ "compValue' " ++ ppShow x

compRC' x = case x of
    A4 "TriangleCtx" a b c d -> IR.TriangleCtx (compCM' a) (compPM' b) (compPO' c) (compPV' d)
    x -> error $ "compRC' " ++ show x

compCM' x = case x of
    A0 "CullNone" -> IR.CullNone
    x -> error $ "compCM' " ++ show x

compPM' x = case x of
    A0 "PolygonFill" -> IR.PolygonFill
    x -> error $ "compPM' " ++ show x

compPO' x = case x of
    A0 "NoOffset" -> IR.NoOffset
    x -> error $ "compPO' " ++ show x

compPV' x = case x of
    A0 "FirstVertex" -> IR.FirstVertex
    x -> error $ "compPV " ++ show x
