{-# LANGUAGE TypeOperators, OverloadedStrings, DataKinds #-}
module Utility where

import LC_API
import LC_Mesh
import Geometry

floatV :: Float -> Exp V Float
floatV = Const

floatF :: Float -> Exp F Float
floatF = Const

intF :: Int32 -> Exp F Int32
intF = Const

v2FF :: V2F -> Exp F V2F
v2FF = Const

v3FF :: V3F -> Exp F V3F
v3FF = Const

v4FF :: V4F -> Exp F V4F
v4FF = Const

iter :: GPU a => Exp F Int32 -> Exp F a -> (Exp F a -> Exp F a) -> Exp F a
iter n s0 fn = Loop st lc sr (tup2 (intF 0,s0))
  where
    st is = tup2 (i @+ intF 1, fn s)
      where
        (i,s) = untup2 is
    lc is = i @< n
      where
        (i,_) = untup2 is
    sr is = s
      where
        (_,s) = untup2 is

-- screen quad rendering
renderScreen :: (Exp F V2F -> FragmentOut (Color V4F :+: ZZ)) -> Exp Obj (Image 1 V4F)
renderScreen = PrjFrameBuffer "" tix0 . renderScreen'

renderScreen' :: (Exp F V2F -> FragmentOut (Color V4F :+: ZZ)) -> Exp Obj (FrameBuffer 1 V4F)
renderScreen' frag = Accumulate fragCtx PassAll frag rast clear
  where
    fragCtx = AccumulationContext Nothing $ ColorOp NoBlending (one' :: V4B):.ZT
    clear   = FrameBuffer (ColorImage n1 (V4 0 0 0 1):.ZT)
    rast    = Rasterize triangleCtx prims
    prims   = Transform vert input
    input   = Fetch "ScreenQuad" Triangle (IV2F "position")

    vert :: Exp V V2F -> VertexOut V2F
    vert uv = VertexOut v4 (Const 1) (NoPerspective uv':.ZT)
      where
        v4      = pack' $ V4 u v (floatV 1) (floatV 1)
        V2 u v  = unpack' uv
        uv'     = uv @* floatV 0.5 @+ floatV 0.5

initUtility :: Renderer -> IO ()
initUtility renderer = do
    let setMesh n m = compileMesh m >>= (\cm -> addMesh renderer n cm [])
    setMesh "ScreenQuad" quad
    return ()
