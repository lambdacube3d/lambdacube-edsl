{-# LANGUAGE TypeOperators, OverloadedStrings #-}
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

-- screen quad rendering
renderScreen :: (Exp F V2F -> FragmentOut (Color V4F :+: ZZ)) -> Exp Obj (Image N1 V4F)
renderScreen = PrjFrameBuffer "" tix0 . renderScreen'

renderScreen' :: (Exp F V2F -> FragmentOut (Color V4F :+: ZZ)) -> Exp Obj (FrameBuffer N1 V4F)
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
