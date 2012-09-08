import LCDSL

import TypeLevel.Number.Nat
import TypeLevel.Number.Nat.Num

--- test
floatV :: Float -> Float :@ F
floatV = undefined

floatF :: Float -> Float :@ F
floatF = undefined
{-
screenQuad :: FrameBuffer N1 (V4F :+: ZZ)
screenQuad = accumulate fragCtx PassAll frag rast clear

fragCtx = AccumulationContext Nothing $ ColorOp NoBlending (V4 True True True True):.ZT
clear   :: FrameBuffer N1 (V4F:+:ZZ)
clear   = undefined --ColorImage n1 (V4 1 0 0 1):.ZT
rast    = rasterize triangleCtx prims
prims   = transform vert inputS
inputS  :: VertexStream Triangle (V2F:+:ZZ)
inputS  = undefined

vert :: (V2F:+:ZZ) :@ V -> (V2F:+:ZZ) :@ VertexOut
vert uv = vertexOut v4 (floatV 1) (NoPerspective v2:.ZT)
  where
    v2      :: V2F :@ V
    v2      = undefined
    v4      :: V4F :@ V
    v4      = undefined
-}
frag :: (V2F:+:ZZ) :@ F -> (Color V4F :+: ZZ) :@ FragmentOut
frag uv' = fragmentOut $ color :. ZT
  where
    color :: V4F :@ F
    color = undefined

instance GPU V4F
instance IsScalar Float
instance IsScalar V4F
instance IsScalar V4B
instance GPU V4B
instance GPU Float
instance GPU V2F
instance GPU ZZ
instance (GPU a, GPU b) => GPU (a :+: b)
