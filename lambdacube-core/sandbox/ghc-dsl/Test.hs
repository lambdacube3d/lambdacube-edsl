import LCDSL
import LCDSLContext
import LCDSLLinAlg
import LCDSLTexture

import TypeLevel.Number.Nat
import TypeLevel.Number.Nat.Num

--- test
{-
floatV :: Float -> Float :@ F
floatV = undefined

floatF :: Float -> Float :@ F
floatF = undefined
-}
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
-}

vert :: V2F -> VertexOut V2F
vert uv = vertexOut v4 1 (linear v2)
  where
    v2      :: V2F
    v2      = undefined
    v4      :: V4F
    v4      = undefined

vert' :: () -> VertexOut ()
vert' _ = vertexOut v4 1 ()
  where
    v4      :: V4F
    v4      = undefined


frag :: () -> FragmentOut (NoDepth, V4F :. Float)
frag uv' = fragmentOut $ color :. f
color = V4 0 0 0 0
f = 0


-- test
aCtx = AccumulationContext Nothing (DepthOp Never True :. (undefined :: FragmentOperation (Color Float)))

rCtx = defaultTriangleCtx

fFilter :: FragmentFilter ()
fFilter = undefined

fs :: () -> FragmentOut (Depth Float,Float)
fs = undefined

inputS :: VertexStream Triangle ()
inputS = undefined

primS = transform vert' inputS

frs :: FragmentStream N1 ()
frs = rasterize rCtx primS

dImg :: Image N1 Float :. Image N1 Float
dImg = undefined

acc = accumulate aCtx fFilter fs frs dImg
