import LCDSL

import TypeLevel.Number.Nat
import TypeLevel.Number.Nat.Num

--- test
floatV :: Float -> Exp V Float
floatV = undefined

floatF :: Float -> Exp F Float
floatF = undefined

screenQuad :: GP (FrameBuffer N1 (V4F :+: ZZ))
screenQuad = Accumulate fragCtx PassAll frag rast clear

fragCtx = AccumulationContext Nothing $ ColorOp NoBlending (V4 True True True True):.ZT
clear   :: GP (FrameBuffer N1 (V4F:+:ZZ))
clear   = FrameBuffer (ColorImage n1 (V4 1 0 0 1):.ZT)
rast    = Rasterize triangleCtx prims
prims   = Transform vert input
input   :: GP (VertexStream Triangle (V2F:+:ZZ))
input   = undefined

vert :: Exp V (V2F:+:ZZ) -> VertexOut (V2F:+:ZZ)
vert uv = VertexOut v4 (Const 1) (NoPerspective v2:.ZT)
  where
    v2      :: Exp V V2F
    v2      = undefined
    v4      :: Exp V V4F
    v4      = undefined

frag :: Exp F (V2F:+:ZZ) -> FragmentOut (Color V4F :+: ZZ)
frag uv' = FragmentOut $ color :. ZT
  where
    color :: Exp F V4F
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
