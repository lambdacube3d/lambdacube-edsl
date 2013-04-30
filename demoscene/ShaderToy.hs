{-# LANGUAGE DataKinds #-}
module ShaderToy (fxFakeRipple, fxWarping, fxMotionBlur) where

import LC_API
import Utility
import Swizzling
import BuiltinVec

smp i c = texture' (Sampler LinearFilter ClampToEdge $ Texture (Texture2D (Float RGBA) n1) (V2 sizeI sizeI) NoMip [i]) c
  where
    sizeI   = 512 :: Word32 -- FIXME: we should keep the original image size

-- port of: https://www.shadertoy.com/view/lds3RH
fxFakeRipple :: Exp F Float -> Exp F V2F -> Exp Obj (Image 1 V4F) -> Exp Obj (Image 1 V4F)
fxFakeRipple iGlobalTime iResolution img = renderScreen $ \_ -> FragmentOut $ color :. ZT
  where
    uv = (xy_ fragCoord') @/ iResolution
    V2 u v = unpack' uv
    w = (floatF 0.5 @- u) @* (x_ iResolution @/ y_ iResolution)
    h = floatF 0.5 @- v
    distanceFromCenter = sqrt' (w @* w @+ h @* h)
    sinArg = distanceFromCenter @* floatF 10 @- iGlobalTime @* floatF 10
    slope = cos' sinArg
    color = smp img $ uv @+ normalize' (pack' $ V2 w h) @* slope @* floatF 0.05


xyy_ v = pack' $ V3 x y y
  where
    V4 x y _ _ = unpack' v

-- port of: https://www.shadertoy.com/view/Xsl3zn
-- License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.
fxWarping :: Exp F Float -> Exp F V2F -> Exp Obj (Image 1 V4F) -> Exp Obj (Image 1 V4F)
fxWarping iGlobalTime iResolution img = renderScreen $ \_ -> FragmentOut $ vec4' col4 (floatF 1) :. ZT
  where
    uv = xy_ fragCoord' @/ iResolution @* floatF 0.5
    d = length' uv
    st = uv @* floatF 0.1 @+ (pack' $ V2 (cos' $ floatF 0.071 @* iGlobalTime @+ d) (sin' $ floatF 0.073 @* iGlobalTime @- d)) @* floatF 0.2
    col1 = xyz_ $ smp img st
    w = x_ col1
    col2 = col1 @* (v3FF one' @- xyy_ (smp img $ uv @* floatF 0.4 @+ xy_ col1 @* floatF 0.1))
    col3 = col2 @* w @* floatF 2
    col4 = col3 @* (floatF 1 @+ floatF 2 @* d)

-- port of: https://www.shadertoy.com/view/Xsf3Rn
-- License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.
fxMotionBlur :: Exp F Float -> Exp F V2F -> Exp Obj (Image 1 V4F) -> Exp Obj (Image 1 V4F)
fxMotionBlur iGlobalTime iResolution img = renderScreen $ \_ -> FragmentOut $ vec4' (total @* w) (floatF 1) :. ZT
  where
    deform :: Exp F V2F -> Exp F Float -> Exp F V3F
    deform p' scale = res
      where
        mtime = scale @+ iGlobalTime
        p = p' @+ pack' (V2 (floatF 0.5 @* sin' (floatF 1.1 @* mtime)) (floatF 0.5 @* sin' (floatF 1.3 @* mtime)))
        a = atan2' (y_ p) (x_ p)
        r = sqrt' $ dot' p p
        s = r @* (floatF 1 @+ floatF 0.5 @* cos' (mtime @* floatF 1.7))
        w = floatF 0.8 @- floatF 0.2 @* cos' (mtime @+ floatF 3 @* a)
        u = floatF 0.1 @* mtime @+ floatF 0.05 @* y_ p @+ floatF 0.05 @* cos' (mtime @+ a @* floatF 2) @/ s
        v = floatF 0.1 @* mtime @+ floatF 0.05 @* x_ p @+ floatF 0.05 @* sin' (mtime @+ a @* floatF 2) @/ s
        res = (xyz_ $ smp img $ pack' (V2 u v) @* floatF 0.5) @* w

    q = xy_ fragCoord' @/ iResolution
    p = q @* floatF 2 @- floatF 1
    total' = snd $ untup2 $ iter (intF 20) (tup2 (floatF 0,v3FF zero')) $ \wt -> let (w1,t1) = untup2 wt in tup2 (w1 @+ floatF 0.01,t1 @+ deform p w1)
    total = total' @/ floatF 20
    V2 qx qy = unpack' q
    w = floatF 2 @* (floatF 0.5 @+ floatF 0.5 @* pow' (floatF 16 @* qx @* qy @* (floatF 1 @- qx) @* (floatF 1 @- qy)) (floatF 0.25))
