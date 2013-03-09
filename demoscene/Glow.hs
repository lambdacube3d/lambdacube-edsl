module Glow where

import LC_API
import Utility

import Blur

data BlendType
    = Additive
    | Screen

data Glow
    = Glow
    { glowBlur  :: Blur
    , glowBlend :: BlendType
    }

glow = Glow
    { glowBlur  = blur
    , glowBlend = Screen
    }

fxGlow :: Glow -> Exp Obj (Image N1 V4F) -> Exp Obj (Image N1 V4F) -> Exp Obj (Image N1 V4F)
fxGlow cfg sceneImg glowImg = renderScreen frag
  where
    sizeI   = 512 :: Word32 -- FIXME: we should keep the original image size
    smp i c = texture' (Sampler LinearFilter Clamp $ Texture (Texture2D (Float RGBA) n1) (V2 sizeI sizeI) NoMip [i]) c
    setA c  = pack' $ V4 r g b (floatF 1)
      where
        V4 r g b _ = unpack' c
    frag uv = FragmentOut $ color :.ZT
      where
        src     = smp (fxBlur blur glowImg) uv
        dst     = smp sceneImg uv
        color   = case glowBlend cfg of
            Additive    -> min' (src @+ dst) (floatF 1)
            Screen      -> setA $ clamp' ((src @+ dst) @- (src @* dst)) (floatF 0) (floatF 1)
