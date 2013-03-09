module Scanlines where

import LC_API
import Utility

data Scanlines = Scanlines
    { scanlinesFrequency :: Exp F Float
    , scanlinesColor :: Exp F V4F
    }

scanlines = Scanlines
    { scanlinesFrequency = floatF 100
    , scanlinesColor = Const $ V4 1 1 1 1
    }

fScanlines :: Scanlines -> Exp F V4F -> Exp F V2F -> Exp F V4F
fScanlines sl fromColor uv = scanlinesColor sl @* fromColor @* r
  where
    r = sin' (v @* floatF (2*pi) @* scanlinesFrequency sl) @/ floatF 2 @+ floatF 0.5
    V2 _ v = unpack' uv

-- Scanlines as a full render pass.
-- Use @fScanlines@ to use it as part of a bigger fragment shader.
fxScanlines :: Scanlines -> Exp Obj (Image N1 V4F) -> Exp F V2F -> Exp F V4F
fxScanlines sl img uv = fScanlines sl c uv
  where
    c = texture' (Sampler LinearFilter Clamp $ Texture (Texture2D (Float RGBA) n1) (V2 512 512) NoMip [img]) uv
