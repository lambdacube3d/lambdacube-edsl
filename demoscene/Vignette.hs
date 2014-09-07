{-# LANGUAGE DataKinds #-}
module Vignette where

import LambdaCube.GL
import Utility
import BuiltinVec

data Vignette = Vignette
    { vignetteOuterRadius :: Exp F Float
    , vignetteInnerRadius :: Exp F Float
    }

vignette = Vignette
    { vignetteOuterRadius = floatF 1
    , vignetteInnerRadius = floatF 0.5
    }

fVignette :: Vignette -> Exp F V2F -> Exp F V4F -> Exp F V4F
fVignette v uv fromColor = fromColor @* smoothstep' o i d
  where
    o = vignetteOuterRadius v
    i = vignetteInnerRadius v
    d = distance' uv (vec2' (floatF 0.5) (floatF 0.5))

-- Vignette from a texture, useable as a render pass.
-- Use @fVignette@ to use it as part of a bigger fragment shader.
fxVignette :: Vignette -> Exp Obj (Image 1 V4F) -> Exp F V2F -> Exp F V4F
fxVignette v img uv = fVignette v uv c
  where
    c = texture' (Sampler LinearFilter ClampToEdge $ Texture (Texture2D (Float RGBA) n1) (V2 512 512) NoMip [img]) uv
