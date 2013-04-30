{-# LANGUAGE DataKinds #-}
module Gamma where

import LC_API
import Utility

data Gamma
    = Gamma
    { gammaBrightness   :: Exp F Float -- 0 is the centre. < 0 = darken, > 1 = brighten
    , gammaContrast     :: Exp F Float -- 1 is the centre. < 1 = lower contrast, > 1 is raise contrast
    , gammaGammaCutoff  :: Exp F Float -- UV cutoff before rendering the image uncorrected
    , gammaInvGamma     :: Exp F Float -- Inverse gamma correction applied to the pixel
    }

gamma = Gamma
    { gammaBrightness   = floatF 0
    , gammaContrast     = floatF 1
    , gammaGammaCutoff  = floatF 1
    , gammaInvGamma     = floatF (1 / 2.2)
    }

fxGamma :: Gamma -> Exp Obj (Image 1 V4F) -> Exp Obj (Image 1 V4F)
fxGamma cfg img = renderScreen frag
  where
    sizeI   = 512 -- FIXME
    smp uv  = texture' (Sampler LinearFilter ClampToEdge $ Texture (Texture2D (Float RGBA) n1) (V2 sizeI sizeI) NoMip [img]) uv
    frag uv = FragmentOut $ finalColor :. ZT
      where
        V2 u _          = unpack' uv
        V4 r g b a      = unpack' $ smp uv
        rgb             = pack' $ V3 r g b
        rgbBrightness   = rgb @+ gammaBrightness cfg
        rgbContrast     = (rgbBrightness @- floatF 0.5) @* gammaContrast cfg @+ floatF 0.5
        rgbClamp        = clamp' rgbContrast (floatF 0) (floatF 1)
        invGamma        = gammaInvGamma cfg
        V3 r' g' b'     = unpack' $ Cond (u @< gammaGammaCutoff cfg)
                            (pow' rgbClamp (pack' $ V3 invGamma invGamma invGamma))
                            rgbClamp
        finalColor      = pack' $ V4 r' g' b' a
