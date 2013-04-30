{-# LANGUAGE DataKinds #-}
module Blur where

import LC_API
import Utility

data Blur
    = Blur
    { blurTextureSize   :: Int
    , blurAmount        :: Int
    , blurScale         :: Exp F Float
    , blurStrength      :: Float
    }

blur = Blur
    { blurTextureSize   = 128
    , blurAmount        = 10
    , blurScale         = floatF 1
    , blurStrength      = 0.2
    }

fxBlur :: Blur -> Exp Obj (Image 1 V4F) -> Exp Obj (Image 1 V4F)
fxBlur cfg img = renderScreen $ frag imgH uvV
  where
    imgH    = renderScreen $ frag img uvH
    sizeT   = fromIntegral sizeI
    sizeI   = fromIntegral $ blurTextureSize cfg
    uvH v   = Const (V2 (v/sizeT) 0) :: Exp F V2F
    uvV v   = Const (V2 0 (v/sizeT)) :: Exp F V2F

    frag i uvFun uv = FragmentOut $ (sample gaussFilter) :. ZT
      where
        smp coord           = texture' (Sampler LinearFilter ClampToEdge $ Texture (Texture2D (Float RGBA) n1) (V2 sizeI sizeI) NoMip [i]) coord
        sample ((o,c):[])   = smp (uv @+ uvFun o) @* floatF c @* blurScale cfg
        sample ((o,c):xs)   = (smp (uv @+ uvFun o) @* floatF c @* blurScale cfg) @+ sample xs

    gauss :: Float -> Float -> Float
    gauss x deviation = (1.0 / sqrt(2.0 * pi * deviation)) * exp(-((x * x) / (2.0 * deviation)))
    gaussFilter :: [(Float,Float)]
    gaussFilter = [(offset, gauss (offset * strength) deviation) | i <- [0..blurAmount cfg], let offset = fromIntegral i - halfBlur]
      where
        halfBlur    = 0.5 * (fromIntegral $ blurAmount cfg)
        deviation   = (halfBlur * 0.35)^2
        strength    = 1 - blurStrength cfg
