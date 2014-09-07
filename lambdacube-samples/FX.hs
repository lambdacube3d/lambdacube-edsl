{-# LANGUAGE TypeOperators, OverloadedStrings, DataKinds #-}
module FX where

import qualified Data.Trie as T
import qualified Data.Vector.Storable as SV

import        LambdaCube.GL
import        LambdaCube.GL.Mesh

-- Geometry for effects
quad :: Mesh
quad = Mesh
    { mAttributes   = T.singleton "position" $ A_V2F $ SV.fromList [V2 a b, V2 a a, V2 b a, V2 b a, V2 b b, V2 a b]
    , mPrimitive    = P_Triangles
    , mGPUData      = Nothing
    }
  where
    a = -1
    b = 1

initUtility :: Renderer -> IO ()
initUtility renderer = do
    let setMesh n m = compileMesh m >>= (\cm -> addMesh renderer n cm [])
    setMesh "ScreenQuad" quad
    return ()

floatV :: Float -> Exp V Float
floatV = Const

floatF :: Float -> Exp F Float
floatF = Const

v2FF :: V2F -> Exp F V2F
v2FF = Const

-- screen quad rendering
renderScreen :: (Exp F V2F -> FragmentOut (Color V4F :+: ZZ)) -> Exp Obj (Image 1 V4F)
renderScreen = PrjFrameBuffer "" tix0 . renderScreen'

renderScreen' :: (Exp F V2F -> FragmentOut (Color V4F :+: ZZ)) -> Exp Obj (FrameBuffer 1 V4F)
renderScreen' frag = Accumulate fragCtx PassAll frag rast clear
  where
    fragCtx = AccumulationContext Nothing $ ColorOp NoBlending (one' :: V4B):.ZT
    clear   = FrameBuffer (ColorImage n1 (V4 0 0 0 1):.ZT)
    rast    = Rasterize triangleCtx prims
    prims   = Transform vert input
    input   = Fetch "ScreenQuad" Triangles (IV2F "position")

    vert :: Exp V V2F -> VertexOut () V2F
    vert uv = VertexOut v4 (Const 1) ZT (NoPerspective uv':.ZT)
      where
        v4      = pack' $ V4 u v (floatV 1) (floatV 1)
        V2 u v  = unpack' uv
        uv'     = uv @* floatV 0.5 @+ floatV 0.5

data Scanlines = Scanlines
    { scanlinesFrequency :: Exp F Float
    , scanlinesHigh :: Exp F V4F
    , scanlinesLow :: Exp F V4F
    }

scanlines = Scanlines
    { scanlinesFrequency = floatF 100
    , scanlinesHigh = Const $ V4 1 1 1 1
    , scanlinesLow = Const $ V4 0 0 0 0
    }

fScanlines :: Scanlines -> Exp F V2F -> Exp F V4F -> Exp F V4F
fScanlines sl uv fromColor = fromColor @* mix' sl1 sl2 r
  where
    r = sin' (v @* floatF (2*pi) @* scanlinesFrequency sl) @/ floatF 2 @+ floatF 0.5
    sl1 = scanlinesLow sl
    sl2 = scanlinesHigh sl
    V2 _ v = unpack' uv

-- Scanlines from a texture, useable as a render pass.
-- Use @fScanlines@ to use it as part of a bigger fragment shader.
fxScanlines :: Scanlines -> Exp Obj (Image 1 V4F) -> Exp F V2F -> Exp F V4F
fxScanlines sl img uv = fScanlines sl uv c
  where
    c = texture' (Sampler LinearFilter ClampToEdge $ Texture (Texture2D (Float RGBA) n1) (V2 512 512) NoMip [img]) uv

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
    d = distance' uv (v2FF $ V2 0.5 0.5)

-- Vignette from a texture, useable as a render pass.
-- Use @fVignette@ to use it as part of a bigger fragment shader.
fxVignette :: Vignette -> Exp Obj (Image 1 V4F) -> Exp F V2F -> Exp F V4F
fxVignette v img uv = fVignette v uv c
  where
    c = texture' (Sampler LinearFilter ClampToEdge $ Texture (Texture2D (Float RGBA) n1) (V2 512 512) NoMip [img]) uv

postProcess :: Exp Obj (Image 1 (V4 Float)) -> Exp Obj (Image 1 V4F)
postProcess base = renderScreen $ FragmentOut . (:.ZT) . f
  where
    f uv = fVignette vign uv $
           --fScanlines sl uv $
           smp' base uv
    sl = scanlines { scanlinesFrequency = floatF 100
                   , scanlinesHigh = Const (V4 0.5 1 1 1)
                   , scanlinesLow = Const (V4 0.2 0.5 0.5 1)
                   }
    vign = vignette { vignetteOuterRadius = floatF 0.9
                    , vignetteInnerRadius = floatF 0.4
                    }
    smp' img uv = texture' (Sampler LinearFilter ClampToEdge $ Texture (Texture2D (Float RGBA) n1) (V2 512 512) NoMip [img]) uv

blurCoefficients :: [(Float, Float)]
blurCoefficients = gaussFilter9

gaussFilter7 :: [(Float, Float)]
gaussFilter7 = 
    [ (-3.0,   0.015625)
    , (-2.0,   0.09375)
    , (-1.0,   0.234375)
    , (0.0,    0.3125)
    , (1.0,    0.234375)
    , (2.0,    0.09375)
    , (3.0,    0.015625)
    ]

gaussFilter9 :: [(Float, Float)]
gaussFilter9 = 
    [ (-4.0,   0.05)
    , (-3.0,   0.09)
    , (-2.0,   0.12)
    , (-1.0,   0.15)
    , (0.0,    0.16)
    , (1.0,    0.15)
    , (2.0,    0.12)
    , (3.0,    0.09)
    , (4.0,    0.05)
    ]

blur :: [(Float, Float)] -> Exp Obj (Image 1 V4F) -> Exp Obj (Image 1 V4F)
blur coefficients img = PrjFrameBuffer "" tix0 $ blur' coefficients img

blur' :: [(Float, Float)] -> Exp Obj (Image 1 V4F) -> Exp Obj (FrameBuffer 1 V4F)
blur' coefficients img = filter1D dirH (PrjFrameBuffer "" tix0 (filter1D dirV img))
  where
    textureSize :: Num a => a
    textureSize = 512

    dirH v = Const (V2 (v / textureSize) 0) :: Exp F V2F
    dirV v = Const (V2 0 (v / textureSize)) :: Exp F V2F
    
    filter1D :: (Float -> Exp F V2F) -> Exp Obj (Image 1 V4F) -> Exp Obj (FrameBuffer 1 V4F)
    filter1D dir img = Accumulate accCtx PassAll frag
                                 (Rasterize triangleCtx prims) clearBuf
      where
        accCtx = AccumulationContext Nothing
                                    (ColorOp NoBlending (one' :: V4B) :. ZT)
        clearBuf = FrameBuffer (ColorImage n1 (zero' :: V4F) :. ZT)
        prims = Transform vert (Fetch "ScreenQuad" Triangles (IV2F "position"))

        vert :: Exp V V2F -> VertexOut () V2F
        vert uv = VertexOut pos (Const 1) ZT (NoPerspective uv' :. ZT)
          where
            uv'    = uv @* floatV 0.5 @+ floatV 0.5
            pos    = pack' (V4 u v (floatV 1) (floatV 1))
            V2 u v = unpack' uv

        frag :: Exp F V2F -> FragmentOut (Color V4F :+: ZZ)
        frag uv = FragmentOut (sample :. ZT)
          where
            sample = foldr1 (@+) [ texture' smp (uv @+ dir ofs) @* floatF coeff
                                 | (ofs, coeff) <- coefficients]
            smp = Sampler LinearFilter ClampToEdge tex
            tex = Texture (Texture2D (Float RGBA) n1)
                          (V2 textureSize textureSize) NoMip [img]
