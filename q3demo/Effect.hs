{-# LANGUAGE OverloadedStrings, PackageImports, TypeOperators #-}
module Effect where

import Data.ByteString.Char8 (ByteString)
import qualified Data.Trie as T
import qualified Data.Vector.Storable as SV

import LC_API
import LC_Mesh

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

floatV :: Float -> Exp V Float
floatV = Const

floatF :: Float -> Exp F Float
floatF = Const

intF :: Int32 -> Exp F Int32
intF = Const

-- screen quad rendering
renderScreen :: (Exp F V2F -> FragmentOut (Color V4F :+: ZZ)) -> Exp Obj (Image N1 V4F)
renderScreen = PrjFrameBuffer "" tix0 . renderScreen'

renderScreen' :: (Exp F V2F -> FragmentOut (Color V4F :+: ZZ)) -> Exp Obj (FrameBuffer N1 V4F)
renderScreen' frag = Accumulate fragCtx PassAll frag rast clear
  where
    fragCtx = AccumulationContext Nothing $ ColorOp NoBlending (one' :: V4B):.ZT
    clear   = FrameBuffer (ColorImage n1 (V4 0 0 0 1):.ZT)
    rast    = Rasterize triangleCtx prims
    prims   = Transform vert input
    input   = Fetch "ScreenQuad" Triangle (IV2F "position")

    vert :: Exp V V2F -> VertexOut V2F
    vert uv = VertexOut v4 (Const 1) (NoPerspective uv':.ZT)
      where
        v4      = pack' $ V4 u v (floatV 1) (floatV 1)
        V2 u v  = unpack' uv
        uv'     = uv @* floatV 0.5 @+ floatV 0.5

initUtility :: Renderer -> IO ()
initUtility renderer = do
    let setMesh n m = compileMesh m >>= (\cm -> addMesh renderer n cm [])
    setMesh "ScreenQuad" quad
    return ()

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

fxGamma :: Gamma -> Exp Obj (Image N1 V4F) -> Exp Obj (Image N1 V4F)
fxGamma cfg img = renderScreen frag
  where
    sizeI   = 1024 -- FIXME
    smp uv  = texture' (Sampler LinearFilter Clamp $ Texture (Texture2D (Float RGBA) n1) (V2 sizeI sizeI) NoMip [img]) uv
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

