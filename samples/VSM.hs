{-# LANGUAGE OverloadedStrings, PackageImports, TypeOperators #-}
module VSM where

import Data.ByteString.Char8 (ByteString)

import LC_API
--import LCLanguage

-- specialized snoc
v3v4 :: Exp s V3F -> Exp s V4F
v3v4 v = let V3 x y z = unpack' v in pack' $ V4 x y z (Const 1)

v4v3 :: Exp s V4F -> Exp s V3F
v4v3 v = let V4 x y z _ = unpack' v in pack' $ V3 x y z

-- specialized snoc
snoc :: Exp s V3F -> Float -> Exp s V4F
snoc v s = let V3 x y z = unpack' v in pack' $ V4 x y z (Const s)

drop4 :: Exp s V4F -> Exp s V3F
drop4 v = let V4 x y z _ = unpack' v in pack' $ V3 x y z

drop3 :: Exp s V3F -> Exp s V2F
drop3 v = let V3 x y _ = unpack' v in pack' $ V2 x y

floatV :: Float -> Exp V Float
floatV = Const

floatF :: Float -> Exp F Float
floatF = Const

intF :: Int32 -> Exp F Int32
intF = Const

-- blur

blur' :: (Exp F V2F -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)) -> GP (FrameBuffer N1 (Float,V4F))
blur' frag = Accumulate fragCtx PassAll frag rast clear
  where
    fragCtx = DepthOp Always True:.ColorOp NoBlending (one' :: V4B):.ZT
    clear   = FrameBuffer (DepthImage n1 1000:.ColorImage n1 (V4 1 0 0 1):.ZT)
    rast    = Rasterize triangleCtx NoGeometryShader prims
    prims   = Transform vert input
    input   = Fetch "postSlot" Triangle (IV2F "position")

    vert :: Exp V V2F -> VertexOut V2F
    vert uv = VertexOut v4 (Const 1) (NoPerspective uv:.ZT)
      where
        v4      = pack' $ V4 u v (floatV 1) (floatV 1)
        V2 u v  = unpack' uv

blurVH :: GP (Image N1 V4F) -> GP (FrameBuffer N1 (Float,V4F))
blurVH img = blur' fragH
  where
    a = 1/512
    mV = Const (V2 0 (-a)) :: Exp F V2F
    pV = Const (V2 0 (a)) :: Exp F V2F
    mH = Const (V2 (-a) 0) :: Exp F V2F
    pH = Const (V2 (a) 0) :: Exp F V2F
    fragH :: Exp F V2F -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    fragH uv' = FragmentOutRastDepth $ (s @+ sm @+ sp) :. ZT
      where
        s :: Exp F V4F
        s = texture' smp uv (Const 0)
        sm = texture' smp (uv @+ mH) (Const 0)
        sp = texture' smp (uv @+ pH) (Const 0)
        V2 u v = unpack' uv
        uv = uv' @* floatF 0.5 @+ floatF 0.5
        smp = Sampler LinearFilter Clamp tex
        tex = Texture (Texture2D (Float RGBA) n1) (V2 512 512) NoMip [PrjFrameBuffer "" tix0 (blur' fragV)]

    fragV :: Exp F V2F -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    fragV uv' = FragmentOutRastDepth $ ((s @+ sm @+ sp) @/ floatF 3) :. ZT
      where
        s :: Exp F V4F
        s = texture' smp uv (Const 0)
        sm = texture' smp (uv @+ mV) (Const 0)  :: Exp F V4F
        sp = texture' smp (uv @+ pV) (Const 0)  :: Exp F V4F
        V2 u v = unpack' uv
        uv = uv' @* floatF 0.5 @+ floatF 0.5
        smp = Sampler LinearFilter Clamp tex
        tex = Texture (Texture2D (Float RGBA) n1) (V2 512 512) NoMip [img]

----------
-- VSM ---
----------
moments :: GP (FrameBuffer N1 (Float,V4F))
moments = Accumulate fragCtx PassAll storeDepth rast clear
  where
    fragCtx = DepthOp Less True:.ColorOp NoBlending (one' :: V4B):.ZT
    clear   = FrameBuffer (DepthImage n1 1000:.ColorImage n1 (V4 0 0 1 1):.ZT)
    rast    = Rasterize triangleCtx NoGeometryShader prims
    prims   = Transform vert input
    input   = Fetch "streamSlot" Triangle (IV3F "position")
    lightViewProj = Uni (IM44F "lightViewProj")

    vert :: Exp V V3F -> VertexOut Float
    vert p = VertexOut v4 (floatV 1) (Smooth depth:.ZT)
      where
        v4    = lightViewProj @*. snoc p 1
        V4 _ _ depth _ = unpack' v4

    storeDepth :: Exp F Float -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    --storeDepth depth' = FragmentOutRastDepth $ (Const $ V2 1 0.2) :. ZT
    storeDepth depth = FragmentOutRastDepth $ pack' (V4 moment1 moment2 (floatF 1) (floatF 1)) :. ZT
      where
        dx = dFdx' depth
        dy = dFdy' depth
        moment1 = depth
        moment2 = depth @* depth @+ floatF 0.25 @* (dx @* dx @+ dy @* dy)

vsm :: GP (FrameBuffer N1 (Float,V4F))
vsm = Accumulate fragCtx PassAll calcLuminance rast clear
  where
    fragCtx = DepthOp Less True:.ColorOp NoBlending (one' :: V4B):.ZT
    clear   = FrameBuffer (DepthImage n1 1000:.ColorImage n1 (V4 1 0 0 1):.ZT)
    rast    = Rasterize triangleCtx NoGeometryShader prims
    prims   = Transform vert input
    input   = Fetch "streamSlot" Triangle (IV3F "position")
    worldViewProj = Uni (IM44F "worldViewProj")
    lightViewProj = Uni (IM44F "lightViewProj")

    vert :: Exp V V3F -> VertexOut(V4F,V4F)
    vert p = VertexOut v4 (floatV 1) (Smooth v4l:.Smooth v4:.ZT)
      where
        v4 = worldViewProj @*. snoc p 1
        v4l = lightViewProj @*. snoc p 1

    calcLuminance :: Exp F (V4F,V4F) -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    calcLuminance lp = FragmentOutRastDepth $ (amb @+ p_max):. ZT
      where
        amb :: Exp F V4F
        amb = Const $ V4 0.1 0.1 0.3 1
        (l,_) = untup2 lp
        V4 tx ty tz tw = unpack' l
        u = tx @/ tw @* floatF 0.5 @+ floatF 0.5
        v = ty @/ tw @* floatF 0.5 @+ floatF 0.5
        V4 m1 m2 _ _ = unpack' $ texture' sampler (pack' $ V2 u v) (floatF 0)
        variance = max' (floatF 0.002) (m2 @- m1 @* m1)
        d = tz @- m1
        p_max = variance @/ (variance @+ d @* d)

    sampler = Sampler LinearFilter Clamp shadowMap
    --Texture gp dim arr t ar
    shadowMap :: Texture GP DIM2 SingleTex (Regular Float) RGBA
    shadowMap = Texture (Texture2D (Float RGBA) n1) (V2 512 512) NoMip [PrjFrameBuffer "shadowMap" tix0 moments]

    shadowMapBlur :: Texture GP DIM2 SingleTex (Regular Float) RGBA
    shadowMapBlur = Texture (Texture2D (Float RGBA) n1) (V2 512 512) NoMip [PrjFrameBuffer "shadowMap" tix0 $ blurVH $ PrjFrameBuffer "blur" tix0 moments]
{-
tx :: GP (Image N1 (V2 Float))
tx = PrjFrameBuffer "" tix0 moments

tex :: GP (Image N1 Float)
tex = undefined

sm :: Texture GP DIM2 SingleTex (Regular Float) RG
sm = Texture (Texture2D (Float RG) n1) AutoMip [tx]

smp :: Exp stage (Sampler DIM2 SingleTex (Regular Float) RG)
smp = Sampler LinearFilter Clamp sm
-}