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

blur' :: (Exp F V2F -> FragmentOut (Depth Float :+: Color V2F :+: ZZ)) -> GP (FrameBuffer N1 (Float,V2F))
blur' frag = Accumulate fragCtx PassAll frag rast clear
  where
    fragCtx = AccumulationContext Nothing $ DepthOp Always False:.ColorOp NoBlending (one' :: V2B):.ZT
    clear   = FrameBuffer (DepthImage n1 1000:.ColorImage n1 (V2 0 0):.ZT)
    rast    = Rasterize triangleCtx prims
    prims   = Transform vert input
    input   = Fetch "postSlot" Triangle (IV2F "position")

    vert :: Exp V V2F -> VertexOut V2F
    vert uv = VertexOut v4 (Const 1) (NoPerspective uv:.ZT)
      where
        v4      = pack' $ V4 u v (floatV 1) (floatV 1)
        V2 u v  = unpack' uv

gaussFilter7 :: [(Float,Float)]
gaussFilter7 = 
    [ (-3.0,   0.015625)
    , (-2.0,   0.09375)
    , (-1.0,   0.234375)
    , (0.0,    0.3125)
    , (1.0,    0.234375)
    , (2.0,    0.09375)
    , (3.0,    0.015625)
    ]

gaussFilter9 :: [(Float,Float)]
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

blurVH :: GP (Image N1 V2F) -> GP (FrameBuffer N1 (Float,V2F))
blurVH img = blur' fragH
  where
    sizeT = 512
    sizeI = floor sizeT
    uvH v = Const (V2 (v/sizeT) 0) :: Exp F V2F
    uvV v = Const (V2 0 (v/sizeT)) :: Exp F V2F
    fragH :: Exp F V2F -> FragmentOut (Depth Float :+: Color V2F :+: ZZ)
    fragH uv' = FragmentOutRastDepth $ (sampleH gaussFilter9) :. ZT
      where
        sampleH ((o,c):[])  = texture' smp (uv @+ uvH o) @* floatF c
        sampleH ((o,c):xs)  = (texture' smp (uv @+ uvH o) @* floatF c) @+ sampleH xs
        V2 u v = unpack' uv
        uv = uv' @* floatF 0.5 @+ floatF 0.5
        smp = Sampler LinearFilter Clamp tex
        tex = Texture (Texture2D (Float RG) n1) (V2 sizeI sizeI) NoMip [PrjFrameBuffer "" tix0 (blur' fragV)]

    fragV :: Exp F V2F -> FragmentOut (Depth Float :+: Color V2F :+: ZZ)
    fragV uv' = FragmentOutRastDepth $ (sampleV gaussFilter9) :. ZT
      where
        sampleV ((o,c):[])  = texture' smp (uv @+ uvV o) @* floatF c
        sampleV ((o,c):xs)  = (texture' smp (uv @+ uvV o) @* floatF c) @+ sampleV xs
        V2 u v = unpack' uv
        uv = uv' @* floatF 0.5 @+ floatF 0.5
        smp = Sampler LinearFilter Clamp tex
        tex = Texture (Texture2D (Float RG) n1) (V2 sizeI sizeI) NoMip [img]

----------
-- VSM ---
----------
moments :: GP (FrameBuffer N1 (Float,V2F))
moments = Accumulate fragCtx PassAll storeDepth rast clear
  where
    fragCtx = AccumulationContext Nothing $ DepthOp Less True:.ColorOp NoBlending (one' :: V2B):.ZT
    clear   = FrameBuffer (DepthImage n1 1000:.ColorImage n1 (V2 0 0):.ZT)
    rast    = Rasterize triangleCtx prims
    prims   = Transform vert input
    input   = Fetch "streamSlot" Triangle (IV3F "position")
    lightViewProj = Uni (IM44F "lightViewProj")

    vert :: Exp V V3F -> VertexOut Float
    vert p = VertexOut v4 (floatV 1) (Smooth depth:.ZT)
      where
        v4    = lightViewProj @*. snoc p 1
        V4 _ _ depth _ = unpack' v4

    storeDepth :: Exp F Float -> FragmentOut (Depth Float :+: Color V2F :+: ZZ)
    --storeDepth depth' = FragmentOutRastDepth $ (Const $ V2 1 0.2) :. ZT
    storeDepth depth = FragmentOutRastDepth $ pack' (V2 moment1 moment2) :. ZT
      where
        dx = dFdx' depth
        dy = dFdy' depth
        moment1 = depth
        moment2 = depth @* depth @+ floatF 0.25 @* (dx @* dx @+ dy @* dy)

vsm :: GP (FrameBuffer N1 (Float,V4F))
vsm = Accumulate fragCtx PassAll calcLuminance rast clear
  where
    fragCtx = AccumulationContext Nothing $ DepthOp Less True:.ColorOp NoBlending (one' :: V4B):.ZT
    clear   = FrameBuffer (DepthImage n1 1000:.ColorImage n1 (V4 1 0 0 1):.ZT)
    rast    = Rasterize triangleCtx prims
    prims   = Transform vert input
    input   = Fetch "streamSlot" Triangle (IV3F "position", IV3F "normal")
    worldViewProj = Uni (IM44F "worldViewProj")
    lightViewProj = Uni (IM44F "lightViewProj")
    scaleU  = Uni (IFloat "scaleU")
    scaleV  = Uni (IFloat "scaleV")

    trimV4 :: Exp s V4F -> Exp s V3F
    trimV4 v = let V4 x y z _ = unpack' v in pack' $ V3 x y z

    trimM4 :: Exp s M44F -> Exp s M33F
    trimM4 v = let V4 i j k _ = unpack' v in pack' $ V3 (trimV4 i) (trimV4 j) (trimV4 k)
    
    vert :: Exp V (V3F, V3F) -> VertexOut (V4F, V3F)
    vert attr = VertexOut v4 (floatV 1) (Smooth v4l:.Smooth n:.ZT)
      where
        v4 = worldViewProj @*. snoc p 1
        v4l = lightViewProj @*. snoc p 1
        n3 = normalize' (trimM4 worldViewProj @*. n)
        (p,n) = untup2 attr

    calcLuminance :: Exp F (V4F, V3F) -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    calcLuminance attr = FragmentOutRastDepth $ ({- amb @+ -}p_max):. ZT
      where
        amb :: Exp F V4F
        amb = Const $ V4 0.1 0.1 0.3 1
        V4 tx ty tz tw = unpack' l
        clampUV x = clamp' x (floatF 0) (floatF 1)
        scale x = x @* floatF 0.5 @+ floatF 0.5
        u = clampUV (scale (tx @/ tw)) @* (scaleU :: Exp F Float)
        v = clampUV (scale (ty @/ tw)) @* (scaleV :: Exp F Float)
        V2 m1 m2 = unpack' $ texture' sampler (pack' $ V2 u v)
        variance = max' (floatF 0.002) (m2 @- m1 @* m1)
        d = max' (floatF 0) (tz @- m1)
        u' = u @- floatF 0.5
        v' = v @- floatF 0.5
        -- assuming light direction of (0 0 -1)
        V3 _ _ nz = unpack' n
        nz' = max' (floatF 0) nz
        intensity = max' (floatF 0) ((floatF 1 @- sqrt' (u' @* u' @+ v' @* v') @* floatF 4) @* nz')
        ltr = (round' (u' @* floatF 10) @* floatF 0.5 @+ floatF 0.5) @* intensity
        ltg = (round' (v' @* floatF 10) @* floatF 0.5 @+ floatF 0.5) @* intensity
        p_max = pack' (V4 ltr ltg intensity (floatF 1)) @* (variance @/ (variance @+ d @* d))
        (l,n) = untup2 attr

    sampler = Sampler LinearFilter Clamp shadowMapBlur
    --Texture gp dim arr t ar
    shadowMap :: Texture GP DIM2 SingleTex (Regular Float) RG
    shadowMap = Texture (Texture2D (Float RG) n1) (V2 512 512) NoMip [PrjFrameBuffer "shadowMap" tix0 moments]

    shadowMapBlur :: Texture GP DIM2 SingleTex (Regular Float) RG
    shadowMapBlur = Texture (Texture2D (Float RG) n1) (V2 512 512) NoMip [PrjFrameBuffer "shadowMap" tix0 $ blurVH $ PrjFrameBuffer "blur" tix0 moments]
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