{-# LANGUAGE OverloadedStrings, PackageImports, TypeOperators #-}
module VSM where

import Data.ByteString.Char8 (ByteString)

import LC_API

import GraphicsUtils

shadowMapSize :: Num a => a
shadowMapSize = 512

blur' :: (Exp F V2F -> FragmentOut (Depth Float :+: Color V2F :+: ZZ)) -> Exp Obj (FrameBuffer N1 (Float,V2F))
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

blurVH :: Exp Obj (Image N1 V2F) -> Exp Obj (FrameBuffer N1 (Float,V2F))
blurVH img = blur' fragH
  where
    uvH v = Const (V2 (v/shadowMapSize) 0) :: Exp F V2F
    uvV v = Const (V2 0 (v/shadowMapSize)) :: Exp F V2F
    fragH :: Exp F V2F -> FragmentOut (Depth Float :+: Color V2F :+: ZZ)
    fragH uv' = FragmentOutRastDepth $ (sampleH gaussFilter9) :. ZT
      where
        sampleH ((o,c):[])  = texture' smp (uv @+ uvH o) @* floatF c
        sampleH ((o,c):xs)  = (texture' smp (uv @+ uvH o) @* floatF c) @+ sampleH xs
        V2 u v = unpack' uv
        uv = uv' @* floatF 0.5 @+ floatF 0.5
        smp = Sampler LinearFilter Clamp tex
        tex = Texture (Texture2D (Float RG) n1) (V2 shadowMapSize shadowMapSize) NoMip [PrjFrameBuffer "" tix0 (blur' fragV)]

    fragV :: Exp F V2F -> FragmentOut (Depth Float :+: Color V2F :+: ZZ)
    fragV uv' = FragmentOutRastDepth $ (sampleV gaussFilter9) :. ZT
      where
        sampleV ((o,c):[])  = texture' smp (uv @+ uvV o) @* floatF c
        sampleV ((o,c):xs)  = (texture' smp (uv @+ uvV o) @* floatF c) @+ sampleV xs
        V2 u v = unpack' uv
        uv = uv' @* floatF 0.5 @+ floatF 0.5
        smp = Sampler LinearFilter Clamp tex
        tex = Texture (Texture2D (Float RG) n1) (V2 shadowMapSize shadowMapSize) NoMip [img]

moments :: Exp Obj (FrameBuffer N1 (Float,V2F))
moments = Accumulate fragCtx PassAll storeDepth rast clear
  where
    fragCtx = AccumulationContext Nothing $ DepthOp Less True:.ColorOp NoBlending (one' :: V2B):.ZT
    clear   = FrameBuffer (DepthImage n1 1000:.ColorImage n1 (V2 0 0):.ZT)
    rast    = Rasterize triangleCtx prims
    prims   = Transform vert input
    input   = Fetch "geometrySlot" Triangle (IV3F "position")
    lightMatrix = Uni (IM44F "lightMatrix")
    modelMatrix = Uni (IM44F "modelMatrix")

    vert :: Exp V V3F -> VertexOut Float
    vert p = VertexOut v4 (floatV 1) (Smooth depth:.ZT)
      where
        v4    = lightMatrix @*. modelMatrix @*. v3v4 p
        V4 _ _ depth _ = unpack' v4

    storeDepth :: Exp F Float -> FragmentOut (Depth Float :+: Color V2F :+: ZZ)
    storeDepth depth = FragmentOutRastDepth $ pack' (V2 moment1 moment2) :. ZT
      where
        dx = dFdx' depth
        dy = dFdy' depth
        moment1 = depth
        moment2 = depth @* depth @+ floatF 0.25 @* (dx @* dx @+ dy @* dy)

vsm :: Exp Obj (FrameBuffer N1 (Float,V4F))
vsm = Accumulate fragCtx PassAll calcLuminance rast clear
  where
    fragCtx = AccumulationContext Nothing $ DepthOp Less True:.ColorOp NoBlending (one' :: V4B):.ZT
    clear   = FrameBuffer (DepthImage n1 1000:.ColorImage n1 (V4 0.1 0.2 0.6 1):.ZT)
    rast    = Rasterize triangleCtx prims
    prims   = Transform vert input
    input   = Fetch "geometrySlot" Triangle (IV3F "position", IV3F "normal")
    cameraMatrix = Uni (IM44F "cameraMatrix")
    lightMatrix = Uni (IM44F "lightMatrix")
    modelMatrix = Uni (IM44F "modelMatrix")
    lightPosition = Uni (IV3F "lightPosition")

    vert :: Exp V (V3F, V3F) -> VertexOut (V3F, V4F, V3F)
    vert attr = VertexOut v4 (floatV 1) (Smooth (v4v3 p'):.Smooth v4l:.Smooth n3:.ZT)
      where
        p' = modelMatrix @*. v3v4 p
        v4 = cameraMatrix @*. p'
        v4l = lightMatrix @*. p'
        n3 = normalize' (v4v3 (modelMatrix @*. n3v4 n))
        (p,n) = untup2 attr

    calcLuminance :: Exp F (V3F, V4F, V3F) -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    calcLuminance attr = FragmentOutRastDepth $ ({- amb @+ -}p_max):. ZT
      where
        amb :: Exp F V4F
        amb = Const $ V4 0.1 0.1 0.3 1
        V4 tx ty tz tw = unpack' l
        clampUV x = clamp' x (floatF 0) (floatF 1)
        scale x = x @* floatF 0.5 @+ floatF 0.5
        u = clampUV (scale (tx @/ tw))
        v = clampUV (scale (ty @/ tw))
        V2 m1 m2 = unpack' $ texture' sampler (pack' $ V2 u v)
        variance = max' (floatF 0.002) (m2 @- m1 @* m1)
        d = max' (floatF 0) (tz @- m1)
        u' = u @- floatF 0.5
        v' = v @- floatF 0.5
        lt = max' (floatF 0) (dot' n (normalize' ((lightPosition :: Exp F V3F) @- wp)))
        intensity = max' (floatF 0) ((floatF 1 @- sqrt' (u' @* u' @+ v' @* v') @* floatF 4) @* lt)
        ltr = scale (round' (u' @* floatF 10)) @* intensity
        ltg = scale (round' (v' @* floatF 10)) @* intensity
        p_max = pack' (V4 ltr ltg intensity (floatF 1)) @* (variance @/ (variance @+ d @* d))
        (wp,l,n) = untup3 attr

    sampler = Sampler LinearFilter Clamp shadowMapBlur
    
    shadowMap :: Texture (Exp Obj) DIM2 SingleTex (Regular Float) RG
    shadowMap = Texture (Texture2D (Float RG) n1) (V2 512 512) NoMip [PrjFrameBuffer "shadowMap" tix0 moments]

    shadowMapBlur :: Texture (Exp Obj) DIM2 SingleTex (Regular Float) RG
    shadowMapBlur = Texture (Texture2D (Float RG) n1) (V2 512 512) NoMip [PrjFrameBuffer "shadowMap" tix0 $ blurVH $ PrjFrameBuffer "blur" tix0 moments]
