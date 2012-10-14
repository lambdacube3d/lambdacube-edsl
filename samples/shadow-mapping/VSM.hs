{-# LANGUAGE OverloadedStrings, PackageImports, TypeOperators #-}
module VSM where

import Data.ByteString.Char8 (ByteString)

import LC_API

import GraphicsUtils

shadowMapSize :: Num a => a
shadowMapSize = 512

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

blur :: [(Float, Float)] -> Exp Obj (Image N1 V2F) -> Exp Obj (FrameBuffer N1 V2F)
blur coefficients img = filter1D dirH (PrjFrameBuffer "" tix0 (filter1D dirV img))
  where
    dirH v = Const (V2 (v / shadowMapSize) 0) :: Exp F V2F
    dirV v = Const (V2 0 (v / shadowMapSize)) :: Exp F V2F
    
    filter1D :: (Float -> Exp F V2F) -> Exp Obj (Image N1 V2F) -> Exp Obj (FrameBuffer N1 V2F)
    filter1D dir img = Accumulate accCtx PassAll frag (Rasterize triangleCtx prims) clearBuf
      where
        accCtx = AccumulationContext Nothing (ColorOp NoBlending (one' :: V2B) :. ZT)
        clearBuf = FrameBuffer (ColorImage n1 (V2 0 0) :. ZT)
        prims = Transform vert (Fetch "postSlot" Triangle (IV2F "position"))

        vert :: Exp V V2F -> VertexOut V2F
        vert uv = VertexOut pos (Const 1) (NoPerspective uv' :. ZT)
          where
            uv'    = uv @* floatV 0.5 @+ floatV 0.5
            pos    = pack' (V4 u v (floatV 1) (floatV 1))
            V2 u v = unpack' uv

        frag :: Exp F V2F -> FragmentOut (Color V2F :+: ZZ)
        frag uv = FragmentOut (sample :. ZT)
          where
            sample = foldr1 (@+) [texture' smp (uv @+ dir ofs) @* floatF coeff | (ofs, coeff) <- coefficients]
            smp = Sampler LinearFilter Clamp tex
            tex = Texture (Texture2D (Float RG) n1) (V2 shadowMapSize shadowMapSize) NoMip [img]
    

moments :: Exp Obj (FrameBuffer N1 (Float, V2F))
moments = Accumulate accCtx PassAll frag (Rasterize triangleCtx prims) clearBuf
  where
    accCtx = AccumulationContext Nothing (DepthOp Less True :. ColorOp NoBlending (one' :: V2B) :. ZT)
    clearBuf = FrameBuffer (DepthImage n1 1000 :. ColorImage n1 (V2 0 0) :. ZT)
    prims = Transform vert (Fetch "geometrySlot" Triangle (IV3F "position"))
    
    lightMatrix = Uni (IM44F "lightMatrix")
    modelMatrix = Uni (IM44F "modelMatrix")

    vert :: Exp V V3F -> VertexOut Float
    vert pos = VertexOut lightPos (floatV 1) (Smooth depth :. ZT)
      where
        lightPos = lightMatrix @*. modelMatrix @*. v3v4 pos
        V4 _ _ depth _ = unpack' lightPos

    frag :: Exp F Float -> FragmentOut (Depth Float :+: Color V2F :+: ZZ)
    frag depth = FragmentOutRastDepth (pack' (V2 moment1 moment2) :. ZT)
      where
        dx = dFdx' depth
        dy = dFdy' depth
        moment1 = depth
        moment2 = depth @* depth @+ floatF 0.25 @* (dx @* dx @+ dy @* dy)

depth :: Exp Obj (FrameBuffer N1 (Float, Float))
depth = Accumulate accCtx PassAll frag (Rasterize triangleCtx prims) clearBuf
  where
    accCtx = AccumulationContext Nothing (DepthOp Less True :. ColorOp NoBlending True :. ZT)
    clearBuf = FrameBuffer (DepthImage n1 1000 :. ColorImage n1 0 :. ZT)
    prims = Transform vert (Fetch "geometrySlot" Triangle (IV3F "position"))
    
    lightMatrix = Uni (IM44F "lightMatrix")
    modelMatrix = Uni (IM44F "modelMatrix")

    vert :: Exp V V3F -> VertexOut Float
    vert pos = VertexOut lightPos (floatV 1) (Smooth depth :. ZT)
      where
        lightPos = lightMatrix @*. modelMatrix @*. v3v4 pos
        V4 _ _ depth _ = unpack' lightPos

    frag :: Exp F Float -> FragmentOut (Depth Float :+: Color Float :+: ZZ)
    frag depth = FragmentOutRastDepth (depth :. ZT)

vsm :: Exp Obj (FrameBuffer N1 (Float, V4F))
vsm = Accumulate accCtx PassAll frag (Rasterize triangleCtx prims) clearBuf
  where
    accCtx = AccumulationContext Nothing (DepthOp Less True :. ColorOp NoBlending (one' :: V4B) :. ZT)
    clearBuf = FrameBuffer (DepthImage n1 1000 :. ColorImage n1 (V4 0.1 0.2 0.6 1) :. ZT)
    prims = Transform vert (Fetch "geometrySlot" Triangle (IV3F "position", IV3F "normal"))

    cameraMatrix = Uni (IM44F "cameraMatrix")
    lightMatrix = Uni (IM44F "lightMatrix")
    modelMatrix = Uni (IM44F "modelMatrix")
    lightPosition = Uni (IV3F "lightPosition")

    vert :: Exp V (V3F, V3F) -> VertexOut (V3F, V4F, V3F)
    vert attr = VertexOut viewPos (floatV 1) (Smooth (v4v3 worldPos) :. Smooth lightPos :. Smooth worldNormal :. ZT)
      where
        worldPos = modelMatrix @*. v3v4 localPos
        viewPos = cameraMatrix @*. worldPos
        lightPos = lightMatrix @*. worldPos
        worldNormal = normalize' (v4v3 (modelMatrix @*. n3v4 localNormal))
        (localPos, localNormal) = untup2 attr

    frag :: Exp F (V3F, V4F, V3F) -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    frag attr = FragmentOutRastDepth (luminance :. ZT)
      where
        V4 lightU lightV lightDepth lightW = unpack' lightPos
        uv = clampUV (scaleUV (pack' (V2 lightU lightV) @/ lightW))
        
        V2 moment1 moment2 = unpack' (texture' sampler uv)
        variance = max' (floatF 0.002) (moment2 @- moment1 @* moment1)
        distance = max' (floatF 0) (lightDepth @- moment1)
        lightProbMax = variance @/ (variance @+ distance @* distance)
        
        lambert = max' (floatF 0) (dot' worldNormal (normalize' (lightPosition @- worldPos)))
        
        uv' = uv @- floatF 0.5
        spotShape = floatF 1 @- length' uv' @* floatF 4
        intensity = max' (floatF 0) (spotShape @* lambert)
        
        V2 spotR spotG = unpack' (scaleUV (round' (uv' @* floatF 10)) @* intensity)
        
        luminance = pack' (V4 spotR spotG intensity (floatF 1)) @* pow' lightProbMax (floatF 2)
        
        clampUV x = clamp' x (floatF 0) (floatF 1)
        scaleUV x = x @* floatF 0.5 @+ floatF 0.5
        
        (worldPos, lightPos, worldNormal) = untup3 attr

    sampler = Sampler LinearFilter Clamp shadowMapBlur
    
    shadowMap :: Texture (Exp Obj) DIM2 SingleTex (Regular Float) RG
    shadowMap = Texture (Texture2D (Float RG) n1) (V2 shadowMapSize shadowMapSize) NoMip [PrjFrameBuffer "shadowMap" tix0 moments]

    shadowMapBlur :: Texture (Exp Obj) DIM2 SingleTex (Regular Float) RG
    shadowMapBlur = Texture (Texture2D (Float RG) n1) (V2 shadowMapSize shadowMapSize) NoMip [PrjFrameBuffer "shadowMap" tix0 blurredMoments]
      where
        blurredMoments = blur blurCoefficients (PrjFrameBuffer "blur" tix0 moments)

sm :: Exp Obj (FrameBuffer N1 (Float, V4F))
sm = Accumulate accCtx PassAll frag (Rasterize triangleCtx prims) clearBuf
  where
    accCtx = AccumulationContext Nothing (DepthOp Less True :. ColorOp NoBlending (one' :: V4B) :. ZT)
    clearBuf = FrameBuffer (DepthImage n1 1000 :. ColorImage n1 (V4 0.1 0.2 0.6 1) :. ZT)
    prims = Transform vert (Fetch "geometrySlot" Triangle (IV3F "position", IV3F "normal"))

    cameraMatrix = Uni (IM44F "cameraMatrix")
    lightMatrix = Uni (IM44F "lightMatrix")
    modelMatrix = Uni (IM44F "modelMatrix")
    lightPosition = Uni (IV3F "lightPosition")

    vert :: Exp V (V3F, V3F) -> VertexOut (V3F, V4F, V3F)
    vert attr = VertexOut viewPos (floatV 1) (Smooth (v4v3 worldPos) :. Smooth lightPos :. Smooth worldNormal :. ZT)
      where
        worldPos = modelMatrix @*. v3v4 localPos
        viewPos = cameraMatrix @*. worldPos
        lightPos = lightMatrix @*. worldPos
        worldNormal = normalize' (v4v3 (modelMatrix @*. n3v4 localNormal))
        (localPos, localNormal) = untup2 attr

    frag :: Exp F (V3F, V4F, V3F) -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    frag attr = FragmentOutRastDepth (luminance :. ZT)
      where
        V4 lightU lightV lightDepth lightW = unpack' lightPos
        uv = clampUV (scaleUV (pack' (V2 lightU lightV) @/ lightW))
        
        surfaceDistance = texture' sampler uv
        lightPortion = Cond (lightDepth @<= surfaceDistance @+ floatF 0.01) (floatF 1) (floatF 0)
        
        lambert = max' (floatF 0) (dot' worldNormal (normalize' (lightPosition @- worldPos)))
        
        --intensity = lambert @* lightPortion
        --luminance = pack' (V4 intensity intensity intensity (floatF 1))
        
        uv' = uv @- floatF 0.5
        spotShape = floatF 1 @- length' uv' @* floatF 4
        intensity = max' (floatF 0) (spotShape @* lambert)
        
        V2 spotR spotG = unpack' (scaleUV (round' (uv' @* floatF 10)) @* intensity)
        
        luminance = pack' (V4 spotR spotG intensity (floatF 1)) @* lightPortion
        
        clampUV x = clamp' x (floatF 0) (floatF 1)
        scaleUV x = x @* floatF 0.5 @+ floatF 0.5
        
        (worldPos, lightPos, worldNormal) = untup3 attr

    sampler = Sampler PointFilter Clamp shadowMap
    
    shadowMap :: Texture (Exp Obj) DIM2 SingleTex (Regular Float) Red
    shadowMap = Texture (Texture2D (Float Red) n1) (V2 shadowMapSize shadowMapSize) NoMip [PrjFrameBuffer "shadowMap" tix0 depth]
