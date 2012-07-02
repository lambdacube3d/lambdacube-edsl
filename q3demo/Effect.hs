{-# LANGUAGE OverloadedStrings, PackageImports, TypeOperators #-}
module Effect (blurVH,screenQuad) where

import Data.ByteString.Char8 (ByteString)
import LC_API

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
    fragCtx = AccumulationContext Nothing $ DepthOp Always False:.ColorOp NoBlending (one' :: V4B):.ZT
    clear   = FrameBuffer (DepthImage n1 1000:.ColorImage n1 (V4 1 0 0 1):.ZT)
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

blurVH :: GP (Image N1 V4F) -> GP (FrameBuffer N1 (Float,V4F))
blurVH img = blur' fragH
  where
    sizeT = 512
    sizeI = floor sizeT
    uvH v = Const (V2 (v/sizeT) 0) :: Exp F V2F
    uvV v = Const (V2 0 (v/sizeT)) :: Exp F V2F
    fragH :: Exp F V2F -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    fragH uv' = FragmentOutRastDepth $ (sampleH gaussFilter7) :. ZT
      where
        sampleH ((o,c):[])  = texture' smp (uv @+ uvH o) (Const 0) @* floatF c
        sampleH ((o,c):xs)  = (texture' smp (uv @+ uvH o) (Const 0) @* floatF c) @+ sampleH xs
        V2 u v = unpack' uv
        uv = uv' @* floatF 0.5 @+ floatF 0.5
        smp = Sampler LinearFilter Clamp tex
        tex = Texture (Texture2D (Float RGBA) n1) (V2 sizeI sizeI) NoMip [PrjFrameBuffer "" tix0 (blur' fragV)]

    fragV :: Exp F V2F -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    fragV uv' = FragmentOutRastDepth $ (sampleV gaussFilter7) :. ZT
      where
        sampleV ((o,c):[])  = texture' smp (uv @+ uvV o) (Const 0) @* floatF c
        sampleV ((o,c):xs)  = (texture' smp (uv @+ uvV o) (Const 0) @* floatF c) @+ sampleV xs
        V2 u v = unpack' uv
        uv = uv' @* floatF 0.5 @+ floatF 0.5
        smp = Sampler LinearFilter Clamp tex
        tex = Texture (Texture2D (Float RGBA) n1) (V2 sizeI sizeI) NoMip [img]

-- draw quad

screenQuad :: GP (FrameBuffer N1 V4F)
screenQuad = Accumulate fragCtx PassAll frag rast clear
  where
    fragCtx = AccumulationContext Nothing $ ColorOp NoBlending (one' :: V4B):.ZT
    clear   = FrameBuffer (ColorImage n1 (V4 1 0 0 1):.ZT)
    rast    = Rasterize triangleCtx prims
    prims   = Transform vert input
    input   = Fetch "postSlot" Triangle (IV2F "position")

    vert :: Exp V V2F -> VertexOut V2F
    vert uv = VertexOut v4 (Const 1) (NoPerspective uv:.ZT)
      where
        v4      = pack' $ V4 u v (floatV 1) (floatV 1)
        V2 u v  = unpack' uv

    frag :: Exp F V2F -> FragmentOut (Color V4F :+: ZZ)
    frag uv' = FragmentOut $ color :. ZT
      where
        color = texture' smp (pack' $ V2 u (floatF 1 @- v)) (Const 0)
        V2 u v = unpack' $ uv' @* floatF 0.5 @+ floatF 0.5
        smp = Sampler LinearFilter Clamp tex
        tex = TextureSlot "ScreenQuad" $ Texture2D (Float RGBA) n1

{-
screenQuad :: GP (FrameBuffer N1 (Float,V4F))
screenQuad = Accumulate fragCtx PassAll frag rast clear
  where
    fragCtx = AccumulationContext Nothing $ DepthOp Lequal True:.ColorOp NoBlending (one' :: V4B):.ZT
    clear   = FrameBuffer (DepthImage n1 1000:.ColorImage n1 (V4 0 1 0 1):.ZT)
    rast    = Rasterize triangleCtx prims
    prims   = Transform vert input
    input   = Fetch "postSlot" Triangle (IV2F "position")

    vert :: Exp V V2F -> VertexOut V2F
    vert uv = VertexOut v4 (Const 1) (NoPerspective uv:.ZT)
      where
        v4      = pack' $ V4 u v (floatV 1) (floatV 1)
        V2 u v  = unpack' uv

    frag :: Exp F V2F -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    frag uv' = FragmentOutRastDepth $ color @+ u :. ZT
      where
        V2 u v = unpack' uv
        uv = uv' @* floatF 0.5 @+ floatF 0.5
        color = Const one'
-}
{-
        color = texture' smp uv (Const 0)
        smp = Sampler LinearFilter Clamp tex
        tex = TextureSlot "ScreenQuad" $ Texture2D (Float RGBA) n1
-}