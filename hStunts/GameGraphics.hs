{-# LANGUAGE OverloadedStrings, TypeOperators, ParallelListComp, DataKinds #-}
module GameGraphics where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as SB
import Data.Int
import Data.Vect.Float hiding (reflect')
import Data.List hiding (transpose)

import LC_API
--import LCLanguage

--import VSM hiding (floatF)

shadowMapSize :: Num a => a
shadowMapSize = 1024

maxCasterDistance :: Float
maxCasterDistance = 100

lightDirection :: Vec3
lightDirection = Vec3 250 (-600) 400

lightFrustumSlices :: [Float]
lightFrustumSlices = [0.3, 10, 35, 150, 700]

gridThicknesses :: [Float]
gridThicknesses = [0.2, 0.11, 0.1, 0.1]

lightFrustumSliceCount = length lightFrustumSlices - 1

-- specialized snoc
snoc :: Exp s V3F -> Float -> Exp s V4F
snoc v s = let V3 x y z = unpack' v in pack' $ V4 x y z (Const s)

trimV4 :: Exp s V4F -> Exp s V3F
trimV4 v = let V4 x y z _ = unpack' v in pack' $ V3 x y z

trimM4 :: Exp s M44F -> Exp s M33F
trimM4 v = let V4 i j k _ = unpack' v in pack' $ V3 (trimV4 i) (trimV4 j) (trimV4 k)

floatV :: Float -> Exp V Float
floatV = Const

floatF :: Float -> Exp F Float
floatF = Const

int32F :: Int32 -> Exp F Int32
int32F = Const

get4Z :: Exp s V4F -> Exp s Float
get4Z v = let V4 _ _ z _ = unpack' v in z

upwards :: Vec3
upwards = Vec3 0 1 0

v2FF :: V2F -> Exp F V2F
v2FF = Const


{-
data Primitive
    = Primitive
    { prType        :: PrimitiveType
    , prTwoSided    :: Bool
    , prZBias       :: Bool
    , prMaterials   :: [Int]
    , prIndices     :: [Int]
    }
    deriving Show

data Model
    = Model
    { mdVertices    :: [(Float,Float,Float)]
    , mdPrimitives  :: [Primitive]
    }
    deriving Show
-}
-- TODO: design graphics pipeline for stunts
{-
  material types:
    - opaque
        - layer 0 (ZBias = False)
        - layer 1 (ZBias = True)
    - transparent

  requirements:
    - opaque materials
    - multi layer trasnparent material with patterns

opaque
opaqueZBias
transparent
-}
stuntsGFX :: Exp Obj (FrameBuffer 1 (Float,V4F))
stuntsGFX = {-blurVH $ PrjFrameBuffer "blur" tix0 $ -}Accumulate fragCtx (Filter fragPassed) frag rast clear
  where
    fragCtx = AccumulationContext Nothing $ DepthOp Less True:.ColorOp NoBlending (one' :: V4B):.ZT
    rastCtx = TriangleCtx CullNone PolygonFill NoOffset LastVertex
    clear   = FrameBuffer (DepthImage n1 100000:.ColorImage n1 (V4 0.36 0.99 0.99 1 :: V4F):.ZT)
    rast    = Rasterize rastCtx $ Transform vert $ Fetch "streamSlot" Triangles input
    input   = (IV3F "position", IV3F "normal", IV4F "colour", IInt "pattern", IFloat "zBias", IFloat "shininess")
    worldPosition = Uni (IM44F "worldPosition")
    worldView = Uni (IM44F "worldView")
    projection = Uni (IM44F "projection")
    lightDirection = Uni (IV3F "lightDirection")
    lightViewProjs = [Uni (IM44F (SB.pack ("lightViewProj" ++ show slice))) | slice <- [1..lightFrustumSliceCount]]
    lightViewScales = [Uni (IV3F (SB.pack ("lightViewScale" ++ show slice))) | slice <- [1..lightFrustumSliceCount]]
    camMat = worldView @.*. worldPosition

    gridPattern :: Exp F Float -> Exp F V3F -> Exp F Int32 -> Exp F Bool
    gridPattern thickness pos pattern = (pattern @== int32F 1) @|| ((pattern @/= int32F 0) @&& isSolid)
      where
        isSolid = solid1 @|| solid2 @|| solid3
        -- TODO this should be done with a noise texture instead of such an expensive operation
        rand = floatF 2 @* (fract' (sin' ((fragCoord' @. (Const (V4 12.9898 78.233 0 0) :: Exp F V4F)) @* floatF 43758.5453)) @- floatF 0.5)
        prod1 = pos @. (Const (V3 2 2 (-2)) :: Exp F V3F)
        prod2 = pos @. (Const (V3 2 (-2) 2) :: Exp F V3F)
        prod3 = pos @. (Const (V3 (-2) 2 2) :: Exp F V3F)
        diff1 = fwidth' prod1
        diff2 = fwidth' prod2
        diff3 = fwidth' prod3
        solid1 = fract' (prod1 @+ rand @* smoothen diff1) @< thickness
        solid2 = fract' (prod2 @+ rand @* smoothen diff2) @< thickness
        solid3 = fract' (prod3 @+ rand @* smoothen diff3) @< thickness
        smoothen x = x @* x

    vert :: Exp V (V3F,V3F,V4F,Int32,Float,Float) -> VertexOut () (M44F,V4F,V3F,V3F,V3F,Int32,Float,Float)
    vert attr = VertexOut projPos (Const 1) ZT (Smooth lightViewPos:.Flat colour:.Smooth pos:.Smooth eyePos:.Flat normal:.Flat pattern:.Flat zBias:.Flat shiny:.ZT)
      where
        viewPos = camMat @*. snoc pos 1
        projPos = projection @*. viewPos
        normal = normalize' (trimM4 camMat @*. n)
        eyePos = trimV4 viewPos
        lightViewPos = pack' $ V4 lightViewPos1 lightViewPos2 lightViewPos3 lightViewPos4
        lightViewPos1 = (lightViewProjs !! 0) @*. worldPosition @*. snoc pos 1
        lightViewPos2 = (lightViewProjs !! 1) @*. worldPosition @*. snoc pos 1
        lightViewPos3 = (lightViewProjs !! 2) @*. worldPosition @*. snoc pos 1
        lightViewPos4 = (lightViewProjs !! 3) @*. worldPosition @*. snoc pos 1
        
        (pos,n,colour,pattern,zBias,shiny) = untup6 attr
    
    fragPassed :: Exp F (M44F,V4F,V3F,V3F,V3F,Int32,Float,Float) -> Exp F Bool
    fragPassed attr = gridPattern (floatF 0.2) pos pattern
      where
        (_lightViewPos,_colour,pos,_eyePos,_normal,pattern,_zBias,_shiny) = untup8 attr

    frag :: Exp F (M44F,V4F,V3F,V3F,V3F,Int32,Float,Float) -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    frag attr = FragmentOutDepth adjustedDepth (clamp' litColour (floatF 0) (floatF 1) :. ZT)
      where
        l = normalize' (trimV4 (worldView @*. snoc lightDirection 0))
        e = normalize' (neg' eyePos)
        n = Cond frontFacing' normal (neg' normal)
        r = normalize' (reflect' l n)
        
        lambert = neg' l @. n
        phong = max' (r @. e) (floatF 0)

        intensity = floatF 0.3 @+ light @* (max' (floatF 0) lambert @* floatF 0.5 @+ pow' phong (floatF 5) @* floatF 0.3)
        highlight = light @* (pow' phong shiny @* min' (floatF 1.0) (shiny @* floatF 0.04))

        litColour = snoc (trimV4 (colour @* intensity @+ (Const (V4 1 1 1 0) :: Exp F V4F) @* highlight)) 1
        
        fragDepth = get4Z fragCoord'
        adjustedDepth = fragDepth @+ max' (exp' (floatF (-15) @- log' fragDepth)) (fwidth' fragDepth) @* zBias

        (lightViewPos,colour,pos,eyePos,normal,pattern,zBias,shiny) = untup8 attr

        light = min' (min' (lightSlice 0) (lightSlice 1)) (min' (lightSlice 2) (lightSlice 3))

        V4 lp1 lp2 lp3 lp4 = unpack' lightViewPos
        lightViewPosVectors = [lp1, lp2, lp3, lp4]
        
        V3 _ _ eyeDist = unpack' (neg' eyePos)
        lightSlice slice = Cond (inSlice @&& inShadow) lightLevel (floatF 1)
          where
            V3 hScale vScale depthScale = unpack' (lightViewScales !! slice :: Exp F V3F)
            --lightLevel = clamp' ({-exp'-} (floatF 1 @+ floatF 5 @* (d @- tz) @/ depthScale)) (floatF 0) (floatF 1) 
            lightLevel = Cond (d @< tz) (floatF 0) (floatF 1)
            inSlice = eyeDist @>= lf slice @&& eyeDist @<= lf (slice+1)
            inShadow = u @>= floatF 0.01 @&& u @<= floatF 0.99 @&& v @>= floatF 0.01 @&& v @<= floatF 0.99
            
            V4 tx ty tz tw = unpack' (lightViewPosVectors !! slice)
            ditherAngle = dot' fragCoord' (Const (V4 (pi/2*1.7) (pi*1.3) 0 0) :: Exp F V4F)
            ditherFactor = floatF 3 @/ floatF shadowMapSize
            u = tx @/ tw @* floatF 0.5 @+ floatF 0.5 @+ sin' ditherAngle @* ditherFactor @* sqrt' hScale
            v = ty @/ tw @* floatF 0.5 @+ floatF 0.5 @+ cos' ditherAngle @* ditherFactor @* sqrt' vScale
            --u' = tx @/ tw @* floatF 0.5 @+ floatF 0.5
            --v' = ty @/ tw @* floatF 0.5 @+ floatF 0.5
            --u = u' @+ sin' ditherAngle @* fwidth' u' @* floatF 0.5
            --v = v' @+ cos' ditherAngle @* fwidth' v' @* floatF 0.5
            d = texture' (shadowSampler (slice+1)) (pack' (V2 u v))

            lf s = floatF (lightFrustumSlices !! s)
        {-
        V4 tx ty tz tw = unpack' lightViewPos1
        u = tx @/ tw @* floatF 0.5 @+ floatF 0.5
        v = ty @/ tw @* floatF 0.5 @+ floatF 0.5
        V4 m1 m2 env _ = unpack' $ texture' sampler (pack' $ V2 u v)
        inShadowTex = Cond (u @>= floatF 0.05 @&& u @<= floatF 0.95 @&& v @>= floatF 0.05 @&& v @<= floatF 0.95) (floatF 0) (floatF 1)
        variance = max' (floatF 0.002) ((m2 @- m1 @* m1) @/ (max' (floatF 1) (exp' (tz @- env))))
        d = max' (floatF 0) (tz @- m1)
        pmax = {-Cond (tz @> env) (floatF 0)-} (variance @/ (variance @+ d @* d))
        light = min' (floatF 1) (max' inShadowTex pmax)
        --light = min' (floatF 1) (max' inShadowTex ((pmax @- floatF 0.5) @* max' (floatF 1) (exp' (floatF 5 @- d)) @+ floatF 0.5))
        --f x = exp' (x @* floatF 0.01)
        --light = min' (floatF 1) (max' inShadowTex (pow' (f m1 @/ f tz) (floatF 20)))
        -}
        
    shadowSampler slice = Sampler LinearFilter ClampToEdge (shadowMap slice)
    
    --shadowMap :: Int -> Texture (Exp Obj) Tex2D SingleTex (Regular Float) Red
    shadowMap slice = Texture (Texture2D (Float Red) n1) (V2 shadowMapSize shadowMapSize) NoMip [PrjFrameBuffer "shadowMap" tix0 (moments slice)]

    --shadowMapBlur :: Int -> Texture (Exp Obj) Tex2D SingleTex (Regular Float) Red
    shadowMapBlur slice = Texture (Texture2D (Float Red) n1) (V2 shadowMapSize shadowMapSize) NoMip [PrjFrameBuffer "shadowMap" tix0 $ blurDepth slice $ PrjFrameBuffer "blur" tix0 (moments slice)]
    
    --shadowMapEnvelope :: Texture GP Tex2D SingleTex (Regular Float) RGBA
    --shadowMapEnvelope = Texture (Texture2D (Float RGBA) n1) (V2 shadowMapSize shadowMapSize) NoMip [PrjFrameBuffer "shadowMap" tix0 $ shadowEnvelope $ PrjFrameBuffer "envelope" tix0 moments]

    --shadowMapBlur :: Texture GP Tex2D SingleTex (Regular Float) RGBA
    --shadowMapBlur = Texture (Texture2D (Float RGBA) n1) (V2 shadowMapSize shadowMapSize) NoMip [PrjFrameBuffer "shadowMap" tix0 $ blurVH $ PrjFrameBuffer "blur" tix0 moments]

    moments :: Int -> Exp Obj (FrameBuffer 1 (Float, Float {-V4F-}))
    moments slice = Accumulate fragCtx (Filter fragPassed) storeDepth rast clear
      where
        fragCtx = AccumulationContext Nothing $ DepthOp Less True:.ColorOp NoBlending (one' :: Bool {-V4B-}):.ZT
        clear   = FrameBuffer (DepthImage n1 1:.ColorImage n1 0 {-(V4 0 0 1 1)-}:.ZT)
        rast    = Rasterize triangleCtx prims
        prims   = Transform vert input
        input   = Fetch "streamSlot" Triangles (IV3F "position", IInt "pattern")
        worldPosition = Uni (IM44F "worldPosition")
        lightViewProj = Uni (IM44F (SB.pack ("lightViewProj" ++ show slice)))
        gridThickness = Uni (IFloat (SB.pack ("gridThickness" ++ show slice)))
    
        vert :: Exp V (V3F, Int32) -> VertexOut () (Float, V3F, Int32)
        vert attr = VertexOut v4 (floatV 1) ZT (Smooth depth:.Smooth pos:.Flat pattern:.ZT)
          where
            v4 = lightViewProj @*. worldPosition @*. snoc pos 1
            V4 _ _ depth _ = unpack' v4
            (pos,pattern) = untup2 attr
    
        fragPassed :: Exp F (Float, V3F, Int32) -> Exp F Bool
        fragPassed attr = gridPattern gridThickness pos pattern
          where
            (_depth,pos,pattern) = untup3 attr
            
        storeDepth :: Exp F (Float, V3F, Int32) -> FragmentOut (Depth Float :+: Color Float {-V4F-} :+: ZZ)
        storeDepth attr = FragmentOutRastDepth $ depth @+ fwidth' depth {-pack' (V4 (depth @+ fwidth' depth) (floatF 0) (floatF 0) (floatF 1))-} :. ZT
          where
            (depth,_pos,_pattern) = untup3 attr
        {-
        storeDepth :: Exp F (Float, V3F, Int32) -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
        storeDepth attr = FragmentOutRastDepth $ pack' (V4 moment1 moment2 (moment1 @* floatF 1.05) (floatF 1)) :. ZT
          where
            dx = dFdx' depth
            dy = dFdy' depth
            moment1 = depth
            moment2 = depth @* depth @+ floatF 0.25 @* (dx @* dx @+ dy @* dy)
            (depth,_pos,_pattern) = untup3 attr
        -}

shadowEnvelope :: Exp Obj (Image 1 V4F) -> Exp Obj (FrameBuffer 1 (Float,V4F))
shadowEnvelope img = Accumulate fragCtx PassAll frag rast clear
  where
    fragCtx = AccumulationContext Nothing $ DepthOp Always False:.ColorOp NoBlending (one' :: V4B):.ZT
    clear   = FrameBuffer (DepthImage n1 1000:.ColorImage n1 (V4 1 0 0 1):.ZT)
    rast    = Rasterize triangleCtx prims
    prims   = Transform vert input
    input   = Fetch "postSlot" Triangles (IV2F "position")

    vert :: Exp V V2F -> VertexOut () V2F
    vert uv = VertexOut v4 (Const 1) ZT (NoPerspective uv:.ZT)
      where
        v4      = pack' $ V4 u v (floatV 1) (floatV 1)
        V2 u v  = unpack' uv

    sizeT = shadowMapSize
    sizeI = floor sizeT
    frag :: Exp F V2F -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    frag uv' = FragmentOutRastDepth $ pack' (V4 moment1 moment2 envelope (floatF 1)) :. ZT
      where
        envelope = mkEnv [(x,y) | x <- [-1,0,1], y <- [-1,0,1], (x,y) /= (0,0)]
        V4 moment1 moment2 moment1' _ = unpack' $ texture' smp uv
        mkEnv [] = moment1'
        mkEnv ((dh,dv):ds) = max' moment1' (mkEnv ds)
          where
            V4 _ _ moment1' _ = unpack' $ texture' smp (uv @+ (Const (V2 (dh/sizeT) (dv/sizeT)) :: Exp F V2F))
        V2 u v = unpack' uv
        uv = uv' @* floatF 0.5 @+ floatF 0.5
        smp = Sampler LinearFilter ClampToEdge tex
        tex = Texture (Texture2D (Float RGBA) n1) (V2 sizeI sizeI) NoMip [img]

-- blur

blur' :: (Exp F V2F -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)) -> Exp Obj (FrameBuffer 1 (Float,V4F))
blur' frag = Accumulate fragCtx PassAll frag rast clear
  where
    fragCtx = AccumulationContext Nothing $ DepthOp Always False:.ColorOp NoBlending (one' :: V4B):.ZT
    clear   = FrameBuffer (DepthImage n1 1000:.ColorImage n1 (V4 1 0 0 1):.ZT)
    rast    = Rasterize triangleCtx prims
    prims   = Transform vert input
    input   = Fetch "postSlot" Triangles (IV2F "position")

    vert :: Exp V V2F -> VertexOut () V2F
    vert uv = VertexOut v4 (Const 1) ZT (NoPerspective uv':.ZT)
      where
        v4      = pack' $ V4 u v (floatV 1) (floatV 1)
        V2 u v  = unpack' uv
        uv'     = uv @* floatV 0.5 @+ floatV 0.5

gaussFilter5 :: [(Float,Float)]
gaussFilter5 = 
    [ (-2.0,   0.1)
    , (-1.0,   0.2)
    , (0.0,    0.4)
    , (1.0,    0.2)
    , (2.0,    0.1)
    ]

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

blurVH :: Exp Obj (Image 1 V4F) -> Exp Obj (FrameBuffer 1 (Float,V4F))
blurVH img = blur' $ frag uvH $ PrjFrameBuffer "" tix0 $ blur' $ frag uvV img 
  where
    sizeT = shadowMapSize
    sizeI = floor sizeT
    uvH v = Const (V2 (v/sizeT) 0) :: Exp F V2F
    uvV v = Const (V2 0 (v/sizeT)) :: Exp F V2F
    
    frag dFn img uv = FragmentOutRastDepth $ pack' (V4 m1 m2 env (floatF 1)) :. ZT
      where
        wsum ((o,c):[])  = texture' smp (uv @+ dFn o) @* floatF c
        wsum ((o,c):xs)  = (texture' smp (uv @+ dFn o) @* floatF c) @+ wsum xs
        mkEnv ((o,c):[])  = texture' smp (uv @+ dFn o)
        mkEnv ((o,c):xs)  = max' (texture' smp (uv @+ dFn o)) (mkEnv xs)
        V4 m1 m2 _ _ = unpack' $ wsum gaussFilter5
        V4 _ _ env _ = unpack' $ mkEnv gaussFilter5
        smp = Sampler LinearFilter ClampToEdge tex
        tex = Texture (Texture2D (Float RGBA) n1) (V2 sizeI sizeI) NoMip [img]

blurDepth :: Int -> Exp Obj (Image 1 Float) -> Exp Obj (FrameBuffer 1 (Float,Float))
blurDepth slice img = blur $ frag uvH $ PrjFrameBuffer "" tix0 $ blur $ frag uvV img 
  where
    sizeT = shadowMapSize
    sizeI = floor sizeT
    blurFactor = 5
    uvH v = (Const (V2 (blurFactor*v/sizeT) 0) :: Exp F V2F) @* xScale
    uvV v = (Const (V2 0 (blurFactor*v/sizeT)) :: Exp F V2F) @* yScale
    lightViewScale = Uni (IV3F (SB.pack ("lightViewScale" ++ show slice))) :: Exp F V3F
    V3 xScale yScale _ = unpack' lightViewScale
    
    frag dFn img uv = FragmentOutRastDepth $ wsum gaussFilter5 :. ZT
      where
        wsum ((o,c):[])  = texture' smp (uv @+ dFn o) @* floatF c
        wsum ((o,c):xs)  = (texture' smp (uv @+ dFn o) @* floatF c) @+ wsum xs
        smp = Sampler LinearFilter ClampToEdge tex
        tex = Texture (Texture2D (Float Red) n1) (V2 sizeI sizeI) NoMip [img]

    blur :: (Exp F V2F -> FragmentOut (Depth Float :+: Color Float :+: ZZ)) -> Exp Obj (FrameBuffer 1 (Float,Float))
    blur frag = Accumulate fragCtx PassAll frag rast clear
      where
        fragCtx = AccumulationContext Nothing $ DepthOp Always False:.ColorOp NoBlending (one' :: Bool):.ZT
        clear   = FrameBuffer (DepthImage n1 1000:.ColorImage n1 0:.ZT)
        rast    = Rasterize triangleCtx prims
        prims   = Transform vert input
        input   = Fetch "postSlot" Triangles (IV2F "position")

        vert :: Exp V V2F -> VertexOut () V2F
        vert uv = VertexOut v4 (Const 1) ZT (NoPerspective uv':.ZT)
          where
            v4      = pack' $ V4 u v (floatV 1) (floatV 1)
            V2 u v  = unpack' uv
            uv'     = uv @* floatV 0.5 @+ floatV 0.5
        
{-
debugShader :: Exp Obj (FrameBuffer 1 (Float,V4F))
debugShader = Accumulate fragCtx PassAll frag rast stuntsGFX
  where
    offset  = NoOffset
    fragCtx = AccumulationContext Nothing $ DepthOp Lequal True:.ColorOp NoBlending (one' :: V4B):.ZT
    rastCtx = TriangleCtx CullNone (PolygonLine 2) offset LastVertex
    rast    = Rasterize rastCtx prims
    prims   = Transform vert input
    input   = Fetch "wireSlot" Triangles (IV3F "position", IV4F "colour")
    worldViewProj = projection @.*. worldView :: Exp a M44F
    worldView = Uni (IM44F "worldView")
    projection = Uni (IM44F "projection")

    vert :: Exp V (V3F,V4F) -> VertexOut () V4F
    vert pc = VertexOut v4 (Const 1) ZT (Smooth c:.ZT)
      where
        v4    = worldViewProj @*. snoc p 1
        (p,c) = untup2 pc

    frag :: Exp F V4F -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    frag v = FragmentOutRastDepth $ (v @* (Const (V4 3 3 3 1) :: Exp F V4F)) :. ZT
-}

-- | Camera transformation matrix.
lookat :: Vec3   -- ^ Camera position.
       -> Vec3   -- ^ Target position.
       -> Vec3   -- ^ Upward direction.
       -> Proj4
lookat pos target up = translateBefore4 (neg pos) (orthogonal $ toOrthoUnsafe r)
  where
    w = normalize $ pos &- target
    u = normalize $ up &^ w
    v = w &^ u
    r = transpose $ Mat3 u v w

-- | Perspective transformation matrix in row major order.
perspective :: Float  -- ^ Near plane clipping distance (always positive).
            -> Float  -- ^ Far plane clipping distance (always positive).
            -> Float  -- ^ Field of view of the y axis, in radians.
            -> Float  -- ^ Aspect ratio, i.e. screen's width\/height.
            -> Mat4
perspective n f fovy aspect = transpose $
    Mat4 (Vec4 (2*n/(r-l))       0       (-(r+l)/(r-l))        0)
         (Vec4     0        (2*n/(t-b))  ((t+b)/(t-b))         0)
         (Vec4     0             0       (-(f+n)/(f-n))  (-2*f*n/(f-n)))
         (Vec4     0             0            (-1)             0)
  where
    t = n*tan(fovy/2)
    b = -t
    r = aspect*t
    l = -r

gridThickness n = gridThicknesses !! (n-1)

lightProjection nearDepth farDepth fieldOfView aspectRatio worldViewMat =
    (lightSpaceOrientation .*. scaling scale .*. translation (Vec3 xtrans ytrans ztrans), scale)
  where
    localFrustum = [Vec4 (x*z) (y*z) (-z) 1 | x <- [-extentX, extentX], y <- [-extentY, extentY], z <- [nearDepth, farDepth]]
      where
        extentY = tan (fieldOfView/2)
        extentX = aspectRatio * extentY
                
    lightSpaceOrientation = lookat zero (neg lightDirection) upwards
    lightCameraFrustum = [v .* fromProjective (inverse worldViewMat .*. lightSpaceOrientation) | v <- localFrustum]
    Vec4 xmin ymin zmin _ = foldl1' (onVec4 min) lightCameraFrustum
    Vec4 xmax ymax zmax _ = foldl1' (onVec4 max) lightCameraFrustum
    (xscale, xtrans) = linearTransform xmin (-1) xmax 1
    (yscale, ytrans) = linearTransform ymin 0.001 ymax 1
    (zscale, ztrans) = linearTransform (min (zmin-0.2*(zmax-zmin)) (zmax-maxCasterDistance)) 0 zmax 1
    scale = Vec3 xscale yscale zscale
                
    onVec4 f (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) = Vec4 (f x1 x2) (f y1 y2) (f z1 z2) (f w1 w2)
    linearTransform x0 y0 x1 y1 = (a, b)
      where
        a = (y1-y0)/(x1-x0)
        b = y0-a*x0

addHUD fb = renderScreen frag
  where
    frag uv = FragmentOutRastDepth $ clamp' smp (floatF 0) (floatF 1) :. ZT
      where
        uv' = pack' (V2 u (floatF 1 @- v))
        V2 u v = unpack' uv
        smp = texture' (Sampler LinearFilter ClampToEdge $ TextureSlot "hudTexture" $ Texture2D (Float RGBA) n1) uv'

    renderScreen = PrjFrameBuffer "" tix0 . renderScreen'

    renderScreen' frag = Accumulate fragCtx PassAll frag rast fb
      where
        fragCtx = AccumulationContext Nothing $ DepthOp {-Less-}Always True:.ColorOp blend (one' :: V4B):.ZT
        rast    = Rasterize triangleCtx prims
        prims   = Transform vert input
        input   = Fetch "hud" Triangles (IV2F "position")

        hudTransfrom :: Exp V M44F
        hudTransfrom = Uni (IM44F "hudTransform")

        vert :: Exp V V2F -> VertexOut () V2F
        vert uv = VertexOut (hudTransfrom @*. v4) (Const 1) ZT (NoPerspective uv':.ZT)
          where
            v4      = pack' $ V4 u v (floatV 0) (floatV 1)
            V2 u v  = unpack' uv
            uv'     = uv @* floatV 0.5 @+ floatV 0.5

pixelize w h i = renderScreen frag
  where
    frag uv = FragmentOut $ smp uv :. ZT
      where
        smp coord = texture' (Sampler PointFilter ClampToEdge $ Texture (Texture2D (Float RGBA) n1) (V2 w h) NoMip [i]) coord

    renderScreen :: (Exp F V2F -> FragmentOut (Color V4F :+: ZZ)) -> Exp Obj (Image 1 V4F)
    renderScreen = PrjFrameBuffer "" tix0 . renderScreen'

    renderScreen' :: (Exp F V2F -> FragmentOut (Color V4F :+: ZZ)) -> Exp Obj (FrameBuffer 1 V4F)
    renderScreen' frag = Accumulate fragCtx PassAll frag rast clear
      where
        fragCtx = AccumulationContext Nothing $ ColorOp NoBlending (one' :: V4B):.ZT
        clear   = FrameBuffer (ColorImage n1 (V4 0 0 0 1):.ZT)
        rast    = Rasterize triangleCtx prims
        prims   = Transform vert input
        input   = Fetch "Quad" Triangles (IV2F "position")

        vert :: Exp V V2F -> VertexOut () V2F
        vert uv = VertexOut v4 (Const 1) ZT (NoPerspective uv':.ZT)
          where
            v4      = pack' $ V4 u v (floatV 1) (floatV 1)
            V2 u v  = unpack' uv
            uv'     = uv @* floatV 0.5 @+ floatV 0.5
