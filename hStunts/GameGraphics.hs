{-# LANGUAGE OverloadedStrings, TypeOperators, ParallelListComp #-}
module GameGraphics where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as SB
import Data.Int
import Data.Vect.Float hiding (reflect')

import LC_API
--import LCLanguage

--import VSM hiding (floatF)

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
stuntsGFX :: GP (FrameBuffer N1 (Float,V4F))
stuntsGFX = {-blurVH $ PrjFrameBuffer "blur" tix0 $ -}Accumulate fragCtx (Filter fragPassed) frag rast clear
  where
    fragCtx = AccumulationContext Nothing $ DepthOp Less True:.ColorOp NoBlending (one' :: V4B):.ZT
    rastCtx = TriangleCtx CullNone PolygonFill NoOffset LastVertex
    clear   = FrameBuffer (DepthImage n1 100000:.ColorImage n1 (V4 0.36 0.99 0.99 1 :: V4F):.ZT)
    rast    = Rasterize rastCtx $ Transform vert $ Fetch "streamSlot" Triangle input
    input   = (IV3F "position", IV3F "normal", IV4F "colour", IInt "pattern", IFloat "zBias", IFloat "shininess")
    worldPosition = Uni (IM44F "worldPosition")
    worldView = Uni (IM44F "worldView")
    projection = Uni (IM44F "projection")
    lightPosition = Uni (IV3F "lightPosition")
    lightViewProj = Uni (IM44F "lightViewProj")
    camMat = worldView @.*. worldPosition

    gridPattern :: Exp F V3F -> Exp F Int32 -> Exp F Bool
    gridPattern pos pattern = (pattern @== int32F 1) @|| ((pattern @/= int32F 0) @&& isSolid)
      where
        isSolid = solid1 @|| solid2 @|| solid3
        -- TODO this should be done with a noise texture instead of such an expensive operation
        rand = floatF 2 @* (fract' (sin' ((fragCoord @. (Const (V4 12.9898 78.233 0 0) :: Exp F V4F)) @* floatF 43758.5453)) @- floatF 0.5)
        prod1 = pos @. (Const (V3 2 2 (-2)) :: Exp F V3F)
        prod2 = pos @. (Const (V3 2 (-2) 2) :: Exp F V3F)
        prod3 = pos @. (Const (V3 (-2) 2 2) :: Exp F V3F)
        diff1 = fwidth' prod1
        diff2 = fwidth' prod2
        diff3 = fwidth' prod3
        solid1 = fract' (prod1 @+ rand @* smoothen diff1) @< floatF 0.2
        solid2 = fract' (prod2 @+ rand @* smoothen diff2) @< floatF 0.2
        solid3 = fract' (prod3 @+ rand @* smoothen diff3) @< floatF 0.2
        smoothen x = x @* x

    vert :: Exp V (V3F,V3F,V4F,Int32,Float,Float) -> VertexOut (V4F,V4F,V3F,V3F,V3F,Int32,Float,Float)
    vert attr = VertexOut projPos (Const 1) (Smooth lightViewPos:.Flat colour:.Smooth pos:.Smooth eyePos:.Smooth normal:.Flat pattern:.Flat zBias:.Flat shiny:.ZT)
      where
        viewPos = camMat @*. snoc pos 1
        projPos = projection @*. viewPos
        normal = normalize' (trimM4 camMat @*. n)
        eyePos = trimV4 viewPos
        lightViewPos = lightViewProj @*. worldPosition @*. snoc pos 1
        
        (pos,n,colour,pattern,zBias,shiny) = untup6 attr
    
    fragPassed :: Exp F (V4F,V4F,V3F,V3F,V3F,Int32,Float,Float) -> Exp F Bool
    fragPassed attr = gridPattern pos pattern
      where
        (_lightViewPos,_colour,pos,_eyePos,_normal,pattern,_zBias,_shiny) = untup8 attr

    frag :: Exp F (V4F,V4F,V3F,V3F,V3F,Int32,Float,Float) -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    frag attr = FragmentOutDepth adjustedDepth (litColour :. ZT)
      where
        l = normalize' (trimV4 (worldView @*. snoc lightPosition 1) @- eyePos)
        e = normalize' (neg' eyePos)
        n = normal
        r = normalize' (reflect' (neg' l) n)
        
        lambert = l @. n
        phong = max' (r @. e) (floatF 0)

        intensity = floatF 0.3 @+ light @* (max' (floatF 0) lambert @* floatF 0.5 @+ pow' phong (floatF 5) @* floatF 0.3)
        highlight = light @* (pow' phong shiny @* min' (floatF 1.0) (shiny @* floatF 0.04))

        litColour = snoc (trimV4 (colour @* intensity @+ (Const (V4 1 1 1 0) :: Exp F V4F) @* highlight)) 1
        
        fragDepth = get4Z fragCoord
        adjustedDepth = fragDepth @+ max' (exp' (floatF (-15) @- log' fragDepth)) (fwidth' fragDepth) @* zBias

        (lightViewPos,colour,pos,eyePos,normal,pattern,zBias,shiny) = untup8 attr

        V4 tx ty tz tw = unpack' lightViewPos
        u = tx @/ tw @* floatF 0.5 @+ floatF 0.5
        v = ty @/ tw @* floatF 0.5 @+ floatF 0.5
        V4 m1 m2 env _ = unpack' $ texture' sampler (pack' $ V2 u v) (floatF 0)
        inShadowTex = Cond (u @>= floatF 0.05 @&& u @<= floatF 0.95 @&& v @>= floatF 0.05 @&& v @<= floatF 0.95) (floatF 0) (floatF 1)
        variance = max' (floatF 0.002) ((m2 @- m1 @* m1) @/ (max' (floatF 1) (exp' (tz @- env))))
        d = max' (floatF 0) (tz @- m1)
        pmax = {-Cond (tz @> env) (floatF 0)-} (variance @/ (variance @+ d @* d))
        light = min' (floatF 1) (max' inShadowTex pmax)
        --light = min' (floatF 1) (max' inShadowTex ((pmax @- floatF 0.5) @* max' (floatF 1) (exp' (floatF 5 @- d)) @+ floatF 0.5))
        --f x = exp' (x @* floatF 0.01)
        --light = min' (floatF 1) (max' inShadowTex (pow' (f m1 @/ f tz) (floatF 20)))
        
    sampler = Sampler LinearFilter Clamp shadowMapBlur
    
    shadowMap :: Texture GP DIM2 SingleTex (Regular Float) RGBA
    shadowMap = Texture (Texture2D (Float RGBA) n1) (V2 512 512) NoMip [PrjFrameBuffer "shadowMap" tix0 moments]
    
    shadowMapEnvelope :: Texture GP DIM2 SingleTex (Regular Float) RGBA
    shadowMapEnvelope = Texture (Texture2D (Float RGBA) n1) (V2 512 512) NoMip [PrjFrameBuffer "shadowMap" tix0 $ shadowEnvelope $ PrjFrameBuffer "envelope" tix0 moments]

    shadowMapBlur :: Texture GP DIM2 SingleTex (Regular Float) RGBA
    shadowMapBlur = Texture (Texture2D (Float RGBA) n1) (V2 512 512) NoMip [PrjFrameBuffer "shadowMap" tix0 $ blurVH $ PrjFrameBuffer "blur" tix0 moments]

    moments :: GP (FrameBuffer N1 (Float,V4F))
    moments = Accumulate fragCtx (Filter fragPassed) storeDepth rast clear
      where
        fragCtx = AccumulationContext Nothing $ DepthOp Less True:.ColorOp NoBlending (one' :: V4B):.ZT
        clear   = FrameBuffer (DepthImage n1 100000:.ColorImage n1 (V4 0 0 1 1):.ZT)
        rast    = Rasterize triangleCtx prims
        prims   = Transform vert input
        input   = Fetch "streamSlot" Triangle (IV3F "position", IInt "pattern")
        worldPosition = Uni (IM44F "worldPosition")
        lightViewProj = Uni (IM44F "lightViewProj")
    
        vert :: Exp V (V3F, Int32) -> VertexOut (Float, V3F, Int32)
        vert attr = VertexOut v4 (floatV 1) (Smooth depth:.Smooth pos:.Flat pattern:.ZT)
          where
            v4 = lightViewProj @*. worldPosition @*. snoc pos 1
            V4 _ _ depth _ = unpack' v4
            (pos,pattern) = untup2 attr
    
        fragPassed :: Exp F (Float, V3F, Int32) -> Exp F Bool
        fragPassed attr = gridPattern pos pattern
          where
            (_depth,pos,pattern) = untup3 attr
            
        storeDepth :: Exp F (Float, V3F, Int32) -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
        storeDepth attr = FragmentOutRastDepth $ pack' (V4 moment1 moment2 (moment1 @* floatF 1.05) (floatF 1)) :. ZT
          where
            dx = dFdx' depth
            dy = dFdy' depth
            moment1 = depth
            moment2 = depth @* depth @+ floatF 0.25 @* (dx @* dx @+ dy @* dy)
            (depth,_pos,_pattern) = untup3 attr

shadowEnvelope :: GP (Image N1 V4F) -> GP (FrameBuffer N1 (Float,V4F))
shadowEnvelope img = Accumulate fragCtx PassAll frag rast clear
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

    sizeT = 512
    sizeI = floor sizeT
    frag :: Exp F V2F -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    frag uv' = FragmentOutRastDepth $ pack' (V4 moment1 moment2 envelope (floatF 1)) :. ZT
      where
        envelope = mkEnv [(x,y) | x <- [-1,0,1], y <- [-1,0,1], (x,y) /= (0,0)]
        V4 moment1 moment2 moment1' _ = unpack' $ texture' smp uv (Const 0)
        mkEnv [] = moment1'
        mkEnv ((dh,dv):ds) = max' moment1' (mkEnv ds)
          where
            V4 _ _ moment1' _ = unpack' $ texture' smp (uv @+ (Const (V2 (dh/sizeT) (dv/sizeT)) :: Exp F V2F)) (Const 0)
        V2 u v = unpack' uv
        uv = uv' @* floatF 0.5 @+ floatF 0.5
        smp = Sampler LinearFilter Clamp tex
        tex = Texture (Texture2D (Float RGBA) n1) (V2 sizeI sizeI) NoMip [img]

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
    vert uv = VertexOut v4 (Const 1) (NoPerspective uv':.ZT)
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

blurVH :: GP (Image N1 V4F) -> GP (FrameBuffer N1 (Float,V4F))
blurVH img = blur' $ frag uvH $ PrjFrameBuffer "" tix0 $ blur' $ frag uvV img 
  where
    sizeT = 512
    sizeI = floor sizeT
    uvH v = Const (V2 (v/sizeT) 0) :: Exp F V2F
    uvV v = Const (V2 0 (v/sizeT)) :: Exp F V2F
    
    frag dFn img uv = FragmentOutRastDepth $ pack' (V4 m1 m2 env (floatF 1)) :. ZT
      where
        wsum ((o,c):[])  = texture' smp (uv @+ dFn o) (Const 0) @* floatF c
        wsum ((o,c):xs)  = (texture' smp (uv @+ dFn o) (Const 0) @* floatF c) @+ wsum xs
        mkEnv ((o,c):[])  = texture' smp (uv @+ dFn o) (Const 0)
        mkEnv ((o,c):xs)  = max' (texture' smp (uv @+ dFn o) (Const 0)) (mkEnv xs)
        V4 m1 m2 _ _ = unpack' $ wsum gaussFilter5
        V4 _ _ env _ = unpack' $ mkEnv gaussFilter5
        smp = Sampler LinearFilter Clamp tex
        tex = Texture (Texture2D (Float RGBA) n1) (V2 sizeI sizeI) NoMip [img]

{-
debugShader :: GP (FrameBuffer N1 (Float,V4F))
debugShader = Accumulate fragCtx PassAll frag rast stuntsGFX
  where
    offset  = NoOffset
    fragCtx = AccumulationContext Nothing $ DepthOp Lequal True:.ColorOp NoBlending (one' :: V4B):.ZT
    rastCtx = TriangleCtx CullNone (PolygonLine 2) offset LastVertex
    rast    = Rasterize rastCtx prims
    prims   = Transform vert input
    input   = Fetch "wireSlot" Triangle (IV3F "position", IV4F "colour")
    worldViewProj = projection @.*. worldView :: Exp a M44F
    worldView = Uni (IM44F "worldView")
    projection = Uni (IM44F "projection")

    vert :: Exp V (V3F,V4F) -> VertexOut V4F
    vert pc = VertexOut v4 (Const 1) (Smooth c:.ZT)
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
