{-# LANGUAGE OverloadedStrings, TypeOperators, ParallelListComp #-}
module GameGraphics where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as SB
import Data.Int
import Data.Vect.Float hiding (reflect')

import LC_API
--import LCLanguage

-- specialized snoc
padV3 :: Exp s V3F -> Float -> Exp s V4F
padV3 v s = let V3 x y z = unpack' v in pack' $ V4 x y z (Const s)

trimV4 :: Exp s V4F -> Exp s V3F
trimV4 v = let V4 x y z _ = unpack' v in pack' $ V3 x y z

trimM4 :: Exp s M44F -> Exp s M33F
trimM4 v = let V4 i j k _ = unpack' v in pack' $ V3 (trimV4 i) (trimV4 j) (trimV4 k)

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
stuntsGFX = Accumulate fragCtx (Filter fragPassed) frag rast clear
  where
    fragCtx = DepthOp Less True:.ColorOp NoBlending (one' :: V4B):.ZT
    rastCtx = TriangleCtx CullNone PolygonFill NoOffset LastVertex
    clear   = FrameBuffer (V2 0 0) (DepthImage n1 100000:.ColorImage n1 (V4 0.36 0.99 0.99 1 :: V4F):.ZT)
    rast    = Rasterize rastCtx NoGeometryShader $ Transform vert $ Fetch "streamSlot" Triangle input
    input   = (IV3F "position", IV3F "normal", IV4F "colour", IInt "pattern", IFloat "zBias", IFloat "shininess")
    worldView = Uni (IM44F "worldView")
    projection = Uni (IM44F "projection")
    lightPosition = Uni (IV3F "lightPosition")

    vert :: Exp V (V3F,V3F,V4F,Int32,Float,Float) -> VertexOut (V4F,V3F,V3F,V3F,Int32,Float,Float)
    vert attr = VertexOut projPos (Const 1) (Flat colour:.Smooth pos:.Smooth eyePos:.Smooth normal:.Flat pattern:.Flat zBias:.Flat shiny:.ZT)
      where
        viewPos = worldView @*. padV3 pos 1
        projPos = projection @*. viewPos
        normal = normalize' (trimM4 worldView @*. n)
        eyePos = trimV4 viewPos
        
        (pos,n,colour,pattern,zBias,shiny) = untup6 attr

    fragPassed :: Exp F (V4F,V3F,V3F,V3F,Int32,Float,Float) -> Exp F Bool
    fragPassed attr = (pattern @== int32F 1) @|| ((pattern @/= int32F 0) @&& isSolid)
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

        (colour,pos,eyePos,normal,pattern,zBias,shiny) = untup7 attr

    frag :: Exp F (V4F,V3F,V3F,V3F,Int32,Float,Float) -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    frag attr = FragmentOutDepth adjustedDepth (litColour :. ZT)
      where
        l = normalize' (lightPosition @- eyePos)
        e = normalize' (neg' eyePos)
        n = normal
        r = normalize' (reflect' (neg' l) n)
        
        lambert = l @. n
        phong = max' (r @. e) (floatF 0)

        intensity = floatF 0.3 @+ max' (floatF 0) lambert @* floatF 0.5 @+ pow' phong (floatF 5) @* floatF 0.3
        highlight = pow' phong shiny @* min' (floatF 1.0) (shiny @* floatF 0.04)

        litColour = padV3 (trimV4 (colour @* intensity @+ ((Const (V4 1 1 1 0) :: Exp F V4F) @* highlight))) 1
        
        fragDepth = get4Z fragCoord
        adjustedDepth = fragDepth @+ max' (exp' (floatF (-15) @- log' fragDepth)) (fwidth' fragDepth) @* zBias

        (colour,pos,eyePos,normal,pattern,zBias,shiny) = untup7 attr

debugShader :: GP (FrameBuffer N1 (Float,V4F))
debugShader = Accumulate fragCtx PassAll frag rast stuntsGFX
  where
    offset  = NoOffset
    fragCtx = DepthOp Lequal True:.ColorOp NoBlending (one' :: V4B):.ZT
    rastCtx = TriangleCtx CullNone (PolygonLine 2) offset LastVertex
    rast    = Rasterize rastCtx NoGeometryShader prims
    prims   = Transform vert input
    input   = Fetch "wireSlot" Triangle (IV3F "position", IV4F "colour")
    worldViewProj = projection @.*. worldView :: Exp a M44F
    worldView = Uni (IM44F "worldView")
    projection = Uni (IM44F "projection")

    vert :: Exp V (V3F,V4F) -> VertexOut V4F
    vert pc = VertexOut v4 (Const 1) (Smooth c:.ZT)
      where
        v4    = worldViewProj @*. padV3 p 1
        (p,c) = untup2 pc

    frag :: Exp F V4F -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    frag v = FragmentOutRastDepth $ (v @* (Const (V4 3 3 3 1) :: Exp F V4F)) :. ZT

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
