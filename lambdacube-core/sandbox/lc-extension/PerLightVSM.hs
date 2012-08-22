{-# LANGUAGE OverloadedStrings, PackageImports, TypeOperators #-}
module PerLightVSM where

import Data.ByteString.Char8 (ByteString)
import Data.Typeable

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

----------
-- VSM ---
----------
lights :: Exp Obj (DataArray M44F)
lights = userInput "lightSlot"

momentsTop :: Exp Obj (DataArray (M44F,Sampler DIM2 SingleTex (Regular Float) RG))
momentsTop = arrayMap mkSM lights
  where
    clear   = frameBuffer (DepthImage n1 1000:.ColorImage n1 (V2 0 0):.ZT)
    slot    = userInput "3D objects" :: Exp Obj (DataArray (PrimitiveBuffer Triangle V3F))
    mkSM l  = tup2 (l,samplerExp LinearFilter Clamp tex)
      where
        fb  = arrayAccumulate (moments l) clear slot
        tex :: Texture (Exp Obj) DIM2 SingleTex (Regular Float) RG
        tex = Texture (Texture2D (Float RG) n1) (V2 512 512) NoMip [prjFrameBuffer "shadowMap" tix0 fb]

moments :: Exp Obj M44F -> Exp Obj (FrameBuffer N1 (Float,V2F)) -> Exp Obj (PrimitiveBuffer Triangle V3F) -> Exp Obj (FrameBuffer N1 (Float,V2F))
moments lightViewProj' fb buf = accumulate fragCtx PassAll storeDepth rast fb
  where
    fragCtx = AccumulationContext Nothing $ DepthOp Less True:.ColorOp NoBlending (one' :: V2B):.ZT
    rast    = rasterize triangleCtx prims
    prims   = transform vert input
    lightViewProj = use lightViewProj'
    input   = fetch buf

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

vsmTop :: Exp Obj (FrameBuffer N1 (Float,V4F))
vsmTop = arrayAccumulate vsm clear slot
  where
    clear   = frameBuffer (DepthImage n1 1000:.ColorImage n1 (V4 1 0 0 1):.ZT)
    slot    = userInput "3D objects" :: Exp Obj (DataArray (PrimitiveBuffer Triangle (V3F,V3F)))

vsm :: Exp Obj (FrameBuffer N1 (Float,V4F)) -> Exp Obj (PrimitiveBuffer Triangle (V3F,V3F)) -> Exp Obj (FrameBuffer N1 (Float,V4F))
vsm fb buf = accumulate fragCtx PassAll calcLuminance rast fb
  where
    fragCtx = AccumulationContext Nothing $ DepthOp Less True:.ColorOp NoBlending (one' :: V4B):.ZT
    rast    = rasterize triangleCtx prims
    prims   = transform vert input
    input   = fetch buf
    (worldViewProj',scaleU',scaleV') = untup3 $ (userInput "global settings" :: Exp Obj (M44F,Float,Float))
    worldViewProj = use worldViewProj'
    scaleU = use scaleU'
    scaleV = use scaleV'
    lightViewProjs  = use $ arrayMap (\a -> let (f,_) = untup2 a in f) momentsTop
    shadowMaps      = use $ arrayMap (\a -> let (_,s) = untup2 a in s) momentsTop

    trimV4 :: Exp s V4F -> Exp s V3F
    trimV4 v = let V4 x y z _ = unpack' v in pack' $ V3 x y z

    trimM4 :: Exp s M44F -> Exp s M33F
    trimM4 v = let V4 i j k _ = unpack' v in pack' $ V3 (trimV4 i) (trimV4 j) (trimV4 k)
    
    vert :: Exp V (V3F, V3F) -> VertexOut (DataArray V4F, V3F)
    vert attr = VertexOut v4 (floatV 1) (Smooth v4l:.Smooth n:.ZT)
      where
        v4 = worldViewProj @*. snoc p 1
        v4l = arrayMap (\lightViewProj -> lightViewProj @*. snoc p 1) lightViewProjs
        n3 = normalize' (trimM4 worldViewProj @*. n)
        (p,n) = untup2 attr

    calcLuminance :: Exp F (DataArray V4F, V3F) -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    calcLuminance attr = FragmentOutRastDepth $ (arrayAccumulate min' (Const $ one') $ arrayZipWith p_max ls shadowMaps):. ZT
      where
        (ls,n) = untup2 attr
        amb :: Exp F V4F
        amb = Const $ V4 0.1 0.1 0.3 1
        p_max l sampler = pack' (V4 ltr ltg intensity (floatF 1)) @* (variance @/ (variance @+ d @* d))
          where
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

-- NEW stuff
data Obj
data PrimitiveBuffer prim a
data DataArray a = DataArray deriving (Show, Typeable)

instance IsFloating (DataArray V4F)
instance GPU (DataArray V4F)
type instance EltRepr (DataArray V4F) = ((), DataArray V4F)
type instance EltRepr' (DataArray V4F) = (DataArray V4F)

class FrequencyOrder stage where
    use :: Exp Obj a -> Exp stage a

instance FrequencyOrder V where
    use = undefined
instance FrequencyOrder G where
    use = undefined
instance FrequencyOrder F where
    use = undefined

{-
  Summary of changes:
    - introduction of DataArray in all levels (Obj,V,G,F),
        however only from GPU types can by used in V,G,F levels,
        'use' function should check this porperty
    - merge GP and Exp, this let us to express computations in Obj level
    - merge Uniform and ObjectSlot handling, now we call it userInput
-}

-- New operations
samplerExp :: GPU (Sampler dim arr t ar)
        => Filter
        -> EdgeMode
        -> Texture (Exp Obj) dim arr t ar
        -> Exp stage (Sampler dim arr t ar)
samplerExp = undefined

fetch           :: Exp Obj (PrimitiveBuffer prim a)
                -> Exp Obj (VertexStream prim a)
fetch           = undefined

-- scalar or array
userInput       :: ByteString -> Exp Obj a
userInput       = undefined

arrayMap        :: (Exp stage a -> Exp stage b) -> Exp stage (DataArray a) -> Exp stage (DataArray b)
arrayMap        = undefined

arrayZipWith    :: (Exp stage a -> Exp stage b -> Exp stage c) -> Exp stage (DataArray a) -> Exp stage (DataArray b) -> Exp stage (DataArray c)
arrayZipWith    = undefined

arrayAccumulate :: (Exp stage a -> Exp stage b -> Exp stage a) -> Exp stage a -> Exp stage (DataArray b) -> Exp stage a
arrayAccumulate = undefined

-- Old operations
transform       :: (GPU a, GPU b)
                => (Exp V a -> VertexOut b)                       -- vertex shader
                -> Exp Obj (VertexStream prim a)
                -> Exp Obj (PrimitiveStream prim N1 V b)
transform       = undefined

reassemble      :: GeometryShader primIn primOut layerCount a b
                -> Exp Obj (PrimitiveStream primIn N1 V a)
                -> Exp Obj (PrimitiveStream primOut layerCount G b)
reassemble      = undefined

rasterize       :: RasterContext prim
                -> Exp Obj (PrimitiveStream prim layerCount stage a)
                -> Exp Obj (FragmentStream layerCount a)
rasterize       = undefined

frameBuffer     :: FrameBuffer layerCount t
                -> Exp Obj (FrameBuffer layerCount (FTRepr' t))
frameBuffer     = undefined

accumulate      :: (GPU a, GPU (FTRepr' b), IsValidOutput b)    -- restriction: depth and stencil optional, arbitrary color component
                => AccumulationContext b
                -> FragmentFilter a
                -> (Exp F a -> FragmentOut (NoStencilRepr b))     -- fragment shader
                -> Exp Obj (FragmentStream layerCount a)
                -> Exp Obj (FrameBuffer layerCount (FTRepr' b))
                -> Exp Obj (FrameBuffer layerCount (FTRepr' b))
accumulate      = undefined

prjFrameBuffer  :: ByteString                       -- internal image output (can be allocated on request)
                -> TupleIdx (EltRepr b) t
                -> Exp Obj (FrameBuffer layerCount b)
                -> Exp Obj (Image layerCount t)
prjFrameBuffer  = undefined

prjImage        :: (Nat idx, LesserEq idx layerCount)
                => ByteString                       -- internal image output (can be allocated on request)
                -> idx
                -> Exp Obj (Image layerCount t)
                -> Exp Obj (Image N1 t)
prjImage        = undefined

-- dynamic extension support
accumulateSet   :: GPU a
                => ByteString
                -> Exp Obj (FrameBuffer layerCount a)
                -> Exp Obj (FrameBuffer layerCount a)
accumulateSet   = undefined
