{-# LANGUAGE TypeOperators, OverloadedStrings #-}
module Graphics where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as SB
import Data.List
import Data.Digest.CRC32
import Data.Maybe

import LC_API

import Material hiding (Blending)


-- specialized snoc
v3v4 :: Exp s V3F -> Exp s V4F
v3v4 v = let V3 x y z = unpack' v in pack' $ V4 x y z (Const 1)

v4v3 :: Exp s V4F -> Exp s V3F
v4v3 v = let V4 x y z _ = unpack' v in pack' $ V3 x y z

-- specialized snoc
snoc :: Exp s V3F -> Float -> Exp s V4F
snoc v s = let V3 x y z = unpack' v in pack' $ V4 x y z (Const s)

snoc' :: Exp s V3F -> Exp s Float -> Exp s V4F
snoc' v s = let V3 x y z = unpack' v in pack' $ V4 x y z s

drop4 :: Exp s V4F -> Exp s V3F
drop4 v = let V4 x y z _ = unpack' v in pack' $ V3 x y z

drop3 :: Exp s V3F -> Exp s V2F
drop3 v = let V3 x y _ = unpack' v in pack' $ V2 x y


-- TODO: requires texture support
deform :: Deform -> Exp V a -> Exp V a
deform = undefined

mkRasterContext :: CommonAttrs -> RasterContext Triangle
mkRasterContext ca = TriangleCtx cull PolygonFill offset LastVertex
  where
    offset  = if caPolygonOffset ca then Offset (-1) (-2) else NoOffset
    cull    = case caCull ca of
        CT_FrontSided   -> CullFront CCW
        CT_BackSided    -> CullBack CCW
        CT_TwoSided     -> CullNone

--mkAccumulationContext :: StageAttrs -> (FragmentOperation (Depth Float)):+:(FragmentOperation (Color V4F)):+:ZZ
mkAccumulationContext sa = DepthOp depthFunc depthWrite:.ColorOp blend (one' :: V4B):.ZT
  where
    depthWrite  = saDepthWrite sa
    depthFunc   = case saDepthFunc sa of
        D_Equal     -> Equal
        D_Lequal    -> Lequal
    blend       = case saBlend sa of
        Nothing     -> NoBlending
        Just (src,dst)  -> Blend (FuncAdd,FuncAdd) ((srcF,dstF),(srcF,dstF)) zero'
          where
            srcF    = cvt src
            dstF    = cvt dst
    cvt b = case b of
        B_DstAlpha          -> DstAlpha
        B_DstColor          -> DstColor
        B_One               -> One
        B_OneMinusDstAlpha  -> OneMinusDstAlpha
        B_OneMinusDstColor  -> OneMinusDstColor
        B_OneMinusSrcAlpha  -> OneMinusSrcAlpha
        B_OneMinusSrcColor  -> OneMinusSrcColor
        B_SrcAlpha          -> SrcAlpha
        B_SrcAlphaSaturate  -> SrcAlphaSaturate
        B_SrcColor          -> SrcColor
        B_Zero              -> Zero

{-
data WaveType
    = WT_Sin
    | WT_Triangle
    | WT_Square
    | WT_Sawtooth
    | WT_InverseSawtooth
    | WT_Noise

data Wave = Wave !WaveType !Float !Float !Float !Float

data Deform
    = D_AutoSprite
    | D_AutoSprite2
    | D_Bulge !Float !Float !Float
    | D_Move !Vec3 !Wave
    | D_Normal !Float !Float
    | D_ProjectionShadow
    | D_Text0
    | D_Text1
    | D_Text2
    | D_Text3
    | D_Text4
    | D_Text5
    | D_Text6
    | D_Text7
    | D_Wave !Float !Wave

data CommonAttrs
    = CommonAttrs
    { caSkyParms        :: !()
    , caFogParms        :: !()
    , caPortal          :: !Bool
    , caSort            :: !Int             -- done
    , caEntityMergable  :: !Bool
    , caFogOnly         :: !Bool
    , caCull            :: !CullType
    , caDeformVertexes  :: ![Deform]
    , caNoMipMaps       :: !Bool
    , caPolygonOffset   :: !Bool            -- done
    , caStages          :: ![StageAttrs]
    }

data StageAttrs
    = StageAttrs
    { saBlend       :: !(Maybe (Blending,Blending))
    , saRGBGen      :: !RGBGen
    , saAlphaGen    :: !AlphaGen
    , saTCGen       :: !TCGen
    , saTCMod       :: ![TCMod]
    , saTexture     :: !StageTexture
    , saDepthWrite  :: !Bool
    , saDepthFunc   :: !DepthFunction
    , saAlphaFunc   :: !(Maybe AlphaFunction)
    }
-}

v3V :: V3F -> Exp V V3F
v3V = Const

floatV :: Float -> Exp V Float
floatV = Const

floatF :: Float -> Exp F Float
floatF = Const

mkColor :: CommonAttrs -> StageAttrs -> Exp V V4F -> Exp V V4F
mkColor ca sa rgbaV = snoc' rgb alpha
  where
    entityRGB       = Uni (IV3F "entityRGB") :: Exp V V3F
    entityAlpha     = Uni (IFloat "entityAlpha") :: Exp V Float
    identityLight   = Uni (IFloat "identityLight") :: Exp V Float
    red             = Const $ V3 1 0 0
    V4 rV gV bV aV  = unpack' rgbaV
    rgb = case saRGBGen sa of
        RGB_Wave _              -> red -- TODO
        RGB_Const r g b         -> v3V $ V3 r g b
        RGB_Identity            -> v3V one'
        RGB_IdentityLighting    -> pack' $ V3 identityLight identityLight identityLight
        RGB_Entity              -> entityRGB
        RGB_OneMinusEntity      -> v3V one' @- entityRGB
        RGB_ExactVertex         -> pack' $ V3 rV gV bV
        RGB_Vertex              -> (pack' $ V3 rV gV bV) @* identityLight
        RGB_LightingDiffuse     -> red -- TODO
        RGB_OneMinusVertex      -> v3V one' @- ((pack' $ V3 rV gV bV) @* identityLight)

    alpha = case saAlphaGen sa of
        A_Wave _            -> floatV 1 -- TODO
        A_Const a           -> floatV a
        A_Portal            -> floatV 1 -- TODO
        A_Identity          -> floatV 1
        A_Entity            -> entityAlpha
        A_OneMinusEntity    -> floatV 1 @- entityAlpha
        A_Vertex            -> aV
        A_LightingSpecular  -> floatV 1 -- TODO
        A_OneMinusVertex    -> floatV 1 @- aV

mkTexCoord :: StageAttrs -> Exp V V2F -> Exp V V2F -> Exp V V2F
mkTexCoord sa uvD uvL = uv
  where
    -- TODO: TCMod
    uv = case saTCGen sa of
        TG_Base         -> uvD
        TG_Lightmap     -> uvL
        TG_Environment  -> Const zero' -- TODO
        TG_Vector _ _   -> Const zero' -- TODO

mkVertexShader :: CommonAttrs -> StageAttrs -> Exp V (V3F,V3F,V2F,V2F,V4F) -> VertexOut (V2F,V4F)
mkVertexShader ca sa pndlc = VertexOut screenPos (Const 1) (Smooth uv:.Smooth color:.ZT)
  where
    worldViewProj   = Uni (IM44F "worldViewProj")
    (p,n,d,l,c)     = untup5 pndlc
    screenPos       = worldViewProj @*. snoc pos 1
    pos             = p -- TODO: deform
    uv              = mkTexCoord sa d l
    color           = mkColor ca sa c

mkFragmentShader :: StageAttrs -> Exp F (V2F,V4F) -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
mkFragmentShader sa uvrgba = FragmentOutRastDepth $ color :. ZT
  where
    (uv,rgba)   = untup2 uvrgba
    stageTex    = saTexture sa
    stageTexN   = SB.pack $ "Tex_" ++ show (crc32 $ SB.pack $ show stageTex)
    color       = case stageTex of
        ST_WhiteImage   -> rgba
        ST_Lightmap     -> rgba @* texColor Clamp "LightMap"
        ST_Map {}       -> rgba @* texColor Wrap  stageTexN
        ST_ClampMap {}  -> rgba @* texColor Clamp stageTexN
        ST_AnimMap {}   -> rgba @* texColor Wrap  stageTexN
    texColor em name = texture' sampler uv (Const 0)
      where
        rgbaTex     = texture' sampler uv (Const 0)
        sampler     = Sampler LinearFilter em $ TextureSlot name (Texture2D (Float RGBA) n1)

mkFilterFunction :: StageAttrs -> FragmentFilter (V2F,V4F)
mkFilterFunction sa = case saAlphaFunc sa of
    Nothing -> PassAll
    Just f  -> Filter $ \uvrgba ->
        let
            (uv,rgba)   = untup2 uvrgba
            V4 _ _ _ a  = unpack' rgba
        in case f of
            A_Gt0   -> a @> floatF 0
            A_Lt128 -> a @< floatF 0.5
            A_Ge128 -> a @>= floatF 0.5

mkStage :: ByteString -> CommonAttrs -> GP (FrameBuffer N1 (Float,V4F)) -> StageAttrs -> GP (FrameBuffer N1 (Float,V4F))
mkStage name ca prevFB sa = Accumulate aCtx fFun fSh (Rasterize rCtx (Transform vSh input)) prevFB
  where
    input   = Fetch name Triangle (IV3F "position", IV3F "normal", IV2F "diffuseUV", IV2F "lightmapUV", IV4F "color")
    rCtx    = mkRasterContext ca
    aCtx    = mkAccumulationContext sa
    vSh     = mkVertexShader ca sa
    fSh     = mkFragmentShader sa
    fFun    = mkFilterFunction sa

mkShader :: GP (FrameBuffer N1 (Float,V4F)) -> (ByteString,CommonAttrs) -> GP (FrameBuffer N1 (Float,V4F))
mkShader fb (name,ca) = foldl' (mkStage name ca) fb $ caStages ca

{-
mkShader' :: GP (FrameBuffer N1 (Float,V4F)) -> (ByteString,CommonAttrs) -> GP (FrameBuffer N1 (Float,V4F))
mkShader' fb (name,ca) = Accumulate fragCtx PassAll frag rast fb
  where
    cull    = CullNone
    offset  = if caPolygonOffset ca then Offset (-1) (-2) else NoOffset
    fragCtx = DepthOp Less True:.ColorOp NoBlending (one' :: V4B):.ZT
    rastCtx = TriangleCtx cull PolygonFill offset LastVertex
    rast    = Rasterize rastCtx prims
    prims   = Transform vert input
    input   = Fetch name Triangle (IV3F "position", IV3F "normal", IV2F "diffuseUV", IV2F "lightmapUV", IV4F "color")
    worldViewProj = Uni (IM44F "worldViewProj")

    vert :: Exp V (V3F,V3F,V2F,V2F,V4F) -> VertexOut V4F
    vert pndlc = VertexOut v4 (Const 1) (Smooth c:.ZT)
      where
        v4    = worldViewProj @*. snoc p 1
        (p,n,d,l,c) = untup5 pndlc

    frag :: Exp F V4F -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    frag a = FragmentOutRastDepth $ a :. ZT
-}

q3GFX :: [(ByteString,CommonAttrs)] -> GP (FrameBuffer N1 (Float,V4F))
q3GFX shl = {-errorShader $ -} foldl' mkShader clear ordered
  where
    ordered = sortBy (\(_,a) (_,b) -> caSort a `compare` caSort b) shl
    clear   = FrameBuffer (DepthImage n1 1000:.ColorImage n1 (zero'::V4F):.ZT)

errorShader :: GP (FrameBuffer N1 (Float,V4F)) -> GP (FrameBuffer N1 (Float,V4F))
errorShader fb = Accumulate fragCtx PassAll frag rast $ errorShaderFill fb
  where
    offset  = NoOffset--Offset (0) (-10)
    fragCtx = DepthOp Lequal True:.ColorOp NoBlending (one' :: V4B):.ZT
    rastCtx = TriangleCtx CullNone (PolygonLine 2) offset LastVertex
    rast    = Rasterize rastCtx prims
    prims   = Transform vert input
    input   = Fetch "missing shader" Triangle (IV3F "position", IV4F "color")
    worldViewProj = Uni (IM44F "worldViewProj")

    vert :: Exp V (V3F,V4F) -> VertexOut V4F
    vert pc = VertexOut v4 (Const 1) (Smooth c:.ZT)
      where
        v4    = worldViewProj @*. snoc p 1
        (p,c) = untup2 pc

    frag :: Exp F V4F -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    frag v = FragmentOutRastDepth $ (v @* (Const (V4 3 3 3 1) :: Exp F V4F)) :. ZT
      where
        V4 r g b a = unpack' v

errorShaderFill :: GP (FrameBuffer N1 (Float,V4F)) -> GP (FrameBuffer N1 (Float,V4F))
errorShaderFill fb = Accumulate fragCtx PassAll frag rast fb
  where
    blend   = NoBlending
    fragCtx = DepthOp Less True:.ColorOp blend (one' :: V4B):.ZT
    rastCtx = TriangleCtx CullNone PolygonFill NoOffset LastVertex
    rast    = Rasterize rastCtx prims
    prims   = Transform vert input
    input   = Fetch "missing shader" Triangle (IV3F "position", IV4F "color")
    worldViewProj = Uni (IM44F "worldViewProj")

    vert :: Exp V (V3F,V4F) -> VertexOut V4F
    vert pc = VertexOut v4 (Const 1) (Smooth c:.ZT)
      where
        v4    = worldViewProj @*. snoc p 1
        (p,c) = untup2 pc

    frag :: Exp F V4F -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    frag v = FragmentOutRastDepth $ v :. ZT

{-
identity - RGBA 1 1 1 1
identity_lighting (identity_light_byte = ilb) - RGBA ilb ilb ilb ilb
lighting_diffuse - ??? check: RB_CalcDiffuseColor
exact_vertex - vertex color
const - constant color
vertex (identity_light = il, vertex_color*il) - RGBA (r*il) (g*il) (b*il) a
one_minus_vertex = (identity_light = il, vertex_color*il) - RGBA ((1-r)*il) ((1-g)*il) ((1-b)*il) a
fog - fog color
waveform (c = clamp 0 1 (wave_value * identity_light)) - RGBA c c c 1
entity - entity's shaderRGB
one_minus_entity - 1 - entity's shaderRGB
-}
{-
rgbExactVertex _ (_,_,_,r:.g:.b:._:.()) = r:.g:.b:.()
rgbVertex _ (_,_,_,r:.g:.b:._:.()) = f r:.f g:.f b:.()
  where
    f a = toGPU identityLight * a
rgbOneMinusVertex _ (_,_,_,r:.g:.b:._:.()) = f r:.f g:.f b:.()
  where
    f a = 1 - toGPU identityLight * a
-- time vertexData
--convRGBGen :: RGBGen -> 
convRGBGen a = case a of
    RGB_Const r g b      -> Const $ V3 r g b
    RGB_Identity         -> Const $ V3 1 1 1
    RGB_IdentityLighting -> Const $ V3 identityLight identityLight identityLight
    RGB_ExactVertex      -> rgbExactVertex
    RGB_Vertex           -> rgbVertex
    RGB_OneMinusVertex   -> rgbOneMinusVertex
    _   -> rgbIdentity
--    RGB_Wave w          
--    RGB_Entity
--    RGB_OneMinusEntity
--    RGB_LightingDiffuse
-}