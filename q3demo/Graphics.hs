{-# LANGUAGE TypeOperators, OverloadedStrings #-}
module Graphics where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as SB
import Data.List
import Data.Digest.CRC32
import Data.Maybe
import Data.Vect
import LC_API

import Material hiding (Blending)

import Debug.Trace

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

mkRasterContext :: CommonAttrs -> RasterContext Triangle
mkRasterContext ca = TriangleCtx cull PolygonFill offset LastVertex
  where
    offset  = if caPolygonOffset ca then Offset (-1) (-2) else NoOffset
    cull    = case caCull ca of
        CT_FrontSided   -> CullFront CCW
        CT_BackSided    -> CullBack CCW
        CT_TwoSided     -> CullNone

--mkAccumulationContext :: StageAttrs -> (FragmentOperation (Depth Float)):+:(FragmentOperation (Color V4F)):+:ZZ
mkAccumulationContext sa = AccumulationContext Nothing $ DepthOp depthFunc depthWrite:.ColorOp blend (one' :: V4B):.ZT
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
    --red             = Const $ V3 1 0 0
    green           = Const $ V3 0 1 0
    V4 rV gV bV aV  = unpack' rgbaV
    rgb = case saRGBGen sa of
        RGB_Wave w              -> let c = mkWave w in pack' $ V3 c c c
        RGB_Const r g b         -> v3V $ V3 r g b
        RGB_Identity            -> v3V one'
        RGB_IdentityLighting    -> pack' $ V3 identityLight identityLight identityLight
        RGB_Entity              -> entityRGB
        RGB_OneMinusEntity      -> v3V one' @- entityRGB
        RGB_ExactVertex         -> pack' $ V3 rV gV bV
        RGB_Vertex              -> (pack' $ V3 rV gV bV) @* identityLight
        RGB_LightingDiffuse     -> green -- TODO
        {-  input:
                entity: ambientLight
                        directedLight
                        lightDir
                model:  position
                        normal
        -}
        RGB_OneMinusVertex      -> v3V one' @- ((pack' $ V3 rV gV bV) @* identityLight)

    alpha = case saAlphaGen sa of
        A_Wave w            -> mkWave w
        A_Const a           -> floatV a
        A_Portal            -> floatV 1 -- TODO
        A_Identity          -> floatV 1
        A_Entity            -> entityAlpha
        A_OneMinusEntity    -> floatV 1 @- entityAlpha
        A_Vertex            -> aV
        A_LightingSpecular  -> floatV 1 -- TODO
        {-  input:
                model:  position
                        normal
                user:   viewOrigin
        -}
        A_OneMinusVertex    -> floatV 1 @- aV

mkWave' :: Exp V Float -> Wave -> Exp V Float
mkWave' off (Wave wFunc base amplitude phase freq) = floatV base @+ a @* floatV amplitude
  where
    time        = Uni (IFloat "time") :: Exp V Float
    u           = off @+ floatV phase @+ floatV freq @* time
    uv          = pack' $ V2 u (Const 0)
    V4 v _ _ _  = unpack' $ texture' sampler uv (Const 0)
    a           = (v @- floatV 0.5) @* floatV 2
    sampler     = Sampler LinearFilter Wrap $ TextureSlot name (Texture2D (Float RGBA) n1)
    name        = case wFunc of
        WT_Sin              -> "SinTable"
        WT_Triangle         -> "TriangleTable"
        WT_Square           -> "SquareTable"
        WT_Sawtooth         -> "SawToothTable"
        WT_InverseSawtooth  -> "InverseSawToothTable"
        WT_Noise            -> "Noise"

mkWave :: Wave -> Exp V Float
mkWave = mkWave' $ floatV 0
{-
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
-}
mkDeform :: Exp V V2F -> Exp V V3F -> Exp V V3F -> Deform -> Exp V V3F
mkDeform uv normal pos d = case d of
    D_Move (Vec3 x y z) w   -> pos @+ v3V (V3 x z (-y)) @* mkWave w
    D_Wave spread w@(Wave _ _ _ _ f)
        | f < 0.000001  -> pos @+ normal @* mkWave w
        | otherwise     ->
            let V3 x y z    = unpack' pos
                off         = (x @+ y @+ z) @* floatV spread
            in pos @+ normal @* mkWave' off w
    D_Bulge w h s   -> let time     = Uni (IFloat "time") :: Exp V Float
                           V2 u _   = unpack' uv
                           off      = u @* floatV w @+ time @* floatV s
                       in pos @+ normal @* sin' off @* floatV h
    _ -> pos

{-
data TCMod
    = TM_EntityTranslate
-}
mkTCMod :: Exp V V3F -> Exp V V2F -> TCMod -> Exp V V2F
mkTCMod pos uv m = case m of
    TM_Scroll su sv -> uv @+ (Const $ V2 su sv :: Exp V V2F) @* (Uni (IFloat "time") :: Exp V Float)
    TM_Scale su sv  -> uv @* (Const $ V2 su sv :: Exp V V2F)
    TM_Stretch w    -> let p    = floatV 8 @/ mkWave w 
                           v0_5 = floatV 0.5
                           off  = v0_5 @- v0_5 @* p
                       in uv @* p @+ pack' (V2 off off)
    TM_Rotate speed -> let time = Uni (IFloat "time") :: Exp V Float
                           fi   = floatV (-speed) @* time
                           s    = sin' fi
                           ms   = s @* floatV (-1)
                           c    = cos' fi
                           mA   = pack' $ V2 c s
                           mB   = pack' $ V2 ms c
                           m    = pack' $ V2 mA mB :: Exp V M22F
                           v0_5 = floatV 0.5
                           off  = pack' $ V2 (v0_5 @- v0_5 @* c @+ v0_5 @* s) (v0_5 @- v0_5 @* s @- v0_5 @* c)
                       in m @*. uv @+ off
    TM_Transform m00 m01 m10 m11 t0 t1  -> let V2 u v   = unpack' uv
                                               u'       = u @* floatV m00 @+ v @* floatV m10 @+ floatV t0
                                               v'       = u @* floatV m01 @+ v @* floatV m11 @+ floatV t1
                                           in pack' $ V2 u' v'
    TM_Turb base amp phase freq ->  let V2 u v      = unpack' uv
                                        V3 x y z    = unpack' pos
                                        time        = Uni (IFloat "time") :: Exp V Float
                                        now         = floatV phase @+ time @* floatV freq
                                        offU        = (x @- y) @* floatV (0.125 / 128) @+ now
                                        offV        = z @* floatV (0.125 / 128) @+ now
                                    in uv @+ sin' (pack' $ V2 offU offV) @* floatV amp
    _ -> uv

mkTexCoord :: Exp V V3F -> Exp V V3F -> StageAttrs -> Exp V V2F -> Exp V V2F -> Exp V V2F
mkTexCoord pos normal sa uvD uvL = foldl' (mkTCMod pos) uv $ saTCMod sa
  where
    uv = case saTCGen sa of
        TG_Base         -> uvD
        TG_Lightmap     -> uvL
        TG_Environment  ->  let viewOrigin  = Uni (IV3F "viewOrigin")
                                viewer      = viewOrigin @- pos
                                d           = normal @. viewer
                                reflected   = normal @* d @* floatV 2 @- viewer
                                V3 _ y z    = unpack' reflected
                                v0_5        = floatV 0.5
--                            in pack' $ V2 (v0_5 @+ y @* v0_5) (v0_5 @- z @* v0_5)
                            in pack' $ V2 (v0_5 @+ z @* v0_5) (v0_5 @+ y @* v0_5)
        TG_Vector (Vec3 sx sy sz) (Vec3 tx ty tz)   -> let s    = Const $ V3 sx sz (-sy) :: Exp V V3F
                                                           t    = Const $ V3 tx tz (-ty) :: Exp V V3F
                                                       in pack' $ V2 (pos @. s) (pos @. t)

mkVertexShader :: CommonAttrs -> StageAttrs -> Exp V (V3F,V3F,V2F,V2F,V4F) -> VertexOut (V2F,V4F)
mkVertexShader ca sa pndlc = VertexOut screenPos (Const 1) (Smooth uv:.Smooth color:.ZT)
  where
    worldViewProj   = Uni (IM44F "worldViewProj")
    (p,n,d,l,c)     = untup5 pndlc
    screenPos       = worldViewProj @*. snoc pos 1
    pos             = foldl' (mkDeform d n) p $ caDeformVertexes ca
    uv              = mkTexCoord pos n sa d l
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
        sampler     = Sampler LinearFilter em $ TextureSlot name (Texture2D (Float RGBA) n1)

mkFilterFunction :: StageAttrs -> FragmentFilter (V2F,V4F)
mkFilterFunction sa = case saAlphaFunc sa of
    Nothing -> PassAll
    Just f  -> Filter $ \uvrgba ->
        let
            V4 _ _ _ a  = unpack' color
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
                sampler     = Sampler LinearFilter em $ TextureSlot name (Texture2D (Float RGBA) n1)
        in case trace ("aplha filter: " ++ show f) f of
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

q3GFX :: [(ByteString,CommonAttrs)] -> GP (FrameBuffer N1 (Float,V4F))
q3GFX shl = errorShader $ foldl' mkShader clear ordered
  where
    ordered = sortBy (\(_,a) (_,b) -> caSort a `compare` caSort b) shl
    clear   = FrameBuffer (DepthImage n1 1000:.ColorImage n1 (zero'::V4F):.ZT)

errorShader :: GP (FrameBuffer N1 (Float,V4F)) -> GP (FrameBuffer N1 (Float,V4F))
errorShader fb = Accumulate fragCtx PassAll frag rast $ errorShaderFill fb
  where
    offset  = NoOffset--Offset (0) (-10)
    fragCtx = AccumulationContext Nothing $ DepthOp Lequal True:.ColorOp NoBlending (one' :: V4B):.ZT
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
    fragCtx = AccumulationContext Nothing $ DepthOp Less True:.ColorOp blend (one' :: V4B):.ZT
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
