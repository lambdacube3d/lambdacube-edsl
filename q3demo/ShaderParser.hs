{-# LANGUAGE OverloadedStrings #-}

module ShaderParser where

import Control.Applicative hiding (many)
import Data.Attoparsec.Char8
import Data.ByteString.Char8 (ByteString)
import Data.Char (toLower)
import Data.List (foldl')
import Data.Vect
import qualified Data.ByteString.Char8 as B
import qualified Data.Trie as T

import Material

-- | Zero or more.
many :: (Alternative f) => f a -> f [a]
many v = many_v
    where many_v = some_v <|> pure []
          some_v = (:) <$> v <*> many_v

-- utility parsers
skipSpace' :: Parser ()
skipSpace' = skipWhile (\c -> elem c " \t")

skip :: Parser ()
skip = skipSpace <* many (comment <* skipSpace)

eol :: Parser ()
eol = choice [string "\r\n" >> return (), satisfy (\c -> c == '\n') >> return (), satisfy (\c -> c == '\r') >> return ()]

skipRest :: Parser ()
skipRest = skipWhile (\c -> notElem c "\n\r{}") <* eol

comment :: Parser ByteString
comment = (stringCI "//" <* skipWhile (\c -> c /= '\n' && c /= '\r')) <|> (string "/*" <* manyTill anyChar (try (string "*/")))

word :: Parser ByteString
word = skipSpace' *> takeTill isSpace

word' :: Parser ByteString
word' = skip *> word

kw :: ByteString -> Parser ()
kw s = ((\w -> if B.map toLower w == s then return () else fail "") =<< word) <?> B.unpack s

kw' :: ByteString -> Parser ()
kw' s = skip *> kw s

val :: a -> ByteString -> Parser a
val v w = const v <$> kw w

float :: Parser Float
float = (\_ c a -> c * read a) <$> skipSpace' <*> option 1 ((const 1 <$> char '+') <|> (const (-1) <$> char '-')) <*>
    ( ((\a _ b -> a ++ "." ++ b) <$> many1 digit <*> char '.' <*> many1 digit) <|>
      ((\_ a -> "0." ++ a) <$> char '.' <*> many1 digit) <|>
      (many1 digit)
    )
    
int :: Parser Int
int = skipSpace' *> decimal

-- q3 entity description parser
entities :: Parser [T.Trie ByteString]
entities = skipSpace *> many entity <* skipSpace

entity :: Parser (T.Trie ByteString)
entity = T.fromList <$> (kw' "{" *> many ((,) <$> str <*> str) <* kw' "}")

str :: Parser ByteString
str = skipSpace *> string "\"" *> takeWhile1 (\c -> c /= '"') <* char '"'

-- q3 shader related parsers

shaders :: Parser [(ByteString,CommonAttrs)]
shaders = skip *> many shader <* skip

shader :: Parser (ByteString,CommonAttrs)
shader = (\n _ l _ -> (bsToLower n,finishShader $ foldl' (\s f -> f s) defaultCommonAttrs l)) <$> word' <*> kw' "{" <*> many shaderAttrs <*> kw' "}"

shaderAttrs :: Parser (CommonAttrs -> CommonAttrs)
shaderAttrs = option id (choice [general, q3map, stage]) <* skipRest

finishShader :: CommonAttrs -> CommonAttrs
finishShader ca = ca
    { caDeformVertexes  = reverse $ caDeformVertexes ca
    , caStages          = fixedStages
    , caSort            = fixedSort
    }
  where
    -- fix sort value
    srt0        = caSort ca
    srt1        = if caIsSky ca then 2 else srt0
    srt2        = if caPolygonOffset ca && srt1 == 0 then 4 else srt1
    srt3        = snd $ foldl' fixBlendSort (True,srt2) fixedStages
      where
        fixBlendSort (False,s) _ = (False,s)
        fixBlendSort (True,s) sa = case saBlend sa of
            Nothing -> (False,s)
            _       -> let s1 = if s /= 0 then s else if saDepthWrite sa then 5 else 9 in (True,s1)

    srt4        = if srt3 == 0 then 3 else srt3
    fixedSort   = if null fixedStages then 7 else srt4

    fixedStages = reverse $ map fixStage $ caStages ca
    fixStage sa = sa
        { saTCMod       = reverse $ saTCMod sa
        , saTCGen       = tcGen
        , saRGBGen      = rgbGen
        , saBlend       = blend
        , saDepthWrite  = depthWr
        }
      where
        (depthWr,blend) = case saBlend sa of
            Just (B_One,B_Zero) -> (True,Nothing)
            a                   -> (saDepthWrite sa,a)
        rgbGen = case saRGBGen sa of
            RGB_Undefined   -> case saBlend sa of
                Nothing                 -> RGB_IdentityLighting
                Just (B_One,B_SrcAlpha) -> RGB_IdentityLighting
                _                       -> RGB_Identity
            a               -> a
        tcGen = case saTCGen sa of
            TG_Undefined    -> case saTexture sa of
                ST_Lightmap -> TG_Lightmap
                _           -> TG_Base
            a               -> a

{-
general =
    skyParms
    fogParms
    portal
    sort
    entityMergable
    fogonly
    cull
    deformVertexes
    nopicmip
    nomipmaps
    polygonOffset
-}

{-
stageAttrs =
    mapP        - texture source
    clampMap    - texture source
    animMap     - texture source

    blendFunc   - paint function parameter
    rgbGen
    alphaGen
    alphaFunc
    tcGen       - vertex function
    tcMod       - vertex function
    depthFunc   - paint function parameter
    depthWrite  - paint function parameter
    detail
-}

pass _ a = a

general = choice [cull, deformVertexes, entityMergable, fogParms, fogonly, nomipmaps, nopicmip, polygonOffset, portal, skyParms, sort]

q3map = choice [q3MapSun, surfaceParm, light, lightning, cloudparams, sky, foggen, tessSize]

stage = (\_ fl _ ca -> ca {caStages = (foldl' (\s f -> f s) defaultStageAttrs fl):caStages ca}) <$> kw' "{" <*> many stageAttrs <*> kw' "}"

stageAttrs :: Parser (StageAttrs -> StageAttrs)
stageAttrs = option id (choice [alphaFunc, alphaGen, animMap, blendFunc, clampMap, depthFunc, depthWrite, detail, mapP, rgbGen, tcGen, tcMod]) <* skipRest

-- utility
waveType = val WT_Sin "sin" <|>
           val WT_Triangle "triangle" <|>
           val WT_Square "square" <|>
           val WT_Sawtooth "sawtooth" <|>
           val WT_InverseSawtooth "inversesawtooth" <|>
           val WT_Noise "noise"

wave = Wave <$> waveType <*> float <*> float <*> float <*> float

--
-- General Shader Keywords
--

fogonly = pass <$> kw "fogonly"

{-
skyParms <farbox> <cloudheight> <nearbox>
  <farbox>:
    "-"         - no farbox
    “env/test”  - would look for files “env/test_rt.tga”, “env/test_lf.tga”, “env/test_ft.tga”, “env/test_bk.tga”, “env/test_up.tga”, “env/test_dn.tga”

  <nearbox>:
    “-“ - ignore (This has not been tested in a long time)
-}

skyParms = (\_ ca -> ca {caIsSky = True}) <$> kw "skyparms" <* (kw "-" <|> (const () <$> word)) <* (kw "-" <|> (const () <$> word)) <* kw "-"

cull = (\_ a ca -> ca {caCull = a}) <$> kw "cull" <*> (
    val CT_FrontSided "front"   <|>
    val CT_TwoSided "none"      <|>
    val CT_TwoSided "twosided"  <|>
    val CT_TwoSided "disable"   <|>
    val CT_BackSided "back"     <|>
    val CT_BackSided "backside" <|>
    val CT_BackSided "backsided")

deformVertexes = (\v ca -> ca {caDeformVertexes = v:caDeformVertexes ca}) <$ kw "deformvertexes" <*> (
    val D_AutoSprite "autosprite" <|>
    val D_AutoSprite2 "autosprite2" <|>
    D_Bulge <$ kw "bulge" <*> float <*> float <*> float <|>
    D_Move <$ kw "move" <*> v3 <*> wave <|>
    D_Normal <$ kw "normal" <*> float <*> float <|> -- amplitude, frequency
    val D_ProjectionShadow "projectionshadow" <|>
    val D_Text0 "text0" <|>
    val D_Text1 "text1" <|>
    val D_Text2 "text2" <|>
    val D_Text3 "text3" <|>
    val D_Text4 "text4" <|>
    val D_Text5 "text5" <|>
    val D_Text6 "text6" <|> 
    val D_Text7 "text7" <|>
    (\s w -> D_Wave (if s == 0 then 100 else 1/s) w) <$ kw "wave" <*> float <*> wave
    )
  where
    v3 = Vec3 <$> float <*> float <*> float

fogParms = pass <$> kw "fogparms" <* kw "(" <* float <* float <* float <* kw ")" <* float
nopicmip = pass <$> kw "nopicmip"
nomipmaps = (\_ ca -> ca {caNoMipMaps = True}) <$> kw "nomipmaps"
entityMergable = pass <$> kw "entitymergable"
polygonOffset = (\_ ca -> ca {caPolygonOffset = True}) <$> kw "polygonoffset"
portal = (\_ ca -> ca {caSort = 1}) <$> kw "portal"

-- sort portal|sky|opaque|banner|underwater|additive|nearest|[number]
sort = (\_ i ca -> ca {caSort = i}) <$> kw "sort" <*> (
    val 1  "portal"     <|>
    val 2  "sky"        <|>
    val 3  "opaque"     <|>
    val 4  "decal"      <|>
    val 5  "seethrough" <|>
    val 6  "banner"     <|>
    val 10 "additive"   <|>
    val 16 "nearest"    <|>
    val 8  "underwater" <|>
    int)

--
-- Stage Specific Keywords
--

{-
one stage is one pass
question: can we render in single pass?
answer: yes, but the backend should optimize it. Now we should build multipass rendering.
-}
bsToLower :: ByteString -> ByteString
bsToLower = B.map toLower

mapP = (\_ v sa -> sa {saTexture = v}) <$> kw "map" <*> (
    val ST_Lightmap "$lightmap" <|> 
    val ST_WhiteImage "$whiteimage" <|> 
    ST_Map . bsToLower <$> word
    )

clampMap = (\v sa -> sa {saTexture = ST_ClampMap $ bsToLower v}) <$> (kw "clampmap" *> word)

animMap = (\_ f v sa -> sa {saTexture = ST_AnimMap f (map bsToLower v)}) <$> kw "animmap" <*> float <*> (B.words <$> takeTill fun)--many1 (skipWhile fun *> takeTill fun) -- FIXME: comment is not supported!
  where
    fun c = c == '\n' || c == '\r'

blendFuncFunc = val (B_One,B_One) "add"
            <|> val (B_DstColor,B_Zero) "filter"
            <|> val (B_SrcAlpha,B_OneMinusSrcAlpha) "blend"

srcBlend = val B_One "gl_one"
       <|> val B_Zero "gl_zero"
       <|> val B_DstColor "gl_dst_color"
       <|> val B_OneMinusDstColor "gl_one_minus_dst_color"
       <|> val B_SrcAlpha "gl_src_alpha"
       <|> val B_OneMinusSrcAlpha "gl_one_minus_src_alpha"
       <|> val B_DstAlpha "gl_dst_alpha"
       <|> val B_OneMinusDstAlpha "gl_one_minus_dst_alpha"
       <|> val B_SrcAlphaSaturate "gl_src_alpha_saturate"

dstBlend = val B_One "gl_one"
       <|> val B_Zero "gl_zero"
       <|> val B_SrcAlpha "gl_src_alpha"
       <|> val B_OneMinusSrcAlpha "gl_one_minus_src_alpha"
       <|> val B_DstAlpha "gl_dst_alpha"
       <|> val B_OneMinusDstAlpha "gl_one_minus_dst_alpha"
       <|> val B_SrcColor "gl_src_color"
       <|> val B_OneMinusSrcColor "gl_one_minus_src_color"

blendFunc = (\_ b sa -> sa {saBlend = Just b, saDepthWrite = dpWr sa}) <$> kw "blendfunc" <*> choice [blendFuncFunc, (,) <$> srcBlend <*> dstBlend]
  where
    dpWr sa = if saDepthMaskExplicit sa then saDepthWrite sa else False

rgbGen = (\_ v sa -> sa {saRGBGen = v, saAlphaGen = alpha sa v}) <$> kw "rgbgen" <*> (
    RGB_Wave <$ kw "wave" <*> wave <|>
    RGB_Const <$ kw "const" <* kw "(" <*> float <*> float <*> float <* kw ")" <|>
    val RGB_Identity "identity" <|> 
    val RGB_IdentityLighting "identitylighting" <|> 
    val RGB_Entity "entity" <|>
    val RGB_OneMinusEntity "oneminusentity" <|>
    val RGB_ExactVertex "exactvertex" <|> 
    val RGB_Vertex "vertex" <|> 
    val RGB_LightingDiffuse "lightingdiffuse" <|>
    val RGB_OneMinusVertex "oneminusvertex"
    )
  where
    alpha sa v = case v of
        RGB_Vertex  -> case saAlphaGen sa of
            A_Identity  -> A_Vertex
            _           -> saAlphaGen sa
        _           -> saAlphaGen sa

alphaGen = (\_ v sa -> sa {saAlphaGen = v}) <$> kw "alphagen" <*> (
    A_Wave <$ kw "wave" <*> wave <|>
    A_Const <$ kw "const" <*> float <|>
    val A_Portal "portal" <* float <|>
    val A_Identity "identity" <|> 
    val A_Entity "entity" <|>
    val A_OneMinusEntity "oneminusentity" <|>
    val A_Vertex "vertex" <|>
    val A_LightingSpecular "lightingspecular" <|>
    val A_OneMinusVertex "oneminusvertex"
    )

tcGen = (\_ v sa -> sa {saTCGen = v}) <$> (kw "texgen" <|> kw "tcgen") <*> (
    val TG_Environment "environment" <|>
    val TG_Lightmap "lightmap" <|>
    val TG_Base "texture" <|>
    val TG_Base "base" <|> 
    TG_Vector <$ kw "vector" <*> v3 <*> v3)
  where
    v3 = (\_ x y z _ -> Vec3 x y z) <$> kw "(" <*> float <*> float <*> float <*> kw ")"

tcMod = (\_ v sa -> sa {saTCMod = v:saTCMod sa}) <$> kw "tcmod" <*> (
    val TM_EntityTranslate "entitytranslate" <|>
    TM_Rotate <$ kw "rotate" <*> float <|>
    TM_Scroll <$ kw "scroll" <*> float <*> float <|>
    TM_Scale <$ kw "scale" <*> float <*> float <|>
    TM_Stretch <$ kw "stretch" <*> wave <|>
    TM_Transform <$ kw "transform" <*> float <*> float <*> float <*> float <*> float <*> float <|>
    TM_Turb <$ kw "turb" <*> float <*> float <*> float <*> float
    )

depthFunc = (\_ v sa -> sa {saDepthFunc = v}) <$> kw "depthfunc" <*> (val D_Lequal "lequal" <|> val D_Equal "equal")
depthWrite = (\_ sa -> sa {saDepthWrite = True, saDepthMaskExplicit = True}) <$> kw "depthwrite"
detail = pass <$> kw "detail"
alphaFunc = (\_ v sa -> sa {saAlphaFunc = Just v}) <$> kw "alphafunc" <*> (val A_Gt0 "gt0" <|> val A_Lt128 "lt128" <|> val A_Ge128 "ge128")

--
-- Q3MAP Specific Shader Keywords
--
cloudparams = pass <$> kw "cloudparms"
lightning = pass <$> kw "lightning"
light = pass <$> (kw "light" <|> kw "light1")
sky = pass <$> kw "sky"
foggen = pass <$> kw "foggen"

tessSize = pass <$> kw "tesssize" <* float

-- q3map_sun <red> <green> <blue> <intensity> <degrees> <elevation>
q3MapSun = pass <$> kw "q3map_sun" <* float <* float <* float <* float <* float <* float

surfaceParm = pass <$> kw "surfaceparm" <* (
        kw "water"      <|> kw "slime"      <|> kw "lava"           <|> kw "playerclip" <|> kw "monsterclip"
    <|> kw "nodrop"     <|> kw "nonsolid"   <|> kw "origin"         <|> kw "trans"      <|> kw "detail"
    <|> kw "structural" <|> kw "areaportal" <|> kw "clusterportal"  <|> kw "donotenter" <|> kw "fog"
    <|> kw "sky"        <|> kw "lightfilter"<|> kw "alphashadow"    <|> kw "hint"       <|> kw "botclip"
    <|> kw "slick"      <|> kw "noimpact"   <|> kw "nomarks"        <|> kw "ladder"     <|> kw "nodamage"
    <|> kw "metalsteps" <|> kw "flesh"      <|> kw "nosteps"        <|> kw "nodraw"     <|> kw "antiportal"
    <|> kw "pointlight" <|> kw "nolightmap" <|> kw "nodlight"       <|> kw "dust"       <|> kw "lightgrid"
    <|> kw "nopicmip"   <|> kw "nomipmaps")
