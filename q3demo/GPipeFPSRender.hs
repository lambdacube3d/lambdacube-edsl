module GPipeFPSRender where

import Control.Monad
import Data.List
import Data.Trie (Trie)
import Data.Vec.LinAlg.Transform3D
import Data.Vec.Nat
import Foreign
import Graphics.GPipe
import System.Directory
import System.FilePath.Posix
import qualified Data.ByteString.Char8 as SB
import qualified Data.Trie as T
import qualified Data.Vec as Vec
import qualified Data.Vect as Vect
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Storable as SV

import BSPLoader
import GPipeFPSMaterial
import GPipeUtils

type VertexData = (Vec.Vec3 (Vertex Float),{-Vec.Vec3 (Vertex Float), -}Vec.Vec2 (Vertex Float),Vec.Vec2 (Vertex Float),Vec.Vec4 (Vertex Float))
type Mesh = PrimitiveStream Triangle VertexData
type FB = FrameBuffer RGBAFormat DepthFormat ()

-- time -> worldProjection -> inFrameBuffer -> resultFrameBuffer
type SurfaceRenderer = Float -> Vertex Float -> Vec.Mat44 (Vertex Float) -> FB -> FB
type Renderer = Texture2D RGBAFormat -> Mesh -> SurfaceRenderer

type RGBFun    = Vertex Float -> VertexData -> Vec.Vec3 (Vertex Float)
type AlphaFun  = Vertex Float -> VertexData -> Vertex Float
type TCFun     = Vertex Float -> VertexData -> Vec.Vec2 (Vertex Float)
type TexFun    = Texture2D RGBAFormat -> Float -> Texture2D RGBAFormat
type SampleFun = Texture2D RGBAFormat -> Vec.Vec2 (Fragment Float) -> Color RGBAFormat (Fragment Float)

type VertexDeformer = Vertex Float -> Vec.Vec3 (Vertex Float) -> Vec.Vec3 (Vertex Float)


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

rgbExactVertex _ (_,_,_,r:.g:.b:._:.()) = r:.g:.b:.()
rgbIdentity _ _ = toGPU (1:.1:.1:.())
rgbIdentityLighting _ _ = toGPU (identityLight:.identityLight:.identityLight:.())
rgbConst r g b _ _ = toGPU (r:.g:.b:.())
rgbVertex _ (_,_,_,r:.g:.b:._:.()) = f r:.f g:.f b:.()
  where
    f a = toGPU identityLight * a
rgbOneMinusVertex _ (_,_,_,r:.g:.b:._:.()) = f r:.f g:.f b:.()
  where
    f a = 1 - toGPU identityLight * a

convRGBGen a = case a of
--    RGB_Wave w          
    RGB_Const r g b      -> rgbConst r g b
    RGB_Identity         -> rgbIdentity
    RGB_IdentityLighting -> rgbIdentityLighting
--    RGB_Entity
--    RGB_OneMinusEntity
    RGB_ExactVertex      -> rgbExactVertex
    RGB_Vertex           -> rgbVertex
--    RGB_LightingDiffuse
    RGB_OneMinusVertex   -> rgbOneMinusVertex
    _   -> rgbIdentity

{-
identity - alpha = 1
const - constant alpha
wave - clamped waveform
lightingspecular - ??? check: RB_CalcSpecularAlpha
entity - entity's shaderRGBA's alpha
oneminusentity - 1 - entity's shaderRGBA's alpha
vertex - vertex alpha
oneminusvertex - 1 - vertex alpha
portal - ???
-}
alphaIdentity _ _ = 1
alphaConst a _ _ = toGPU a
alphaVertex _ (_,_,_,_:._:._:.a:.()) = a
alphaOneMinusVertex _ (_,_,_,_:._:._:.a:.()) = 1 - a

convAlphaGen a = case a of
--    A_Wave w
    A_Const a           -> alphaConst a
--    A_Portal
    A_Identity          -> alphaIdentity
--    A_Entity
--    A_OneMinusEntity
    A_Vertex            -> alphaVertex
--    A_LightingSpecular
    A_OneMinusVertex    -> alphaOneMinusVertex
    _ -> alphaIdentity

tgBase (_,uv,_,_) = uv
tgLightmap (_,_,uv,_) = uv
tgVector u v (p,_,_,_) = (dot p (toGPU u)):.(dot p (toGPU v)):.()

convTCGen a = case a of
    TG_Base         -> tgBase
    TG_Lightmap     -> tgLightmap
--    TG_Environment -- TODO, check: RB_CalcEnvironmentTexCoords
    TG_Vector u v   -> tgVector u v
    _ -> tgBase

tmScroll su sv t (u:.v:.()) = fract' (u+t*toGPU su):.fract' (v+t*toGPU sv):.()
tmScale su sv _ (u:.v:.()) = (u*toGPU su):.(v*toGPU sv):.()

convTCMod a = case a of
    --TM_EntityTranslate
    --TM_Rotate Float
    TM_Scroll u v       -> tmScroll u v
    TM_Scale u v        -> tmScale u v
--    TM_Stretch Wave
--    TM_Transform Float Float Float Float Float Float
--    TM_Turb Float Float Float Float
    _ -> \_ uv -> uv 

shaderRenderer :: CommonAttrs -> (Int,Renderer)
shaderRenderer ca = (caSort ca, \lm obj time' time cWorldProjection fb -> foldl' (\f r -> r lm obj time' time cWorldProjection f) fb $ map (stage ca) $ caStages ca)

stage ca sa = stageRenderer (saDepthFunc sa) depthWrite blend vertexFun rgbGen alphaGen tcFun texFun sampleFun
  where
--    tcGen = undefined
--    tcMod = undefined
    alphaGen = convAlphaGen $ saAlphaGen sa
    rgbGen = convRGBGen $ saRGBGen sa
    mipmap = not $ caNoMipMaps ca
    vertexFun t v = v
    tcFun t vd = foldl' (\uv f -> f t uv) ((convTCGen $ saTCGen sa) vd) (map convTCMod $ saTCMod sa)
    depthWrite = if NoBlending == blend then True else True --saDepthWrite sa
    blend = case saBlend sa of
        Nothing -> NoBlending
        Just b  -> Blend (FuncAdd,FuncAdd) (b,(SrcAlpha,OneMinusSrcAlpha)) (RGBA (0:.0:.0:.()) 1)
    texFun = case saTexture sa of
        ST_Map t        -> \_ _ -> loadQ3Texture mipmap $ SB.unpack t
        ST_ClampMap t   -> \_ _ -> loadQ3Texture mipmap $ SB.unpack t
        ST_AnimMap f l  -> \_ t -> let
            txl = map (loadQ3Texture mipmap . SB.unpack) l
            i = floor $ (fromIntegral $ length l) * fract' (t*f)
            in txl !! i
        ST_Lightmap     -> \lm _ -> lm
        ST_WhiteImage   -> \_ _ -> whiteImage
    sampleFun = case saTexture sa of
        ST_ClampMap _   -> \t uv -> sample (Sampler Linear Clamp) t uv
        ST_WhiteImage   -> \_ _ -> RGBA (1:.1:.1:.()) 1
        _               -> \t uv -> sample (Sampler Linear Wrap) t uv

stageRenderer :: ComparisonFunction -> Bool -> Blending -> VertexDeformer -> RGBFun -> AlphaFun -> TCFun -> TexFun -> SampleFun -> Renderer
stageRenderer depthFun depthWrite blending vertexFun rgbFun alphaFun tcFun texFun sampleFun lmTex obj time' time cWorldProjection fb =
    paintColorRastDepth depthFun depthWrite blending (RGBA (Vec.vec True) True) (rast obj) fb
  where
    rast obj = fmap frag $ rasterizeBack $ fmap vert obj
    vert vd@(v3,_,_,_) = (cWorldProjection `multmv` v4,(rgbFun time vd, alphaFun time vd, tcFun time vd))
      where
        v4 = Vec.snoc (vertexFun time v3) 1
    frag (rgb,a,uv) = RGBA (rgb * rgb') (a * a')
      where
        RGBA rgb' a' = sampleFun (texFun lmTex time') uv

renderSurfaces :: Float -> Vertex Float -> Vec.Mat44 (Vertex Float) -> V.Vector (Int,(Int,SurfaceRenderer)) -> FB
renderSurfaces time' time worldProjection faces = V.foldl' (foldl' (\fb (_,fun) -> fun time' time worldProjection fb)) cleanFB $ batch $ sorted
  where
    maxSort = 256
    cleanFB = newFrameBufferColorDepth (RGBA (0:.0:.0:.()) 1) 1000
    sorted  = V.accumulate (\l e -> e:l) (V.replicate maxSort []) faces
    batch v = V.map (sortBy (\(a,_) (b,_) -> a `compare` b)) v
{-
#define LIGHTMAP_2D			-4		// shader is for 2D rendering
#define LIGHTMAP_BY_VERTEX	-3		// pre-lit triangle models
#define LIGHTMAP_WHITEIMAGE	-2
#define	LIGHTMAP_NONE		-1
-}
imageRenderer lmidx txName = shaderRenderer $ defaultCommonAttrs {caStages = sa:if lmidx < 0 then [] else {-saLM:-}[]}
  where
    sa = defaultStageAttrs
        { saTexture = ST_Map txName
--        , saBlend = Just (SrcColor,Zero)
--        , saBlend = Just (SrcColor,DstColor)
        }
    saLM = defaultStageAttrs
        { saTexture = ST_Lightmap
        , saTCGen = TG_Lightmap
--        , saBlend = Just (SrcColor,One)
        , saBlend = Just (SrcColor,DstColor)
        }

compileBSP :: Trie CommonAttrs -> BSPLevel -> V.Vector (Int,(Int,SurfaceRenderer))
compileBSP shaderMap bsp = V.map convertSurface $ blSurfaces bsp
  where
    lightmaps = V.map (textureFromByteString True 3 128 128 . lmMap) $ blLightmaps bsp
    shaders = V.map (\s -> T.lookup (shName s) shaderMap) $ blShaders bsp
    convertSurface sf = (shidx,(srShaderNum sf,sh (lightmap $ srLightmapNum sf) geom))
      where
        shaderName = shName $ (blShaders bsp) V.! (srShaderNum sf)
        (shidx,sh) = case shaders V.! srShaderNum sf of
            Just s  -> shaderRenderer s
            Nothing -> imageRenderer (srLightmapNum sf) shaderName
        geom :: Mesh
        geom = case srSurfaceType sf of
            Planar       -> toIndexedGPUStream TriangleList v i
            TriangleSoup -> toIndexedGPUStream TriangleList v i
            Patch        -> toGPUStream TriangleList $ concatMap (pointToCube (0:.1:.0:.1:.())) v
            Flare        -> toGPUStream TriangleList $ concatMap (pointToCube (1:.0:.0:.1:.())) v
        v = V.toList $ V.take (srNumVertices sf) $ V.drop (srFirstVertex sf) vertices
        i = V.toList $ V.take (srNumIndices sf) $ V.drop (srFirstIndex sf) indices
        lightmap lidx | 0 <= lidx && lidx < V.length lightmaps = lightmaps V.! lidx
                      | otherwise = whiteImage

    vertices = V.map convertVertex $ blDrawVertices bsp
    indices  = blDrawIndices bsp
    convertVertex (DrawVertex p dt lt n c) = (v3 p,v2 dt,v2 lt,v4 c)
    v2 (Vect.Vec2 i j) = i:.j:.()
    v3 (Vect.Vec3 i j k) = i:.j:.k:.()
    v4 (Vect.Vec4 i j k l) = i:.j:.k:.l:.()

isClusterVisible :: BSPLevel -> Int -> Int -> Bool
isClusterVisible bl a b
    | a >= 0 = 0 /= (visSet .&. (shiftL 1 (b .&. 7)))
    | otherwise = True
  where
    Visibility nvecs szvecs vecs = blVisibility bl
    i = a * szvecs + (shiftR b 3)
    visSet = vecs V.! i

findLeafIdx bl camPos i
    | i >= 0 = if dist >= 0 then findLeafIdx bl camPos f else findLeafIdx bl camPos b
    | otherwise = (-i) - 1
  where 
    node    = blNodes bl V.! i
    (f,b)   = ndChildren node 
    plane   = blPlanes bl V.! ndPlaneNum node
    dist    = plNormal plane `Vect.dotprod` camPos - plDist plane

cullSurfaces :: BSPLevel -> Vect.Vec3 -> Frustum -> V.Vector a -> V.Vector a
cullSurfaces bsp cam frust surfaces = case leafIdx < 0 || leafIdx >= V.length leaves of
    True    -> unsafePerformIO $ print ("findLeafIdx error") >> return surfaces
    False   -> unsafePerformIO $ print ("findLeafIdx ok",leafIdx,camCluster) >> return (V.ifilter (\i _ -> surfaceMask V.! i) surfaces)
  where
    leafIdx = findLeafIdx bsp cam 0
    leaves = blLeaves bsp
    camCluster = lfCluster $ leaves V.! leafIdx
    visibleLeafs = V.filter (\a -> (isClusterVisible bsp camCluster $ lfCluster a) && inFrustum a) leaves
    surfaceMask = unsafePerformIO $ do
        let leafSurfaces = blLeafSurfaces bsp
        mask <- MV.replicate (V.length surfaces) False
        V.forM_ visibleLeafs $ \l ->
            V.forM_ (V.slice (lfFirstLeafSurface l) (lfNumLeafSurfaces l) leafSurfaces) $ \i ->
                MV.write mask i True
        V.unsafeFreeze mask
    inFrustum a = boxInFrustum (lfMaxs a) (lfMins a) frust

-- Utility code
tableTexture :: [Float] -> Texture1D LuminanceFormat
tableTexture t = unsafePerformIO $ SV.unsafeWith (SV.fromList t) $ \p -> newTexture FloatFormat Luminance16 (length t) [castPtr p]

funcTableSize = 1024 :: Float
sinTexture = tableTexture [sin (i*2*pi/(funcTableSize-1)) | i <- [0..funcTableSize-1]]
squareTexture = tableTexture [if i < funcTableSize / 2 then 1 else -1 | i <- [0..funcTableSize-1]]
sawToothTexture = tableTexture [i / funcTableSize | i <- [0..funcTableSize-1]]
inverseSawToothTexture = tableTexture $ reverse [i / funcTableSize | i <- [0..funcTableSize-1]]
triangleTexture = tableTexture $ l1 ++ map ((-1)*) l1
  where
    n = funcTableSize / 4
    l0 = [i / n | i <- [0..n-1]]
    l1 = l0 ++ reverse l0

whiteImage = textureFromByteString False 4 8 8 $ SB.replicate (8*8*4) '\255'
defaultImage = textureFromByteString True 4 16 16 $ SB.pack $ concatMap (replicate 4) [if e x || e y then '\255' else '\32' | y <- [0..15], x <- [0..15]]
  where
    e  0 = True
    e 15 = True
    e  _ = False

loadQ3Texture :: Bool -> String -> Texture2D RGBAFormat
loadQ3Texture mipmap name' = unsafePerformIO $ do
    let name = "fps/" ++ name'
        n1 = replaceExtension name "tga"
        n2 = replaceExtension name "jpg"
    b0 <- doesFileExist name
    b1 <- doesFileExist n1
    b2 <- doesFileExist n2
    return $ maybe defaultImage id $ textureFromFile mipmap $ if b0 then name else if b1 then n1 else n2
