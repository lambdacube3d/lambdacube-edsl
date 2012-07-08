{-# LANGUAGE OverloadedStrings, PackageImports, TupleSections #-}

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Control.Applicative hiding (Const)
import Control.Monad
import Data.Word
import Data.Attoparsec.Char8
import Data.ByteString.Char8 (ByteString)
import Data.Char
import Data.IORef
import Data.List (isPrefixOf,partition)
import Data.Trie (Trie)
import Data.Vect
import Data.Vect.Float.Instances ()
import FRP.Elerea.Param
import System.Directory
import System.Environment
import System.FilePath
import qualified Data.ByteString.Char8 as SB
import qualified Data.Set as Set
import qualified Data.Trie as T
import qualified Data.Trie.Internal as T
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV

import Debug.Trace

import TypeLevel.Number.Nat.Num
import Graphics.Rendering.OpenGL.Raw.Core32

import Data.Bitmap
import Data.Digest.CRC32
import Codec.Image.STB hiding (Image)

import LC_API
import LC_Mesh

import Effect

import BSP
import Graphics
import Material
import Render
import ShaderParser
import Zip

import Items
import qualified MD3 as MD3

-- Utility code
tableTexture :: [Float] -> ByteString -> Trie InputSetter -> IO ()
tableTexture t n s = do
    let width       = length t
        v           = V.fromList t
        bitmap      = createSingleChannelBitmap (width,1) $ \x y -> floor $ min 255 $ max 0 $ 128 + 128 * v V.! x
        oneBitmap   = createSingleChannelBitmap (width,1) $ \x y -> 255
        texture     = uniformFTexture2D n s

    tex <- compileTexture2DRGBAF False False $ combineChannels [bitmap,bitmap,bitmap,oneBitmap]
    texture tex

setupTables :: Trie InputSetter -> IO ()
setupTables s = do
    let funcTableSize = 1024 :: Float
        sinTexture              = [sin (i*2*pi/(funcTableSize-1)) | i <- [0..funcTableSize-1]]
        squareTexture           = [if i < funcTableSize / 2 then 1 else -1 | i <- [0..funcTableSize-1]]
        sawToothTexture         = [i / funcTableSize | i <- [0..funcTableSize-1]]
        inverseSawToothTexture  = reverse [i / funcTableSize | i <- [0..funcTableSize-1]]
        triangleTexture         = l1 ++ map ((-1)*) l1
          where
            n = funcTableSize / 4
            l0 = [i / n | i <- [0..n-1]]
            l1 = l0 ++ reverse l0
    
    tableTexture sinTexture "SinTable" s
    tableTexture squareTexture "SquareTable" s
    tableTexture sawToothTexture "SawToothTable" s
    tableTexture inverseSawToothTexture "InverseSawToothTable" s
    tableTexture triangleTexture "TriangleTable" s

quad :: Mesh
quad = Mesh
    { mAttributes   = T.singleton "position" $ A_V2F $ SV.fromList [V2 a b, V2 a a, V2 b a, V2 b a, V2 b b, V2 a b]
    , mPrimitive    = P_Triangles
    , mGPUData      = Nothing
    }
  where
    a = -1
    b = 1

main :: IO ()
main = do
    ar <- loadArchive

    let imageShader txName = defaultCommonAttrs {caStages = sa:saLM:[]}
          where
            sa = defaultStageAttrs
                { saTexture     = ST_Map txName
                , saBlend       = Nothing
                , saDepthWrite  = True
                , saRGBGen      = RGB_IdentityLighting
                }
            saLM = defaultStageAttrs
                { saTexture = ST_Lightmap
                , saBlend   = Just (B_DstColor,B_Zero)
                , saTCGen   = TG_Lightmap
                , saRGBGen  = RGB_Identity
                }

    args <- getArgs
    let bspMap = T.fromList [(SB.pack $ takeBaseName n, decompress' e) | e <- ar, let n = eFilePath e, ".bsp" == takeExtensionCI n, isPrefixOfCI "maps" n]
        bspName = case args of
            []     -> head $ T.keys bspMap
            (n:xs) -> SB.pack n
        Just bspData = T.lookup bspName bspMap
        bsp = readBSP bspData
        shNames = Set.fromList $ map shName $ V.toList $ blShaders bsp
        shMap' = shaderMap ar
        (normalShNames,textureShNames) = partition (\n -> T.member n shMap') $ Set.toList shNames
        normalShNameSet     = Set.fromList normalShNames
        textureShNameSet    = Set.fromList textureShNames
        normalShMap     = T.mapBy (\n sh -> if Set.member n normalShNameSet then Just sh else Nothing) shMap'
        --textureShMap    = T.fromList [(n,defaultCommonAttrs {caStages = [defaultStageAttrs {saTexture = ST_Map n, saDepthWrite = True}]}) | n <- Set.toList textureShNameSet]
        textureShMap    = T.fromList [(n,imageShader n) | n <- Set.toList textureShNameSet]
        shMap = T.unionL normalShMap textureShMap
        -- create gfx network to render active materials
        {-
        TODO: gfx network should be created from shaderMap and bsp
          shader data source
            - shader descriptor
            - image file: tga or jpg
        -}
        lcnet :: GP (Image N1 V4F)
        lcnet = PrjFrameBuffer "outFB" tix0 $ q3GFX $ T.toList shMap
        lcnetMenu :: GP (Image N1 V4F)
        lcnetMenu = PrjFrameBuffer "outFB" tix0 screenQuad

        -- extract spawn points
        ents = parseEntities (SB.unpack bspName) $ blEntities bsp
        spawn e = case T.lookup "classname" e of
            Just "info_player_deathmatch" -> True
            _ -> False
        Just sp0 = T.lookup "origin" $ head $ filter spawn ents
        [x0,y0,z0] = map read $ words $ SB.unpack sp0
        p0 = Vec3 x0 y0 z0

    windowSize <- initCommon "LC DSL Quake 3 Demo"

    -- CommonAttrs
    renderer <- compileRenderer $ ScreenOut lcnet
    menuRenderer <- compileRenderer $ ScreenOut lcnetMenu
    print "renderer created"
    --print $ slotUniform renderer
    --print $ slotStream renderer

    compiledQuad <- compileMesh quad
    --addMesh renderer "postSlot" compiledQuad []

    let slotU           = uniformSetter renderer
        draw _          = render renderer >> swapBuffers
        entityRGB       = uniformV3F "entityRGB" slotU
        entityAlpha     = uniformFloat "entityAlpha" slotU
        identityLight   = uniformFloat "identityLight" slotU
        worldMat        = uniformM44F "worldMat" slotU

    worldMat idmtx'
    entityRGB one'
    entityAlpha 1
    identityLight 1.4
    setupTables slotU

    putStrLn "loading textures:"
    -- load textures
    let archiveTrie     = T.fromList [(SB.pack $ eFilePath a,a) | a <- ar]
        redBitmap       = createSingleChannelBitmap (32,32) $ \x y -> if (x+y) `mod` 2 == 0 then 255 else 0
        zeroBitmap      = emptyBitmap (32,32) 1
        oneBitmap       = createSingleChannelBitmap (32,32) $ \x y -> 255

    defaultTexture <- compileTexture2DRGBAF True False $ combineChannels [redBitmap,redBitmap,zeroBitmap,oneBitmap]
    animTex <- fmap concat $ forM (Set.toList $ Set.fromList $ map (\(s,m) -> (saTexture s,m)) $
               concatMap (\sh -> [(s,caNoMipMaps sh) | s <- caStages sh]) $ T.elems shMap) $ \(stageTex,noMip) -> do
        let texSlotName = SB.pack $ "Tex_" ++ show (crc32 $ SB.pack $ show stageTex)
            setTex isClamped img  = uniformFTexture2D texSlotName slotU =<< loadQ3Texture (not noMip) isClamped defaultTexture archiveTrie img
        case stageTex of
            ST_Map img          -> setTex False img >> return []
            ST_ClampMap img     -> setTex True img >> return []
            ST_AnimMap t imgs   -> do
                txList <- mapM (loadQ3Texture (not noMip) False defaultTexture archiveTrie) imgs
                --return [(1 / t / fromIntegral (length imgs),cycle $ zip (repeat (uniformFTexture2D texSlotName slotU)) txList)]
                return [(1/t,cycle $ zip (repeat (uniformFTexture2D texSlotName slotU)) txList)]
            _ -> return []

    putStrLn $ "loading: " ++ show bspName
    objs <- addBSP renderer bsp

    -- setup menu
    levelShots <- sequence [(n,) <$> loadQ3Texture True True defaultTexture archiveTrie (SB.append "levelshots/" n) | n <- T.keys bspMap]
    menuObj <- addMesh menuRenderer "postSlot" compiledQuad ["ScreenQuad"]
    let menuObjUnis = objectUniformSetter menuObj
    --uniformFTexture2D "ScreenQuad" menuObjUnis defaultTexture
    uniformFTexture2D "ScreenQuad" menuObjUnis $ snd $ head levelShots

    -- add entities
{-
data Item
    = Item
    { itClassName   :: String
    , itPickupSound :: String
    , itWorldModel  :: [String]
    , itIcon        :: String
    , itPickupName  :: String
    , itQuantity    :: Int
    , itType        :: ItemType
    , itTag         :: Tag
    , itPreCaches   :: String
    , itSounds      :: String
    } deriving Show

items =
readMD3 :: LB.ByteString -> MD3Model
-}
    -- load items
    let itemModels = T.fromList [(SB.pack $ itClassName it, [ MD3.readMD3 $ decompress' e | n <- itWorldModel it
                                                            , let Just e = T.lookup (SB.pack n) archiveTrie
                                                            ]) | it <- items]
    {-
        "origin" "1012 2090 108"
        "angle" "180"
        "model" "models/mapobjects/visor_posed.md3"
        "classname" "misc_model"
    -}
    {-
        {
        "origin" "1120 2128 16"
        "classname" "item_armor_shard"
        }
    -}
    forM_ ents $ \e -> case T.lookup "classname" e of
        Nothing -> return ()
        Just k  -> case T.lookup k itemModels of
            Just ml -> do
                putStrLn $ "add model: " ++ SB.unpack k
                let Just o = T.lookup "origin" e
                    [x,y,z] = map read $ words $ SB.unpack o
                    p = Vec3 x y z
                forM_ ml $ \md3 -> do
                    lcmd3 <- addMD3 renderer md3 ["worldMat"]
                    forM_ (lcmd3Object lcmd3) $ \obj -> do
                        let unis    = objectUniformSetter $  obj
                            woldMat = uniformM44F "worldMat" unis
                            sm = fromProjective (scaling $ Vec3 s s s)
                            s  = 0.005 / 64 * 2 -- FIXE: what is the correct value?
                        woldMat $ mat4ToM44F $ sm .*. (fromProjective $ translation p)
            Nothing -> when (k == "misc_model") $ case T.lookup "model" e of
                Nothing -> return ()
                Just m  -> do
                    -- TODO
                    return ()

    (mousePosition,mousePositionSink) <- external (0,0)
    (fblrPress,fblrPressSink) <- external (False,False,False,False,False)


    s <- fpsState
    sc <- start $ do
        anim <- animateMaps animTex
        u <- scene bsp objs (setScreenSize renderer) p0 slotU windowSize mousePosition fblrPress anim
        return $ draw <$> u
    driveNetwork sc (readInput s mousePositionSink fblrPressSink)

    dispose renderer
    print "renderer destroyed"
    closeWindow

animateMaps :: [(Float, [(SetterFun TextureData, TextureData)])] -> SignalGen Float (Signal [(Float, [(SetterFun TextureData, TextureData)])])
animateMaps l0 = stateful l0 $ \dt l -> zipWith (f $ dt * timeScale) l timing
  where
    timeScale = 1
    timing  = map fst l0
    f :: Float -> (Float,[(SetterFun TextureData,TextureData)]) -> Float -> (Float,[(SetterFun TextureData,TextureData)])
    f dt (t,a) t0
        | t - dt <= 0   = (t-dt+t0,tail a)
        | otherwise     = (t-dt,a)

scene :: BSPLevel
      -> V.Vector Object
      -> (Word -> Word -> IO ())
      -> Vec3
      -> T.Trie InputSetter
      -> Signal (Int, Int)
      -> Signal (Float, Float)
      -> Signal (Bool, Bool, Bool, Bool, Bool)
      -> Signal [(Float, [(SetterFun TextureData, TextureData)])]
      -> SignalGen Float (Signal ())
scene bsp objs setSize p0 slotU windowSize mousePosition fblrPress anim = do
    time <- stateful 0 (+)
    last2 <- transfer ((0,0),(0,0)) (\_ n (_,b) -> (b,n)) mousePosition
    let mouseMove = (\((ox,oy),(nx,ny)) -> (nx-ox,ny-oy)) <$> last2
    cam <- userCamera p0 mouseMove fblrPress
    let matSetter   = uniformM44F "viewProj" slotU
        viewOrigin  = uniformV3F "viewOrigin" slotU
        orientation = uniformM44F "orientation" slotU
        viewMat     = uniformM44F "viewMat" slotU
        timeSetter  = uniformFloat "time" slotU
        setupGFX (w,h) (cam,dir,up,_) time anim = do
            let cm = fromProjective (lookat cam (cam + dir) up)
                pm = perspective 0.01 15 (pi/2) (fromIntegral w / fromIntegral h)
                sm = fromProjective (scaling $ Vec3 s s s)
                s  = 0.005
                V4 orientA orientB orientC _ = mat4ToM44F $! cm .*. sm
                Vec3 cx cy cz = cam
                near = 0.01/s
                far  = 15/s
                fovDeg = 90
                frust = frustum fovDeg (fromIntegral w / fromIntegral h) (near) (far) cam (cam+dir) up
            timeSetter $ time / 1
            putStrLn $ "time: " ++ show time
            viewOrigin $ V3 cx cy cz
            viewMat $ mat4ToM44F cm
            --orientation $ V4 orientA orientB orientC $ V4 0 0 0 1
            matSetter $! mat4ToM44F $! cm .*. sm .*. pm
            forM_ anim $ \(_,a) -> let (s,t) = head a in s t
            setSize (fromIntegral w) (fromIntegral h)
            cullSurfaces bsp cam frust objs
    r <- effectful4 setupGFX windowSize cam time anim
    return r

vec4ToV4F :: Vec4 -> V4F
vec4ToV4F (Vec4 x y z w) = V4 x y z w

mat4ToM44F :: Mat4 -> M44F
mat4ToM44F (Mat4 a b c d) = V4 (vec4ToV4F a) (vec4ToV4F b) (vec4ToV4F c) (vec4ToV4F d)

readInput :: State
          -> ((Float, Float) -> IO a)
          -> ((Bool, Bool, Bool, Bool, Bool) -> IO c)
          -> IO (Maybe Float)
readInput s mousePos fblrPress = do
    t <- getTime
    resetTime

    (x,y) <- getMousePosition
    mousePos (fromIntegral x,fromIntegral y)

    fblrPress =<< ((,,,,) <$> keyIsPressed KeyLeft <*> keyIsPressed KeyUp <*> keyIsPressed KeyDown <*> keyIsPressed KeyRight <*> keyIsPressed KeyRightShift)

    updateFPS s t
    k <- keyIsPressed KeyEsc
    return $ if k then Nothing else Just (realToFrac t)

-- FRP boilerplate
driveNetwork :: (p -> IO (IO a)) -> IO (Maybe p) -> IO ()
driveNetwork network driver = do
    dt <- driver
    case dt of
        Just dt -> do
            join $ network dt
            driveNetwork network driver
        Nothing -> return ()

-- OpenGL/GLFW boilerplate

initCommon :: String -> IO (Signal (Int, Int))
initCommon title = do
    initialize
    openWindow defaultDisplayOptions
        { displayOptions_numRedBits         = 8
        , displayOptions_numGreenBits       = 8
        , displayOptions_numBlueBits        = 8
        , displayOptions_numAlphaBits       = 8
        , displayOptions_numDepthBits       = 24
        , displayOptions_windowIsResizable  = True
--        , displayOptions_width              = 1280
--        , displayOptions_height             = 800
--        , displayOptions_displayMode    = Fullscreen
        }
    setWindowTitle title

    (windowSize,windowSizeSink) <- external (0,0)
    setWindowSizeCallback $ \w h -> do
        glViewport 0 0 (fromIntegral w) (fromIntegral h)
        putStrLn $ "window size changed " ++ show (w,h)
        windowSizeSink (fromIntegral w, fromIntegral h)

    return windowSize

-- FPS tracking

data State = State { frames :: IORef Int, t0 :: IORef Double }

fpsState :: IO State
fpsState = State <$> newIORef 0 <*> newIORef 0

updateFPS :: State -> Double -> IO ()
updateFPS state t1 = do
    let t = 1000*t1
        fR = frames state
        tR = t0 state
    modifyIORef fR (+1)
    t0' <- readIORef tR
    writeIORef tR $ t0' + t
    when (t + t0' >= 5000) $ do
    f <- readIORef fR
    let seconds = (t + t0') / 1000
        fps = fromIntegral f / seconds
    putStrLn (show (round fps) ++ " FPS - " ++ show f ++ " frames in " ++ show seconds)
    writeIORef tR 0
    writeIORef fR 0

-- Continuous camera state (rotated with mouse, moved with arrows)
userCamera :: Real p => Vec3 -> Signal (Float, Float) -> Signal (Bool, Bool, Bool, Bool, Bool)
           -> SignalGen p (Signal (Vec3, Vec3, Vec3, (Float, Float)))
userCamera p mposs keyss = transfer2 (p,zero,zero,(0,0)) calcCam mposs keyss
  where
    d0 = Vec4 0 (-1) 0 1
    u0 = Vec4 0 0 (-1) 1
    calcCam dt (dmx,dmy) (ka,kw,ks,kd,turbo) (p0,_,_,(mx,my)) = (p',d,u,(mx',my'))
      where
        f0 c n = if c then (&+ n) else id
        p'  = foldr1 (.) [f0 ka (v &* (-t)),f0 kw (d &* t),f0 ks (d &* (-t)),f0 kd (v &* t)] p0
        k   = if turbo then 500 else 100
        t   = k * realToFrac dt
        mx' = dmx + mx
        my' = dmy + my
        rm  = fromProjective $ rotationEuler $ Vec3 (mx' / 100) (my' / 100) 0
        d   = trim $ rm *. d0 :: Vec3
        u   = trim $ rm *. u0 :: Vec3
        v   = normalize $ d &^ u

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

-- | Pure orientation matrix defined by Euler angles.
rotationEuler :: Vec3 -> Proj4
rotationEuler (Vec3 a b c) = orthogonal $ toOrthoUnsafe $ rotMatrixZ a .*. rotMatrixX b .*. rotMatrixY (-c)

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

-- pk3 handling

takeExtensionCI = map toLower . takeExtension
isPrefixOfCI a b = isPrefixOf a $ map toLower b

loadArchive :: IO Archive
loadArchive = concat <$> (mapM readArchive =<< filter (\n -> ".pk3" == takeExtensionCI n) <$> getDirectoryContents ".")

shaderMap :: Archive -> T.Trie CommonAttrs
shaderMap ar = T.fromList $ concat [eval n $ parse shaders d | (n,d) <- l]
  where
    l = [(n,decompress e) | e <- ar, let n = eFilePath e, ".shader" == takeExtensionCI n, isPrefixOfCI "scripts" n]
    eval n f = case f of
        Done "" r   -> r
        Done rem r  -> error $ show (n,"Input is not consumed", rem, map fst r)
        Fail _ c _  -> error $ show (n,"Fail",c)
        Partial f'  -> eval n (f' "")

parseEntities :: String -> SB.ByteString -> [T.Trie SB.ByteString]
parseEntities n s = eval n $ parse entities s
  where
    eval n f = case f of
        Done "" r   -> r
        Done rem r  -> error $ show (n,"Input is not consumed", rem, r)
        Fail _ c _  -> error $ show (n,"Fail",c)
        Partial f'  -> eval n (f' "")



loadQ3Texture :: Bool -> Bool -> TextureData -> Trie Entry -> ByteString -> IO TextureData
loadQ3Texture isMip isClamped defaultTex ar name = do
    let name' = SB.unpack name
        n1 = SB.pack $ replaceExtension name' "tga"
        n2 = SB.pack $ replaceExtension name' "jpg"
        b0 = T.member name ar
        b1 = T.member n1 ar
        b2 = T.member n2 ar
        fname   = if b0 then name else if b1 then n1 else n2
    case T.lookup fname ar of
        Nothing -> return defaultTex
        Just d  -> do
            eimg <- decodeImage $ decompress d
            putStrLn $ "  load: " ++ SB.unpack fname
            case eimg of
                Left msg    -> putStrLn ("    error: " ++ msg) >> return defaultTex
                Right img   -> compileTexture2DRGBAF isMip isClamped img
