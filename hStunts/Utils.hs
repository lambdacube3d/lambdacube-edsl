{-# LANGUAGE PackageImports #-}

module Utils where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.IORef

import FRP.Elerea.Param
import "GLFW-b" Graphics.UI.GLFW as GLFW

import Graphics.Rendering.OpenGL.Raw.Core32

import Data.Vect.Float

import Graphics.Text.TrueType( loadFontFile, Font )
import Codec.Picture( PixelRGBA8( .. ), writePng, Image(..) )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Data.Vector.Storable (unsafeWith)
import Foreign 
import System.IO.Unsafe

import LC_API (TextureData(..))

-- Reactive helper functions

integral :: (Real p, Fractional t) => t -> Signal t -> SignalGen p (Signal t)
integral v0 s = transfer v0 (\dt v v0 -> v0+v*realToFrac dt) s

driveNetwork :: (MonadIO m) => (p -> IO (m a)) -> IO (Maybe p) -> m ()
driveNetwork network driver = do
    dt <- liftIO driver
    case dt of
        Just dt -> do
            join . liftIO $ network dt
            driveNetwork network driver
        Nothing -> return ()

edge :: Signal Bool -> SignalGen p (Signal Bool)
edge input = do
    input' <- delay True input
    let rising prev curr = not prev && curr
    return $ rising <$> input' <*> input

-- OpenGL/GLFW boilerplate

initCommon :: String -> IO (Signal (Int, Int))
initCommon title = do
    initialize
    openWindow defaultDisplayOptions
        { displayOptions_numRedBits     = 8
        , displayOptions_numGreenBits   = 8
        , displayOptions_numBlueBits    = 8
        , displayOptions_numAlphaBits   = 8
        , displayOptions_numDepthBits   = 24
        , displayOptions_width          = 800
        , displayOptions_height         = 600
        , displayOptions_openGLVersion      = (3,2)
        , displayOptions_openGLProfile      = CoreProfile
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
    putStrLn (show f ++ " frames in " ++ show seconds ++ " seconds = "++ show fps ++ " FPS")
    writeIORef tR 0
    writeIORef fR 0

-- | Pure orientation matrix defined by Euler angles.
rotationEuler :: Vec3 -> Proj4
rotationEuler (Vec3 a b c) = orthogonal $ toOrthoUnsafe $ rotMatrixY a .*. rotMatrixX b .*. rotMatrixZ c

-- Continuous camera state (rotated with mouse, moved with arrows)
userCamera :: Real p => Vec3 -> Signal (Float, Float) -> Signal (Bool, Bool, Bool, Bool, Bool)
           -> SignalGen p (Signal (Vec3, Vec3, Vec3, (Float, Float)))
userCamera p mposs keyss = transfer2 (p,zero,zero,(0,0)) calcCam mposs keyss
  where
    d0 = Vec4 0 0 (-1) 1
    u0 = Vec4 0 1 0 1
    calcCam dt (dmx,dmy) (ka,kw,ks,kd,turbo) (p0,_,_,(mx,my)) = (p',d,u,(mx',my'))
      where
        f0 c n = if c then (&+ n) else id
        p'  = foldr1 (.) [f0 ka (v &* (-t)),f0 kw (d &* t),f0 ks (d &* (-t)),f0 kd (v &* t)] p0
        k   = if turbo then 100 else 30
        t   = k * realToFrac dt
        mx' = dmx + mx
        my' = dmy + my
        rm  = fromProjective $ rotationEuler $ Vec3 (mx' / 100) (my' / 100) 0
        d   = trim $ rm *. d0 :: Vec3
        u   = trim $ rm *. u0 :: Vec3
        v   = normalize $ d &^ u

followCamera :: Float -> Float -> Float -> Signal Proj4 -> SignalGen p (Signal (Vec3, Vec3))
followCamera height minDist maxDist target = transfer (Vec3 (-maxDist) height 0, Vec3 1 0 0) follow target
  where
    follow _dt tproj (pos,_dir) = (pos',tpos &- pos')
      where
        Mat4 _ _ _ tpos4 = fromProjective (tproj .*. translation (Vec3 0 height 0))
        tpos = trim tpos4
        tdir = tpos &- pos
        dist = len tdir
        pos'
            | dist < minDist = pos &+ (normalize tdir &* (dist-minDist))
            | dist > maxDist = pos &+ (normalize tdir &* (dist-maxDist))
            | otherwise      = pos

renderText :: Int -> Int -> Float -> Float -> Font -> Int -> String -> IO TextureData
renderText w h x y font fontSize text = do
    let img = renderDrawing w h (PixelRGBA8 0 0 0 0)
                . withTexture (uniformTexture $ PixelRGBA8 255 0 0 255) $
                printTextAt font fontSize (V2 x y) text
    compileImageToTexture2DRGBAF False True img

compileImageToTexture2DRGBAF isMip isClamped (Image width height iData) = do
    glPixelStorei gl_UNPACK_ALIGNMENT 1
    to <- alloca $! \pto -> glGenTextures 1 pto >> peek pto
    glBindTexture gl_TEXTURE_2D to
    let wrapMode = case isClamped of
            True    -> gl_CLAMP_TO_EDGE
            False   -> gl_REPEAT
        (minFilter,maxLevel) = case isMip of
            False   -> (gl_LINEAR,0)
            True    -> (gl_LINEAR_MIPMAP_LINEAR, floor $ log (fromIntegral $ max width height) / log 2)
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fromIntegral wrapMode
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fromIntegral wrapMode
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral minFilter
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fromIntegral gl_LINEAR
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_BASE_LEVEL 0
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAX_LEVEL $ fromIntegral maxLevel
    unsafeWith iData $ \ptr -> do
        let nchn = 4
            internalFormat  = fromIntegral gl_RGBA8
            dataFormat      = fromIntegral $ case nchn of
                3   -> gl_RGB
                4   -> gl_RGBA
                _   -> error "unsupported texture format!"
        glTexImage2D gl_TEXTURE_2D 0 internalFormat (fromIntegral width) (fromIntegral height) 0 dataFormat gl_UNSIGNED_BYTE $ castPtr ptr
    when isMip $ glGenerateMipmap gl_TEXTURE_2D
    return $ TextureData to
