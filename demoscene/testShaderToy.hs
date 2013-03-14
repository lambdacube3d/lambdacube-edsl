{-# LANGUAGE OverloadedStrings, PackageImports, TypeOperators, MultiParamTypeClasses #-}

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Control.Applicative hiding (Const)
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Data.IORef
import Data.Word
import Data.Vect
import Data.Vect.Float.Instances ()
import FRP.Elerea.Param
import qualified Data.ByteString.Char8 as SB
import qualified Data.Trie as T
import qualified Data.Vector.Storable as SV
import System.Environment

import TypeLevel.Number.Nat.Num
import Data.Typeable

import LC_API

import Graphics.Rendering.OpenGL.Raw.Core32
import LC_Mesh
import Codec.Image.STB hiding (Image)

import Data.Maybe
import Data.Bitmap.Pure

import Utility
import ShaderToy
import BuiltinVec

n_time = "time"
n_size = "size"
n_background = "background"

smp n uv = texture' (Sampler LinearFilter Clamp $ TextureSlot n $ Texture2D (Float RGBA) n1) uv
background tex = renderScreen $ \uv -> FragmentOut $ smp tex uv :. ZT

main :: IO ()
main = do
    let lcnet :: Exp Obj (Image N1 V4F)
        --lcnet = fxFakeRipple (Uni $ IFloat n_time) (Uni $ IV2F n_size) (background n_background)
        --lcnet = fxWarping (Uni $ IFloat n_time) (Uni $ IV2F n_size) (background n_background)
        lcnet = fxMotionBlur (Uni $ IFloat n_time) (Uni $ IV2F n_size) (background n_background)

    windowSize <- initCommon "LC DSL 2D Demo"

    renderer <- compileRenderer $ ScreenOut lcnet
    print $ slotUniform renderer
    print $ slotStream renderer
    print "renderer created"

    (mousePosition,mousePositionSink) <- external (0,0)
    (fblrPress,fblrPressSink) <- external (False,False,False,False,False)

    initUtility renderer

    args <- getArgs
    let slotU           = uniformSetter renderer
        setBackground   = uniformFTexture2D n_background slotU
        draw _          = render renderer >> swapBuffers
        bgname   = case args of
            []  -> "free_high_res_texture_30.jpg"
            n:_ -> n
    Right img <- loadImage bgname
    setBackground =<< compileTexture2DRGBAF True False img

    s <- fpsState
    sc <- start $ do
        u <- scene (setScreenSize renderer) slotU windowSize mousePosition fblrPress
        return $ draw <$> u
    driveNetwork sc (readInput s mousePositionSink fblrPressSink)

    dispose renderer
    print "renderer destroyed"
    closeWindow

scene :: (Word -> Word -> IO ())
      -> T.Trie InputSetter
      -> Signal (Int, Int)
      -> Signal (Float, Float)
      -> Signal (Bool, Bool, Bool, Bool, Bool)
      -> SignalGen Float (Signal ())
scene setSize slotU windowSize mousePosition fblrPress = do
    time <- stateful 0 (+)
    let setTime = uniformFloat n_time slotU
        setFXSize = uniformV2F n_size slotU
        setupGFX (w,h) t' (x,y) = do
            setSize (fromIntegral w) (fromIntegral h)
            let t = 1.5 * t'
            setTime t
            setFXSize $ V2 (fromIntegral w) (fromIntegral h)
            return ()
    r <- effectful3 setupGFX windowSize time mousePosition
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
        , displayOptions_width              = 512
        , displayOptions_height             = 512
        , displayOptions_windowIsResizable  = True
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
    putStrLn (show (round fps) ++ " FPS - " ++ show f ++ " frames in ")
    writeIORef tR 0
    writeIORef fR 0
