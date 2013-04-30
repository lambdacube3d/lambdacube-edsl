{-# LANGUAGE OverloadedStrings, PackageImports, TypeOperators, DataKinds #-}

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

import LC_API

import Graphics.Rendering.OpenGL.Raw.Core32
import LC_Mesh
import Codec.Image.STB hiding (Image)

import Math.Noise
import Math.Noise.Modules.Billow
import Data.Maybe
import Data.Bitmap.Pure

import Utility
import Scanlines
import BuiltinVec

points :: Mesh
points = Mesh
    { mAttributes   = T.fromList
        [ ("position", A_V2F $ SV.fromList
            [ V2 a b, V2 a a, V2 b a, V2 b b
            ]
          )
        , ("vid", A_Int $ SV.fromList [0..7])
        ]
    , mPrimitive    = P_Points
    , mGPUData      = Nothing
    }
  where
    a = -0.5
    b = 0.5

sprites :: Exp Obj (Image 1 V4F)
sprites = PrjFrameBuffer "" tix0 $ Accumulate fragCtx PassAll frag rast clear
  where
    fragCtx = AccumulationContext Nothing $ ColorOp blend (one' :: V4B):.ZT
    blend   = Blend (FuncAdd, FuncAdd) ((SrcAlpha, One), (SrcAlpha, OneMinusSrcAlpha)) zero'
    clear   = renderScreen' $ \uv -> FragmentOut $ (smp "background" uv) :. ZT
    rast    = Rasterize rCtx prims
    rCtx    = PointCtx ProgramPointSize 10 UpperLeft
    prims   = Transform vert input
    input   = Fetch "points" Points (IV2F "position")

    vert :: Exp V V2F -> VertexOut () ()
    vert uv = VertexOut (vec4' uv (floatV 1) (floatV 1)) (Const 20) ZT ZT

    offset = Uni (IV2F "offset") :: Exp F V2F
    smp n uv = texture' (Sampler LinearFilter ClampToEdge $ TextureSlot n $ Texture2D (Float RGBA) n1) uv
    frag :: Exp F () -> FragmentOut (Color V4F :+: ZZ)
    frag _ = FragmentOut $ (smp "explosion" $ (pointCoord' @* floatF 0.25 @+ offset)) :. ZT

main :: IO ()
main = do
    let lcnet :: Exp Obj (Image 1 V4F)
        lcnet = renderScreen $ (FragmentOut.(:.ZT).fxScanlines sl sprites)
        sl    = scanlines { scanlinesFrequency = floatF 128
                          , scanlinesHigh = Const $ V4 0.9 1 1 1
                          , scanlinesLow = Const $ V4 0.45 0.5 0.5 1
                          }

    windowSize <- initCommon "LC DSL 2D Demo"

    renderer <- compileRenderer $ ScreenOut lcnet
    print $ slotUniform renderer
    print $ slotStream renderer
    print "renderer created"
    initUtility renderer

    (mousePosition,mousePositionSink) <- external (0,0)
    (fblrPress,fblrPressSink) <- external (False,False,False,False,False)

    compiledPoints <- compileMesh points
    obj <- addMesh renderer "points" compiledPoints []

    args <- getArgs
    let objU    = objectUniformSetter obj
        slotU   = uniformSetter renderer
        diffuse = uniformFTexture2D "explosion" slotU
        background  = uniformFTexture2D "background" slotU
        draw _  = render renderer >> swapBuffers
        fname   = case args of
            []  -> "textures/Explosion.png"
            n:_ -> n

    Right img <- loadImage fname
    diffuse =<< compileTexture2DRGBAF False True img
    Right img <- loadImage "textures/space.jpg"
    background =<< compileTexture2DRGBAF False True img

    s <- fpsState
    sc <- start $ do
        u <- scene (setScreenSize renderer) slotU objU windowSize mousePosition fblrPress
        return $ draw <$> u
    driveNetwork sc (readInput s mousePositionSink fblrPressSink)

    dispose renderer
    print "renderer destroyed"
    closeWindow

scene :: (Word -> Word -> IO ())
      -> T.Trie InputSetter
      -> T.Trie InputSetter
      -> Signal (Int, Int)
      -> Signal (Float, Float)
      -> Signal (Bool, Bool, Bool, Bool, Bool)
      -> SignalGen Float (Signal ())
scene setSize slotU objU windowSize mousePosition fblrPress = do
    let offs = reverse $ 
            [ V2 0 0,    V2 0.25 0,    V2 0.5 0,    V2 0.75 0
            , V2 0 0.25, V2 0.25 0.25, V2 0.5 0.25, V2 0.75 0.25
            , V2 0 0.5,  V2 0.25 0.5,  V2 0.5 0.5,  V2 0.75 0.5
            , V2 0 0.75, V2 0.25 0.75, V2 0.5 0.75, V2 0.75 0.75
            ] :: [V2F]
        step = 0.025
    time <- stateful 0 (+)
    o <- stateful (0,cycle offs) (\t (st,o) -> if st < step then (st+t,o) else (st+t-step,tail o))
    let setupGFX (w,h) t' (_,ofs) = do
            let offset = uniformV2F "offset" slotU
            offset $ head ofs
            setSize (fromIntegral w) (fromIntegral h)
            return ()
    r <- effectful3 setupGFX windowSize time o
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
