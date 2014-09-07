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

import LambdaCube.GL
import LambdaCube.GL.Mesh

import Graphics.Rendering.OpenGL.Raw.Core32
import Codec.Image.STB hiding (Image)

import Math.Noise
import Math.Noise.Modules.Billow
import Data.Maybe
import Data.Bitmap.Pure

quad :: Mesh
quad = Mesh
    { mAttributes   = T.fromList
        [ ("position", A_V2F $ SV.fromList
            [ V2 a b, V2 a a, V2 b a, V2 b b
            , V2 a2 b2, V2 a2 a2, V2 b2 a2, V2 b2 b2
            ]
          )
        , ("vid", A_Int $ SV.fromList [0..7])
        ]
    , mPrimitive    = P_Points
    , mGPUData      = Nothing
    }
  where
    a = -1
    b = 1
    a2 = -0.4
    b2 = 0.4

floatV :: Float -> Exp V Float
floatV = Const

floatF :: Float -> Exp F Float
floatF = Const

intF :: Int32 -> Exp F Int32
intF = Const

screenQuad :: Exp Obj (FrameBuffer 1 V4F)
screenQuad = Accumulate fragCtx PassAll frag rast clear
  where
    fragCtx = AccumulationContext Nothing $ ColorOp NoBlending (one' :: V4B):.ZT
    clear   = FrameBuffer (ColorImage n1 (V4 1 0 0 1):.ZT)
    rast    = Rasterize rCtx prims
    rCtx    = PointCtx ProgramPointSize 10 UpperLeft
    prims   = Transform vert input
    input   = Fetch "postSlot" Points (IV2F "position")

    vert :: Exp V V2F -> VertexOut () ()
    vert uv = VertexOut v4 (Const 200) ZT ZT
      where
        v4      = pack' $ V4 u v (floatV 1) (floatV 1)
        V2 u v  = unpack' uv

    up = Uni (IFloat "up") :: Exp F Float
    down = Uni (IFloat "down")
    frag :: Exp F () -> FragmentOut (Color V4F :+: ZZ)
    frag _ = FragmentOut $ c :. ZT
      where
        c = Cond (primitiveID' @% intF 2 @== intF 0)
                (smp "ScreenQuad" $ pointCoord')
                (smp "ScreenQuad2" $ pointCoord')
        smp n uv = texture' (Sampler LinearFilter ClampToEdge $ TextureSlot n $ Texture2D (Float RGBA) n1) uv

main :: IO ()
main = do
    let lcnet :: Exp Obj (Image 1 V4F)
        lcnet = PrjFrameBuffer "outFB" tix0 screenQuad

    windowSize <- initCommon "LC DSL 2D Demo"

    renderer <- compileRenderer $ ScreenOut lcnet
    print $ slotUniform renderer
    print $ slotStream renderer
    print "renderer created"

    (mousePosition,mousePositionSink) <- external (0,0)
    (fblrPress,fblrPressSink) <- external (False,False,False,False,False)

    compiledQuad <- compileMesh quad
    obj <- addMesh renderer "postSlot" compiledQuad []

    args <- getArgs
    let objU    = objectUniformSetter obj
        slotU   = uniformSetter renderer
        diffuse = uniformFTexture2D "ScreenQuad" slotU
        diffuse2 = uniformFTexture2D "ScreenQuad2" slotU
        draw _  = render renderer >> swapBuffers
        fname   = case args of
            []  -> "Panels_Diffuse.png"
            n:_ -> n

    let p   = perlin
        clamp :: Double -> Word8
        clamp = floor . max 0 . min 255
        calc w h i j = (\v ->  (v + 1.0) * 127.5 ) $ noiseClampedVal
          where
            boundBottomX :: Double
            boundBottomX = 0.0
            boundBottomY :: Double
            boundBottomY = 0.0
            boundUpperX :: Double
            boundUpperX = 10.0
            boundUpperY :: Double
            boundUpperY = 10.0
            xsize = w
            ysize = h
            xIncrement :: Double
            xIncrement = (boundUpperX - boundBottomX) / (fromIntegral xsize)
            yIncrement :: Double
            yIncrement = (boundUpperY - boundBottomY) / (fromIntegral ysize)
            xPos x = ((fromIntegral x) * xIncrement)  +  boundBottomX
            yPos y = ((fromIntegral y) * yIncrement)  +  boundBottomY

            noiseF :: NoiseModule
            noiseF = gen perlin { perlinFrequency = 1.1, perlinOctaves = 9 }
            --noiseF = gen billow { billowFrequency = 0.6, billowOctaves = 5 }

            -- Actual noise computation, getValue returns Maybe Double
            noiseValue = fromMaybe (-1.0) $ getValue noiseF (xPos i, yPos j, 2.123)
            -- Make sure the noiseValue is in the [-1.0, 1.0] range
            noiseClampedVal = if noiseValue > 1.0 
                                 then 1.0
                                 else if noiseValue < (-1.0) then (-1.0)
                                                             else noiseValue
        
        ch  = createSingleChannelBitmap (512,512) Nothing (\i j -> clamp $ calc 512 512 i j)-- $ \x y ->
        img = combineChannels [ch,ch,ch] Nothing

    diffuse =<< compileTexture2DRGBAF False True img
    Right img2 <- loadImage fname
    diffuse2 =<< compileTexture2DRGBAF False True img2

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
    time <- stateful 0 (+)
    let up      = uniformFloat "up" slotU
        down    = uniformFloat "down" slotU
        setupGFX (w,h) t' = do
            setSize (fromIntegral w) (fromIntegral h)
            let s = sin t * 0.5 + 0.5
                t = 1.5 * t'
            --down s
            --up (s+0.028)
            return ()
    r <- effectful2 setupGFX windowSize time
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
