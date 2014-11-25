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

import Graphics.Rendering.OpenGL.Raw.Core32
import LambdaCube.GL.Mesh
import Codec.Image.STB hiding (Image)

import Math.Noise
import Math.Noise.Modules.Billow
import Data.Maybe
import Data.Bitmap.Pure

import Geometry
import Utility
import Blur
import Glow
--import OldFilm


screenQuad :: Exp Obj (FrameBuffer 1 (V4F,V4F))
screenQuad = Accumulate fragCtx PassAll frag rast clear
  where
    fragCtx = AccumulationContext Nothing $ ColorOp NoBlending (one' :: V4B) :. ColorOp NoBlending (one' :: V4B) :. ZT
    clear   = FrameBuffer (ColorImage n1 (V4 1 0 0 1):.ColorImage n1 (V4 0 1 0 1):.ZT)
    rast    = Rasterize triangleCtx prims
    prims   = Transform vert input
    input   = Fetch "postSlot" Triangles (IV2F "position")

    vert :: Exp V V2F -> VertexOut () V2F
    vert uv = VertexOut v4 (Const 1) ZT (NoPerspective uv:.ZT)
      where
        v4      = pack' $ V4 u v (floatV 1) (floatV 1)
        V2 u v  = unpack' uv

    up = Uni (IFloat "up") :: Exp F Float
    down = Uni (IFloat "down")
    frag :: Exp F V2F -> FragmentOut (Color V4F :+: Color V4F :+: ZZ)
    frag uv' = FragmentOut $ c :. c2 :. ZT
      where
        c = Cond (down @< r @&& r @< up) (Const (V4 1 0 0 1)) $
            Cond (down @< r @&& x @< floatF 128) texel $ pack' $ V4 tR tG tB $ floatF 1--texel2--(Const (V4 0 0 0 1))
        c2 = Cond (down @< r @&& r @< up) (Const (V4 1 1 0 1)) $ Const (V4 0 0 0 1)
        texel = smp "ScreenQuad" uv
        texel2 = smp "ScreenQuad2" uv
        V4 r g b a = unpack' texel
        V4 x y z w = unpack' fragCoord'
        V4 tR _ _ _ = unpack' $ smp "ScreenQuad2" $ uv @+ (Const (V2 0.1 0) :: Exp F V2F) @* up
        V4 _ tG _ _ = unpack' $ smp "ScreenQuad2" $ uv @+ (Const (V2 0.11 0) :: Exp F V2F) @* (up @+ r @* floatF 0.1)
        V4 _ _ tB _ = unpack' $ smp "ScreenQuad2" $ uv @+ (Const (V2 0.117 0) :: Exp F V2F) @* (up @+ r @* floatF 0.1)
        V2 u v = unpack' uv
        uv = uv' @* floatF 0.5 @+ floatF 0.5
        smp n uv = texture' (Sampler LinearFilter ClampToEdge $ TextureSlot n $ Texture2D (Float RGBA) n1) uv

dummy512 img = renderScreen frag
  where
    frag :: Exp F V2F -> FragmentOut (Color V4F :+: ZZ)
    frag uv = FragmentOut $ smp img uv :. ZT
      where
        sizeI = 512 :: Word32
        smp i coord = texture' (Sampler LinearFilter ClampToEdge $ Texture (Texture2D (Float RGBA) n1) (V2 sizeI sizeI) NoMip [i]) coord
    
renderRGB :: Exp Obj (FrameBuffer 1 (V4F,V4F,V4F))
renderRGB = Accumulate fragCtx PassAll frag rast clear
  where
    fragCtx = AccumulationContext Nothing $ ColorOp NoBlending (one' :: V4B):. ColorOp NoBlending (one' :: V4B):.ColorOp NoBlending (one' :: V4B):.ZT
    clear   = FrameBuffer (ColorImage n1 (V4 0 0 0 1):.ColorImage n1 (V4 0 0 0 1):.ColorImage n1 (V4 0 0 0 1):.ZT)
    rast    = Rasterize triangleCtx prims
    prims   = Transform vert input
    input   = Fetch "ScreenQuad" Triangles (IV2F "position")

    vert :: Exp V V2F -> VertexOut () ()
    vert uv = VertexOut v4 (Const 1) ZT ZT
      where
        v4      = pack' $ V4 u v (floatV 1) (floatV 1)
        V2 u v  = unpack' uv

    frag :: Exp F () -> FragmentOut (Color V4F :+: Color V4F :+: Color V4F :+: ZZ)
    frag _ = FragmentOut $ Const (V4 1 0 0 1) :. Const (V4 0 1 0 1) :. Const (V4 0 0 1 1) :. ZT

n_time = "time"

main :: IO ()
main = do
    let lcnet :: Exp Obj (Image 1 V4F)
        lcnet = dummy512 $ fxBlur blur glowImg
        --lcnet = fxOldFilm oldFilm { ofTimeLapse = Uni (IFloat n_time) } sceneImg -- $ fxGlow glow sceneImg glowImg
        --lcnet = fxBlur blur $ sceneImg
        glowImg = dummy512 $ PrjFrameBuffer "" tix0 screenQuad
        sceneImg = PrjFrameBuffer "" tix1 screenQuad

    windowSize <- initCommon "LC DSL 2D Demo"

    renderer <- compileRenderer $ ScreenOut lcnet
    print $ slotUniform renderer
    print $ slotStream renderer
    print "renderer created"
    initUtility renderer

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
            []  -> "textures/Panels_Diffuse.png"
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
        setTime = uniformFloat n_time slotU
        setupGFX (w,h) t' = do
            setSize (fromIntegral w) (fromIntegral h)
            let s = sin t * 0.5 + 0.5
                t = 1.5 * t'
            down s
            up (s+0.028)
            setTime t
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
