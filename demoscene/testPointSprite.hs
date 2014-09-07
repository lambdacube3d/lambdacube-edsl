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

import Data.Maybe
import Data.Bitmap.Pure

import BuiltinVec
import Utility
import Scanlines
import Vignette

n_rotation = "rotation"
n_position = "position"
n_backgroundSlot = "backgroundSlot"
n_backgroundTex = "background"
n_diffuseTex = "ScreenQuad"
n_postSlot = "postSlot"

quad :: Mesh
quad = Mesh
    { mAttributes   = T.singleton n_position $ A_V2F $ SV.fromList [V2 a b, V2 a a, V2 b a, V2 b a, V2 b b, V2 a b]
    , mPrimitive    = P_Triangles
    , mGPUData      = Nothing
    }
  where
    a = -1
    b = 1

intF :: Int32 -> Exp F Int32
intF = Const

smp :: ByteString -> Exp F V2F -> Exp F V4F
smp n uv = texture' (Sampler LinearFilter ClampToEdge $ TextureSlot n $ Texture2D (Float RGBA) n1) uv

background tex = Accumulate fragCtx PassAll frag rast clear
  where
    fragCtx = AccumulationContext Nothing $ ColorOp NoBlending (one' :: V4B):.ZT
    clear   = FrameBuffer (ColorImage n1 (V4 0 0 0 1):.ZT)
    rast    = Rasterize rCtx prims
    rCtx    = triangleCtx
    prims   = Transform vert input
    input   = Fetch n_backgroundSlot Triangles (IV2F n_position)

    frag uv' = FragmentOut $ smp tex uv :. ZT
      where
        uv = uv' @* floatF 0.5 @+ floatF 0.5

    vert :: Exp V V2F -> VertexOut () V2F
    vert uv = VertexOut out (Const 1) ZT (NoPerspective uv:.ZT)
      where
        out = vec4' uv (floatV 1) (floatV 1)

model :: Exp Obj (FrameBuffer 1 V4F)
model = Accumulate fragCtx PassAll frag rast (background n_backgroundTex)
  where
    blend   = Blend (FuncAdd, FuncAdd) ((SrcAlpha, One), (SrcAlpha, OneMinusSrcAlpha)) zero'
    fragCtx = AccumulationContext Nothing $ ColorOp blend (one' :: V4B):.ZT
    rast    = Rasterize rCtx prims
    rCtx    = PointCtx ProgramPointSize 10 UpperLeft
    prims   = Transform vert input
    input   = Fetch n_postSlot Points (IV3F n_position)

    vert :: Exp V V3F -> VertexOut () ()
    vert uvw = VertexOut out (Const 20) ZT ZT
      where
        out = rotation @*. vec4' uvw (floatV 1)

    rotation = Uni (IM44F n_rotation)

    frag :: Exp F () -> FragmentOut (Color V4F :+: ZZ)
    frag _ = FragmentOut $ a :. ZT
      where
        V4 x _ _ _ = unpack' $
            (smp n_diffuseTex $ pointCoord')
        a = vec4' x x x x @* floatF 0.3

postProcess base = renderScreen $ FragmentOut . (:.ZT) . f
  where
    f uv = fVignette vign uv $
           fScanlines sl uv $
           smp' base uv
    sl = scanlines { scanlinesFrequency = floatF 100
                   , scanlinesHigh = Const (V4 0.5 1 1 1)
                   , scanlinesLow = Const (V4 0.2 0.5 0.5 1)
                   }
    vign = vignette { vignetteOuterRadius = floatF 0.9
                    , vignetteInnerRadius = floatF 0.4
                    }
    smp' img uv = texture' (Sampler LinearFilter ClampToEdge $ Texture (Texture2D (Float RGBA) n1) (V2 512 512) NoMip [img]) uv

main :: IO ()
main = do
    let lcnet :: Exp Obj (Image 1 V4F)
        lcnet = postProcess $ PrjFrameBuffer "outFB" tix0 model

    windowSize <- initCommon "LC DSL 2D Demo"

    renderer <- compileRenderer $ ScreenOut lcnet
    print $ slotUniform renderer
    print $ slotStream renderer
    print "renderer created"

    (mousePosition,mousePositionSink) <- external (0,0)
    (fblrPress,fblrPressSink) <- external (False,False,False,False,False)

    mesh <- loadMesh "models/Monkey.lcmesh"
    obj <- compileMesh mesh { mPrimitive = P_Points, mGPUData = Nothing }
           >>= addMesh renderer n_postSlot `flip` []
    compileMesh quad
      >>= addMesh renderer n_backgroundSlot `flip` []
    initUtility renderer

    args <- getArgs
    let objU    = objectUniformSetter obj
        slotU   = uniformSetter renderer
        diffuse = uniformFTexture2D n_diffuseTex slotU
        backgroundTexture = uniformFTexture2D n_backgroundTex slotU
        draw _  = render renderer >> swapBuffers
        fname   = case args of
            []  -> "textures/particle_base.png"
            n:_ -> n
        bgname   = case args of
            []  -> "textures/background.jpg"
            _:n:_ -> n
    Right img2 <- loadImage fname
    diffuse =<< compileTexture2DRGBAF False True img2
    Right bgImg <- loadImage bgname
    backgroundTexture =<< compileTexture2DRGBAF False True bgImg

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
    let setRotation = uniformM44F n_rotation slotU
        setupGFX (w,h) t' (x,y) = do
            setSize (fromIntegral w) (fromIntegral h)
            let s = sin t * 0.5 + 0.5
                t = 1.5 * t'
                rx = rotMatrixProj4 (pi - x*2*pi/fromIntegral w) (Vec3 0 1 0)
                ry = rotMatrixProj4 (pi - y*2*pi/fromIntegral h) (Vec3 1 0 0)
            setRotation $ mat4ToM44F $ fromProjective $ rx .*. ry
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
