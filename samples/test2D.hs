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
import Text.Show.Pretty (ppShow)

import LC_API2
import LC_GL_API

import Graphics.Rendering.OpenGL.Raw.Core32
import Codec.Image.STB hiding (Image)

quad :: Mesh
quad = Mesh
    { mAttributes   = T.singleton "position" $ A_V2F $ SV.fromList [V2 a b, V2 a a, V2 b a, V2 b a, V2 b b, V2 a b]
    , mPrimitive    = P_Triangles
    , mGPUData      = Nothing
    }
  where
    a = -1
    b = 1

floatV :: Float -> Exp V Float
floatV = Const

floatF :: Float -> Exp F Float
floatF = Const

{-
            => { smpMinFilter   :: Filter mip
               , smpMagFilter   :: Filter TexNoMip
               , smpEdgeMode    :: edgeMode     -- Wrap S,T,R
               , smpBorderColor :: borderColor
               , smpMinLod      :: Maybe Float
               , smpMaxLod      :: Maybe Float
               , smpLodBias     :: Float
               , smpCompareFunc :: CompareMode t
               , smpTexture     :: Texture (Exp Obj) dim arr t ar canMip
               }
-}
simpleSmp minF magF edgeMode tex = Sampler minF magF edgeMode zero' Nothing Nothing 0 NoCompare tex

screenQuad :: Exp Obj (FrameBuffer 1 V4F)
screenQuad = Accumulate fragCtx PassAll frag rast clear
  where
    clear   = FrameBuffer (ColorImage n1 (V4 1 0 0 1):.ZT)
    fragCtx = AccumulationContext Nothing $ ColorOp NoBlending (one' :: V4B):.ZT
    rast    = Rasterize triangleCtx prims
    prims   = Transform vert input
    input   = Fetch "postSlot" Triangles (IV2F "position")

    vert :: Exp V V2F -> VertexOut () V2F
    vert uv = VertexOut v4 (Const 1) ZT (NoPerspective uv:.ZT)
      where
        v4      = pack' $ V4 u v (floatV 1) (floatV 1)
        V2 u v  = unpack' uv

    frag :: Exp F V2F -> FragmentOut (Color V4F :+: ZZ)
    frag uv' = FragmentOut $ color :. ZT
      where
        color = texture' smp uv
        V2 u v = unpack' uv
        uv = uv' @* floatF 0.5 @+ floatF 0.5
        smp = simpleSmp Linear Linear (ClampToEdge,ClampToEdge) tex
        tex = TextureSlot "ScreenQuad" $ Texture2D (Float RGBA) n1

lcout :: GPOutput SingleOutput
lcout = ScreenOut $ PrjFrameBuffer "outFB" tix0 screenQuad

main :: IO ()
main = do
    let lcnet :: Exp Obj (Image 1 V4F)
        lcnet = PrjFrameBuffer "outFB" tix0 screenQuad

    windowSize <- initCommon "LC DSL 2D Demo"

    let Right ppl = compilePipeline $ ScreenOut lcnet
        schema = schemaFromPipeline ppl
    putStrLn $ ppShow ppl
    putStrLn ""
    putStrLn $ ppShow schema
    renderer <- allocPipeline ppl
    --print $ slotUniform renderer
    --print $ slotStream renderer
    print "renderer created"

    (mousePosition,mousePositionSink) <- external (0,0)
    (fblrPress,fblrPressSink) <- external (False,False,False,False,False)

    input <- mkGLPipelineInput schema
    compiledQuad <- compileMesh quad
    obj <- addMesh input "postSlot" compiledQuad []

    setPipelineInput renderer $ Just input
    sortSlotObjects input

    args <- getArgs
    let objU    = objectUniformSetter obj
        slotU   = uniformSetter input
        diffuse = uniformFTexture2D "ScreenQuad" slotU
        draw _  = renderPipeline renderer >> swapBuffers >> putStrLn "[end frame]"
        fname   = case args of
            []  -> "Panels_Diffuse.png"
            n:_ -> n

    Right img <- loadImage fname
    diffuse =<< compileTexture2DRGBAF False True img
    
    s <- fpsState
    sc <- start $ do
        u <- scene (setScreenSize input) slotU objU windowSize mousePosition fblrPress
        return $ draw <$> u
    driveNetwork sc (readInput s mousePositionSink fblrPressSink)

    disposePipeline renderer
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
    let setupGFX (w,h) = do
            setSize (fromIntegral w) (fromIntegral h)
            return ()
    r <- effectful1 setupGFX windowSize
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
