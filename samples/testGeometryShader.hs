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

points :: Mesh
points = Mesh
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

intF :: Int32 -> Exp F Int32
intF = Const

intG :: Int32 -> Exp G Int32
intG = Const

v4FG :: V4F -> Exp G V4F
v4FG = Const

screenQuad :: Exp Obj (FrameBuffer 6 V4F)
screenQuad = Accumulate fragCtx PassAll frag rast clear
  where
    clear   = FrameBuffer (ColorImage n6 (V4 0 0.1 0.2 1):.ZT)
    fragCtx = AccumulationContext Nothing $ ColorOp NoBlending (one' :: V4B):.ZT
    rast    = Rasterize rCtx prims'
    rCtx    = triangleCtx--PointCtx ProgramPointSize 10 UpperLeft
    prims   = Transform vert input
    prims'  = Reassemble geom prims
    input   = Fetch "quadSlot" Triangles (IV2F "position")

    vert :: Exp V V2F -> VertexOut () ()
    vert uv = VertexOut v4 (Const 1) ZT ZT
      where
        v4      = pack' $ V4 u v (floatV 1) (floatV 1)
        V2 u v  = unpack' uv

    geom = GeometryShader n6 TrianglesOutput 18 funCnt funPrim funVert
      where
        --funCnt :: Exp G (V4F,Float,(),()) -> Exp G ((),Int32)
        funCnt a    = tup2 (s,intG 6)
          where
            s = tup2 (intG 0,a)
        funPrim s   = tup5 (layer, layer, tup2 (layer @+ intG 1,a), tup3 (c,intG 0,a), intG 3)
          where
            (layer,a) = untup2 s
            b = 0.7
            c0 = v4FG $ V4 b 0 0 1
            c1 = v4FG $ V4 0 b 0 1
            c2 = v4FG $ V4 0 0 b 1
            c3 = v4FG $ V4 b b 0 1
            c4 = v4FG $ V4 b 0 b 1
            c5 = v4FG $ V4 0 b b 1
            j n v o = Cond (layer @== intG n) v o
            c = j 0 c0 $ j 1 c1 $ j 2 c2 $ j 3 c3 $ j 4 c4 c5
        funVert st  = GeometryOut (tup3 (c,i @+ intG 1, a3)) p s ZT (Flat c:.ZT)
          where
            (c,i,a3) = untup3 st
            (a0,a1,a2) = untup3 a3
            (p,s,_,_) = untup4 $ Cond (i @== intG 0) a0 (Cond (i @== intG 1) a1 a2)

    frag :: Exp F V4F -> FragmentOut (Color V4F :+: ZZ)
    frag c = FragmentOut $ c :. ZT

screenQuad2 :: Exp Obj (FrameBuffer 1 V4F)
screenQuad2 = Accumulate fragCtx PassAll frag rast clear
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

    frag :: Exp F () -> FragmentOut (Color V4F :+: ZZ)
    frag _ = FragmentOut $ c :. ZT
      where
        V2 u v = unpack' pointCoord'
        c = Cond (primitiveID' @% intF 2 @== intF 0)
                (smp $ pack' $ V3 u v (floatF 0))
                (smp $ pack' $ V3 u v (floatF 1))
        smp uv = texture' (Sampler LinearFilter ClampToEdge tex) uv
        tex = Texture (Texture2D (Float RGBA) n6) (V2 128 128) NoMip [PrjFrameBuffer "" tix0 screenQuad]
        --tex = Texture (TextureCube (Float RGBA)) (V2 128 128) NoMip [PrjFrameBuffer "" tix0 screenQuad]

main :: IO ()
main = do
    let lcnet :: Exp Obj (Image 1 V4F)
        lcnet = PrjFrameBuffer "outFB" tix0 screenQuad2

    windowSize <- initCommon "LC DSL 2D Demo"

    renderer <- compileRenderer $ ScreenOut lcnet
    print $ slotUniform renderer
    print $ slotStream renderer
    print "renderer created"

    (mousePosition,mousePositionSink) <- external (0,0)
    (fblrPress,fblrPressSink) <- external (False,False,False,False,False)

    compiledPoints <- compileMesh points
    obj <- addMesh renderer "postSlot" compiledPoints []

    compiledQuad <- compileMesh quad
    obj <- addMesh renderer "quadSlot" compiledQuad []

    args <- getArgs
    let objU    = objectUniformSetter obj
        slotU   = uniformSetter renderer
        draw _  = render renderer >> swapBuffers

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
    let setupGFX (w,h) t' = do
            setSize (fromIntegral w) (fromIntegral h)
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
