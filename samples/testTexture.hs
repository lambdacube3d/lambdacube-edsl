{-# LANGUAGE OverloadedStrings, PackageImports, TypeOperators #-}

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Control.Applicative hiding (Const)
import Control.Concurrent.STM
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Data.IORef
import Data.Vect
import Data.Vect.Float.Instances ()
import FRP.Elerea.Param
import qualified Data.ByteString.Char8 as SB
import qualified Data.Trie as T

import TypeLevel.Number.Nat.Num

import LCAPI
import LCLanguage
import LCGL

import Graphics.Rendering.OpenGL.Raw.Core32
import LCMesh

import qualified Criterion.Measurement as C

import VSM

simple :: GP (FrameBuffer N1 (Float,V4F))
simple = Accumulate fragCtx PassAll frag rast clear
  where
    fragCtx = DepthOp Less True:.ColorOp NoBlending (one' :: V4B):.ZT
    clear   = FrameBuffer (V2 640 480) (DepthImage n1 1000:.ColorImage n1 (zero'::V4F):.ZT)
    rast    = Rasterize triangleCtx NoGeometryShader prims
    prims   = Transform vert input
    input   = Fetch "streamSlot" Triangle (IV3F "position", IV3F "normal")
    worldViewProj = Uni (IM44F "worldViewProj")

    vert :: Exp V (V3F,V3F) -> VertexOut V3F
    vert pn = VertexOut v4 (Const 1) (Flat (drop4 v4):.ZT)
      where
        v4    = worldViewProj @*. snoc p 1
        (p,n) = untup2 pn

    frag :: Exp F V3F -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    frag a = FragmentOutRastDepth $ snoc c 1 :. ZT
      where
        c = texture' sampler (drop3 a) (Const 0)

    sampler = Sampler LinearFilter Clamp diffuse
    diffuse = TextureSlot "diffuse" $ Texture2D (Float RGB) n1

main :: IO ()
main = do
    let lcnet :: GP (Image N1 V4F)
        lcnet = PrjFrameBuffer "outFB" tix0 simple

    windowSize <- initCommon "LC DSL Texture Demo"

    (t,renderer) <- C.time $ compileRenderer [ScreenOut lcnet]
    putStrLn $ C.secs t ++ " - compile renderer"
    print $ slotUniform renderer
    print $ slotStream renderer
    print "renderer created"

    (mousePosition,mousePositionSink) <- external (0,0)
    (fblrPress,fblrPressSink) <- external (False,False,False,False,False)

    (t,mesh) <- C.time $ loadMesh "Monkey.lcmesh"
    putStrLn $ C.secs t ++ " - loadMesh Monkey.lcmesh"
    (t,mesh2) <- C.time $ loadMesh "Scene.lcmesh"
    putStrLn $ C.secs t ++ " - loadMesh Scene.lcmesh"
    (t,obj) <- C.time $ addMesh renderer "streamSlot" mesh []
    putStrLn $ C.secs t ++ " - addMesh Monkey.lcmesh"
    (t,obj2) <- C.time $ addMesh renderer "streamSlot" mesh2 []
    putStrLn $ C.secs t ++ " - addMesh Scene.lcmesh"

    -- TODO:
    -- texture specification:
    --  create buffer with image data
    --  create Texture type from uploaded image data
    --      idea: we should reuse TexSizeRepr for data specification
    --  set sampler uniform to required texture
{-
  alternative A:
    pixelBuffer <- compileBuffer $
        [ Array ArrWord8 (3 * width * height) imagePixelData0
        , Array ArrWord8 (3 * width * height) imagePixelData1
        ]
    texture <- compileTexture $ TextureData (Texture2D (Float RGB) N1) (V2 128 128) Mip $
        [ ImageData pixelBuffer 0 -- mip levels
        , ImageData pixelBuffer 1
        ]
  alternative B:
    texture <- compileTexture $ TextureData (Texture2D (Float RGB) N1) (V2 128 128) Mip $
        [ Array ArrWord8 (3 * width * height) imagePixelData0
        , Array ArrWord8 (3 * width * height) imagePixelData1
        ]
  alternative C:
    texture <- compileTexture $ TextureData (Texture2D (Float RGB) N1) (V2 128 128) Mip
    updateTexture texture $
        [ Array ArrWord8 (3 * width * height) imagePixelData0
        , Array ArrWord8 (3 * width * height) imagePixelData1
        ]
-}

    let objU  = objectUniformSetter obj
        slotU = uniformSetter renderer
        draw _ = do
            (t,_) <- C.time $ render renderer >> swapBuffers
            --putStrLn $ C.secs t ++ " - render frame"
            return ()
    s <- fpsState
    sc <- start $ do
        u <- scene slotU objU windowSize mousePosition fblrPress
        return $ draw <$> u
    driveNetwork sc (readInput s mousePositionSink fblrPressSink)

    finalize renderer
    print "renderer destroyed"
    closeWindow

scene :: T.Trie InputSetter
      -> T.Trie InputSetter
      -> Signal (Int, Int)
      -> Signal (Float, Float)
      -> Signal (Bool, Bool, Bool, Bool, Bool)
      -> SignalGen Float (Signal ())
scene slotU objU windowSize mousePosition fblrPress = do
    time <- stateful 0 (+)
    last2 <- transfer ((0,0),(0,0)) (\_ n (_,b) -> (b,n)) mousePosition
    let mouseMove = (\((ox,oy),(nx,ny)) -> (nx-ox,ny-oy)) <$> last2
    cam <- userCamera (Vec3 (-4) 0 0) mouseMove fblrPress
    let Just (SM44F matSetter) = T.lookup "worldViewProj" slotU
        Just (SFloat timeSetter) = T.lookup "time" slotU
        setupGFX (w,h) (cam,dir,up,_) time = do
            let cm = fromProjective (lookat cam (cam + dir) up)
                pm = perspective 0.1 50 (pi/2) (fromIntegral w / fromIntegral h)
            (t,_) <- C.time $ atomically $ do
                --timeSetter time
                matSetter $! mat4ToM44F $! cm .*. pm
            --putStrLn $ C.secs t ++ " - worldViewProj uniform setup via STM"
            return ()
    r <- effectful3 setupGFX windowSize cam time
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
            (t,_) <- C.time $ join $ network dt
            --putStrLn $ C.secs t ++ " - FRP loop"
            --putStrLn ""
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
--        , displayOptions_width              = 1280
--        , displayOptions_height             = 720
        , displayOptions_windowIsResizable  = True
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
    putStrLn (show (round fps) ++ " FPS - " ++ show f ++ " frames in " ++ C.secs seconds)
    writeIORef tR 0
    writeIORef fR 0

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
        k   = if turbo then 5 else 1
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
rotationEuler (Vec3 a b c) = orthogonal $ toOrthoUnsafe $ rotMatrixY a .*. rotMatrixX b .*. rotMatrixZ c

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
