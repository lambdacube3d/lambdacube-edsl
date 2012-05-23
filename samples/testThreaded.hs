{-# LANGUAGE OverloadedStrings, PackageImports #-}

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Control.Applicative hiding (Const)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Data.IORef
import Data.Vect
import Data.Vect.Float.Instances ()
import FRP.Elerea.Param
import qualified Data.ByteString.Char8 as SB
import qualified Data.Trie as T

import LCAPI
import LCLanguage

import Graphics.Rendering.OpenGL.Raw.Core32
import LCMesh

import qualified Criterion.Measurement as C

manVert :: Exp (V3F,V3F,V2F) -> Exp (V4F,V2F)
manVert = inlineFun (IM44F "worldViewProj", IFloat "time")
                    (IV3F "position", IV3F "normal", IV2F "UVTex")
                    (IV4F "gl_Position", IV2F "c") $ SB.pack $ unlines
    [ "mat4 m = worldViewProj;"
    , "vec3 n = (m * vec4(normal.xyz,0.0)).xyz;"
    , "c = UVTex;"
    , "gl_Position = m * vec4(position, 1.0) + vec4((0.05*sin(position.x+time*2.0))*n,1.0);"
    ]

manFrag :: Exp V2F -> Exp V3F
manFrag = inlineFun (IFloat "time")
                    (IV2F "c")
                    (IV3F "gl_FragColor") $ SB.pack $ unlines
    [ "gl_FragColor = vec4(fract(0.1*time),fract(c),1.0);"
    ]

hom4 :: Exp V3F -> Exp V4F
hom4 v = let V3 x y z = unpack' v in pack' $ V4 x y z (Const 1)

vert :: Exp (V3F,V3F,V2F) -> Exp (V4F, ())
vert pn = tup2 (v4, Const ())
  where
    worldViewProj = Uni (IM44F "worldViewProj")
    time  = cos' $ Uni (IFloat "time")
    v3    = p @* time
    v4    = worldViewProj @*. hom4 v3
    n4    = worldViewProj @*. hom4 n
    (p,n,_) = untup3 pn

frag :: Exp () -> Exp V3F
frag _ = Const (V3 1 0 0)

simple :: GP (PrimitiveStream p (V3F,V3F,V2F)) -> GP (FrameBuffer (RGBFormat Float) DepthFormat ())
simple objs = PaintColorRastDepth Less True NoBlending (RGBMask $ V3 True True True) frag (rast objs) clear
  where
    clear = NewFrameBufferColorDepth (RGB $ V3 0 0 1) 1000
    rast obj = RasterizeFront vert obj

simpleInline :: GP (PrimitiveStream p (V3F,V3F,V2F)) -> GP (FrameBuffer (RGBFormat Float) DepthFormat ())
simpleInline objs = PaintColorRastDepth Less True NoBlending (RGBMask $ V3 True True True) manFrag (rast objs) clear
  where
    clear = NewFrameBufferColorDepth (RGB $ V3 0 0 1) 1000
    rast obj = RasterizeFront manVert obj

main :: IO ()
main = do
    let lcnet :: GP (FrameBuffer (RGBFormat Float) DepthFormat ())
        lcnet = simple $ GPUStream "streamSlot" Triangle (IV3F "position", IV3F "normal", IV2F "UVTex")
        --lcnet = simpleInline $ GPUStream "streamSlot" Triangle (IV3F "position", IV3F "normal", IV2F "UVTex")

    windowSize <- initCommon "LC DSL Demo 2"

    (t,renderer) <- C.time $ compileRenderer lcnet
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
    (t,obj2) <- C.time $ addMesh renderer "streamSlot" mesh2 ["time"]
    putStrLn $ C.secs t ++ " - addMesh Scene.lcmesh"

    doneTVar <- newTVarIO False
    forkOS $ do
        let objU  = objectUniformSetter obj
            slotU = uniformSetter renderer
        sc <- start $ do
            u <- scene slotU objU windowSize mousePosition fblrPress
            v <- logFPS "FRP loop" =<< input
            let p = (,) <$> u <*> v
            return $ return <$> p
        driveNetwork (Just 100) sc (readInput mousePositionSink fblrPressSink)
        atomically $ writeTVar doneTVar True

    timeTVar <- newTVarIO =<< C.getTime
    let draw = do
            (t,_) <- C.time $ render renderer >> swapBuffers
            --putStrLn $ C.secs t ++ " - render frame"
            done <- readTVarIO doneTVar
            time <- C.getTime
            time0 <- readTVarIO timeTVar
            atomically $ writeTVar timeTVar time
            --putStrLn $ "time " ++ show time
            return $ if done then Nothing else Just $ realToFrac $ time - time0
    sc <- start $ do
        v <- controlFPS "render loop" 50 =<< input
        return $ return <$> v
    driveNetwork Nothing sc draw

    dispose renderer
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
                timeSetter time
                matSetter $! mat4ToM44F $! cm .*. pm
            --putStrLn $ C.secs t ++ " - worldViewProj uniform setup via STM"
            return ()
    r <- effectful3 setupGFX windowSize cam time
    return r

integral :: (Real p, Fractional t) => t -> Signal t -> SignalGen p (Signal t)
integral v0 s = transfer v0 (\dt v v0 -> v0+v*realToFrac dt) s

controlFPS :: String -> Float -> Signal Float -> SignalGen Float (Signal ())
controlFPS name fps dtime = do
    debug <- stateful (Nothing,0,0,False) (\t (_,i,t0,b) -> if t0 >= 5 then (Just (i+1,t+t0),1,t,True) else (Nothing,i+1,t+t0,False))
    -- http://en.wikipedia.org/wiki/PID_controller#Pseudocode
    -- TODO: tune PID - kp, ki, kd
    let kp = 0.1 -- 0.2
        ki = 0.7 -- 0.8
        kd = 0.2 -- 0.4
        setpoint = 1000000 / fps
        dt = pure 1000000 * dtime
        process_feedback = dt
        error = pure setpoint - process_feedback
    previous_error <- delay 1 error
    i <- integral 0 error
    let d = (error - previous_error) / dt
        output = (\a b c -> min 100000 $ max 0 (floor $ kp*a + ki*b + kd*c)) <$> error <*> i <*> d
        act usec (debug,_,_,loopEnded) = do
            --putStrLn $ name ++ " - " ++ show usec
            when loopEnded $ do
                let Just (count,loopTime) = debug
                putStrLn $ show (round $ fromIntegral count / loopTime) ++ " FPS - " ++ name
            threadDelay usec
    effectful2 act output debug

logFPS :: String -> Signal Float -> SignalGen Float (Signal ())
logFPS name dtime = do
    debug <- stateful (Nothing,0,0,False) (\t (_,i,t0,b) -> if t0 >= 5 then (Just (i+1,t+t0),1,t,True) else (Nothing,i+1,t+t0,False))
    let act (debug,_,_,loopEnded) = do
            when loopEnded $ do
                let Just (count,loopTime) = debug
                putStrLn $ show (round $ fromIntegral count / loopTime) ++ " FPS - " ++ name
    effectful1 act debug

vec4ToV4F :: Vec4 -> V4F
vec4ToV4F (Vec4 x y z w) = V4 x y z w

mat4ToM44F :: Mat4 -> M44F
mat4ToM44F (Mat4 a b c d) = V4 (vec4ToV4F a) (vec4ToV4F b) (vec4ToV4F c) (vec4ToV4F d)

readInput :: ((Float, Float) -> IO a)
          -> ((Bool, Bool, Bool, Bool, Bool) -> IO c)
          -> IO (Maybe Float)
readInput mousePos fblrPress = do
    t <- getTime
    resetTime

    (x,y) <- getMousePosition
    mousePos (fromIntegral x,fromIntegral y)

    fblrPress =<< ((,,,,) <$> keyIsPressed KeyLeft <*> keyIsPressed KeyUp <*> keyIsPressed KeyDown <*> keyIsPressed KeyRight <*> keyIsPressed KeyRightShift)

    k <- keyIsPressed KeyEsc
    return $ if k then Nothing else Just (realToFrac t)

-- FRP boilerplate
driveNetwork :: Maybe Double -> (p -> IO (IO a)) -> IO (Maybe p) -> IO ()
driveNetwork mfps network driver = do
    (t0,dt) <- C.time $ driver
    case dt of
        Just dt -> do
            (t1,_) <- C.time $ join $ network dt
            case mfps of
                Nothing  -> return ()
                Just fps -> threadDelay $! floor $! 1000000 * max 0 (1/fps - t0 - t1)
            driveNetwork mfps network driver
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
