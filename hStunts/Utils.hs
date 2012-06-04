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
        , displayOptions_width          = 512
        , displayOptions_height         = 512
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
