{-# LANGUAGE PackageImports #-}

module Common.Utils where

import Control.Applicative
import Control.Monad
import Data.Time.Clock
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.Raw.Core32 (glViewport)
import FRP.Elerea.Param

measureDuration :: IO a -> IO (NominalDiffTime, a)
measureDuration action = do
    startTime <- getCurrentTime
    result <- action
    endTime <- getCurrentTime
    return (diffUTCTime endTime startTime, result)

initWindow :: String -> Int -> Int -> IO (Window,Signal (Int, Int))
initWindow title width height = do
    GLFW.init
    defaultWindowHints
    mapM_ windowHint
      [ WindowHint'ContextVersionMajor 3
      , WindowHint'ContextVersionMinor 3
      , WindowHint'OpenGLProfile OpenGLProfile'Core
      , WindowHint'OpenGLForwardCompat True
      ]
    Just win <- createWindow width height title Nothing Nothing
    makeContextCurrent $ Just win

    (windowSize, windowSizeSink) <- external (width, height)
    setWindowSizeCallback win $ Just $ \_ w h -> do
        glViewport 0 0 (fromIntegral w) (fromIntegral h)
        windowSizeSink (fromIntegral w, fromIntegral h)

    return (win,windowSize)

driveNetwork :: (p -> IO (IO a)) -> IO (Maybe p) -> IO ()
driveNetwork network driver = do
    dt <- driver
    case dt of
        Just dt -> do
            join (network dt)
            driveNetwork network driver
        Nothing -> return ()

risingEdge :: Signal Bool -> SignalGen p (Signal Bool)
risingEdge signal = do
    signal' <- delay True signal
    memo $ liftA2 (&&) signal (not <$> signal') 

toggle :: Signal Bool -> SignalGen p (Signal Bool)
toggle = transfer False (const (/=))
