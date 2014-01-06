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

initWindow :: String -> Int -> Int -> IO (Signal (Int, Int))
initWindow title width height = do
    initialize
    openWindow defaultDisplayOptions
        { displayOptions_numRedBits         = 8
        , displayOptions_numGreenBits       = 8
        , displayOptions_numBlueBits        = 8
        , displayOptions_numAlphaBits       = 8
        , displayOptions_numDepthBits       = 24
        , displayOptions_width              = width
        , displayOptions_height             = height
        , displayOptions_windowIsResizable  = True
        , displayOptions_openGLVersion      = (3,2)
        , displayOptions_openGLProfile      = CoreProfile
--        , displayOptions_openGLForwardCompatible = True
--        , displayOptions_displayMode    = Fullscreen
        }
    setWindowTitle title

    (windowSize, windowSizeSink) <- external (0, 0)
    setWindowSizeCallback $ \w h -> do
        glViewport 0 0 (fromIntegral w) (fromIntegral h)
        windowSizeSink (fromIntegral w, fromIntegral h)

    return windowSize

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
