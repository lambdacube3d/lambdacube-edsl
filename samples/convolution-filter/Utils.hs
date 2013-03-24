{-# LANGUAGE PackageImports #-}

module Utils where

import Data.Time.Clock
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.Raw.Core32 (glViewport)

windowWidth, windowHeight :: Num a => a
windowWidth = 512
windowHeight = 512

measureDuration :: IO a -> IO (NominalDiffTime, a)
measureDuration action = do
    startTime <- getCurrentTime
    result <- action
    endTime <- getCurrentTime
    return (diffUTCTime endTime startTime, result)

initWindow :: String -> IO ()
initWindow title = do
    initialize
    openWindow defaultDisplayOptions
        { displayOptions_numRedBits         = 8
        , displayOptions_numGreenBits       = 8
        , displayOptions_numBlueBits        = 8
        , displayOptions_numAlphaBits       = 8
        , displayOptions_numDepthBits       = 24
        , displayOptions_width              = windowWidth
        , displayOptions_height             = windowHeight
        , displayOptions_windowIsResizable  = False -- because we cannot resize the render texture on the fly
        , displayOptions_openGLVersion      = (3,2)
        , displayOptions_openGLProfile      = CoreProfile
--        , displayOptions_openGLForwardCompatible = True
--        , displayOptions_displayMode    = Fullscreen
        }
    setWindowTitle title
    setWindowSizeCallback $ \w h -> do
        glViewport 0 0 (fromIntegral w) (fromIntegral h)


