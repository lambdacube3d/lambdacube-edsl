{-# OPTIONS -cpp #-}
{-# LANGUAGE OverloadedStrings, PackageImports, TypeOperators, DataKinds #-}

import Control.Applicative hiding (Const)
import Control.Monad
import qualified Data.ByteString.Char8 as SB
import qualified Data.Trie as T
import Data.Vect
import Data.Vect.Float.Instances ()
import FRP.Elerea.Param
import "GLFW-b" Graphics.UI.GLFW as GLFW

import LambdaCube.GL
import LambdaCube.GL.Mesh

import Utils
import GraphicsUtils
import VSM

#ifdef CAPTURE
import Graphics.Rendering.OpenGL.Raw.Core32
import Codec.Image.DevIL
import Text.Printf
import Foreign

withFrameBuffer :: Int -> Int -> Int -> Int -> (Ptr Word8 -> IO ()) -> IO ()
withFrameBuffer x y w h fn = allocaBytes (w*h*4) $ \p -> do
    glReadPixels (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) gl_RGBA gl_UNSIGNED_BYTE $ castPtr p
    fn p
#endif

main :: IO ()
main = do
#ifdef CAPTURE
    ilInit
#endif
    
    let pipeline :: Exp Obj (Image 1 V4F)
        pipeline = PrjFrameBuffer "outFB" tix0 vsm

    windowSize <- initWindow "LambdaCube 3D Shadow Mapping Demo"

    (duration, renderer) <- measureDuration $ compileRenderer (ScreenOut pipeline)
    putStrLn $ "Renderer compiled - " ++ show duration
    
    putStrLn "Renderer uniform slots:"
    forM_ (T.toList (slotUniform renderer)) $ \(name, slot) -> do
        putStrLn $ "  " ++ SB.unpack name
        forM_ (T.toList slot) $ \(inputName, inputType) -> do
            putStrLn $ "    " ++ SB.unpack inputName ++ " :: " ++ show inputType
    
    putStrLn "Renderer stream slots:"
    forM_ (T.toList (slotStream renderer)) $ \(name, (primitive, attributes)) -> do
        putStrLn $ "  " ++ SB.unpack name ++ " - " ++ show primitive
        forM_ (T.toList attributes) $ \(attributeName, attributeType) -> do
            putStrLn $ "    " ++ SB.unpack attributeName ++ " :: " ++ show attributeType

    quadMesh <- compileMesh quad
    addMesh renderer "postSlot" quadMesh []

    cubeMesh <- compileMesh (cube 1)
    
    (duration, cubeObjects) <- measureDuration $ replicateM 6 $ addMesh renderer "geometrySlot" cubeMesh ["modelMatrix"]
    putStrLn $ "Cube meshes added - " ++ show duration

    let objectSlots = map objectUniformSetter cubeObjects
        sceneSlots = uniformSetter renderer

        draw command = do
            render renderer
            command
            swapBuffers

    sceneSignal <- start $ do
        thread <- scene (setScreenSize renderer) sceneSlots objectSlots windowSize
        return $ draw <$> thread
    driveNetwork sceneSignal readInput

    dispose renderer
    putStrLn "Renderer destroyed."

    closeWindow

scene setSize sceneSlots (planeSlot:cubeSlots) windowSize = do
    pause <- toggle =<< risingEdge =<< effectful (keyIsPressed (CharKey 'P'))
    time <- transfer 0 (\dt paused time -> time + if paused then 0 else dt) pause 
    
    capture <- risingEdge =<< effectful (keyIsPressed (CharKey 'C'))
    frameCount <- stateful (0 :: Int) (const (+1))
    
    fpsTracking <- stateful (0, 0, Nothing) $ \dt (time, count, _) -> 
        let time' = time + dt
            done = time > 5
        in if done
           then (0, 0, Just (count / time'))
           else (time', count + 1, Nothing)

    mousePosition <- effectful $ do
        (x, y) <- getMousePosition
        return $ Vec2 (fromIntegral x) (fromIntegral y)
    directionControl <- effectful $ (,,,,)
                 <$> keyIsPressed KeyLeft
                 <*> keyIsPressed KeyUp
                 <*> keyIsPressed KeyDown
                 <*> keyIsPressed KeyRight
                 <*> keyIsPressed KeyRightShift
    
    mousePosition' <- delay zero mousePosition
    camera <- userCamera (Vec3 (-4) 0 0) (mousePosition - mousePosition') directionControl
    
    let setCameraMatrix = uniformM44F "cameraMatrix" sceneSlots . fromMat4
        setLightMatrix = uniformM44F "lightMatrix" sceneSlots . fromMat4
        setLightPosition = uniformV3F "lightPosition" sceneSlots . fromVec3
        setPlaneModelMatrix = uniformM44F "modelMatrix" planeSlot . fromMat4
        setCubeModelMatrices = [uniformM44F "modelMatrix" cubeSlot . fromMat4 | cubeSlot <- cubeSlots]
        
        setupRendering ((_, _, fps), frameCount, capture) (windowWidth, windowHeight) (cameraPosition, cameraDirection, cameraUp, _) time = do
            let aspect = fromIntegral windowWidth / fromIntegral windowHeight
                
                cameraView = fromProjective (lookat cameraPosition (cameraPosition &+ cameraDirection) cameraUp)
                cameraProjection = perspective 0.1 50 (pi/2) aspect

                lightPosition = Vec3 (5 * sin time) 2 10
                lightDirection = Vec3 0 (-0.2) (-1)
                lightUp = Vec3 0 1 0
                
                lightView = fromProjective (lookat lightPosition (lightPosition &+ lightDirection) lightUp)
                lightProjection = perspective 0.1 100 (pi/2) aspect
            
            case fps of
                Just value -> putStrLn $ "FPS: " ++ show value
                Nothing -> return ()
            
            setCameraMatrix (cameraView .*. cameraProjection)
            setLightMatrix (lightView .*. lightProjection)
            setLightPosition lightPosition
            
            setPlaneModelMatrix (fromProjective $ scaling (Vec3 12 12 1) .*. translation (Vec3 0 (-2) (-12)))
            forM_ (zip setCubeModelMatrices [0..]) $ \(setCubeModelMatrix, i) -> do
                let t = i * 2 * pi / 5
                    s = (t + 2) * 0.3
                    trans = scaling (Vec3 s s s) .*. rotationEuler (Vec3 0 0 s) .*. translation (Vec3 (t * 0.3) (sin t * 4) (cos t * 4))
                setCubeModelMatrix (fromProjective trans)
            setSize (fromIntegral windowWidth) (fromIntegral windowHeight)
            
            return $ do
#ifdef CAPTURE
                when capture $ do
                    glFinish
                    withFrameBuffer 0 0 windowWidth windowHeight $ writeImageFromPtr (printf "frame%08d.jpg" frameCount) (windowHeight, windowWidth)
#endif
                return ()
            
    
    effectful4 setupRendering ((,,) <$> fpsTracking <*> frameCount <*> capture) windowSize camera time

readInput :: IO (Maybe Float)
readInput = do
    t <- getTime
    resetTime

    k <- keyIsPressed KeyEsc
    return $ if k then Nothing else Just (realToFrac t)
