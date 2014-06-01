{-# LANGUAGE OverloadedStrings, PackageImports #-}
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent
import Data.Word
import Data.IORef
import Data.List hiding (transpose)
import FRP.Elerea.Param
import "GLFW-b" Graphics.UI.GLFW as GLFW

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as SB

import qualified Data.Vector.Storable as V
import qualified Data.Trie as T
--import Control.Concurrent.STM
import Data.Vect.Float

import LC_API
--import LCGL
import LC_Mesh

import Physics.Bullet.Raw
import Physics.Bullet.Raw.Class

import System.Directory
import System.Environment
import System.Exit

import GameData
import GamePhysics
import GameGraphics
import Utils
import MeshUtil

import qualified Graphics.Rasterific as R
import qualified Graphics.Rasterific.Texture as R

#ifdef CAPTURE
import Graphics.Rendering.OpenGL.Raw.Core32
import Codec.Image.DevIL
import Text.Printf
import Foreign
#endif

import Graphics.Text.TrueType( loadFontFile, Font )
import Codec.Picture( PixelRGBA8( .. ), writePng, Image(..) )

type Sink a = a -> IO ()

data CameraMode = FollowNear | FollowFar | UserControl

lightPosition :: Vec3
lightPosition = Vec3 400 800 400

#ifdef CAPTURE
-- framebuffer capture function
withFrameBuffer :: Int -> Int -> Int -> Int -> (Ptr Word8 -> IO ()) -> IO ()
withFrameBuffer x y w h fn = allocaBytes (w*h*4) $ \p -> do
    glReadPixels (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) gl_RGBA gl_UNSIGNED_BYTE $ castPtr p
    fn p
#endif

captureRate :: Float
captureRate = 30

main :: IO ()
main = do
#ifdef CAPTURE
    ilInit
#endif
    
    let mediaPath = "."

    gameOk <- doesFileExist (mediaPath ++ "/STUNTS11.ZIP")
    unless gameOk $ do
        putStrLn "Missing game file! Please download the original game from"
        putStrLn "<http://downloads.pigsgrame.de/STUNTS11.ZIP> and move it to"
        putStrLn "the same folder as this executable."
        putStrLn "For reference, the above file should be 1077864 bytes."
        exitFailure

    args <- getArgs

    let carNum = case filter ("--car=" `isPrefixOf`) args of
            [] -> 4
            n:_ -> read (drop 6 n)

        trkFile = case filter ("--track=" `isPrefixOf`) args of
            [] -> "zct114.trk"
            n:_ -> drop 8 n

    -- load game data
    StuntsData carsData terrainMesh trackMesh startPos <- readStuntsData carNum $ SB.pack trkFile

    -- setup graphics
    windowSize <- initCommon "Stunts NextGen powered by LambdaCube Engine"

    Right font <- loadFontFile "ProstoOne-Regular.ttf"
    cpuDrawThread <- newIORef True

    let gfxNet = PrjFrameBuffer "outFB" tix0 stuntsGFX
    renderer <- compileRenderer $ ScreenOut $ addHUD "timeTexture" $ addHUD "titleTexture" gfxNet
    let draw captureA = render renderer >> captureA >> swapBuffers

        quad :: Mesh
        quad = Mesh
            { mAttributes   = T.singleton "position" $ A_V2F $ V.fromList [V2 a b, V2 a a, V2 b a, V2 b a, V2 b b, V2 a b]
            , mPrimitive    = P_Triangles
            , mGPUData      = Nothing
            }
          where
            a = -1
            b = 1

        unis = uniformSetter renderer

    uniformFTexture2D "titleTexture" unis =<< renderText 512 512 50 0 font 32 "Stunts"
    --titleSetter =<< renderText font 32 "Stunts"

    compiledQuad <- compileMesh quad
    addMesh renderer "postSlot" compiledQuad []
    addMesh renderer "ScreenQuad" compiledQuad []
    {-
    forM_ (terrainMesh ++ trackMesh) $ \m -> do
        cm <- compileMesh m
        addMesh renderer "streamSlot" cm []
    -}
    cm <- compileMesh $ joinMesh $ terrainMesh ++ trackMesh
    addMesh renderer "streamSlot" cm []

    carUnis <- forM (map carMesh carsData) $ mapM $ \m -> do
        cm <- compileMesh m
        objectUniformSetter <$> addMesh renderer "streamSlot" cm ["worldView", "worldPosition"]

    wheelsUnis <- forM (map wheels carsData) $ mapM $ \(_,_,_,ml) -> forM ml $ \m -> do
        cm <- compileMesh m
        objectUniformSetter <$> addMesh renderer "streamSlot" cm ["worldView", "worldPosition"]

    -- setup physics
    physicsWorld <- mkPhysicsWorld
    addStaticPlane physicsWorld upwards 0 1 1
    addStaticShape physicsWorld trackMesh 1 1
    addStaticShape physicsWorld terrainMesh 1000 1000
    let (sO,Vec3 sX sY sZ) = startPos
    raycastVehicles <- forM carsData $ \carData ->
                       createCar physicsWorld (carMesh carData) (wheels carData) $ translateAfter4 (Vec3 sX (sY + 1) sZ) $ rotMatrixProj4 sO upwards
    let cars = zipWith3 Car raycastVehicles carUnis wheelsUnis

    -- setup FRP network
    (mousePosition,mousePositionSink) <- external (0,0)
    (_mousePress,mousePressSink) <- external False
    (debugPress,debugPressSink) <- external False
    (fblrPress,fblrPressSink) <- external (False,False,False,False,False)
    (cameraPress,cameraPressSink) <- external (False,False,False)
    (capturePress,capturePressSink) <- external False
    (carSwitchPress, carSwitchPressSink) <- external False
    (carInputPress, carInputPressSink) <- external $ replicate 5 False

    capRef <- newIORef False
    s <- fpsState
    sc <- start $ do
        u <- cpuDrawThread font scene (setScreenSize renderer) cars (uniformSetter renderer) physicsWorld windowSize mousePosition fblrPress carInputPress cameraPress capturePress carSwitchPress debugPress capRef
        return $ draw <$> u

    driveNetwork sc (readInput physicsWorld s
        mousePositionSink mousePressSink fblrPressSink carInputPressSink
        cameraPressSink capturePressSink carSwitchPressSink debugPressSink capRef)

    dispose renderer
    closeWindow
    terminate

data Car bc = Car
    { raycastVehicle :: bc
    , carUnis :: [T.Trie InputSetter]
    , wheelsUnis :: [[T.Trie InputSetter]]
    }

scene :: (BtDynamicsWorldClass bc,
          BtRaycastVehicleClass v)
      => (Word -> Word -> IO ())
      -> [Car v]
      -> IORef Bool
      -> Font
      -> [[T.Trie InputSetter]]
      -> bc
      -> Signal (Int, Int)
      -> Signal (Float, Float)
      -> Signal (Bool, Bool, Bool, Bool, Bool)
      -> Signal [Bool]
      -> Signal (Bool, Bool, Bool)
      -> Signal Bool
      -> Signal Bool
      -> Signal Bool
      -> IORef Bool
      -> SignalGen Float (Signal (IO ()))
scene setSize cars cpuDrawThread font uniforms physicsWorld windowSize mousePosition fblrPress carInputPress cameraPress capturePress carSwitchPress debugPress capRef = do
    isFirstFrame <- stateful True $ const $ const False
    carId <- transfer2 (Nothing, 10) (\_ isFirstFrame isPressed (_, prev) ->
                         if isPressed || isFirstFrame
                         then (Just prev, (prev + 1) `mod` 11)
                         else (Nothing, prev))
             isFirstFrame
             =<< edge carSwitchPress
    time <- stateful 0 (+)
    frameCount <- stateful (0 :: Int) (\_ c -> c + 1)

    capture <- transfer2 False (\_ cap cap' on -> on /= (cap && not cap')) capturePress =<< delay False capturePress

    last2 <- transfer ((0,0),(0,0)) (\_ n (_,b) -> (b,n)) mousePosition
    let mouseMove = (\((ox,oy),(nx,ny)) -> (nx-ox,ny-oy)) <$> last2

        pickMode _ (True,_,_) _ = FollowNear
        pickMode _ (_,True,_) _ = FollowFar
        pickMode _ (_,_,True) _ = UserControl
        pickMode _ _ mode       = mode

        selectCam FollowNear  (cam,dir) _ _      = lookat cam (cam &+ dir) upwards
        selectCam FollowFar   _ (cam,dir) _      = lookat cam (cam &+ dir) upwards
        selectCam UserControl _ _ (cam,dir,up,_) = lookat cam (cam &+ dir) up

    dt <- input
    carAndWheelsPos <- (\f -> effectful4 f carInputPress dt carId isFirstFrame) $ \carInput dt (prevId, currId) isFirstFrame -> do
        let vehicle = raycastVehicle car
            car = cars !! currId
        case prevId of
          Nothing -> return ()
          Just prevId -> do
              let prevCar = cars !! prevId
              state <- getCarMotionState $ raycastVehicle prevCar
              removeCar physicsWorld $ raycastVehicle prevCar
              addCar physicsWorld $ raycastVehicle car
              when (not isFirstFrame) $ do
                  setCarMotionState (raycastVehicle car) state
        steerCar dt vehicle carInput
        btDynamicsWorld_stepSimulation physicsWorld dt 10 (1 / 200)
        wheelsMat <- updateCar vehicle
        carMat <- rigidBodyProj4 =<< btRaycastVehicle_getRigidBody vehicle
        return (carMat, wheelsMat)
    let carPos = fst <$> carAndWheelsPos

    followCamNear <- followCamera 2 4 6 carPos
    followCamFar <- followCamera 20 40 60 carPos
    userCam <- userCamera (Vec3 (-4) 0 0) mouseMove fblrPress
    camMode <- transfer FollowNear pickMode cameraPress
    let camera = selectCam <$> camMode <*> followCamNear <*> followCamFar <*> userCam

    let Just (SM44F worldViewSetter) = T.lookup "worldView" uniforms
        Just (SM44F positionSetter) = T.lookup "worldPosition" uniforms
        Just (SM44F projectionSetter) = T.lookup "projection" uniforms
        Just (SV3F lightDirectionSetter) = T.lookup "lightDirection" uniforms
        setupGFX ((w, h), capturing, frameCount, dt, t) worldViewMat (prevCarId, carId) (carMat, wheelsMats) = do

            let car = cars !! carId
                fieldOfView = pi/2
                aspectRatio = fromIntegral w / fromIntegral h
                projection nearDepth farDepth = perspective nearDepth farDepth fieldOfView aspectRatio
                carPositionMats car = [s | u <- carUnis car, let Just (SM44F s) = T.lookup "worldPosition" u]
                carViewMats car = [s | u <- carUnis car, let Just (SM44F s) = T.lookup "worldView" u]
                wheelsPositionU car = [[s | u <- wu, let Just (SM44F s) = T.lookup "worldPosition" u] | wu <- wheelsUnis car]
                wheelsViewU car = [[s | u <- wu, let Just (SM44F s) = T.lookup "worldView" u] | wu <- wheelsUnis car]
            
            lightDirectionSetter $! vec3ToV3F $! lightDirection
            worldViewSetter $! mat4ToM44F $! fromProjective worldViewMat
            positionSetter $! mat4ToM44F $! idmtx
            projectionSetter $! mat4ToM44F $! projection 0.1 50000
            forM_ (zip3 lightFrustumSlices (tail lightFrustumSlices) [1..]) $ \(near, far, slice) -> do
                let (lightViewProj, scale) = lightProjection near far fieldOfView aspectRatio worldViewMat
                uniformFloat (SB.pack ("gridThickness" ++ show slice)) uniforms $! gridThickness slice
                uniformV3F (SB.pack ("lightViewScale" ++ show slice)) uniforms $! vec3ToV3F scale
                uniformM44F (SB.pack ("lightViewProj" ++ show slice)) uniforms $! mat4ToM44F $! fromProjective lightViewProj

            let setView car worldViewMat = do
                  forM_ (carViewMats car) $ \s -> s $! mat4ToM44F $! fromProjective worldViewMat
                  forM_ (zip (wheelsViewU car) wheelsMats) $ \(sl,wu) -> forM_ sl $ \s -> s $! mat4ToM44F $! fromProjective worldViewMat

            forM_ (carPositionMats car) $ \s -> s $! mat4ToM44F $! fromProjective carMat 
            forM_ (zip (wheelsPositionU car) wheelsMats) $ \(sl,wu) -> forM_ sl $ \s -> s $! mat4ToM44F $! fromProjective wu

            setView car worldViewMat
            case prevCarId of
              Nothing -> return ()
              Just prevCarId -> do
                  let car = cars !! prevCarId
                  setView car $ scaling zero
                  forM_ (carPositionMats car) $ \s -> s $! mat4ToM44F $! zero
                  forM_ (zip (wheelsPositionU car) wheelsMats) $ \(sl,wu) -> forM_ sl $ \s -> s $! mat4ToM44F $! zero

            setSize (fromIntegral w) (fromIntegral h)
            
            --uniformFloat "time" uniforms t
            done <- readIORef cpuDrawThread
            when done $ do
                writeIORef cpuDrawThread False
                forkIO $ do
                    let hud = R.renderDrawing 512 512 (PixelRGBA8 0 0 0 0) $ do
                                R.withTexture (R.uniformTexture $ PixelRGBA8 255 0 0 255) $ do
                                    R.printTextAt font 36 (R.V2 250 0) (show t)
                    uniformFTexture2D "timeTexture" uniforms =<< compileImageToTexture2DRGBAF False True hud
                    threadDelay 100000
                    writeIORef cpuDrawThread True
                return ()
            return $ do
#ifdef CAPTURE
                when capturing $ do
                    glFinish
                    withFrameBuffer 0 0 w h $ \p -> writeImageFromPtr (printf "frame%08d.jpg" frameCount) (h,w) p
                writeIORef capRef capturing
#endif
                return ()
    effectful4 setupGFX
                   ((,,,,) <$> windowSize <*> capture <*> frameCount <*> dt <*> time)
                   camera
                   carId
                   carAndWheelsPos

vec3ToV3F :: Vec3 -> V3F
vec3ToV3F (Vec3 x y z) = V3 x y z

vec4ToV4F :: Vec4 -> V4F
vec4ToV4F (Vec4 x y z w) = V4 x y z w

mat4ToM44F :: Mat4 -> M44F
mat4ToM44F (Mat4 a b c d) = V4 (vec4ToV4F a) (vec4ToV4F b) (vec4ToV4F c) (vec4ToV4F d)

readInput :: (BtDynamicsWorldClass dw)
          => dw
          -> State
          -> Sink (Float, Float)
          -> Sink Bool
          -> Sink (Bool, Bool, Bool, Bool, Bool)
          -> Sink [Bool]
          -> Sink (Bool, Bool, Bool)
          -> Sink Bool
          -> Sink Bool
          -> Sink Bool
          -> IORef Bool
          -> IO (Maybe Float)
readInput physicsWorld s mousePos mouseBut fblrPress carInputPress cameraPress capturePress carSwitchPress debugPress capRef = do
    t <- getTime
    resetTime

    (x,y) <- getMousePosition
    mousePos (fromIntegral x,fromIntegral y)

    mouseBut =<< mouseButtonIsPressed MouseButton0
    fblrPress =<< (,,,,) <$> keyIsPressed KeyLeft <*> keyIsPressed KeyUp <*> keyIsPressed KeyDown <*> keyIsPressed KeyRight <*> keyIsPressed KeyRightShift
    cameraPress =<< (,,) <$> keyIsPressed (CharKey '1') <*> keyIsPressed (CharKey '2') <*> keyIsPressed (CharKey '3')
    debugPress =<< keyIsPressed KeySpace
    capturePress =<< keyIsPressed (CharKey 'P')
    carSwitchPress =<< keyIsPressed (CharKey 'E')
    k <- keyIsPressed KeyEsc
    carInputPress =<< forM "AWSDR" (\c -> keyIsPressed (CharKey c))

    -- step physics
    isCapturing <- readIORef capRef
    let dt = if isCapturing then recip captureRate else realToFrac t

    updateFPS s t
    return $ if k then Nothing else Just dt
