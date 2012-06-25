{-# LANGUAGE OverloadedStrings, PackageImports #-}
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
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

type Sink a = a -> IO ()

data CameraMode = FollowNear | FollowFar | UserControl

lightPosition :: Vec3
lightPosition = Vec3 400 800 400

upwards :: Vec3
upwards = Vec3 0 1 0

main :: IO ()
main = do
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
    StuntsData carMesh wheels terrainMesh trackMesh startPos carSim <- readStuntsData carNum $ SB.pack trkFile

    -- setup graphics
    windowSize <- initCommon "Stunts NextGen powered by LambdaCube Engine"

    let gfxNet = PrjFrameBuffer "outFB" tix0 stuntsGFX
    renderer <- compileRenderer $ ScreenOut gfxNet
    let draw _ = render renderer >> swapBuffers

        quad :: Mesh
        quad = Mesh
            { mAttributes   = T.singleton "position" $ A_V2F $ V.fromList [V2 a b, V2 a a, V2 b a, V2 b a, V2 b b, V2 a b]
            , mPrimitive    = P_Triangles
            , mGPUData      = Nothing
            }
          where
            a = -1
            b = 1
    compiledQuad <- compileMesh quad
    addMesh renderer "postSlot" compiledQuad []
    {-
    forM_ (terrainMesh ++ trackMesh) $ \m -> do
        cm <- compileMesh m
        addMesh renderer "streamSlot" cm []
    -}
    cm <- compileMesh $ joinMesh $ terrainMesh ++ trackMesh
    addMesh renderer "streamSlot" cm []

    carUnis <- forM carMesh $ \m -> do
        cm <- compileMesh m
        objectUniformSetter <$> addMesh renderer "streamSlot" cm ["worldView", "worldPosition"]

    wheelsUnis <- forM wheels $ \(_,_,_,ml) -> forM ml $ \m -> do
        cm <- compileMesh m
        objectUniformSetter <$> addMesh renderer "streamSlot" cm ["worldView", "worldPosition"]

    -- setup physics
    physicsWorld <- mkPhysicsWorld
    addStaticPlane physicsWorld upwards 0 1 1
    addStaticShape physicsWorld trackMesh 1 1
    addStaticShape physicsWorld terrainMesh 1000 1000
    let (sO,Vec3 sX sY sZ) = startPos
    car <- addCar physicsWorld carMesh wheels $ translateAfter4 (Vec3 sX (sY + 1) sZ) $ rotMatrixProj4 sO upwards

    -- setup FRP network
    (mousePosition,mousePositionSink) <- external (0,0)
    (_mousePress,mousePressSink) <- external False
    (debugPress,debugPressSink) <- external False
    (fblrPress,fblrPressSink) <- external (False,False,False,False,False)
    (cameraPress,cameraPressSink) <- external (False,False,False)
    (carPos,carPosSink) <- external idmtx
    (wheelPos,wheelPosSink) <- external []

    s <- fpsState
    sc <- start $ do
        u <- scene carUnis wheelsUnis (uniformSetter renderer) physicsWorld carPos wheelPos windowSize mousePosition fblrPress cameraPress debugPress
        return $ draw <$> u

    driveNetwork sc (readInput physicsWorld car s carPosSink wheelPosSink
        mousePositionSink mousePressSink fblrPressSink
        cameraPressSink debugPressSink)

    dispose renderer
    closeWindow
    terminate

scene :: BtDynamicsWorldClass bc
      => [T.Trie InputSetter]
      -> [[T.Trie InputSetter]]
      -> T.Trie InputSetter
      -> bc
      -> Signal Proj4
      -> Signal [Proj4]
      -> Signal (Int, Int)
      -> Signal (Float, Float)
      -> Signal (Bool, Bool, Bool, Bool, Bool)
      -> Signal (Bool, Bool, Bool)
      -> Signal Bool
      -> SignalGen Float (Signal ())
scene carUnis wheelsUnis uniforms physicsWorld carPos wheelPos windowSize mousePosition fblrPress cameraPress debugPress = do
    time <- stateful 0 (+)
    frameCount <- stateful (0 :: Int) (\_ c -> c + 1)

    last2 <- transfer ((0,0),(0,0)) (\_ n (_,b) -> (b,n)) mousePosition
    let mouseMove = (\((ox,oy),(nx,ny)) -> (nx-ox,ny-oy)) <$> last2

        pickMode _ (True,_,_) _ = FollowNear
        pickMode _ (_,True,_) _ = FollowFar
        pickMode _ (_,_,True) _ = UserControl
        pickMode _ _ mode       = mode

        selectCam FollowNear  (cam,dir) _ _      = lookat cam (cam &+ dir) upwards
        selectCam FollowFar   _ (cam,dir) _      = lookat cam (cam &+ dir) upwards
        selectCam UserControl _ _ (cam,dir,up,_) = lookat cam (cam &+ dir) up

    followCamNear <- followCamera 2 4 6 carPos
    followCamFar <- followCamera 20 40 60 carPos
    userCam <- userCamera (Vec3 (-4) 0 0) mouseMove fblrPress
    camMode <- transfer FollowNear pickMode cameraPress
    let camera = selectCam <$> camMode <*> followCamNear <*> followCamFar <*> userCam

    let Just (SM44F cameraSetter) = T.lookup "worldView" uniforms
        Just (SM44F positionSetter) = T.lookup "worldPosition" uniforms
        Just (SM44F projectionSetter) = T.lookup "projection" uniforms
        lightViewProj = uniformM44F "lightViewProj" uniforms
        Just (SV3F lightPositionSetter) = T.lookup "lightPosition" uniforms
        carPositionMats = [s | u <- carUnis, let Just (SM44F s) = T.lookup "worldPosition" u]
        carViewMats = [s | u <- carUnis, let Just (SM44F s) = T.lookup "worldView" u]
        wheelsPositionU = [[s | u <- wu, let Just (SM44F s) = T.lookup "worldPosition" u] | wu <- wheelsUnis]
        wheelsViewU = [[s | u <- wu, let Just (SM44F s) = T.lookup "worldView" u] | wu <- wheelsUnis]
        setupGFX (w,h) cm carMat wheelsMats = do
            let pm = perspective 0.1 50000 (pi/2) (fromIntegral w / fromIntegral h)
                carPos = _4 (fromProjective carMat)
                lm = fromProjective (lookat lightPosition (trim carPos) upwards)
                lpm = perspective 0.1 50000 (pi/150) 1 {- .*. fromProjective (scaling (Vec3 (512 / fromIntegral w) (512 / fromIntegral h) 1)) -}

            lightPositionSetter $! vec3ToV3F $! lightPosition
            cameraSetter $! mat4ToM44F $! fromProjective cm
            positionSetter $! mat4ToM44F $! idmtx
            projectionSetter $! mat4ToM44F $! pm
            lightViewProj $! mat4ToM44F $! lm .*. lpm
            forM_ carPositionMats $ \s -> s $! mat4ToM44F $! fromProjective carMat
            forM_ carViewMats $ \s -> s $! mat4ToM44F $! fromProjective cm
            forM_ (zip wheelsPositionU wheelsMats) $ \(sl,wu) -> forM_ sl $ \s -> s $! mat4ToM44F $! fromProjective wu
            forM_ (zip wheelsViewU wheelsMats) $ \(sl,wu) -> forM_ sl $ \s -> s $! mat4ToM44F $! fromProjective cm
    
    effectful4 setupGFX windowSize camera carPos wheelPos

vec3ToV3F :: Vec3 -> V3F
vec3ToV3F (Vec3 x y z) = V3 x y z

vec4ToV4F :: Vec4 -> V4F
vec4ToV4F (Vec4 x y z w) = V4 x y z w

mat4ToM44F :: Mat4 -> M44F
mat4ToM44F (Mat4 a b c d) = V4 (vec4ToV4F a) (vec4ToV4F b) (vec4ToV4F c) (vec4ToV4F d)

readInput :: (BtDynamicsWorldClass dw, BtRaycastVehicleClass v)
          => dw
          -> v
          -> State
          -> Sink Proj4
          -> Sink [Proj4]
          -> Sink (Float, Float)
          -> Sink Bool
          -> Sink (Bool, Bool, Bool, Bool, Bool)
          -> Sink (Bool, Bool, Bool)
          -> Sink Bool
          -> IO (Maybe Float)
readInput physicsWorld car s carPos wheelPos mousePos mouseBut fblrPress cameraPress debugPress = do
    t <- getTime
    resetTime

    (x,y) <- getMousePosition
    mousePos (fromIntegral x,fromIntegral y)

    mouseBut =<< mouseButtonIsPressed MouseButton0
    fblrPress =<< (,,,,) <$> keyIsPressed KeyLeft <*> keyIsPressed KeyUp <*> keyIsPressed KeyDown <*> keyIsPressed KeyRight <*> keyIsPressed KeyRightShift
    cameraPress =<< (,,) <$> keyIsPressed (CharKey '1') <*> keyIsPressed (CharKey '2') <*> keyIsPressed (CharKey '3')
    debugPress =<< keyIsPressed KeySpace
    k <- keyIsPressed KeyEsc

    -- step physics
    let dt = realToFrac t
    steerCar dt car =<< forM "AWSDR" (\c -> keyIsPressed (CharKey c))
    btDynamicsWorld_stepSimulation physicsWorld dt 10 (1 / 200)
    wheelPos =<< updateCar car

    carPos =<< rigidBodyProj4 =<< btRaycastVehicle_getRigidBody car
    updateFPS s t
    return $ if k then Nothing else Just dt
