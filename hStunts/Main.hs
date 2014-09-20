{-# LANGUAGE OverloadedStrings, PackageImports #-}
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent
import Data.Word
import Data.Maybe
import Data.IORef
import Data.List hiding (transpose)
import FRP.Elerea.Param
import "GLFW-b" Graphics.UI.GLFW as GLFW

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as SB

import qualified Data.Vector as VG
import qualified Data.Vector.Storable as V
import qualified Data.Trie as T
--import Control.Concurrent.STM
import Data.Vect.Float

import LambdaCube.GL
import LambdaCube.GL.Mesh

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
import Text.Printf

import Graphics.Text.TrueType( decodeFont, Font )
import Codec.Picture( PixelRGBA8( .. ), writePng, Image(..) )
import qualified Codec.Picture as JP
import Stunts.Loader(Bitmap(..))
import qualified Stunts.Loader as L

type Sink a = a -> IO ()

data CameraMode = FollowNear | FollowFar | UserControl | InsideCar deriving (Eq,Ord)

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

        retroMode = not $ null $ filter ("--retro" `isPrefixOf`) args

    -- load game data
    StuntsData carsData terrainMesh trackMesh startPos <- readStuntsData carNum $ SB.pack trkFile

    -- setup graphics
    windowSize <- initCommon "Stunts NextGen powered by LambdaCube Engine"

    let Right font1 = decodeFont $ readZipFile "Prototype.ttf"
        Right font2 = decodeFont $ readZipFile "Capture_it.ttf"
    cpuDrawThread <- newIORef True

    renderer <- compileRenderer $ ScreenOut $ (if retroMode then pixelize 320 240 else id) $ addHUD stuntsGFX

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

    let speedHud = R.renderDrawing 128 128 (PixelRGBA8 0 0 0 0) $ do
                R.withTexture (R.uniformTexture $ PixelRGBA8 255 0 0 255) $ do
                    R.stroke 4 R.JoinRound (R.CapRound, R.CapRound) $
                        R.circle (R.V2 64 64) 64
                    R.fill $ R.polygon [R.V2 0 64, R.V2 64 58, R.V2 64 70]
    --uniformFTexture2D "speedTexture" unis =<< compileImageToTexture2DRGBAF False True speedHud
    --uniformM44F "hudTransform" unis $ mat4ToM44F one

    --images <- mapM (\b -> compileImageToTexture2DRGBAF False True (Image (width b) (height b) (image b) :: JP.Image PixelRGBA8)) bitmaps
    {-
    forM (zip [0..] bitmaps) $ \(n,(rn,b)) -> do
        let fn = SB.unpack rn
        unless ((take 3 fn) `elem` ["!cg", "!eg", "!pa"]) $ do
            unless (unknown2 b `elem` [[1,2,4,8],[1,2,20,8],[1,2,36,8]]) $ do
                putStr "BAD: "
                print (n,unknown1 b, unknown2 b,width b, height b)
            writePng (printf "png/%04d%s_%d_%d.png" (n :: Int) fn (positionX b) (positionY b)) (Image (width b) (height b) (image b) :: JP.Image PixelRGBA8)
    -}
    compiledQuad <- compileMesh quad
    let hudUnis = ["hudTexture","hudTransform"]
    addMesh renderer "Quad" compiledQuad []
    titleHud <- addMesh renderer "hud" compiledQuad hudUnis
    speedHudObj <- addMesh renderer "hud" compiledQuad hudUnis
    uniformFTexture2D "hudTexture" (objectUniformSetter speedHudObj) =<< compileImageToTexture2DRGBAF False True speedHud
    {-
        2d hud:
            screen width
            screen height

          hud images:
            width
            height
            transformd2D
    -}

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

    let carBmps = carBitmaps <$> carsData

    -- setup physics
    physicsWorld <- mkPhysicsWorld
    addStaticPlane physicsWorld upwards 0 1 1
    addStaticShape physicsWorld trackMesh 1 1
    addStaticShape physicsWorld terrainMesh 1000 1000
    let (sO,Vec3 sX sY sZ) = startPos
    raycastVehicles <- forM carsData $ \carData ->
                       createCar physicsWorld (carMesh carData) (wheels carData) $ translateAfter4 (Vec3 sX (sY + 1) sZ) $ rotMatrixProj4 sO upwards
    let cars = zipWith5 Car raycastVehicles carUnis wheelsUnis carBmps (map carSimModel carsData)

    -- setup FRP network
    (mousePosition,mousePositionSink) <- external (0,0)
    (_mousePress,mousePressSink) <- external False
    (keyPress, keyPressSink) <- external $ const False <$> VG.fromList [minBound .. maxBound :: Command]
    let command c = (VG.! fromEnum c) <$> keyPress

    capRef <- newIORef False
    s <- fpsState
    sc <- start $ do
        u <- scene (setScreenSize renderer) cars cpuDrawThread [font1,font2] carNum (uniformSetter renderer) physicsWorld windowSize mousePosition command capRef titleHud
        return $ draw <$> u

    driveNetwork sc (readInput physicsWorld s mousePositionSink mousePressSink keyPressSink capRef)

    dispose renderer
    closeWindow
    terminate

data Car bc = Car
    { raycastVehicle :: bc
    , carUnis :: [T.Trie InputSetter]
    , wheelsUnis :: [[T.Trie InputSetter]]
    , carBitmaps2 :: T.Trie Bitmap
    , carData :: L.Car
    }

scene :: (BtDynamicsWorldClass bc,
          BtRaycastVehicleClass v)
      => (Word -> Word -> IO ())
      -> [Car v]
      -> IORef Bool
      -> [Font]
      -> Int
      -> T.Trie InputSetter
      -> bc
      -> Signal (Int, Int)
      -> Signal (Float, Float)
      -> (Command -> Signal Bool)
      -> IORef Bool
      -> Object
      -> SignalGen Float (Signal (IO ()))
scene setSize cars cpuDrawThread font initCarNum uniforms physicsWorld windowSize mousePosition command capRef titleHud = do
    isFirstFrame <- stateful True $ const $ const False
    carId <- transfer2 (Nothing, (initCarNum + 10) `mod` 11) (\_ isFirstFrame isPressed (_, prev) ->
                         if isPressed || isFirstFrame
                         then (Just prev, (prev + 1) `mod` 11)
                         else (Nothing, prev))
             isFirstFrame
             =<< edge (command SwitchCar)
    time <- stateful 0 (+)
    frameCount <- stateful (0 :: Int) (\_ c -> c + 1)

    capture <- transfer2 False (\_ cap cap' on -> on /= (cap && not cap')) (command Capture) =<< delay False (command Capture)

    last2 <- transfer ((0,0),(0,0)) (\_ n (_,b) -> (b,n)) mousePosition
    let mouseMove = (\((ox,oy),(nx,ny)) -> (nx-ox,ny-oy)) <$> last2

        pickMode _ True _ _ _ _ = FollowNear
        pickMode _ _ True _ _ _ = FollowFar
        pickMode _ _ _ True _ _ = UserControl
        pickMode _ _ _ _ True _ = InsideCar
        pickMode _ _ _ _ _ mode = mode

        selectCam FollowNear  (cam,dir) _ _ _     = lookat cam (cam &+ dir) upwards
        selectCam FollowFar   _ (cam,dir) _ _     = lookat cam (cam &+ dir) upwards
        selectCam UserControl _ _ (cam,dir,up,_) _ = lookat cam (cam &+ dir) up
        selectCam InsideCar _ _ _ a = a

    dt <- input
    let carInputPress = mapM command [SteerLeft,Accelerate,Brake,SteerRight,RestoreCar]
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
    let fblrPress = (,,,,) <$> command FreeCamLeft <*> command FreeCamUp
                           <*> command FreeCamDown <*> command FreeCamRight <*> command FreeCamTurbo
    userCam <- userCamera (Vec3 (-4) 0 0) mouseMove fblrPress
    camMode <- transfer4 InsideCar pickMode
               (command SwitchToNearCamera) (command SwitchToFarCamera) (command SwitchToFreeCamera) (command SwitchToCarCamera)

    let carCamera = fn <$> carPos <*> carId
        fn m (prevId,currId) = lookat cam dir u
          where
            car = cars !! currId
            cam = Vec3 cx cy cz
            dir = Vec3 dx dy dz
            u  = Vec3 ux uy uz
            h = 1.4 --scaleFactor * (fromIntegral $ L.cockpitHeight $ carData car) / 20
            Vec4 cx cy cz _ = (Vec4 0 h 0 1) .* fromProjective m
            Vec4 dx dy dz _ = (Vec4 0 0 10 1) .* fromProjective m
            Vec4 ux uy uz _ = (Vec4 0 1 0 0) .* fromProjective m
    let camera = selectCam <$> camMode <*> followCamNear <*> followCamFar <*> userCam <*> carCamera

    let worldViewSetter = uniformM44F "worldView" uniforms
        positionSetter = uniformM44F "worldPosition" uniforms
        projectionSetter = uniformM44F "projection" uniforms
        lightDirectionSetter = uniformV3F "lightDirection" uniforms
        setupGFX ((w, h), capturing, frameCount, dt, updateHud, camMode) worldViewMat (prevCarId, carId) (carMat, wheelsMats) = do

            let car = cars !! carId
                fieldOfView = pi/2
                aspectRatio = fromIntegral w / fromIntegral h
                projection nearDepth farDepth = perspective nearDepth farDepth fieldOfView aspectRatio
                carPositionMats car = map (uniformM44F "worldPosition") $ carUnis car
                carViewMats car = map (uniformM44F "worldView") $ carUnis car
                wheelsPositionU car = [[uniformM44F "worldPosition" u | u <- wu] | wu <- wheelsUnis car]
                wheelsViewU car = [[uniformM44F "worldView" u | u <- wu] | wu <- wheelsUnis car]

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

            case (camMode == InsideCar) of
                True    -> do
                    forM_ (carPositionMats car) $ \s -> s $! mat4ToM44F zero
                    forM_ (zip (wheelsPositionU car) wheelsMats) $ \(sl,wu) -> forM_ sl $ \s -> s $! mat4ToM44F zero
                    uniformM44F "hudTransform" (objectUniformSetter titleHud) $ mat4ToM44F one
                False   -> do
                    forM_ (carPositionMats car) $ \s -> s $! mat4ToM44F $! fromProjective carMat 
                    forM_ (zip (wheelsPositionU car) wheelsMats) $ \(sl,wu) -> forM_ sl $ \s -> s $! mat4ToM44F $! fromProjective wu
                    uniformM44F "hudTransform" (objectUniformSetter titleHud) $ mat4ToM44F zero
            currentSpeed <- btRaycastVehicle_getCurrentSpeedKmHour $ raycastVehicle car
            --uniformFloat "speed" uniforms currentSpeed
            --print currentSpeed

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
            --when (done && updateHud) $ do
            when (isJust prevCarId) $ do
                writeIORef cpuDrawThread False
                forkIO $ do
                    -- only render simple stuff for now
                    let dashElems = catMaybes
                            [ solidImage "dash"
                            , alphaImage "dast" "dasm"
                            , solidImage "roof"
                            , solidImage "whl2"
                            ]
                        solidImage name = do
                            bitmap <- T.lookup (SB.pack name) (carBitmaps2 car)
                            let posX = fromIntegral $ positionX bitmap
                                posY = fromIntegral $ positionY bitmap
                            return (Image (width bitmap) (height bitmap) (image bitmap) :: JP.Image PixelRGBA8, R.V2 posX posY)
                        alphaImage cName aName = do
                            (color, pos) <- solidImage cName
                            (alpha, _  ) <- solidImage aName
                            return (combineColorAlpha color alpha, pos)
                    let (x,y) = head $ drop 10 $ L.speedometerNeedle $ carData car
                        (ox,oy) = L.speedometerCentre $ carData car
                        hud = R.renderDrawing 320 200 (PixelRGBA8 0 0 0 0) $ do
                                {-
                                R.withTexture (R.uniformTexture $ PixelRGBA8 255 255 0 255) $ do
                                    R.printTextAt (font !! 1) 42 (R.V2 60 7) "Stunts"
                                R.withTexture (R.uniformTexture $ PixelRGBA8 255 69 0 255) $ do
                                    R.printTextAt (font !! 0) 16 (R.V2 25 60) $ L.scoreboardName $ carData car -- carNames !! carId
                                -}
                                mapM_ (\(img, pos) -> R.drawImage img 0 pos) dashElems
                                {-
                                R.withTexture (R.uniformTexture $ PixelRGBA8 255 255 255 255) $ do
                                    R.stroke 1 R.JoinRound (R.CapRound, R.CapRound) $
                                         R.line (R.V2 (fromIntegral ox) (fromIntegral oy)) (R.V2 (fromIntegral x) (fromIntegral y))
                                -}
                    print (ox,oy,L.speedometerNeedle $ carData car)
                    let ch = fromIntegral $ L.cockpitHeight $ carData car
                        h = scaleFactor * ch / 20
                    print (ch,h)

                    uniformFTexture2D "hudTexture" (objectUniformSetter titleHud) =<< compileImageToTexture2DRGBAF False True hud
                    threadDelay 100000
                    --writeIORef cpuDrawThread True
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
                   ((,,,,,) <$> windowSize <*> capture <*> frameCount <*> dt <*> command SwitchCar <*> camMode )
                   camera
                   carId
                   carAndWheelsPos

combineColorAlpha :: JP.Pixel a => JP.Image a -> JP.Image a -> JP.Image a
combineColorAlpha color alpha = JP.generateImage f w h
  where
    w = JP.imageWidth color
    h = JP.imageHeight color
    f x y = JP.mixWithAlpha (\ _ c a -> c) (\ c a -> a) (JP.pixelAt color x y) (JP.pixelAt alpha x y)

vec3ToV3F :: Vec3 -> V3F
vec3ToV3F (Vec3 x y z) = V3 x y z

vec4ToV4F :: Vec4 -> V4F
vec4ToV4F (Vec4 x y z w) = V4 x y z w

mat4ToM44F :: Mat4 -> M44F
mat4ToM44F (Mat4 a b c d) = V4 (vec4ToV4F a) (vec4ToV4F b) (vec4ToV4F c) (vec4ToV4F d)

data Command
    -- Car control
    = Accelerate
    | Brake
    | SteerLeft
    | SteerRight
    | RestoreCar
    | SwitchCar
    -- Switch camera
    | SwitchToCarCamera
    | SwitchToNearCamera
    | SwitchToFarCamera
    | SwitchToFreeCamera
    -- Free camera controls
    | FreeCamLeft
    | FreeCamRight
    | FreeCamUp
    | FreeCamDown
    | FreeCamTurbo
    -- Misc
    | Capture
    deriving (Enum, Bounded)

keyMapping k =
    case k of
      Accelerate         -> CharKey 'W'
      Brake              -> CharKey 'S'
      SteerLeft          -> CharKey 'A'
      SteerRight         -> CharKey 'D'
      RestoreCar         -> CharKey 'R'
      SwitchCar          -> CharKey 'E'
      SwitchToNearCamera -> CharKey '1'
      SwitchToFarCamera  -> CharKey '2'
      SwitchToFreeCamera -> CharKey '3'
      SwitchToCarCamera  -> CharKey '4'
      FreeCamLeft        -> KeyLeft
      FreeCamRight       -> KeyRight
      FreeCamUp          -> KeyUp
      FreeCamDown        -> KeyDown
      FreeCamTurbo       -> KeyRightShift
      Capture            -> CharKey 'P'

readInput :: (BtDynamicsWorldClass dw)
          => dw
          -> State
          -> Sink (Float, Float)
          -> Sink Bool
          -> Sink (VG.Vector Bool)
          -> IORef Bool
          -> IO (Maybe Float)
readInput physicsWorld s mousePos mouseBut keys capRef = do
    t <- getTime
    resetTime

    (x,y) <- getMousePosition
    mousePos (fromIntegral x,fromIntegral y)

    mouseBut =<< mouseButtonIsPressed MouseButton0

    keys =<< VG.mapM (keyIsPressed.keyMapping) (VG.fromList [minBound .. maxBound])
    k <- keyIsPressed KeyEsc

    -- step physics
    isCapturing <- readIORef capRef
    let dt = if isCapturing then recip captureRate else realToFrac t

    updateFPS s t
    return $ if k then Nothing else Just dt
