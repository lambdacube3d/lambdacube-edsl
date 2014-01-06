{-# OPTIONS -cpp #-}
{-# LANGUAGE OverloadedStrings, PackageImports, TypeOperators, DataKinds #-}

import Control.Applicative hiding (Const)
import Control.Monad
import qualified Data.ByteString.Char8 as SB
import qualified Data.Trie as T
import Data.Vect hiding (reflect')
import Data.Vect.Float.Instances ()
import FRP.Elerea.Param
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Text.Printf

import LC_API
import LC_Mesh

import Common.Utils
import Common.GraphicsUtils

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
        pipeline = PrjFrameBuffer "outFB" tix0 sceneRender

    windowSize <- initWindow "LambdaCube 3D Cube Map Demo" 1280 720

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
    
    reflectorMesh <- compileMesh (capsule 2.5 3.5 25) -- (sphere 5 25)
    (duration, reflectorObject) <- measureDuration $ addMesh renderer "reflectSlot" reflectorMesh ["modelMatrix"]

    putStrLn $ "Reflector mesh added - " ++ show duration
    
    let objectSlots = reflectorSlot : map objectUniformSetter cubeObjects
        reflectorSlot = objectUniformSetter reflectorObject
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

scene setSize sceneSlots (reflectorSlot:planeSlot:cubeSlots) windowSize = do
    pause <- toggle =<< risingEdge =<< effectful (keyIsPressed (CharKey 'P'))
    time <- transfer 0 (\dt paused time -> time + if paused then 0 else dt) pause 
    
    capture <- toggle =<< risingEdge =<< effectful (keyIsPressed (CharKey 'C'))
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
    camera <- userCamera (Vec3 (-4) 0 10) (mousePosition - mousePosition') directionControl
    
    let setViewCameraMatrix = uniformM44F "viewCameraMatrix" sceneSlots . fromMat4
        setViewCameraPosition = uniformV3F "viewCameraPosition" sceneSlots . fromVec3
        setCubeCameraMatrix i = uniformM44F (cubeMatrixName i) sceneSlots . fromMat4
        setCubeCameraPosition = uniformV3F "cubeCameraPosition" sceneSlots . fromVec3
        setLightPosition = uniformV3F "lightPosition" sceneSlots . fromVec3
        setPlaneModelMatrix = uniformM44F "modelMatrix" planeSlot . fromMat4
        setCubeModelMatrices = [uniformM44F "modelMatrix" cubeSlot . fromMat4 | cubeSlot <- cubeSlots]
        setReflectorModelMatrix = uniformM44F "modelMatrix" reflectorSlot . fromMat4
        
        setupRendering ((_, _, fps), frameCount, capture) (windowWidth, windowHeight) (cameraPosition, cameraDirection, cameraUp, _) time = do
            let aspect = fromIntegral windowWidth / fromIntegral windowHeight
                
                cameraView = fromProjective (lookat cameraPosition (cameraPosition &+ cameraDirection) cameraUp)
                cameraProjection = perspective 0.1 50 (pi/2) aspect

                lightPosition = Vec3 (15 * sin time) 2 10
                reflectorPosition = Vec3 (-8) (5 * sin (time * 0.25)) 0
                
                cubeCameraProjection = perspective 0.1 50 (pi/2) 1
                cubeLookAt dir up = fromProjective (lookat reflectorPosition (reflectorPosition &+ dir) up)
                cubeCameraMatrix 1 = cubeLookAt (Vec3 1 0 0) (Vec3 0 (-1) 0)
                cubeCameraMatrix 2 = cubeLookAt (Vec3 (-1) 0 0) (Vec3 0 (-1) 0)
                cubeCameraMatrix 3 = cubeLookAt (Vec3 0 1 0) (Vec3 0 0 1)
                cubeCameraMatrix 4 = cubeLookAt (Vec3 0 (-1) 0) (Vec3 0 0 (-1))
                cubeCameraMatrix 5 = cubeLookAt (Vec3 0 0 1) (Vec3 0 (-1) 0)
                cubeCameraMatrix 6 = cubeLookAt (Vec3 0 0 (-1)) (Vec3 0 (-1) 0)
            
            case fps of
                Just value -> putStrLn $ "FPS: " ++ show value
                Nothing -> return ()
            
            setViewCameraMatrix (cameraView .*. cameraProjection)
            setViewCameraPosition cameraPosition
            setLightPosition lightPosition
            
            setCubeCameraPosition reflectorPosition
            setReflectorModelMatrix (fromProjective (translation reflectorPosition))
            forM_ [1..6] $ \index -> setCubeCameraMatrix index (cubeCameraMatrix index .*. cubeCameraProjection)
    
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

sceneRender :: Exp Obj (FrameBuffer 1 (Float, V4F))
sceneRender = Accumulate accCtx PassAll reflectFrag (Rasterize rastCtx reflectPrims) directRender
  where
    directRender = Accumulate accCtx PassAll frag (Rasterize rastCtx directPrims) clearBuf
    cubeMapRender = Accumulate accCtx PassAll frag (Rasterize rastCtx cubePrims) clearBuf6
    
    accCtx = AccumulationContext Nothing (DepthOp Less True :. ColorOp NoBlending (one' :: V4B) :. ZT)
    rastCtx = triangleCtx { ctxCullMode = CullFront CCW }
    clearBuf = FrameBuffer (DepthImage n1 1000 :. ColorImage n1 (V4 0.1 0.2 0.6 1) :. ZT)
    clearBuf6 = FrameBuffer (DepthImage n6 1000 :. ColorImage n6 (V4 0.05 0.1 0.3 1) :. ZT)
    worldInput = Fetch "geometrySlot" Triangles (IV3F "position", IV3F "normal")
    reflectInput = Fetch "reflectSlot" Triangles (IV3F "position", IV3F "normal")
    directPrims = Transform directVert worldInput
    cubePrims = Reassemble geom (Transform cubeMapVert worldInput)
    reflectPrims = Transform directVert reflectInput

    lightPosition = Uni (IV3F "lightPosition")
    viewCameraMatrix = Uni (IM44F "viewCameraMatrix")
    viewCameraPosition = Uni (IV3F "viewCameraPosition")
    cubeCameraMatrix i = Uni (IM44F (cubeMatrixName i))
    cubeCameraPosition = Uni (IV3F "cubeCameraPosition")
    modelMatrix = Uni (IM44F "modelMatrix")
    
    transformGeometry :: Exp f V4F -> Exp f V3F -> Exp f M44F -> (Exp f V4F, Exp f V4F, Exp f V3F)
    transformGeometry localPos localNormal viewMatrix = (viewPos, worldPos, worldNormal)
      where
        worldPos = modelMatrix @*. localPos
        viewPos = viewMatrix @*. worldPos
        worldNormal = normalize' (v4v3 (modelMatrix @*. n3v4 localNormal))

    directVert :: Exp V (V3F, V3F) -> VertexOut () (V3F, V3F, V3F)
    directVert attr = VertexOut viewPos (floatV 1) ZT (Smooth (v4v3 worldPos) :. Smooth worldNormal :. Flat viewCameraPosition :. ZT)
      where
        (localPos, localNormal) = untup2 attr
        (viewPos, worldPos, worldNormal) = transformGeometry (v3v4 localPos) localNormal viewCameraMatrix

    cubeMapVert :: Exp V (V3F, V3F) -> VertexOut () V3F
    cubeMapVert attr = VertexOut (v3v4 localPos) (floatV 1) ZT (Smooth localNormal :. ZT)
      where
        (localPos, localNormal) = untup2 attr

    geom :: GeometryShader Triangle Triangle () () 6 V3F (V3F, V3F, V3F) 
    geom = GeometryShader n6 TrianglesOutput 18 init prim vert
      where
        init attr = tup2 (primInit, intG 6)
          where
            primInit = tup2 (intG 0, attr)
        prim primState = tup5 (layer, layer, primState', vertInit, intG 3) 
          where
            (layer, attr) = untup2 primState
            primState' = tup2 (layer @+ intG 1, attr)
            vertInit = tup3 (intG 0, viewMatrix, attr)
            viewMatrix = indexG (map cubeCameraMatrix [1..6]) layer
        vert vertState = GeometryOut vertState' viewPos pointSize ZT (Smooth (v4v3 worldPos) :. Smooth worldNormal :. Flat cubeCameraPosition :. ZT)
          where
            (index, viewMatrix, attr) = untup3 vertState
            vertState' = tup3 (index @+ intG 1, viewMatrix, attr)
            (attr0, attr1, attr2) = untup3 attr
            (localPos, pointSize, _, localNormal) = untup4 (indexG [attr0, attr1, attr2] index)
            (viewPos, worldPos, worldNormal) = transformGeometry localPos localNormal viewMatrix

    frag :: Exp F (V3F, V3F, V3F) -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    frag attr = FragmentOutRastDepth (luminance :. ZT)
      where
        lambert = max' (floatF 0) (worldNormal @. normalize' (lightPosition @- worldPos))
        reflectedRay = normalize' (reflect' (worldPos @- (cameraPosition :: Exp F V3F)) worldNormal)
        directLight = normalize' (lightPosition @- worldPos)
        phong = max' (floatF 0) (reflectedRay @. directLight)
        colour = pack' (V3 (floatF 0.7) (floatF 0.05) (floatF 0))
        luminance = v3v4 (colour @* lambert @+ pow' phong (floatF 10))
        (worldPos, worldNormal, cameraPosition) = untup3 attr

    reflectFrag :: Exp F (V3F, V3F, V3F) -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    reflectFrag attr = FragmentOutRastDepth (luminance :. ZT)
      where
        reflectedRay = reflect' (worldPos @- (cameraPosition :: Exp F V3F)) worldNormal
        luminance = reflectionSample reflectedRay
        (worldPos, worldNormal, cameraPosition) = untup3 attr
        reflectionSample dir = texture' (Sampler LinearFilter ClampToEdge reflectionMap) dir
        reflectionMap = Texture (TextureCube (Float RGBA)) (V2 256 256) NoMip [PrjFrameBuffer "" tix0 cubeMapRender]

indexG :: GPU a => [Exp G a] -> Exp G Int32 -> Exp G a
indexG xs index = go xs 0
  where
    go [x] _ = x
    go (x:xs) i = Cond (index @== intG i) x (go xs (i+1))

cubeMatrixName :: Int -> SB.ByteString
cubeMatrixName i = SB.pack (printf "cubeCameraMatrix%d" i)

