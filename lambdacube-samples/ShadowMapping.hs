{-# OPTIONS -cpp #-}
{-# LANGUAGE OverloadedStrings, PackageImports, TypeOperators, DataKinds, FlexibleContexts #-}

import Control.Applicative hiding (Const)
import Control.Monad
import qualified Data.ByteString.Char8 as SB
import qualified Data.Trie as T
import Data.Vect
import Data.Vect.Float.Instances ()
import FRP.Elerea.Param
import "GLFW-b" Graphics.UI.GLFW as GLFW

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
        pipeline = PrjFrameBuffer "outFB" tix0 vsm

    windowSize <- initWindow "LambdaCube 3D Shadow Mapping Demo" 1280 720

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

shadowMapSize :: Num a => a
shadowMapSize = 512

blurCoefficients :: [(Float, Float)]
blurCoefficients = gaussFilter9

gaussFilter7 :: [(Float, Float)]
gaussFilter7 = 
    [ (-3.0,   0.015625)
    , (-2.0,   0.09375)
    , (-1.0,   0.234375)
    , (0.0,    0.3125)
    , (1.0,    0.234375)
    , (2.0,    0.09375)
    , (3.0,    0.015625)
    ]

gaussFilter9 :: [(Float, Float)]
gaussFilter9 = 
    [ (-4.0,   0.05)
    , (-3.0,   0.09)
    , (-2.0,   0.12)
    , (-1.0,   0.15)
    , (0.0,    0.16)
    , (1.0,    0.15)
    , (2.0,    0.12)
    , (3.0,    0.09)
    , (4.0,    0.05)
    ]

blur :: [(Float, Float)] -> Exp Obj (Image 1 V2F) -> Exp Obj (FrameBuffer 1 V2F)
blur coefficients img = filter1D dirH (PrjFrameBuffer "" tix0 (filter1D dirV img))
  where
    dirH v = Const (V2 (v / shadowMapSize) 0) :: Exp F V2F
    dirV v = Const (V2 0 (v / shadowMapSize)) :: Exp F V2F
    
    filter1D :: (Float -> Exp F V2F) -> Exp Obj (Image 1 V2F) -> Exp Obj (FrameBuffer 1 V2F)
    filter1D dir img = Accumulate accCtx PassAll frag
                                 (Rasterize triangleCtx prims) clearBuf
      where
        accCtx = AccumulationContext Nothing
                                    (ColorOp NoBlending (one' :: V2B) :. ZT)
        clearBuf = FrameBuffer (ColorImage n1 (V2 0 0) :. ZT)
        prims = Transform vert (Fetch "postSlot" Triangles (IV2F "position"))

        vert :: Exp V V2F -> VertexOut () V2F
        vert uv = VertexOut pos (Const 1) ZT (NoPerspective uv' :. ZT)
          where
            uv'    = uv @* floatV 0.5 @+ floatV 0.5
            pos    = pack' (V4 u v (floatV 1) (floatV 1))
            V2 u v = unpack' uv

        frag :: Exp F V2F -> FragmentOut (Color V2F :+: ZZ)
        frag uv = FragmentOut (sample :. ZT)
          where
            sample = foldr1 (@+) [ texture' smp (uv @+ dir ofs) @* floatF coeff
                                 | (ofs, coeff) <- coefficients]
            smp = Sampler LinearFilter ClampToEdge tex
            tex = Texture (Texture2D (Float RG) n1)
                          (V2 shadowMapSize shadowMapSize) NoMip [img]
    

moments :: Exp Obj (FrameBuffer 1 (Float, V2F))
moments = Accumulate accCtx PassAll frag (Rasterize triangleCtx prims) clearBuf
  where
    accCtx = AccumulationContext Nothing (DepthOp Less True :. ColorOp NoBlending (one' :: V2B) :. ZT)
    clearBuf = FrameBuffer (DepthImage n1 1000 :. ColorImage n1 (V2 0 0) :. ZT)
    prims = Transform vert (Fetch "geometrySlot" Triangles (IV3F "position"))
    
    lightMatrix = Uni (IM44F "lightMatrix")
    modelMatrix = Uni (IM44F "modelMatrix")

    vert :: Exp V V3F -> VertexOut () Float
    vert pos = VertexOut lightPos (floatV 1) ZT (Smooth depth :. ZT)
      where
        lightPos = lightMatrix @*. modelMatrix @*. v3v4 pos
        V4 _ _ depth _ = unpack' lightPos

    frag :: Exp F Float -> FragmentOut (Depth Float :+: Color V2F :+: ZZ)
    frag depth = FragmentOutRastDepth (pack' (V2 moment1 moment2) :. ZT)
      where
        dx = dFdx' depth
        dy = dFdy' depth
        moment1 = depth
        moment2 = depth @* depth @+ floatF 0.25 @* (dx @* dx @+ dy @* dy)

depth :: Exp Obj (FrameBuffer 1 (Float, Float))
depth = Accumulate accCtx PassAll frag (Rasterize triangleCtx prims) clearBuf
  where
    accCtx = AccumulationContext Nothing (DepthOp Less True :. ColorOp NoBlending True :. ZT)
    clearBuf = FrameBuffer (DepthImage n1 1000 :. ColorImage n1 0 :. ZT)
    prims = Transform vert (Fetch "geometrySlot" Triangles (IV3F "position"))
    
    lightMatrix = Uni (IM44F "lightMatrix")
    modelMatrix = Uni (IM44F "modelMatrix")

    vert :: Exp V V3F -> VertexOut () Float
    vert pos = VertexOut lightPos (floatV 1) ZT (Smooth depth :. ZT)
      where
        lightPos = lightMatrix @*. modelMatrix @*. v3v4 pos
        V4 _ _ depth _ = unpack' lightPos

    frag :: Exp F Float -> FragmentOut (Depth Float :+: Color Float :+: ZZ)
    frag depth = FragmentOutRastDepth (depth :. ZT)

vsm :: Exp Obj (FrameBuffer 1 (Float, V4F))
vsm = Accumulate accCtx PassAll frag (Rasterize triangleCtx prims) clearBuf
  where
    accCtx = AccumulationContext Nothing
                (DepthOp Less True :. ColorOp NoBlending (one' :: V4B) :. ZT)
    clearBuf = FrameBuffer (  DepthImage n1 1000
                           :. ColorImage n1 (V4 0.1 0.2 0.6 1) :. ZT)
    prims = Transform vert (Fetch "geometrySlot" Triangles (IV3F "position", IV3F "normal"))

    cameraMatrix = Uni (IM44F "cameraMatrix")
    lightMatrix = Uni (IM44F "lightMatrix")
    modelMatrix = Uni (IM44F "modelMatrix")
    lightPosition = Uni (IV3F "lightPosition")

    vert :: Exp V (V3F, V3F) -> VertexOut () (V3F, V4F, V3F)
    vert attr = VertexOut viewPos (floatV 1) ZT (Smooth (v4v3 worldPos) :. Smooth lightPos :. Smooth worldNormal :. ZT)
      where
        worldPos = modelMatrix @*. v3v4 localPos
        viewPos = cameraMatrix @*. worldPos
        lightPos = lightMatrix @*. worldPos
        worldNormal = normalize' (v4v3 (modelMatrix @*. n3v4 localNormal))
        (localPos, localNormal) = untup2 attr

    frag :: Exp F (V3F, V4F, V3F) -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    frag attr = FragmentOutRastDepth (luminance :. ZT)
      where
        V4 lightU lightV lightDepth lightW = unpack' lightPos
        uv = clampUV (scaleUV (pack' (V2 lightU lightV) @/ lightW))
        
        V2 moment1 moment2 = unpack' (texture' sampler uv)
        variance = max' (floatF 0.002) (moment2 @- moment1 @* moment1)
        distance = max' (floatF 0) (lightDepth @- moment1)
        lightProbMax = variance @/ (variance @+ distance @* distance)
        
        lambert = max' (floatF 0) (dot' worldNormal (normalize' (lightPosition @- worldPos)))
        
        uv' = uv @- floatF 0.5
        spotShape = floatF 1 @- length' uv' @* floatF 4
        intensity = max' (floatF 0) (spotShape @* lambert)
        
        V2 spotR spotG = unpack' (scaleUV (round' (uv' @* floatF 10)) @* intensity)
        
        luminance = pack' (V4 spotR spotG intensity (floatF 1)) @* pow' lightProbMax (floatF 2)
        
        clampUV x = clamp' x (floatF 0) (floatF 1)
        scaleUV x = x @* floatF 0.5 @+ floatF 0.5
        
        (worldPos, lightPos, worldNormal) = untup3 attr

    sampler = Sampler LinearFilter ClampToEdge shadowMapBlur
    
    --shadowMap :: Exp Obj (Texture Tex2D SingleTex (Regular Float) RG)
    shadowMap = Texture (Texture2D (Float RG) n1) (V2 shadowMapSize shadowMapSize) NoMip [PrjFrameBuffer "shadowMap" tix0 moments]

    --shadowMapBlur :: Exp Obj (Texture Tex2D SingleTex (Regular Float) RG)
    shadowMapBlur = Texture (Texture2D (Float RG) n1) (V2 shadowMapSize shadowMapSize) NoMip [PrjFrameBuffer "shadowMap" tix0 blurredMoments]
      where
        blurredMoments = blur blurCoefficients (PrjFrameBuffer "blur" tix0 moments)

sm :: Exp Obj (FrameBuffer 1 (Float, V4F))
sm = Accumulate accCtx PassAll frag (Rasterize triangleCtx prims) clearBuf
  where
    accCtx = AccumulationContext Nothing (DepthOp Less True :. ColorOp NoBlending (one' :: V4B) :. ZT)
    clearBuf = FrameBuffer (DepthImage n1 1000 :. ColorImage n1 (V4 0.1 0.2 0.6 1) :. ZT)
    prims = Transform vert (Fetch "geometrySlot" Triangles (IV3F "position", IV3F "normal"))

    cameraMatrix = Uni (IM44F "cameraMatrix")
    lightMatrix = Uni (IM44F "lightMatrix")
    modelMatrix = Uni (IM44F "modelMatrix")
    lightPosition = Uni (IV3F "lightPosition")

    vert :: Exp V (V3F, V3F) -> VertexOut () (V3F, V4F, V3F)
    vert attr = VertexOut viewPos (floatV 1) ZT (Smooth (v4v3 worldPos) :. Smooth lightPos :. Smooth worldNormal :. ZT)
      where
        worldPos = modelMatrix @*. v3v4 localPos
        viewPos = cameraMatrix @*. worldPos
        lightPos = lightMatrix @*. worldPos
        worldNormal = normalize' (v4v3 (modelMatrix @*. n3v4 localNormal))
        (localPos, localNormal) = untup2 attr

    frag :: Exp F (V3F, V4F, V3F) -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    frag attr = FragmentOutRastDepth (luminance :. ZT)
      where
        V4 lightU lightV lightDepth lightW = unpack' lightPos
        uv = clampUV (scaleUV (pack' (V2 lightU lightV) @/ lightW))
        
        surfaceDistance = texture' sampler uv
        lightPortion = Cond (lightDepth @<= surfaceDistance @+ floatF 0.01) (floatF 1) (floatF 0)
        
        lambert = max' (floatF 0) (dot' worldNormal (normalize' (lightPosition @- worldPos)))
        
        --intensity = lambert @* lightPortion
        --luminance = pack' (V4 intensity intensity intensity (floatF 1))
        
        uv' = uv @- floatF 0.5
        spotShape = floatF 1 @- length' uv' @* floatF 4
        intensity = max' (floatF 0) (spotShape @* lambert)
        
        V2 spotR spotG = unpack' (scaleUV (round' (uv' @* floatF 10)) @* intensity)
        
        luminance = pack' (V4 spotR spotG intensity (floatF 1)) @* lightPortion
        
        clampUV x = clamp' x (floatF 0) (floatF 1)
        scaleUV x = x @* floatF 0.5 @+ floatF 0.5
        
        (worldPos, lightPos, worldNormal) = untup3 attr

    sampler = Sampler PointFilter ClampToEdge shadowMap
    
    --shadowMap :: Exp Obj (Texture Tex2D SingleTex (Regular Float) Red)
    shadowMap = Texture (Texture2D (Float Red) n1) (V2 shadowMapSize shadowMapSize) NoMip [PrjFrameBuffer "shadowMap" tix0 depth]
