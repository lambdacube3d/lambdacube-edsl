{-# LANGUAGE OverloadedStrings, PackageImports, TypeOperators, ParallelListComp, DataKinds #-}

import Control.Monad
import Control.Monad.Fix
import qualified Data.ByteString.Char8 as SB
import qualified Data.Trie as T
import Data.Time.Clock
import Data.Vect
import Data.Vect.Float.Instances ()
import qualified Data.Vector.Storable as V
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Text.Printf

import LambdaCube.GL
import LambdaCube.GL.Mesh

import Utils
import GraphicsUtils

weights = gaussianSamples 1000 101
dirH = V2 1 0
dirV = V2 0 1

finalImage :: Exp Obj (FrameBuffer 1 V4F)
finalImage = filterPass dirV (filterPass dirH originalImage)
  where
    filterPass dir = convolve dir weights . projectBuffer
    projectBuffer = PrjFrameBuffer "" tix0

--finalImage = additiveSample "verticalFilter" (projectBuffer (additiveSample "horizontalFilter" (projectBuffer originalImage)))
--finalImage = originalImage

main :: IO ()
main = do
    let pipeline :: Exp Obj (Image 1 V4F)
        pipeline = PrjFrameBuffer "outFB" tix0 finalImage

    initWindow "LambdaCube 3D Convolution Filter Demo"

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
    
    horizontalSamplingMesh <- compileMesh (samplingQuads dirH weights)
    verticalSamplingMesh <- compileMesh (samplingQuads dirV weights)
    addMesh renderer "horizontalFilter" horizontalSamplingMesh []
    addMesh renderer "verticalFilter" verticalSamplingMesh []
    
    addMesh renderer "geometrySlot" quadMesh []

    startTime <- getCurrentTime
    flip fix (0, startTime) $ \loop (frameCount, lastTime) -> do
        input <- readInput
        case input of
            Nothing -> return ()
            Just dt -> do
                (w, h) <- getWindowDimensions
                setScreenSize renderer (fromIntegral w) (fromIntegral h)
                render renderer
                swapBuffers
                currentTime <- getCurrentTime
                let elapsedTime = realToFrac (diffUTCTime currentTime lastTime) :: Float
                    next = case elapsedTime > 5.0 of 
                        True -> (0, currentTime)
                        False -> frameCount' `seq` (frameCount', lastTime)
                          where
                            frameCount' = frameCount+1
                when (fst next == 0) $
                    printf "%d frames in %0.3f seconds (%0.2f ms/f)\n" (round frameCount :: Int) elapsedTime (1000 * elapsedTime / frameCount) 
                loop next

    dispose renderer
    putStrLn "Renderer destroyed."

    closeWindow

readInput :: IO (Maybe Float)
readInput = do
    t <- getTime
    resetTime

    k <- keyIsPressed KeyEsc
    return $ if k then Nothing else Just (realToFrac t)

-- the threshold and offsetWeight optimisations can be commented out independently
gaussianSamples :: Float -> Int -> [(Float, Float)]
gaussianSamples tolerance = normalise . threshold tolerance . offsetWeight . withOffsets . binomialCoefficients

binomialCoefficients :: Int -> [Float]
binomialCoefficients n = iterate next [1] !! (n-1)
  where
    next xs = [x+y | x <- xs ++ [0] | y <- 0:xs]

withOffsets :: [Float] -> [(Float, Float)]
withOffsets cs = [(o, c) | c <- cs | o <- [-lim..lim]]
  where
    lim = fromIntegral (length cs `quot` 2)

offsetWeight :: [(Float, Float)] -> [(Float, Float)]
offsetWeight [] = []
offsetWeight [ow] = [ow] 
offsetWeight ((o1,w1):(o2,w2):ows) = (o1+w2/w', w') : offsetWeight ows
  where
    w' = w1+w2

threshold :: Float -> [(Float, Float)] -> [(Float, Float)]
threshold t ocs = [oc | oc@(_, c) <- ocs, c*t >= m]
  where
    m = maximum [c | (_, c) <- ocs]

normalise :: [(Float, Float)] -> [(Float, Float)]
normalise ocs = [(o, c/s) | (o, c) <- ocs]
  where
    s = sum [c | (_, c) <- ocs]

originalImage :: Exp Obj (FrameBuffer 1 V4F)
originalImage = Accumulate accCtx PassAll frag (Rasterize triangleCtx prims) clearBuf
  where
    accCtx = AccumulationContext Nothing (ColorOp NoBlending (one' :: V4B) :. ZT)
    clearBuf = FrameBuffer (ColorImage n1 (V4 0 0 0 1) :. ZT)
    prims = Transform vert (Fetch "geometrySlot" Triangles (IV2F "position"))
    
    vert :: Exp V V2F -> VertexOut () ()
    vert pos = VertexOut pos' (floatV 1) ZT ZT
      where
        V2 x y = unpack' pos
        pos' = pack' (V4 x y (floatV 0) (floatV 1))
    
    frag :: Exp F () -> FragmentOut (Color V4F :+: ZZ)
    frag _ = FragmentOut (col :. ZT)
      where
        V4 x y _ _ = unpack' fragCoord'
        x' = sqrt' x @* floatF 16
        y' = sqrt' y @* floatF 16
        r = Cond ((x' @+ y') @% (floatF 50) @< (floatF 25)) (floatF 0) (floatF 1)
        g = floatF 0
        b = Cond ((x' @- y') @% (floatF 50) @< (floatF 25)) (floatF 0) (floatF 1)
        col = pack' (V4 r g b (floatF 1))

convolve :: V2F -> [(Float, Float)] -> Exp Obj (Image 1 V4F) -> Exp Obj (FrameBuffer 1 V4F)
convolve (V2 dx dy) weights img = Accumulate accCtx PassAll frag (Rasterize triangleCtx prims) clearBuf
  where
    resX = windowWidth
    resY = windowHeight
    dir' :: Exp F V2F
    dir' = Const (V2 (dx / fromIntegral resX) (dy / fromIntegral resY))
    
    accCtx = AccumulationContext Nothing (ColorOp NoBlending (one' :: V4B) :. ZT)
    clearBuf = FrameBuffer (ColorImage n1 (V4 0 0 0 1) :. ZT)
    prims = Transform vert (Fetch "postSlot" Triangles (IV2F "position"))

    vert :: Exp V V2F -> VertexOut () V2F
    vert uv = VertexOut pos (Const 1) ZT (NoPerspective uv' :. ZT)
      where
        uv'    = uv @* floatV 0.5 @+ floatV 0.5
        pos    = pack' (V4 u v (floatV 1) (floatV 1))
        V2 u v = unpack' uv

    frag :: Exp F V2F -> FragmentOut (Color V4F :+: ZZ)
    frag uv = FragmentOut (sample :. ZT)
      where
        sample = foldr1 (@+) [ texture' smp (uv @+ dir' @* floatF ofs) @* floatF coeff
                             | (ofs, coeff) <- weights]
        smp = Sampler LinearFilter ClampToEdge tex
        tex = Texture (Texture2D (Float RGBA) n1) (V2 resX resY) NoMip [img]

additiveSample :: SB.ByteString -> Exp Obj (Image 1 V4F) -> Exp Obj (FrameBuffer 1 V4F)
additiveSample slot img = Accumulate accCtx PassAll frag (Rasterize triangleCtx prims) clearBuf
  where
    resX = windowWidth
    resY = windowHeight
    
    accCtx = AccumulationContext Nothing (ColorOp blendEquation (one' :: V4B) :. ZT)
    blendEquation = Blend (FuncAdd, FuncAdd) ((SrcAlpha, One), (SrcAlpha, One)) (V4 1 1 1 1)
    clearBuf = FrameBuffer (ColorImage n1 (V4 0 0 0 1) :. ZT)
    prims = Transform vert (Fetch slot Triangles (IV2F "position", IV2F "uv", IFloat "alpha"))

    vert :: Exp V (V2F, V2F, Float) -> VertexOut () (V2F, Float)
    vert attr = VertexOut pos' (Const 1) ZT (NoPerspective uv :. Flat alpha :. ZT)
      where
        pos'   = pack' (V4 x y (floatV 1) (floatV 1))
        V2 x y = unpack' pos
        (pos, uv, alpha) = untup3 attr

    frag :: Exp F (V2F, Float) -> FragmentOut (Color V4F :+: ZZ)
    frag attr = FragmentOut (pack' (V4 r g b alpha) :. ZT)
      where
        V4 r g b _ = unpack' (texture' smp uv)
        smp = Sampler LinearFilter ClampToEdge tex
        tex = Texture (Texture2D (Float RGBA) n1) (V2 resX resY) NoMip [img]
        (uv, alpha) = untup2 attr

samplingQuads :: V2F -> [(Float, Float)] -> Mesh
samplingQuads (V2 dx dy) weights = Mesh
    { mAttributes = T.fromList
                    [ ("position", A_V2F $ V.fromList (concat (replicate (length weights) quadCoords)))
                    , ("uv", A_V2F $ V.fromList (concatMap makeUVs weights))
                    , ("alpha", A_Float $ V.fromList (concatMap makeAlphas weights))
                    ]
    , mPrimitive = P_Triangles
    , mGPUData = Nothing
    }
  where
    infixr 0 ^
    (^) = V2
    quadCoords = [-1 ^ 1, -1 ^ -1, 1 ^ -1, 1 ^ -1, 1 ^ 1, -1 ^ 1]
    makeUVs (ofs, _) = [V2 (x*0.5+0.5+dx*ofs/resX) (y*0.5+0.5+dy*ofs/resY) | V2 x y <- quadCoords]
    makeAlphas (_,w) = map (const w) quadCoords
    resX = windowWidth
    resY = windowHeight
