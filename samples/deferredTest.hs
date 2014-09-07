{-# LANGUAGE OverloadedStrings, PackageImports, TypeOperators, DataKinds #-}

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Control.Applicative hiding (Const)
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Data.IORef
import Data.Word
import Data.Vect
import Data.Vect.Float.Instances ()
import FRP.Elerea.Param
import qualified Data.ByteString.Char8 as SB
import qualified Data.Trie as T
import qualified Data.Vector.Storable as V

import LambdaCube.GL
import LambdaCube.GL.Mesh

import Graphics.Rendering.OpenGL.Raw.Core32
import Codec.Image.STB hiding (Image)

--import qualified Criterion.Measurement as C

import VSM
{-
simpleTexturing :: Exp Obj (FrameBuffer 1 (Float,V4F))
simpleTexturing = Accumulate fragCtx PassAll frag rast clear
  where
    rastCtx :: RasterContext Triangle
    rastCtx = TriangleCtx (CullFront CW) PolygonFill NoOffset LastVertex

    fragCtx :: AccumulationContext (Depth Float :+: Color V4F :+: ZZ)
    fragCtx = AccumulationContext Nothing $ DepthOp Less True:.ColorOp NoBlending (one' :: V4B):.ZT

    clear :: Exp Obj (FrameBuffer 1 (Float,V4F))
    clear   = FrameBuffer (DepthImage n1 1000:.ColorImage n1 (zero'::V4F):.ZT)

    rast :: Exp Obj (FragmentStream 1 V2F)
    rast    = Rasterize rastCtx prims

    prims :: Exp Obj (PrimitiveStream Triangle () 1 V V2F)
    prims   = Transform vert input

    input :: Exp Obj (VertexStream Triangle (V3F,V2F))
    input   = Fetch "scene" Triangles (IV3F "position", IV2F "UVTex")

    worldViewProj :: Exp V M44F
    worldViewProj = Uni (IM44F "worldViewProj")

    vert :: Exp V (V3F,V2F) -> VertexOut () V2F
    vert puv = VertexOut v4 (Const 1) ZT (Smooth uv:.ZT)
      where
        (p,uv)  = untup2 puv
        v4 :: Exp V V4F
        v4      = worldViewProj @*. snoc p 1

    frag :: Exp F V2F -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    frag uv = FragmentOutRastDepth $ c :. ZT
      where
        V2 u v  = unpack' uv
        c' :: Exp F V4F
        c' = pack' $ V4 u v (floatF 0) (floatF 1)

        c :: Exp F V4F
        c = texture' smp uv

        smp :: Exp F (Sampler Tex2D SingleTex (Regular Float) RGBA)
        smp = Sampler LinearFilter ClampToEdge tex

        --tex :: Texture (Exp Obj) Tex2D SingleTex (Regular Float) RGBA
        tex = TextureSlot "diffuse" (Texture2D (Float RGBA) n1)
-}
deferredGeometry :: Exp Obj (FrameBuffer 1 (Float,V3F,V3F,V2F)) -- depth, position, normal, uv
deferredGeometry = Accumulate fragCtx PassAll frag rast clear
  where
    rastCtx :: RasterContext Triangle
    rastCtx = TriangleCtx (CullFront CW) PolygonFill NoOffset LastVertex

    fragCtx :: AccumulationContext (Depth Float :+: Color V3F :+: Color V3F :+: Color V2F :+: ZZ)
    fragCtx = AccumulationContext Nothing $
                    DepthOp Less True :.
                    ColorOp NoBlending (one' :: V3B) :.
                    ColorOp NoBlending (one' :: V3B) :.
                    ColorOp NoBlending (one' :: V2B) :.
                    ZT

    clear :: Exp Obj (FrameBuffer 1 (Float,V3F,V3F,V2F))
    clear   = FrameBuffer (DepthImage n1 1000:.ColorImage n1 (zero'::V3F):.ColorImage n1 (zero'::V3F):.ColorImage n1 (zero'::V2F):.ZT)

    rast :: Exp Obj (FragmentStream 1 (V3F,V3F,V2F))
    rast    = Rasterize rastCtx prims

    prims :: Exp Obj (PrimitiveStream Triangle () 1 V (V3F,V3F,V2F))
    prims   = Transform vert input

    input :: Exp Obj (VertexStream Triangle (V3F,V3F,V2F))
    input   = Fetch "scene" Triangles (IV3F "position", IV3F "normal", IV2F "UVTex")

    worldViewProj :: Exp V M44F
    worldViewProj = Uni (IM44F "worldViewProj")


    vert :: Exp V (V3F,V3F,V2F) -> VertexOut () (V3F,V3F,V2F)
    vert pnt = VertexOut v4 (Const 1) ZT (Smooth (prj1 p):.Smooth (prj0 n):.Smooth t:.ZT)
      where
        (p,n,t) = untup3 pnt
        v4 :: Exp V V4F
        v4 = worldViewProj @*. snoc p 1

    frag :: Exp F (V3F,V3F,V2F) -> FragmentOut (Depth Float :+: Color V3F :+: Color V3F :+: Color V2F :+: ZZ)
    frag pnt = FragmentOutRastDepth $ p :. n :. t :. ZT
      where
        (p,n,t) = untup3 pnt
        V2 u v  = unpack' t

prj1 a = drop4 $ worldView @*. snoc a 1
prj0 a = drop4 $ worldView @*. snoc a 0

worldView :: Exp f M44F
worldView = Uni (IM44F "worldView")

sample rgb tix coord = texture' (Sampler PointFilter ClampToEdge $ Texture (Texture2D (Float rgb) n1) (V2 w h) NoMip [PrjFrameBuffer "" tix deferredGeometry]) coord
  where
    w = 512
    h = 512

deferredLighting :: Exp Obj (Image 1 V4F)
deferredLighting = renderScreen frag
  where
    pos  = sample RGB tix2
    norm = sample RGB tix1
    uv   = sample RG tix0

    dTex = texture' (Sampler LinearFilter ClampToEdge $ TextureSlot "diffuse" (Texture2D (Float RGBA) n1))
    nTex = texture' (Sampler LinearFilter ClampToEdge $ TextureSlot "normal" (Texture2D (Float RGB) n1))

    frag xy = FragmentOut $ c :. ZT
      where
        pointlight = v3F $ V3 1 1 1
        pointlight1 = v3F $ V3 (-5) (-1) 1
        dlight = normalize' $ prj1 pointlight @- pos xy
        dlight1 = normalize' $ prj1 pointlight1 @- pos xy
        color = v4F $ V4 0 0 1 1
        color1 = v4F $ V4 1 0 0 1
        tc = uv xy
        nTx = nTex tc
        n = normalize' $ norm xy
        c = dTex tc @* ((color @* dot' n dlight) @+ (color1 @* dot' n dlight1))

renderScreen :: (Exp F V2F -> FragmentOut (Color V4F :+: ZZ)) -> Exp Obj (Image 1 V4F)
renderScreen = PrjFrameBuffer "" tix0 . renderScreen'

renderScreen' :: (Exp F V2F -> FragmentOut (Color V4F :+: ZZ)) -> Exp Obj (FrameBuffer 1 V4F)
renderScreen' frag = Accumulate fragCtx PassAll frag rast clear
  where
    fragCtx = AccumulationContext Nothing $ ColorOp NoBlending (one' :: V4B):.ZT
    clear   = FrameBuffer (ColorImage n1 (V4 0 0 0 1):.ZT)
    rast    = Rasterize triangleCtx prims
    prims   = Transform vert input
    input   = Fetch "Quad" Triangles (IV2F "position")

    vert :: Exp V V2F -> VertexOut () V2F
    vert uv = VertexOut v4 (Const 1) ZT (NoPerspective uv':.ZT)
      where
        v4      = pack' $ V4 u v (floatV 1) (floatV 1)
        V2 u v  = unpack' uv
        uv'     = uv @* floatV 0.5 @+ floatV 0.5

quad :: Mesh
quad = Mesh
    { mAttributes   = T.singleton "position" $ A_V2F $ V.fromList [V2 a b, V2 a a, V2 b a, V2 b a, V2 b b, V2 a b]
    , mPrimitive    = P_Triangles
    , mGPUData      = Nothing
    }
  where
    a = -1
    b = 1

main :: IO ()
main = do
    let lcnet = deferredLighting

    windowSize <- initCommon "LC DSL TextureSlot Demo"

    renderer <- compileRenderer $ ScreenOut lcnet
    --putStrLn $ C.secs t ++ " - compile renderer"
    print $ slotUniform renderer
    print $ slotStream renderer
    print "renderer created"

    (mousePosition,mousePositionSink) <- external (0,0)
    (fblrPress,fblrPressSink) <- external (False,False,False,False,False)

    compiledQuad <- compileMesh quad
    addMesh renderer "Quad" compiledQuad []

    mesh <- loadMesh "Monkey.lcmesh"
    --putStrLn $ C.secs t ++ " - loadMesh Monkey.lcmesh"
    mesh2 <- loadMesh "Scene.lcmesh"
    --putStrLn $ C.secs t ++ " - loadMesh Scene.lcmesh"

    obj <- addMesh renderer "scene" mesh []
    --putStrLn $ C.secs t ++ " - addMesh Monkey.lcmesh"
    obj2 <- addMesh renderer "scene" mesh2 []
    --putStrLn $ C.secs t ++ " - addMesh Scene.lcmesh"

    let objU  = objectUniformSetter obj
        slotU = uniformSetter renderer
        diffuse = uniformFTexture2D "diffuse" slotU
        normal  = uniformFTexture2D "normal" slotU
        draw _ = do
            render renderer >> swapBuffers
            --putStrLn $ C.secs t ++ " - render frame"
            return ()
    Right img <- loadImage "Panels_Diffuse.png"
    diffuse =<< compileTexture2DRGBAF True False img

    Right img <- loadImage "Panels_Normal_Obj.png"
    normal =<< compileTexture2DRGBAF True False img
    
    s <- fpsState
    sc <- start $ do
        u <- scene (setScreenSize renderer) slotU objU windowSize mousePosition fblrPress
        return $ draw <$> u
    driveNetwork sc (readInput s mousePositionSink fblrPressSink)

    dispose renderer
    print "renderer destroyed"
    closeWindow

scene :: (Word -> Word -> IO ())
      -> T.Trie InputSetter
      -> T.Trie InputSetter
      -> Signal (Int, Int)
      -> Signal (Float, Float)
      -> Signal (Bool, Bool, Bool, Bool, Bool)
      -> SignalGen Float (Signal ())
scene setSize slotU objU windowSize mousePosition fblrPress = do
    time <- stateful 0 (+)
    last2 <- transfer ((0,0),(0,0)) (\_ n (_,b) -> (b,n)) mousePosition
    let mouseMove = (\((ox,oy),(nx,ny)) -> (nx-ox,ny-oy)) <$> last2
    cam <- userCamera (Vec3 (-4) 0 0) mouseMove fblrPress
    let setMVP  = uniformM44F "worldViewProj" slotU
        setMV   = uniformM44F "worldView" slotU
        setupGFX (w,h) (cam,dir,up,_) time = do
            let cm = fromProjective (lookat cam (cam + dir) up)
                pm = perspective 0.1 50 (pi/2) (fromIntegral w / fromIntegral h)
            setMV $! mat4ToM44F $! cm
            setMVP $! mat4ToM44F $! cm .*. pm
            setSize (fromIntegral w) (fromIntegral h)
            return ()
    r <- effectful3 setupGFX windowSize cam time
    return r

vec4ToV4F :: Vec4 -> V4F
vec4ToV4F (Vec4 x y z w) = V4 x y z w

mat4ToM44F :: Mat4 -> M44F
mat4ToM44F (Mat4 a b c d) = V4 (vec4ToV4F a) (vec4ToV4F b) (vec4ToV4F c) (vec4ToV4F d)

readInput :: State
          -> ((Float, Float) -> IO a)
          -> ((Bool, Bool, Bool, Bool, Bool) -> IO c)
          -> IO (Maybe Float)
readInput s mousePos fblrPress = do
    t <- getTime
    resetTime

    (x,y) <- getMousePosition
    mousePos (fromIntegral x,fromIntegral y)

    fblrPress =<< ((,,,,) <$> keyIsPressed KeyLeft <*> keyIsPressed KeyUp <*> keyIsPressed KeyDown <*> keyIsPressed KeyRight <*> keyIsPressed KeyRightShift)

    updateFPS s t
    k <- keyIsPressed KeyEsc
    return $ if k then Nothing else Just (realToFrac t)

-- FRP boilerplate
driveNetwork :: (p -> IO (IO a)) -> IO (Maybe p) -> IO ()
driveNetwork network driver = do
    dt <- driver
    case dt of
        Just dt -> do
            join $ network dt
            --putStrLn $ C.secs t ++ " - FRP loop"
            --putStrLn ""
            driveNetwork network driver
        Nothing -> return ()

-- OpenGL/GLFW boilerplate

initCommon :: String -> IO (Signal (Int, Int))
initCommon title = do
    initialize
    openWindow defaultDisplayOptions
        { displayOptions_numRedBits         = 8
        , displayOptions_numGreenBits       = 8
        , displayOptions_numBlueBits        = 8
        , displayOptions_numAlphaBits       = 8
        , displayOptions_numDepthBits       = 24
        , displayOptions_width              = 512
        , displayOptions_height             = 512
        , displayOptions_windowIsResizable  = True
        , displayOptions_openGLVersion      = (3,2)
        , displayOptions_openGLProfile      = CoreProfile
--        , displayOptions_displayMode    = Fullscreen
        }
    setWindowTitle title

    (windowSize,windowSizeSink) <- external (0,0)
    setWindowSizeCallback $ \w h -> do
        glViewport 0 0 (fromIntegral w) (fromIntegral h)
        putStrLn $ "window size changed " ++ show (w,h)
        windowSizeSink (fromIntegral w, fromIntegral h)

    return windowSize

-- FPS tracking

data State = State { frames :: IORef Int, t0 :: IORef Double }

fpsState :: IO State
fpsState = State <$> newIORef 0 <*> newIORef 0

updateFPS :: State -> Double -> IO ()
updateFPS state t1 = do
    let t = 1000*t1
        fR = frames state
        tR = t0 state
    modifyIORef fR (+1)
    t0' <- readIORef tR
    writeIORef tR $ t0' + t
    when (t + t0' >= 5000) $ do
    f <- readIORef fR
    let seconds = (t + t0') / 1000
        fps = fromIntegral f / seconds
    --putStrLn (show (round fps) ++ " FPS - " ++ show f ++ " frames in " ++ C.secs seconds)
    writeIORef tR 0
    writeIORef fR 0

-- Continuous camera state (rotated with mouse, moved with arrows)
userCamera :: Real p => Vec3 -> Signal (Float, Float) -> Signal (Bool, Bool, Bool, Bool, Bool)
           -> SignalGen p (Signal (Vec3, Vec3, Vec3, (Float, Float)))
userCamera p mposs keyss = transfer2 (p,zero,zero,(0,0)) calcCam mposs keyss
  where
    d0 = Vec4 0 0 (-1) 1
    u0 = Vec4 0 1 0 1
    calcCam dt (dmx,dmy) (ka,kw,ks,kd,turbo) (p0,_,_,(mx,my)) = (p',d,u,(mx',my'))
      where
        f0 c n = if c then (&+ n) else id
        p'  = foldr1 (.) [f0 ka (v &* (-t)),f0 kw (d &* t),f0 ks (d &* (-t)),f0 kd (v &* t)] p0
        k   = if turbo then 5 else 1
        t   = k * realToFrac dt
        mx' = dmx + mx
        my' = dmy + my
        rm  = fromProjective $ rotationEuler $ Vec3 (mx' / 100) (my' / 100) 0
        d   = trim $ rm *. d0 :: Vec3
        u   = trim $ rm *. u0 :: Vec3
        v   = normalize $ d &^ u

-- | Perspective transformation matrix in row major order.
perspective :: Float  -- ^ Near plane clipping distance (always positive).
            -> Float  -- ^ Far plane clipping distance (always positive).
            -> Float  -- ^ Field of view of the y axis, in radians.
            -> Float  -- ^ Aspect ratio, i.e. screen's width\/height.
            -> Mat4
perspective n f fovy aspect = transpose $
    Mat4 (Vec4 (2*n/(r-l))       0       (-(r+l)/(r-l))        0)
         (Vec4     0        (2*n/(t-b))  ((t+b)/(t-b))         0)
         (Vec4     0             0       (-(f+n)/(f-n))  (-2*f*n/(f-n)))
         (Vec4     0             0            (-1)             0)
  where
    t = n*tan(fovy/2)
    b = -t
    r = aspect*t
    l = -r

-- | Pure orientation matrix defined by Euler angles.
rotationEuler :: Vec3 -> Proj4
rotationEuler (Vec3 a b c) = orthogonal $ toOrthoUnsafe $ rotMatrixY a .*. rotMatrixX b .*. rotMatrixZ c

-- | Camera transformation matrix.
lookat :: Vec3   -- ^ Camera position.
       -> Vec3   -- ^ Target position.
       -> Vec3   -- ^ Upward direction.
       -> Proj4
lookat pos target up = translateBefore4 (neg pos) (orthogonal $ toOrthoUnsafe r)
  where
    w = normalize $ pos &- target
    u = normalize $ up &^ w
    v = w &^ u
    r = transpose $ Mat3 u v w
