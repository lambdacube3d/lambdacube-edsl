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
import qualified Data.ByteString.Lazy as LB
import qualified Data.Trie as T
import qualified Data.Vector.Storable as SV
import System.Environment

import LC_API

import Graphics.Rendering.OpenGL.Raw.Core32
import LC_Mesh
import Codec.Image.STB hiding (Image)

import Data.Maybe
import Data.Bitmap.Pure

import BuiltinVec
import Utility
import Scanlines
import Vignette

import Thrift.ContentProvider_Client
import Thrift.Content_Types
import ThriftUtils
import Data.Bitmap.IO
import Foreign.Ptr (castPtr)

n_rotation = "rotation"
n_position = "position"
n_backgroundSlot = "ScreenQuad"
n_backgroundTex = "background"
n_diffuseTex = "ScreenQuad"
n_postSlot = "postSlot"

smp :: ByteString -> Exp F V2F -> Exp F V4F
smp n uv = texture' (Sampler LinearFilter Clamp $ TextureSlot n $ Texture2D (Float RGBA) n1) uv

background tex = Accumulate fragCtx PassAll frag rast clear
  where
    fragCtx = AccumulationContext Nothing $ ColorOp NoBlending (one' :: V4B):.ZT
    clear   = FrameBuffer (ColorImage n1 (V4 0 0 0 1):.ZT)
    rast    = Rasterize rCtx prims
    rCtx    = triangleCtx
    prims   = Transform vert input
    input   = Fetch n_backgroundSlot Triangles (IV2F n_position)

    frag uv' = FragmentOut $ smp tex uv :. ZT
      where
        uv = uv' @* floatF 0.5 @+ floatF 0.5

    vert :: Exp V V2F -> VertexOut () V2F
    vert uv = VertexOut out (Const 1) ZT (NoPerspective uv:.ZT)
      where
        out = vec4' uv (floatV 1) (floatV 1)

model :: Exp Obj (FrameBuffer 1 V4F)
model = Accumulate fragCtx PassAll frag rast (background n_backgroundTex)
  where
    blend   = Blend (FuncAdd, FuncAdd) ((SrcAlpha, One), (SrcAlpha, OneMinusSrcAlpha)) zero'
    fragCtx = AccumulationContext Nothing $ ColorOp blend (one' :: V4B):.ZT
    rast    = Rasterize rCtx prims
    rCtx    = PointCtx ProgramPointSize 10 UpperLeft
    prims   = Transform vert input
    input   = Fetch n_postSlot Points (IV3F n_position)

    vert :: Exp V V3F -> VertexOut () ()
    vert uvw = VertexOut out (Const 20) ZT ZT
      where
        out = rotation @*. vec4' uvw (floatV 1)

    rotation = Uni (IM44F n_rotation)

    frag :: Exp F () -> FragmentOut (Color V4F :+: ZZ)
    frag _ = FragmentOut $ a :. ZT
      where
        V4 x _ _ _ = unpack' $
            (smp n_diffuseTex $ pointCoord')
        a = vec4' x x x x @* floatF 0.3

postProcess base = renderScreen $ FragmentOut . (:.ZT) . f
  where
    f uv = fVignette vign uv $
           fScanlines sl uv $
           smp' base uv
    sl = scanlines { scanlinesFrequency = floatF 100
                   , scanlinesHigh = Const (V4 0.5 1 1 1)
                   , scanlinesLow = Const (V4 0.2 0.5 0.5 1)
                   }
    vign = vignette { vignetteOuterRadius = floatF 0.9
                    , vignetteInnerRadius = floatF 0.4
                    }
    smp' img uv = texture' (Sampler LinearFilter Clamp $ Texture (Texture2D (Float RGBA) n1) (V2 512 512) NoMip [img]) uv

{-
LC side:
    name
    type

Blender side:
    name/data path
-}
{-
updates:
    startup:
        - textures
        - objects in object sets
    per frame:
        - per object uniforms
        - global uniforms
-}

{-
  Hint:
    uniform and slot types will be queried from renderer
-}
-- list of pairs: lc uniform name, blender full data path e.g. D.objects['Cube'].location or D.textures['Texture']
setRemoteUniform :: Slot -> [(ByteString,ByteString)] -> Renderer -> IO ()
setRemoteUniform slot unis renderer = do
    return ()

-- lc slot name, [(object uniform name, blender object property)], return a pair: per frame object uniform update, remove objects
mkRemoteObjectSet :: Slot -> ByteString -> [(ByteString,ByteString)] -> Renderer -> IO (IO (), IO ())
mkRemoteObjectSet slot slotName unis renderer = do
    let act = do
            --pl <- query slot ["bpy.data.objects['Cube'].location"]
            --print pl
            return ()

    return (act, return ())

textures :: [(ByteString,ByteString)]
textures =
    [ (n_diffuseTex,        "D.textures['Diffuse']")
    , (n_backgroundTex,     "D.textures['Background']")
    ]

objectUniforms :: [(ByteString,ByteString)]
objectUniforms =
    [ (n_rotation,  "rotation")
    , (n_position,  "location")
    ]

main :: IO ()
main = do
    let lcnet :: Exp Obj (Image 1 V4F)
        lcnet = postProcess $ PrjFrameBuffer "outFB" tix0 model

    windowSize <- initCommon "LC DSL 2D Demo"

    renderer <- compileRenderer $ ScreenOut lcnet
    print $ slotUniform renderer
    print $ slotStream renderer
    print "renderer created"

    initUtility renderer
    slot <- protocol
    setRemoteUniform slot textures renderer
    (updateObjUnis,rmObjs) <- mkRemoteObjectSet slot n_postSlot objectUniforms renderer

    let draw _  = render renderer >> swapBuffers
        setupGFX (w,h) = (setScreenSize renderer) (fromIntegral w) (fromIntegral h)
        remoteInputs = [updateObjUnis]

    -- temp:
    let slotU   = uniformSetter renderer
        diffuse = uniformFTexture2D n_backgroundTex slotU
        diff    = uniformFTexture2D n_diffuseTex slotU
        texSize = 128
        setRotation = uniformM44F n_rotation slotU
        rx = rotMatrixProj4 0 (Vec3 0 1 0)
        ry = rotMatrixProj4 0 (Vec3 1 0 0)
        vec4ToV4F :: Vec4 -> V4F
        vec4ToV4F (Vec4 x y z w) = V4 x y z w

        mat4ToM44F :: Mat4 -> M44F
        mat4ToM44F (Mat4 a b c d) = V4 (vec4ToV4F a) (vec4ToV4F b) (vec4ToV4F c) (vec4ToV4F d)

    setRotation $ mat4ToM44F $ fromProjective $ rx .*. ry
    [imgData] <- downloadTexture slot "Tex" IT_RGBA8 (fromIntegral texSize) (fromIntegral texSize)
    [imgData2] <- downloadTexture slot "Tex2" IT_RGBA8 (fromIntegral texSize) (fromIntegral texSize)

    pl <- query slot ["bpy.data.objects['Cube'].location"]
    print pl
    let alig = Just 1
        toTex img = SB.useAsCString (SB.concat $ LB.toChunks img) $ \ptr -> do
            bitmap <- copyBitmapFromPtr (texSize,texSize) 4 0 (castPtr ptr) alig
            compileTexture2DRGBAF False True $ unsafeFreezeBitmap bitmap
    tex <- toTex imgData
    tex2 <- toTex imgData2
    diffuse tex
    diff tex2

    mesh <- remoteMesh slot "Cube"
    obj <- compileMesh mesh { mPrimitive = P_Points, mGPUData = Nothing }
           >>= addMesh renderer n_postSlot `flip` []
    -- temp end

    (mousePosition,mousePositionSink) <- external (0,0)
    (fblrPress,fblrPressSink) <- external (False,False,False,False,False)
    s <- fpsState

    sc <- start $ do
        u <- effectful1 setupGFX windowSize
        return $ draw <$> u
    driveNetwork sc (readInput s remoteInputs mousePositionSink fblrPressSink)

    dispose renderer
    print "renderer destroyed"
    closeWindow

readInput :: State
          -> [IO ()]
          -> ((Float, Float) -> IO a)
          -> ((Bool, Bool, Bool, Bool, Bool) -> IO c)
          -> IO (Maybe Float)
readInput s acts mousePos fblrPress = do
    sequence_ acts
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
    putStrLn (show (round fps) ++ " FPS - " ++ show f ++ " frames in ")
    writeIORef tR 0
    writeIORef fR 0
