{-# LANGUAGE OverloadedStrings, PackageImports #-}

import Graphics.Rendering.OpenGL.Raw.Core32 (glViewport)
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Data.Monoid
import Control.Monad
import Data.Vect
import qualified Data.Trie as T
import qualified Data.Vector.Storable as SV

import LambdaCube.GL
import LambdaCube.GL.Mesh

import System.Environment

import Codec.Image.STB hiding (Image)

import ToDeBruijn
import ParseTrifectaLC

--  Our vertices. Tree consecutive floats give a 3D vertex; Three consecutive vertices give a triangle.
--  A cube has 6 faces with 2 triangles each, so this makes 6*2=12 triangles, and 12*3 vertices
g_vertex_buffer_data =
    [ ( 1.0, 1.0,-1.0)
    , ( 1.0,-1.0,-1.0)
    , (-1.0,-1.0,-1.0)
    , ( 1.0, 1.0,-1.0)
    , (-1.0,-1.0,-1.0)
    , (-1.0, 1.0,-1.0)
    , ( 1.0, 1.0,-1.0)
    , ( 1.0, 1.0, 1.0)
    , ( 1.0,-1.0, 1.0)
    , ( 1.0, 1.0,-1.0)
    , ( 1.0,-1.0, 1.0)
    , ( 1.0,-1.0,-1.0)
    , ( 1.0, 1.0, 1.0)
    , (-1.0,-1.0, 1.0)
    , ( 1.0,-1.0, 1.0)
    , ( 1.0, 1.0, 1.0)
    , (-1.0, 1.0, 1.0)
    , (-1.0,-1.0, 1.0)
    , (-1.0, 1.0, 1.0)
    , (-1.0,-1.0,-1.0)
    , (-1.0,-1.0, 1.0)
    , (-1.0, 1.0, 1.0)
    , (-1.0, 1.0,-1.0)
    , (-1.0,-1.0,-1.0)
    , ( 1.0, 1.0,-1.0)
    , (-1.0, 1.0,-1.0)
    , (-1.0, 1.0, 1.0)
    , ( 1.0, 1.0,-1.0)
    , (-1.0, 1.0, 1.0)
    , ( 1.0, 1.0, 1.0)
    , ( 1.0, 1.0,-1.0)
    , ( 1.0, 1.0, 1.0)
    , (-1.0, 1.0, 1.0)
    , ( 1.0, 1.0,-1.0)
    , (-1.0, 1.0, 1.0)
    , (-1.0, 1.0,-1.0)
    ]

--  Two UV coordinatesfor each vertex. They were created with Blender.
g_uv_buffer_data =
    [ (0.0, 0.0)
    , (0.0, 1.0)
    , (1.0, 1.0)
    , (0.0, 0.0)
    , (1.0, 1.0)
    , (1.0, 0.0)
    , (0.0, 0.0)
    , (1.0, 0.0)
    , (1.0, 1.0)
    , (0.0, 0.0)
    , (1.0, 1.0)
    , (0.0, 1.0)
    , (1.0, 0.0)
    , (0.0, 1.0)
    , (1.0, 1.0)
    , (1.0, 0.0)
    , (0.0, 0.0)
    , (0.0, 1.0)
    , (0.0, 0.0)
    , (1.0, 1.0)
    , (0.0, 1.0)
    , (0.0, 0.0)
    , (1.0, 0.0)
    , (1.0, 1.0)
    , (0.0, 0.0)
    , (1.0, 0.0)
    , (1.0, 1.0)
    , (0.0, 0.0)
    , (1.0, 1.0)
    , (0.0, 1.0)
    , (0.0, 0.0)
    , (0.0, 1.0)
    , (1.0, 1.0)
    , (0.0, 0.0)
    , (1.0, 1.0)
    , (1.0, 0.0)
    ]

myCube :: Mesh
myCube = Mesh
    { mAttributes   = T.fromList
        [ ("position", A_V4F $ SV.fromList [V4 x y z 1 | (x,y,z) <- g_vertex_buffer_data])
        , ("vertexUV", A_V2F $ SV.fromList [V2 u v | (u,v) <- g_uv_buffer_data])
        ]
    , mPrimitive    = P_Triangles
    , mGPUData      = Nothing
    }

rendererFromDSL :: String -> IO Renderer
rendererFromDSL fname = do
  Right lcAST <- parseLC fname
  let lcNet = case toNF mempty lcAST of
        [N a] -> a
        a -> error $ show a
  compileRendererFromCore lcNet

main :: IO ()
main = do
    win <- initWindow "LambdaCube 3D DSL Sample" 1024 768
    let keyIsPressed k = fmap (==KeyState'Pressed) $ getKey win k

    n <- getArgs

    gpuCube <- compileMesh myCube

    let setup = do
          renderer <- rendererFromDSL $ case n of
            [fn]  -> fn
            _     -> "gfx01.lc"
          addMesh renderer "stream" gpuCube []
          putStrLn "reloaded"
          return renderer
    --let cm  = fromProjective (lookat (Vec3 4 0.5 (-0.6)) (Vec3 0 0 0) (Vec3 0 1 0))
    let cm  = fromProjective (lookat (Vec3 3 1.3 0.3) (Vec3 0 0 0) (Vec3 0 1 0))
        pm  = perspective 0.1 100 (pi/4) (1024 / 768)
        loop renderer = do
            let uniformMap      = uniformSetter renderer
                texture         = uniformFTexture2D "myTextureSampler" uniformMap
                mvp             = uniformM44F "MVP" uniformMap
                setWindowSize   = setScreenSize renderer

            setWindowSize 1024 768
            Just t <- getTime
            let angle = pi / 24 * realToFrac t
                mm = fromProjective $ rotationEuler $ Vec3 angle 0 0
            mvp $! mat4ToM44F $! mm .*. cm .*. pm
            render renderer
            swapBuffers win >> pollEvents

            k <- keyIsPressed Key'Escape
            reload <- keyIsPressed Key'R
            rend' <- if not reload then return renderer else do
              dispose renderer
              setup
            when k $ dispose rend'
            unless k $ loop rend'
    loop =<< setup

    destroyWindow win
    terminate

vec4ToV4F :: Vec4 -> V4F
vec4ToV4F (Vec4 x y z w) = V4 x y z w

mat4ToM44F :: Mat4 -> M44F
mat4ToM44F (Mat4 a b c d) = V4 (vec4ToV4F a) (vec4ToV4F b) (vec4ToV4F c) (vec4ToV4F d)

initWindow :: String -> Int -> Int -> IO Window
initWindow title width height = do
    GLFW.init
    defaultWindowHints
    mapM_ windowHint
      [ WindowHint'ContextVersionMajor 3
      , WindowHint'ContextVersionMinor 3
      , WindowHint'OpenGLProfile OpenGLProfile'Core
      , WindowHint'OpenGLForwardCompat True
      ]
    Just win <- createWindow width height title Nothing Nothing
    makeContextCurrent $ Just win

    setWindowSizeCallback win $ Just $ \_ w h -> do
        glViewport 0 0 (fromIntegral w) (fromIntegral h)

    return win

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
