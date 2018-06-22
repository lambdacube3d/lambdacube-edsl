{-# LANGUAGE OverloadedStrings, PackageImports, TypeOperators, DataKinds, FlexibleContexts #-}

import qualified Graphics.UI.GLFW as GLFW
import Control.Monad
import Data.Vect
import qualified Data.Trie as T
import qualified Data.Vector.Storable as SV

import LambdaCube.GL
import LambdaCube.GL.Mesh

import Codec.Image.STB hiding (Image)

import Paths_lambdacube_samples (getDataFileName)

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

cube :: Mesh
cube = Mesh
    { mAttributes   = T.fromList
        [ ("vertexPosition_modelspace", A_V3F $ SV.fromList [V3 x y z | (x,y,z) <- g_vertex_buffer_data])
        , ("vertexUV",                  A_V2F $ SV.fromList [V2 u v | (u,v) <- g_uv_buffer_data])
        ]
    , mPrimitive    = P_Triangles
    , mGPUData      = Nothing
    }

texturing :: Exp Obj (VertexStream Triangle (V3F,V2F)) -> Exp Obj (FrameBuffer 1 (Float,V4F))
texturing objs = Accumulate fragmentCtx PassAll fragmentShader fragmentStream emptyFB
  where
    rasterCtx :: RasterContext Triangle
    rasterCtx = TriangleCtx (CullFront CW) PolygonFill NoOffset LastVertex

    fragmentCtx :: AccumulationContext (Depth Float :+: (Color (V4 Float) :+: ZZ))
    fragmentCtx = AccumulationContext Nothing $ DepthOp Less True:.ColorOp NoBlending (one' :: V4B):.ZT

    emptyFB :: Exp Obj (FrameBuffer 1 (Float,V4F))
    emptyFB = FrameBuffer (DepthImage n1 1000:.ColorImage n1 (V4 0 0 0.4 1):.ZT)

    fragmentStream :: Exp Obj (FragmentStream 1 V2F)
    fragmentStream = Rasterize rasterCtx primitiveStream

    primitiveStream :: Exp Obj (PrimitiveStream Triangle () 1 V V2F)
    primitiveStream = Transform vertexShader objs

    modelViewProj :: Exp V M44F
    modelViewProj = Uni (IM44F "MVP")

    vertexShader :: Exp V (V3F,V2F) -> VertexOut () V2F
    vertexShader puv = VertexOut v4 (Const 1) ZT (Smooth uv:.ZT)
      where
        v4 :: Exp V V4F
        v4 = modelViewProj @*. v3v4 p
        (p,uv) = untup2 puv

    fragmentShader :: Exp F V2F -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    fragmentShader uv = FragmentOutRastDepth $ color tex uv :. ZT
      where
        tex = TextureSlot "myTextureSampler" $ Texture2D (Float RGBA) n1

v3v4 :: Exp s V3F -> Exp s V4F
v3v4 v = let V3 x y z = unpack' v in pack' $ V4 x y z (Const 1)

color t uv = texture' (smp t) uv
smp t = Sampler LinearFilter ClampToEdge t

main :: IO ()
main = do
    GLFW.init
    GLFW.defaultWindowHints
    mapM_ GLFW.windowHint
      [ GLFW.WindowHint'ContextVersionMajor 3
      , GLFW.WindowHint'ContextVersionMinor 3
      , GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
      , GLFW.WindowHint'OpenGLForwardCompat True
      ]
    Just win <- GLFW.createWindow 1024 768 "LambdaCube 3D Textured Cube" Nothing Nothing
    GLFW.makeContextCurrent $ Just win

    let frameImage :: Exp Obj (Image 1 V4F)
        frameImage = PrjFrameBuffer "" tix0 $ texturing $ Fetch "stream" Triangles (IV3F "vertexPosition_modelspace", IV2F "vertexUV")

    renderer <- compileRenderer $ ScreenOut frameImage

    let uniformMap      = uniformSetter renderer
        texture         = uniformFTexture2D "myTextureSampler" uniformMap
        mvp             = uniformM44F "MVP" uniformMap
        setWindowSize   = setScreenSize renderer

    setWindowSize 1024 768
    Right img <- loadImage =<< getDataFileName "hello.png"
    texture =<< compileTexture2DRGBAF True False img

    gpuCube <- compileMesh cube
    addMesh renderer "stream" gpuCube []

    let keyIsPressed k = fmap (==GLFW.KeyState'Pressed) $ GLFW.getKey win k
        cm  = fromProjective (lookat (Vec3 4 3 3) (Vec3 0 0 0) (Vec3 0 1 0))
        pm  = perspective 0.1 100 (pi/4) (1024 / 768)
        loop = do
            Just t <- GLFW.getTime
            let angle = pi / 2 * realToFrac t
                mm = fromProjective $ rotationEuler $ Vec3 angle 0 0
            mvp $! mat4ToM44F $! mm .*. cm .*. pm
            render renderer
            GLFW.swapBuffers win >> GLFW.pollEvents

            k <- keyIsPressed GLFW.Key'Escape
            unless k $ loop
    loop

    dispose renderer
    GLFW.destroyWindow win
    GLFW.terminate

vec4ToV4F :: Vec4 -> V4F
vec4ToV4F (Vec4 x y z w) = V4 x y z w

mat4ToM44F :: Mat4 -> M44F
mat4ToM44F (Mat4 a b c d) = V4 (vec4ToV4F a) (vec4ToV4F b) (vec4ToV4F c) (vec4ToV4F d)

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
