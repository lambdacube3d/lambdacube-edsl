{-# LANGUAGE OverloadedStrings, PackageImports, TypeOperators, DataKinds, FlexibleContexts, GADTs #-}

import Graphics.Rendering.OpenGL.Raw.Core32
import qualified Graphics.UI.GLFW as GLFW
import Control.Monad
import Data.Vect
import qualified Data.Trie as T
import qualified Data.Vector.Storable as SV

import        LambdaCube.GL
import        LambdaCube.GL.Mesh

import Common.Utils
import Common.GraphicsUtils

import Codec.Image.STB hiding (Image)
import FX

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
        [ ("vertexPosition_modelspace", A_V3F $ SV.fromList [V3 x y z | (x,y,z) <- g_vertex_buffer_data])
        , ("vertexUV",                  A_V2F $ SV.fromList [V2 u v | (u,v) <- g_uv_buffer_data])
        ]
    , mPrimitive    = P_Triangles
    , mGPUData      = Nothing
    }

texturing :: Exp Obj (Texture Tex2D SingleTex (Regular Float) RGBA) -> Exp Obj (VertexStream Triangle (V3F,V2F)) -> Exp Obj (FrameBuffer 1 (Float,V4F))
texturing tex objs = Accumulate fragmentCtx PassAll fragmentShader fragmentStream emptyFB
  where
    rasterCtx :: RasterContext Triangle
    rasterCtx = TriangleCtx (CullNone) PolygonFill NoOffset LastVertex

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

color t uv = texture' (smp t) uv
smp t = Sampler LinearFilter ClampToEdge t

main :: IO ()
main = do
    (win,windowSize) <- initWindow "LambdaCube 3D Textured Cube" 1024 768
    let keyIsPressed k = fmap (==KeyState'Pressed) $ getKey win k

    let texture = TextureSlot "myTextureSampler" $ Texture2D (Float RGBA) n1
        frameImage :: Exp Obj (Image 1 V4F)
        frameImage = PrjFrameBuffer "" tix0 $ texturing texture (Fetch "stream" Triangles (IV3F "vertexPosition_modelspace", IV2F "vertexUV"))

        fx img = PrjFrameBuffer "" tix0 $ texturing (imgToTex $ postProcess $ img) (Fetch "stream" Triangles (IV3F "vertexPosition_modelspace", IV2F "vertexUV"))
        imgToTex img = Texture (Texture2D (Float RGBA) n1) (V2 512 512) NoMip [img]

    renderer <- compileRenderer $ SamplerOut "image" $ Sampler LinearFilter ClampToEdge $ Texture (Texture2D (Float RGBA) n1) (V2 512 512) NoMip [iterate fx frameImage !! 1]

    renderer2 <- do
      let f uv = texture' (Sampler LinearFilter ClampToEdge $ TextureSlot "imageToShow" $ Texture2D (Float RGBA) n1 ) uv
      compileRenderer $ ScreenOut $ renderScreen $ FragmentOut . (:.ZT) . f

    initUtility renderer
    initUtility renderer2

    let uniformMap      = uniformSetter renderer
        texture         = uniformFTexture2D "myTextureSampler" uniformMap
        mvp             = uniformM44F "MVP" uniformMap
        Just samplerOut = T.lookup "image" $ samplerOutput renderer

    let uniformMap2     = uniformSetter renderer2
        texture2        = uniformFTexture2D "imageToShow" uniformMap2
        setWindowSize2  = setScreenSize renderer2

    setWindowSize2 640 480

    Right img <- loadImage "hello.png" -- "uvtemplate.bmp"
    texture =<< compileTexture2DRGBAF True False img
    texture2 samplerOut

    gpuCube <- compileMesh myCube
    addMesh renderer "stream" gpuCube []

    --let cm  = fromProjective (lookat (Vec3 4 0.5 (-0.6)) (Vec3 0 0 0) (Vec3 0 1 0))
    let cm  = fromProjective (lookat (Vec3 3 1.3 0.3) (Vec3 0 0 0) (Vec3 0 1 0))
        pm  = perspective 0.1 100 (pi/4) (1024 / 768)
        loop = do
            Just t <- getTime
            let angle = pi / 24 * realToFrac t
                mm = fromProjective $ rotationEuler $ Vec3 angle 0 0
            mvp $! mat4ToM44F $! mm .*. cm .*. pm
            render renderer
            render renderer2
            swapBuffers win >> pollEvents

            k <- keyIsPressed Key'Escape
            unless k $ loop
    loop

    dispose renderer2
    dispose renderer
    destroyWindow win
    terminate

vec4ToV4F :: Vec4 -> V4F
vec4ToV4F (Vec4 x y z w) = V4 x y z w

mat4ToM44F :: Mat4 -> M44F
mat4ToM44F (Mat4 a b c d) = V4 (vec4ToV4F a) (vec4ToV4F b) (vec4ToV4F c) (vec4ToV4F d)
