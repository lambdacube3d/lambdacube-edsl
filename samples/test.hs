{-# LANGUAGE OverloadedStrings, PackageImports, TypeOperators, DataKinds #-}

import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Data.Trie as T
import Data.Vect.Float
import Data.Word
import Foreign
import qualified Data.Vector.Storable as V

import "GLFW-b" Graphics.UI.GLFW as GLFW

import LCType
import LCAPIType
import LCDSLType
import LCHOAS
import LCGL
--import LCShow
import LCConvert
import LCLanguage

-- specialized snoc
snoc :: Exp s V3F -> Float -> Exp s V4F
snoc v s = let V3 x y z = unpack' v in pack' $ V4 x y z (Const s)

drop4 :: Exp s V4F -> Exp s V3F
drop4 v = let V4 x y z _ = unpack' v in pack' $ V3 x y z

drop3 :: Exp s V3F -> Exp s V2F
drop3 v = let V3 x y _ = unpack' v in pack' $ V2 x y

simple :: Exp Obj (VertexStream Triangle (V3F,V3F)) -> Exp Obj (FrameBuffer 1 V4F)
simple objs = Accumulate fragCtx PassAll frag rast clear
  where
    worldViewProj = Uni (IM44F "worldViewProj")
    clear   = FrameBuffer (ColorImage n1 (zero'::V4F):.ZT)
    fragCtx = AccumulationContext Nothing $ ColorOp NoBlending (one' :: V4B):.ZT
    rast    = Rasterize triangleCtx prims
    prims   = Transform vert objs

    vert :: Exp V (V3F,V3F) -> VertexOut () V3F
    vert pn = VertexOut v4 (Const 1) ZT (Flat (drop4 v4):.ZT)
      where
        v4    = worldViewProj @*. snoc p 1
        (p,n) = untup2 pn

    frag :: Exp F V3F -> FragmentOut (Color V4F :+: ZZ)
    frag a = FragmentOut $ snoc a 1 :. ZT

    diffuse     = TextureSlot "diffuse" $ Texture2D (Float RGB) 1
    sampler     = Sampler PointFilter Repeat diffuse
    texSize     = textureSize' sampler $ Const 0
    renderTex   = Texture (Texture2D (Float RGBA) 1) AutoMip (PrjFrameBuffer "" tix0 $ simple objs)

    frag' :: Exp F V3F -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    frag' a = FragmentOutRastDepth $ snoc (texture' sampler (drop3 a)) 1 :. ZT

{-
-- API overview
  -- low level
  compileBuffer   :: [Array] -> IO Buffer
  compileRenderer :: Exp Obj (FrameBuffer c d s) -> IO Renderer

  -- high level
  addObject       :: Renderer -> ByteString -> Maybe IndexStream -> Trie Stream -> [ByteString] -> IO Object
  removeObject    :: Renderer -> Object -> IO ()
  objectUniforms  :: Object -> Trie InputSetter
-}

initGL :: String -> IO ()
initGL title = do
    initialize
    openWindow defaultDisplayOptions
        { displayOptions_numRedBits     = 8
        , displayOptions_numGreenBits   = 8
        , displayOptions_numBlueBits    = 8
        , displayOptions_numDepthBits   = 24
        , displayOptions_width          = 800
        , displayOptions_height         = 600
--        , displayOptions_displayMode    = Fullscreen
        }
    setWindowTitle title

-- simple use case
main :: IO ()
main = do
    let --lcnet :: Exp Obj (FrameBuffer Z (Color :+: Z) (V4F :+: Z))
        lcnet :: Exp Obj (Image 1 V4F)
        lcnet  = PrjFrameBuffer "outFB" tix0 $ simple $ Fetch "streamSlot" Triangles (IV3F "position", IV3F "normal")
        --lcnet' = convertGP lcnet
{-
    dumpGP lcnet'
    print $ D.uniformInput lcnet'
    print $ D.streamInput lcnet'
-}
    initGL "LCGL Demo"

    -- create user data, it is independent from GFX network
    let vecs = V.fromList [Vec3 0 1 0, Vec3 0 1 0, Vec3 0 1 0]
        with' w a f = w a (\p -> f $ castPtr p)
    buffer <- compileBuffer [Array ArrFloat (3 * V.length vecs) $ with' V.unsafeWith vecs]

    let mesh = T.fromList [("position", Stream TV3F buffer 0 0 (V.length vecs))]
    -- create/load renderer
    renderer <- compileRenderer [ScreenOut lcnet]

    print $ slotUniform renderer
    print $ slotStream renderer
    print "renderer created"

    -- add user objects to GFX network
    obj <- addObject renderer "streamSlot" TriangleList Nothing mesh ["worldViewProj"]
    let objU = objectUniformSetter obj
        Just (SM44F matSetter) = T.lookup "worldViewProj" objU

    -- render loop
    replicateM 100 $ do
        matSetter $ V4 (V4 1 0 0 0) (V4 0 1 0 0) (V4 0 0 1 0) (V4 0 0 0 1)
        render renderer

    dispose renderer
    print "renderer destroyed"
    closeWindow
