module Backend where

import Debug.Trace
import Prelude.Unsafe (unsafeIndex)

import qualified Graphics.WebGLRaw as GL
import Control.Monad.Eff.Exception
import Control.Monad.Eff.WebGL
import Control.Monad.Eff
import Control.Monad
import Data.Foldable
import Data.Traversable
import qualified Data.StrMap as StrMap
import Data.Tuple

import IR
import Util

type GFX a = forall e . Eff (webgl :: WebGl, trace :: Trace, err :: Exception | e) a

setupRasterContext :: RasterContext -> GFX Unit
setupRasterContext = cvt
  where
    cff :: FrontFace -> GL.GLenum
    cff CCW = GL._CCW
    cff CW  = GL._CW

    -- not presented in WebGL
    {-
    setProvokingVertex :: ProvokingVertex -> IO ()
    setProvokingVertex pv = glProvokingVertex $ case pv of
        FirstVertex -> gl_FIRST_VERTEX_CONVENTION
        LastVertex  -> gl_LAST_VERTEX_CONVENTION
    -}

    setPointSize :: PointSize -> GFX Unit
    setPointSize ps = case ps of
        ProgramPointSize    -> return unit
        PointSize s         -> trace "PointSize is not supported!"

    cvt :: RasterContext -> GFX Unit
    cvt (PointCtx ps fts sc) = do
        setPointSize ps
        {-
        glPointParameterf gl_POINT_FADE_THRESHOLD_SIZE (realToFrac fts)
        glPointParameterf gl_POINT_SPRITE_COORD_ORIGIN $ realToFrac $ case sc of
            LowerLeft   -> gl_LOWER_LEFT
            UpperLeft   -> gl_UPPER_LEFT
        -}

    cvt (LineCtx lw pv) = do
        GL.lineWidth_ lw
        --setProvokingVertex pv

    cvt (TriangleCtx cm pm po pv) = do
        -- cull mode
        case cm of
            CullNone    -> GL.disable_ GL._CULL_FACE
            CullFront f -> do
                GL.enable_    GL._CULL_FACE
                GL.cullFace_  GL._FRONT
                GL.frontFace_ $ cff f
            CullBack f -> do
                GL.enable_    GL._CULL_FACE
                GL.cullFace_  GL._BACK
                GL.frontFace_ $ cff f

        -- polygon mode
        -- not presented
        {-
        case pm of
            PolygonPoint ps -> do
                setPointSize ps
                glPolygonMode gl_FRONT_AND_BACK gl_POINT
            PolygonLine lw  -> do
                GL.lineWidth_ lw
                glPolygonMode gl_FRONT_AND_BACK gl_LINE
            PolygonFill  -> glPolygonMode gl_FRONT_AND_BACK gl_FILL
        -}
        -- polygon offset
        -- not presented: glDisable gl_POLYGON_OFFSET_POINT
        -- not presented: glDisable gl_POLYGON_OFFSET_LINE
        GL.disable_ GL._POLYGON_OFFSET_FILL
        case po of
            NoOffset -> return unit
            Offset f u -> do
                GL.polygonOffset_ f u
                GL.enable_ GL._POLYGON_OFFSET_FILL

        -- provoking vertex
        -- not presented: setProvokingVertex pv

setupAccumulationContext :: AccumulationContext -> GFX Unit
setupAccumulationContext {accViewportName = n, accOperations = ops} = cvt ops
  where
    cvt :: [FragmentOperation] -> GFX Unit
    cvt (StencilOp a b c : DepthOp f m : xs) = do
        -- TODO
        cvtC 0 xs
    cvt (StencilOp a b c : xs) = do
        -- TODO
        cvtC 0 xs
    cvt (DepthOp df dm : xs) = do
        -- TODO
        GL.disable_ GL._STENCIL_TEST
        let glDF = comparisonFunctionToGLType df
        case glDF == comparisonFunctionToGLType Always && dm == false of
            true    -> GL.disable_ GL._DEPTH_TEST
            false   -> do
                GL.enable_ GL._DEPTH_TEST
                GL.depthFunc_ glDF
                GL.depthMask_ dm
        cvtC 0 xs
    cvt xs = do 
        GL.disable_ GL._DEPTH_TEST
        GL.disable_ GL._STENCIL_TEST
        cvtC 0 xs

    cvtC :: Int -> [FragmentOperation] -> GFX Unit
    cvtC i (ColorOp b m : xs) = do
        -- TODO
        case b of
            NoBlending -> do
                -- FIXME: requires GL 3.1
                --glDisablei gl_BLEND $ fromIntegral gl_DRAW_BUFFER0 + fromIntegral i
                GL.disable_ GL._BLEND -- workaround
                -- not presented: GL.disable_ GL._COLOR_LOGIC_OP
            BlendLogicOp op -> do
                GL.disable_ GL._BLEND
                -- not presented: GL.enable_  GL._COLOR_LOGIC_OP
                -- not presented: GL.logicOp_ $ logicOperationToGLType op
                trace "not presented: BlendLogicOp"
            Blend eq fac (V4 r g b a) -> do
                -- not presented: glDisable gl_COLOR_LOGIC_OP
                -- FIXME: requires GL 3.1
                --glEnablei gl_BLEND $ fromIntegral gl_DRAW_BUFFER0 + fromIntegral i
                GL.enable_ GL._BLEND -- workaround
                GL.blendEquationSeparate_ (blendEquationToGLType eq.colorEq) (blendEquationToGLType eq.alphaEq)
                GL.blendFuncSeparate_ (blendingFactorToGLType fac.colorF.src) (blendingFactorToGLType fac.colorF.dst)
                                    (blendingFactorToGLType fac.alphaF.src) (blendingFactorToGLType fac.alphaF.dst)
                GL.blendColor_ r g b a
        case m of
          VBool r           -> GL.colorMask_ r true true true
          VV2B (V2 r g)     -> GL.colorMask_ r g true true
          VV3B (V3 r g b)   -> GL.colorMask_ r g b true
          VV4B (V4 r g b a) -> GL.colorMask_ r g b a
          _                 -> GL.colorMask_ true true true true
        cvtC (i + 1) xs
    cvtC _ [] = return unit


clearRenderTarget :: [{semantic :: ImageSemantic, value :: Value}] -> GFX Unit
clearRenderTarget values = do
    let setClearValue arg@{mask:m,index:i} val = case val of
            {semantic = Depth, value = VFloat v} -> do
                GL.depthMask_ true
                GL.clearDepth_ v
                return {mask:m .|. GL._DEPTH_BUFFER_BIT, index:i}
            {semantic = Stencil, value = VWord v} -> do
                GL.clearStencil_ v
                return {mask:m .|. GL._STENCIL_BUFFER_BIT, index:i}
            {semantic = Color, value = c} -> do
                case c of
                  VFloat r            -> GL.clearColor_ r 0 0 1
                  VV2F (V2 r g)       -> GL.clearColor_ r g 0 1
                  VV3F (V3 r g b)     -> GL.clearColor_ r g b 1
                  VV4F (V4 r g b a)   -> GL.clearColor_ r g b a
                  _                   -> GL.clearColor_ 0 0 0 1
                GL.colorMask_ true true true true
                return {mask:m .|. GL._COLOR_BUFFER_BIT, index:i+1}
            _ -> do
                trace "internal error (clearRenderTarget)"
                return arg
    m <- foldM setClearValue {mask:0,index:0} values
    GL.clear_ m.mask

type GLProgram =
  { program       :: GL.WebGLProgram
  , objects       :: [GL.WebGLShader]
  , inputUniforms :: StrMap.StrMap GL.WebGLUniformLocation
  , inputStreams  :: StrMap.StrMap {location :: GL.GLint, slotAttribute :: String}
  }

compileProgram :: StrMap.StrMap InputType -> Program -> GFX GLProgram
compileProgram uniTrie p = do
    po <- GL.createProgram_
    let createAndAttach src t = do
          o <- GL.createShader_ t
          GL.shaderSource_ o src
          GL.compileShader_ o
          log <- GL.getShaderInfoLog_ o
          trace log
          status <- GL.getShaderParameter_ o GL._COMPILE_STATUS
          when (status /= true) $ throwException $ error "compileShader failed!"
          GL.attachShader_ po o
          --putStr "    + compile shader source: " >> printGLStatus
          return o

    objV <- createAndAttach p.vertexShader GL._VERTEX_SHADER
    objF <- createAndAttach p.fragmentShader GL._FRAGMENT_SHADER

    GL.linkProgram_ po
    prgLog <- GL.getProgramInfoLog_ po
    trace prgLog

    -- check link status
    status <- GL.getProgramParameter_ po GL._LINK_STATUS
    when (status /= true) $ throwException $ error "link program failed!"

    uniformLocation <- StrMap.fromList <$> flip traverse (StrMap.toList p.programUniforms) (\(Tuple uniName uniType) -> do
      loc <- GL.getUniformLocation_ po uniName
      return $ Tuple uniName loc)

    streamLocation <- StrMap.fromList <$> flip traverse (StrMap.toList p.programStreams) (\(Tuple streamName s) -> do
      loc <- GL.getAttribLocation_ po streamName
      return $ Tuple streamName {location: loc, slotAttribute: s.name})

    return {program: po, objects: [objV,objF], inputUniforms: uniformLocation, inputStreams: streamLocation}

type WebGLPipeline =
  { targets   :: [RenderTarget]
  , programs  :: [GLProgram]
  , commands  :: [Command]
  }

allocPipeline :: Pipeline -> GFX WebGLPipeline
allocPipeline p = do
  {-  support:
        programs
        commands
      not supported yet:
        textures
        framebuffer object targets
  -}
  {-
    - samplers -- not presented
    - textures
    - targets (frabebuffer objects)
    - programs
    - commands
  -}
  prgs <- traverse (compileProgram StrMap.empty) p.programs
  return {targets: p.targets, programs: prgs, commands: p.commands}
{-
allocPipeline p = do
    let uniTrie = uniforms $ schemaFromPipeline p
    smps <- V.mapM compileSampler $ samplers p
    texs <- V.mapM compileTexture $ textures p
    trgs <- V.mapM (compileRenderTarget (textures p) texs) $ targets p
    prgs <- V.mapM (compileProgram uniTrie) $ programs p
    -- texture unit mapping ioref trie
    texUnitMapRefs <- T.fromList <$> mapM (\k -> (k,) <$> newIORef 0) (S.toList $ S.fromList $ concat $ V.toList $ V.map (T.keys . toTrie . programInTextures) $ programs p)
    let (cmds,st) = runState (mapM (compileCommand texUnitMapRefs smps texs trgs prgs) $ commands p) initCGState
    input <- newIORef Nothing
    -- default Vertex Array Object
    vao <- alloca $! \pvao -> glGenVertexArrays 1 pvao >> peek pvao
    return $ GLPipeline
        { glPrograms        = prgs
        , glTextures        = texs
        , glSamplers        = smps
        , glTargets         = trgs
        , glCommands        = cmds
        , glSlotPrograms    = V.map slotPrograms $ IR.slots p
        , glInput           = input
        , glSlotNames       = V.map (pack . slotName) $ IR.slots p
        , glVAO             = vao
        , glTexUnitMapping  = texUnitMapRefs
        }
-}

renderPipeline :: WebGLPipeline -> GFX Unit
renderPipeline p = do
  flip traverse p.commands $ \cmd -> do
    case cmd of
      SetRasterContext rCtx -> setupRasterContext rCtx
      SetAccumulationContext aCtx -> setupAccumulationContext aCtx
      ClearRenderTarget t -> clearRenderTarget t
      SetProgram i -> GL.useProgram_ $ (p.programs `unsafeIndex` i).program
      _ -> return unit
  return unit

disposePipeline :: WebGLPipeline -> GFX Unit
disposePipeline _ = return unit

{-
shaders :: Shaders {aVertexPosition :: Attribute Vec3, uPMatrix :: Uniform Mat4, uMVMatrix:: Uniform Mat4}
shaders = Shaders

  """precision mediump float;

  void main(void) {
    gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
      }
  """

  """
      attribute vec3 aVertexPosition;

      uniform mat4 uMVMatrix;
      uniform mat4 uPMatrix;

      void main(void) {
          gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);
      }
  """

    withShaders shaders (\s -> alert s)
      \ bindings -> do
        trace "Shaders and bindings ready"
        clearColor 0.0 1.0 0.0 1.0
        enable DEPTH_TEST

        canvasWidth <- getCanvasWidth context
        canvasHeight <- getCanvasHeight context
        viewport 0 0 canvasWidth canvasHeight
        clear [COLOR_BUFFER_BIT, DEPTH_BUFFER_BIT]

        let pMatrix = M.makePerspective 45 (canvasWidth / canvasHeight) 0.1 100.0
        setUniformFloats bindings.uPMatrix (M.toArray pMatrix)

        let mvMatrix = M.translate (V.vec3 (-1.5) 0.0 (-7.0))
                          M.identity
        setUniformFloats bindings.uMVMatrix (M.toArray mvMatrix)

        buf1 <- makeBufferFloat [0.0,  1.0,  0.0,
                           (-1.0), (-1.0),  0.0,
                            1.0, (-1.0),  0.0]
        drawArr TRIANGLES buf1 bindings.aVertexPosition

        let mvMatrix' = M.translate (V.vec3 3.0 0.0 0.0)
                          mvMatrix
        setUniformFloats bindings.uMVMatrix (M.toArray mvMatrix')

        buf2 <- makeBufferFloat [1.0,  1.0,  0.0,
                           (-1.0), 1.0,  0.0,
                            1.0, (-1.0),  0.0,
                           (-1.0), (-1.0),  0.0]
        drawArr TRIANGLE_STRIP buf2 bindings.aVertexPosition

        trace "WebGL completed"
-}

