module LC_GL where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Bits
import Data.ByteString.Char8 (ByteString)
import Data.IORef
import Data.IntMap (IntMap)
import Data.Set (Set)
import Data.Trie as T
import Data.Vector (Vector,(!))
import qualified Data.ByteString.Char8 as SB
import qualified Data.Foldable as F
import qualified Data.IntMap as IM
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Graphics.Rendering.OpenGL.Raw.Core32
import Foreign

-- LC IR imports
import LC_G_Type
import LC_G_APIType
import LC_U_APIType
import LC_B2_IR as IR

import LC_GL_Type
import LC_GL_Util

import LC_GL_Input

setupRasterContext :: RasterContext -> IO ()
setupRasterContext = cvt
  where
    cff :: FrontFace -> GLenum
    cff CCW = gl_CCW
    cff CW  = gl_CW

    setProvokingVertex :: ProvokingVertex -> IO ()
    setProvokingVertex pv = glProvokingVertex $ case pv of
        FirstVertex -> gl_FIRST_VERTEX_CONVENTION
        LastVertex  -> gl_LAST_VERTEX_CONVENTION

    setPointSize :: PointSize -> IO ()
    setPointSize ps = case ps of
        ProgramPointSize    -> glEnable gl_PROGRAM_POINT_SIZE
        PointSize s         -> do
            glDisable gl_PROGRAM_POINT_SIZE
            glPointSize $ realToFrac s

    cvt :: RasterContext -> IO ()
    cvt (PointCtx ps fts sc) = do
        setPointSize ps
        glPointParameterf gl_POINT_FADE_THRESHOLD_SIZE (realToFrac fts)
        glPointParameterf gl_POINT_SPRITE_COORD_ORIGIN $ realToFrac $ case sc of
            LowerLeft   -> gl_LOWER_LEFT
            UpperLeft   -> gl_UPPER_LEFT

    cvt (LineCtx lw pv) = do
        glLineWidth (realToFrac lw)
        setProvokingVertex pv

    cvt (TriangleCtx cm pm po pv) = do
        -- cull mode
        case cm of
            CullNone    -> glDisable gl_CULL_FACE
            CullFront f -> do
                glEnable    gl_CULL_FACE
                glCullFace  gl_FRONT
                glFrontFace $ cff f
            CullBack f -> do
                glEnable    gl_CULL_FACE
                glCullFace  gl_BACK
                glFrontFace $ cff f

        -- polygon mode
        case pm of
            PolygonPoint ps -> do
                setPointSize ps
                glPolygonMode gl_FRONT_AND_BACK gl_POINT
            PolygonLine lw  -> do
                glLineWidth (realToFrac lw)
                glPolygonMode gl_FRONT_AND_BACK gl_LINE
            PolygonFill  -> glPolygonMode gl_FRONT_AND_BACK gl_FILL

        -- polygon offset
        glDisable gl_POLYGON_OFFSET_POINT
        glDisable gl_POLYGON_OFFSET_LINE
        glDisable gl_POLYGON_OFFSET_FILL
        case po of
            NoOffset -> return ()
            Offset f u -> do
                glPolygonOffset (realToFrac f) (realToFrac u)
                glEnable $ case pm of
                    PolygonPoint _  -> gl_POLYGON_OFFSET_POINT
                    PolygonLine  _  -> gl_POLYGON_OFFSET_LINE
                    PolygonFill     -> gl_POLYGON_OFFSET_FILL

        -- provoking vertex
        setProvokingVertex pv

setupAccumulationContext :: AccumulationContext -> IO ()
setupAccumulationContext (AccumulationContext n ops) = cvt ops
  where
    cvt :: [FragmentOperation] -> IO ()
    cvt (StencilOp a b c : DepthOp f m : xs) = do
        -- TODO
        cvtC 0 xs
    cvt (StencilOp a b c : xs) = do
        -- TODO
        cvtC 0 xs
    cvt (DepthOp df dm : xs) = do
        -- TODO
        glDisable gl_STENCIL_TEST
        case df == Always && dm == False of
            True    -> glDisable gl_DEPTH_TEST
            False   -> do
                glEnable gl_DEPTH_TEST
                glDepthFunc $! comparisonFunctionToGLType df
                glDepthMask (cvtBool dm)
        cvtC 0 xs
    cvt xs = do 
        glDisable gl_DEPTH_TEST
        glDisable gl_STENCIL_TEST
        cvtC 0 xs

    cvtC :: Int -> [FragmentOperation] -> IO ()
    cvtC i (ColorOp b m : xs) = do
        -- TODO
        case b of
            NoBlending -> do
                -- FIXME: requires GL 3.1
                --glDisablei gl_BLEND $ fromIntegral gl_DRAW_BUFFER0 + fromIntegral i
                glDisable gl_BLEND -- workaround
                glDisable gl_COLOR_LOGIC_OP
            BlendLogicOp op -> do
                glDisable   gl_BLEND
                glEnable    gl_COLOR_LOGIC_OP
                glLogicOp $ logicOperationToGLType op
            Blend (cEq,aEq) ((scF,dcF),(saF,daF)) (V4 r g b a) -> do
                glDisable gl_COLOR_LOGIC_OP
                -- FIXME: requires GL 3.1
                --glEnablei gl_BLEND $ fromIntegral gl_DRAW_BUFFER0 + fromIntegral i
                glEnable gl_BLEND -- workaround
                glBlendEquationSeparate (blendEquationToGLType cEq) (blendEquationToGLType aEq)
                glBlendFuncSeparate (blendingFactorToGLType scF) (blendingFactorToGLType dcF)
                                    (blendingFactorToGLType saF) (blendingFactorToGLType daF)
                glBlendColor (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)
        let cvt True    = 1
            cvt False   = 0
            (mr,mg,mb,ma) = case m of
                VBool r             -> (cvt r, 1, 1, 1)
                VV2B (V2 r g)       -> (cvt r, cvt g, 1, 1)
                VV3B (V3 r g b)     -> (cvt r, cvt g, cvt b, 1)
                VV4B (V4 r g b a)   -> (cvt r, cvt g, cvt b, cvt a)
                _           -> (1,1,1,1)
        glColorMask mr mg mb ma
        cvtC (i + 1) xs
    cvtC _ [] = return ()

    cvtBool :: Bool -> GLboolean
    cvtBool True  = 1
    cvtBool False = 0

clearRenderTarget :: [(ImageSemantic,Value)] -> IO ()
clearRenderTarget values = do
    let setClearValue (m,i) value = case value of
            (Depth, VFloat v) -> do
                glClearDepth $ realToFrac v
                return (m .|. gl_DEPTH_BUFFER_BIT, i)
            (Stencil, VWord v) -> do
                glClearStencil $ fromIntegral v
                return (m .|. gl_STENCIL_BUFFER_BIT, i)
            (Color, c) -> do
                let (r,g,b,a) = case c of
                        VFloat r            -> (realToFrac r, 0, 0, 1)
                        VV2F (V2 r g)       -> (realToFrac r, realToFrac g, 0, 1)
                        VV3F (V3 r g b)     -> (realToFrac r, realToFrac g, realToFrac b, 1)
                        VV4F (V4 r g b a)   -> (realToFrac r, realToFrac g, realToFrac b, realToFrac a)
                        _                   -> (0,0,0,1)
                glClearColor r g b a
                return (m .|. gl_COLOR_BUFFER_BIT, i+1)
            _ -> error "internal error (clearRenderTarget)"
    (mask,_) <- foldM setClearValue (0,0) values
    glClear $ fromIntegral mask


printGLStatus = checkGL >>= print
printFBOStatus = checkFBO >>= print

compileProgram :: Program -> IO GLProgram
compileProgram p = do
    po <- glCreateProgram
    let createAndAttach src t = do
            o <- glCreateShader t
            compileShader o [src]
            glAttachShader po o
            putStr "    + compile shader source: " >> printGLStatus
            return o

    objs <- sequence $ createAndAttach (vertexShader p) gl_VERTEX_SHADER : createAndAttach (fragmentShader p) gl_FRAGMENT_SHADER : case geometryShader p of
        Nothing -> []
        Just s  -> [createAndAttach s gl_GEOMETRY_SHADER]

    forM_ (zip (programOutput p) [0..]) $ \((n,t),i) -> SB.useAsCString n $ \pn -> do
        putStrLn ("variable " ++ show n ++ " attached to color number #" ++ show i)
        glBindFragDataLocation po i $ castPtr pn
    putStr "    + setup shader output mapping: " >> printGLStatus

    glLinkProgram po
    printProgramLog po

    -- check link status
    status <- glGetProgramiv1 gl_LINK_STATUS po
    when (status /= fromIntegral gl_TRUE) $ fail "link program failed!"

    -- check program input
    (uniforms,uniformsType) <- queryUniforms po
    (attributes,attributesType) <- queryStreams po
    when (uniformsType /= programUniforms p `unionL` programInTextures p) $ fail "shader program uniform input mismatch!"
    when (attributesType /= fmap snd (programStreams p)) $ fail "shader program stream input mismatch!"

    return $ GLProgram
        { shaderObjects = objs
        , programObject = po
        , inputUniforms = uniforms
        , inputStreams  = attributes
        , attributeMap  = fmap fst (programStreams p)
        }

compileSampler :: SamplerDescriptor -> IO GLSampler
compileSampler s = return $ GLSampler {}

compileRenderTarget :: RenderTarget -> IO GLRenderTarget
compileRenderTarget = undefined

allocPipeline :: Pipeline -> IO GLPipeline
allocPipeline p = do
    {-
        allocate:
            - samplers
            - textures
            - framebuffer objects (render targets)
            done - programs
    -}
    smps <- V.mapM compileSampler $ samplers p
    texs <- V.mapM compileTexture $ textures p
    trgs <- V.mapM compileRenderTarget $ targets p
    prgs <- V.mapM compileProgram $ programs p
    let (cmds,st) = runState (mapM (compileCommand smps texs trgs prgs) $ commands p) initCGState
        slotCount = V.length $ IR.slots p
        progCount = V.length $ programs p
        slotProgs = V.create $ do
            v <- MV.new slotCount
            MV.set v []
            F.forM_ (slotPrograms st) $ \(slot,prog) -> do
                xs <- MV.read v slot
                MV.write v slot (prog:xs)
            return v
    slotCmds <- newIORef $ V.replicate slotCount (V.replicate progCount [])
    input <- newIORef Nothing
    return $ GLPipeline
        { glPrograms        = prgs
        , glTextures        = texs
        , glSamplers        = smps
        , glTargets         = trgs
        , glCommands        = cmds
        , glSlotCommands    = slotCmds
        , glSlotPrograms    = slotProgs
        , glInput           = input
        }

disposePipeline :: GLPipeline -> IO ()
disposePipeline = undefined

setPipelineInput :: GLPipeline -> GLPipelineInput -> IO ()
setPipelineInput = undefined
{-
  track state:
    - render target
    - binded textures
-}

{-
  render steps:
    - update uniform buffers
    - new command: set uniform buffer (binds uniform buffer to program's buffer slot)
    - render slot steps:
        - set uniform buffer or set uniforms separately
        - set vertex and index array
        - call draw command
-}

renderSlot :: [GLRenderCommand] -> IO ()
renderSlot cmds = forM_ cmds $ \cmd -> case cmd of
    _   -> return ()

renderPipeline :: GLPipeline -> IO ()
renderPipeline glp = forM_ (glCommands glp) $ \cmd -> case cmd of
    GLSetRasterContext rCtx         -> setupRasterContext rCtx
    GLSetAccumulationContext aCtx   -> setupAccumulationContext aCtx
    GLSetRenderTarget rt            -> glBindFramebuffer gl_DRAW_FRAMEBUFFER rt
    GLSetProgram p                  -> glUseProgram p
    GLSetSamplerUniform i tu        -> glUniform1i i tu
    GLSetTexture tu target tx       -> glActiveTexture tu >> glBindTexture target tx
    GLClearRenderTarget vals        -> clearRenderTarget vals
    GLGenerateMipMap tu target      -> glActiveTexture tu >> glGenerateMipmap target
    GLRenderSlot slot prog          -> readIORef (glSlotCommands glp) >>= (\v -> renderSlot $ (v ! slot) ! prog)
    {-
        attribute mapping
    -}
    
    {-
    GLSetSampler
    GLSaveImage
    GLLoadImage
    -}

{-
    { shaderObjects :: [GLuint]
    , programObject :: GLuint
    , inputUniforms :: Trie GLint
    , inputStreams  :: Trie GLuint
    , attributeMap  :: Trie ByteString
    }
-}
data CGState
    = CGState
    { currentProgram    :: ProgramName
    , textureBinding    :: IntMap GLTexture
    , slotPrograms      :: Set (SlotName,ProgramName)
    }

initCGState = CGState
    { currentProgram    = error "CGState: empty currentProgram"
    , textureBinding    = IM.empty
    , slotPrograms      = S.empty
    }

type CG a = State CGState a

compileCommand :: Vector GLSampler -> Vector GLTexture -> Vector GLRenderTarget -> Vector GLProgram -> Command -> CG GLCommand
compileCommand samplers textures targets programs cmd = case cmd of
    SetRasterContext rCtx       -> return $ GLSetRasterContext rCtx
    SetAccumulationContext aCtx -> return $ GLSetAccumulationContext aCtx
    SetRenderTarget rt          -> return $ GLSetRenderTarget $ framebufferObject $ targets ! rt
    SetProgram p                -> do
                                    modify (\s -> s {currentProgram = p})
                                    return $ GLSetProgram $ programObject $ programs ! p
    SetSamplerUniform n tu      -> do
                                    p <- currentProgram <$> get
                                    case T.lookup n (inputUniforms $ programs ! p) of
                                        Nothing -> fail "internal error (SetSamplerUniform)!"
                                        Just i  -> return $ GLSetSamplerUniform i $ fromIntegral tu
    SetTexture tu t             -> do
                                    let tex = textures ! t
                                    modify (\s -> s {textureBinding = IM.insert tu tex $ textureBinding s})
                                    return $ GLSetTexture (gl_TEXTURE0 + fromIntegral tu) (glTextureTarget tex) (glTextureObject tex)
{-
    SetSampler tu s             -> liftIO $ do
                                        glBindSampler (fromIntegral tu) (samplerObject $ glSamplers glp ! s)
-}
    RenderSlot slot             -> do
                                    p <- currentProgram <$> get
                                    modify (\s -> s {slotPrograms = S.insert (slot,p) $ slotPrograms s})
                                    return $ GLRenderSlot slot p
    ClearRenderTarget vals      -> return $ GLClearRenderTarget vals
    GenerateMipMap tu           -> do
                                    tb <- textureBinding <$> get
                                    case IM.lookup tu tb of
                                        Nothing     -> fail "internal error (GenerateMipMap)!"
                                        Just tex    -> return $ GLGenerateMipMap (gl_TEXTURE0 + fromIntegral tu) (glTextureTarget tex)
{-
    SaveImage _ _               -> undefined
    LoadImage _ _               -> undefined
-}
