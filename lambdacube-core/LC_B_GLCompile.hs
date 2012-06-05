module LC_B_GLCompile where

import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Data.IORef
import Data.List as L
import Data.Maybe
import Data.Set (Set)
import Data.Map (Map)
import Data.Trie as T
import Foreign
import qualified Data.ByteString.Char8 as SB
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Traversable as T
import qualified Data.Vector as V

import Graphics.Rendering.OpenGL.Raw.Core32

import LC_G_Type
import LC_G_APIType
import LC_U_APIType
import LC_U_DeBruijn

import LC_B_GLType
import LC_B_GLUtil
import LC_B_GLSLCodeGen
import LC_B_Traversals

data ShaderSource
    = VertexShaderSrc   !ByteString
    | GeometryShaderSrc !ByteString
    | FragmentShaderSrc !ByteString

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
        PointSizeRast   -> glEnable gl_PROGRAM_POINT_SIZE
        PointSize s     -> do
            glDisable gl_PROGRAM_POINT_SIZE
            glPointSize $ realToFrac s

    cvt :: RasterContext -> IO ()
    cvt (PointCtx) = return ()
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

setupAccumulationContext :: [FragmentOperation] -> IO ()
setupAccumulationContext = cvt
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

{-
  compile steps:
    - collect all render buffers and render textures and allocate the GL resources
    - create setup actions all FBO-s (including clear targets action)
        - compile Image setup function for each
        - compile FragmentOperation function for each
    - compile shader programs

  render stages:
    - draw pass:
        - bind FBO
        - clear FBO targets
        - bind program
        - execute draw actions
    - execute next draw pass
    - blit ScreenOut to Back buffer if necessary

  hints:
    - we will have one GLProgram and one FBO per Accumulate
-}
-- TODO:
--  according context create FBO attachments
--      we always use Textures (without mipmap, as a single image) as FBO attachments
--      RenderBuffer can be use if it not fed to a sampler and it has olny one layer
--  question:
--      what is needed to create a Texture:
--          size            - will be stored in FrameBuffer :: GP (FrameBuffer sh t)
--          internal format - for each component (float,int or word)
{-
    glGenTextures(1, &color_tex);
    glBindTexture(GL_TEXTURE_2D, color_tex);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, 256, 256, 0, GL_BGRA, GL_UNSIGNED_BYTE, NULL);

    void glFramebufferTexture(GLenum target, GLenum attachment, GLuint texture, GLint level);
    void glDrawBuffers( GLsizei n, const GLenum *bufs );
-}
{-
    scissor:
        enable/disable: SCISSOR_TEST
        void Scissor( int left, int bottom, sizei width, sizei height );

    multisample:
        enable/disable: SAMPLE_ALPHA_TO_COVERAGE, SAMPLE_ALPHA_TO_ONE, SAMPLE_COVERAGE, SAMPLE_MASK
        void SampleCoverage( clampf value, boolean invert );
        void SampleMaski( uint maskNumber, bitfield mask );

    stencil:
        enable/disable: STENCIL_TEST
        void StencilFunc( enum func, int ref, uint mask );
        void StencilFuncSeparate( enum face, enum func, int ref, uint mask );
        void StencilOp( enum sfail, enum dpfail, enum dppass );
        void StencilOpSeparate( enum face, enum sfail, enum dpfail, enum dppass );

    depth:
        enable/disable: DEPTH_TEST
        void DepthFunc( enum func );

    blending:
        enable/disable:
          target: BLEND
          index:  DRAW_BUFFERi
            void Enablei( enum target, uint index );
            void Disablei( enum target, uint index );
          FRAMEBUFFER_SRGB
      Blend Equation:
        void BlendEquation( enum mode );
        void BlendEquationSeparate( enum modeRGB, enum modeAlpha );
        void BlendFuncSeparate( enum srcRGB, enum dstRGB, enum srcAlpha, enum dstAlpha );
        void BlendFunc( enum src, enum dst );
        void BlendColor( clampf red, clampf green, clampf blue, clampf alpha );

    dither:
        enable/disable: DITHER

    logic operation:
        enable/disable: COLOR_LOGIC_OP
        void LogicOp( enum op );

    Selecting a Buffer for Writing:
        void DrawBuffer( enum buf );
        void DrawBuffers( sizei n, const enum *bufs );

    Fine Control of Buffer Updates:
        void ColorMask( boolean r, boolean g, boolean b, boolean a );
        void ColorMaski( uint buf, boolean r, boolean g, boolean b, boolean a );
        void DepthMask( boolean mask );
        void StencilMask( uint mask );
        void StencilMaskSeparate( enum face, uint mask );

    Clearing the Buffers:
        void Clear( bitfield buf );
        void ClearColor( clampf r, clampf g, clampf b, clampf a );
        void ClearDepth( clampd d );
        void ClearStencil( int s );
        void ClearBuffer{if ui}v( enum buffer, int drawbuffer, const T*value);
        void ClearBufferfi( enum buffer, int drawbuffer, float depth, int stencil );

    Reading and Copying Pixels:
        void ReadPixels( int x, int y, sizei width, sizei height, enum format, enum type, void *data );
        void ReadBuffer( enum src );
        void ClampColor( enum target, enum clamp );
        
        void BlitFramebuffer( int srcX0, int srcY0, int srcX1, int srcY1, int dstX0, int dstY0, int dstX1, int dstY1, bitfield mask, enum filter );
-}
{-
  NOTE:
    We have to validate context, because we can support only the same Blend and LogicOperation for all render targets,
        however blending or LogicOp can be disabled separatly to each render target.
-}

compileClearFrameBuffer :: GP -> IO ()
compileClearFrameBuffer (FrameBuffer fb) = cvt fb
  where
    -- we have to handle depth and stencil specially, available configurations:
    --  depth
    --  stencil
    --  depth-stencil
    cvt :: [Image] -> IO ()
    cvt (StencilImage sh1 s : DepthImage sh2 d : xs) = do
        -- TODO
        cvtC 0 xs
    cvt (StencilImage sh s : xs) = do
        -- TODO
        cvtC 0 xs
    cvt (DepthImage sh d : xs) = do
        let --renderGL3   = with d $ \pd -> glClearBufferfv gl_DEPTH 0 $ castPtr pd
        glClearDepth $ realToFrac d
        glClear $ fromIntegral gl_DEPTH_BUFFER_BIT
        --print "     * glClear gl_DEPTH_BUFFER_BIT"
        cvtC 0 xs
    cvt xs = cvtC 0 xs

    cvtC :: Int -> [Image] -> IO ()
    cvtC i (ColorImage sh c : xs) = do
        -- for GL3:
        --with c' $ \pc -> glClearBufferfv gl_COLOR (fromIntegral $ gl_DRAW_BUFFER0 + fromIntegral i) $ castPtr pc
        let (r,g,b,a) = case c of
                VFloat r            -> (realToFrac r, 0, 0, 1)
                VV2F (V2 r g)       -> (realToFrac r, realToFrac g, 0, 1)
                VV3F (V3 r g b)     -> (realToFrac r, realToFrac g, realToFrac b, 1)
                VV4F (V4 r g b a)   -> (realToFrac r, realToFrac g, realToFrac b, realToFrac a)
                _                   -> (0,0,0,1)
        glClearColor r g b a
        glClear $ fromIntegral gl_COLOR_BUFFER_BIT
    cvtC i [] = return ()

-- TODO
{-
  hint:
    sampler names are generated, only texture slots are named by user
    one texture can be attached to more samplers
    user feed textures not samplers to gfx network

  texturing support:
    collect all sampler and texture definitions
    create sampler <-> texture name map
    sort previous passes
    create sampler setup action
    add texture slots to uniform input trie

  resources to create
    samplers
        sampler setup action
    textures
        hint: only if it is an output of a previous pass
-}
{-
    void GenSamplers( sizei count, uint *samplers );
    void BindSampler( uint unit, uint sampler );
    void DeleteSamplers( sizei count, const uint *samplers );
    void SamplerParameter{if}v( uint sampler, enum pname, T param );
    void SamplerParameterI{u ui}v( uint sampler, enum pname, T *params );
        pname:
            TEXTURE_WRAP_S
            TEXTURE_WRAP_T
            TEXTURE_WRAP_R
            TEXTURE_MIN_FILTER
            TEXTURE_MAG_FILTER
            TEXTURE_BORDER_COLOR
            TEXTURE_MIN_LOD
            TEXTURE_MAX_LOD
            TEXTURE_LOD_BIAS
            TEXTURE_COMPARE_MODE
            TEXTURE_COMPARE_FUNC
    void DeleteSamplers( sizei count, const uint *samplers );

    void ActiveTexture( enum texture );
        TEXTUREi = TEXTURE0 + i
    void BindTexture( enum target, uint texture );
        target:
            TEXTURE_1D
            TEXTURE_2D
            TEXTURE_3D
            TEXTURE_1D_ARRAY
            TEXTURE_2D_ARRAY
            TEXTURE_RECTANGLE
            TEXTURE_BUFFER
            TEXTURE_CUBE_MAP
            TEXTURE_2D_MULTISAMPLE
            TEXTURE_2D_MULTISAMPLE_ARRAY
-}


-- FIXME: simple solution, does not support sharing
-- result: (RenderAction, DisposeAction, UniformLocation, StreamLocation)
compileRenderFrameBuffer :: [(Exp,String)] -> [(Exp,String)] -> IORef ObjectSet -> GP -> IO (IO (), IO (), Trie GLint, Trie GLuint)
compileRenderFrameBuffer samplerNames slotSamplerNames objsIORef (Accumulate aCtx ffilter fsh (Rasterize rCtx (Transform vsh (Fetch slotName _ slotInput))) fb) = do
    --rndr <- compileFrameBuffer fb rndr'
    po <- glCreateProgram
    let (shl,fragOuts) = {-case gs of
            NoGeometryShader    -> -}([VertexShaderSrc srcV, FragmentShaderSrc srcF], (map fst outF)) {-
            _                   -> ([VertexShaderSrc srcV, GeometryShaderSrc srcG, FragmentShaderSrc srcF], (map fst outF)) -}
        allSamplerNames = samplerNames ++ slotSamplerNames 
        samplerNameMap  = Map.fromList allSamplerNames
        (srcV,outV) = codeGenVertexShader samplerNameMap slotInput vsh
        (srcG,outG) = ("",outV)--codeGenGeometryShader samplerNameMap outV gs
        (srcF,outF) = codeGenFragmentShader samplerNameMap outG ffilter fsh
        createAndAttach [] _ = return $! Nothing
        createAndAttach sl t = do
            mapM_ SB.putStrLn sl
            o <- glCreateShader t
            compileShader o sl
            glAttachShader po o
            return $! Just o
    vsh <- createAndAttach [s | VertexShaderSrc s <- shl] gl_VERTEX_SHADER
    gsh <- createAndAttach [s | GeometryShaderSrc s <- shl] gl_GEOMETRY_SHADER
    fsh <- createAndAttach [s | FragmentShaderSrc s <- shl] gl_FRAGMENT_SHADER

    let printGLStatus = checkGL >>= print
    putStr "    + compile shader source: " >> printGLStatus
    -- connect Fragment output to FBO
    forM_ (zip fragOuts [0..]) $ \(n,i) -> SB.useAsCString n $ \pn -> do
        print (n,i)
        glBindFragDataLocation po i $ castPtr pn
    putStr "    + setup shader output mapping: " >> printGLStatus
    glLinkProgram po
    printProgramLog po

    -- check link status
    status <- glGetProgramiv1 gl_LINK_STATUS po
    when (status /= fromIntegral gl_TRUE) $ fail "link program failed!"

    -- query active uniforms, attributes and samplers
    (uLoc,uType) <- queryUniforms po
    (sLoc,sType) <- queryStreams po

    -- set sampler mapping
    glUseProgram po
    forM_ (zip [0..] (map (SB.pack . snd) allSamplerNames)) $ \(tuIdx,n) -> case T.lookup n uLoc of
        Nothing -> fail $ "missing sampler from shader: " ++ show n
        Just i  -> (setSampler i tuIdx) >> putStr ("    + setup texture unit mapping (smp " ++ show i ++ " <-> TexUnit " ++ show tuIdx ++": ") >> printGLStatus

    -- HINT: we get the uniform location now, so we have to provide this info to the renderer
    let uLoc' = foldl' (\t (_,n) -> setSamplerLoc t (SB.pack n)) uLoc allSamplerNames
        renderSmpNamesS = Set.fromList $ map (SB.pack . snd) samplerNames
        renderSmpCount  = Set.size renderSmpNamesS
        slotSmpName     = map (SB.pack . snd) slotSamplerNames

        setSamplerLoc :: Trie GLint -> ByteString -> Trie GLint
        setSamplerLoc t n
            | Set.member n renderSmpNamesS  = T.delete n t
            | otherwise                     = T.adjust (\_ -> fromIntegral $ renderSmpCount + idx) n t
              where
                Just idx = elemIndex n slotSmpName

        disposeFun = glDeleteProgram po >> mapM_ glDeleteShader (catMaybes [vsh,gsh,fsh])
        renderFun = do
            ObjectSet drawObjs objsMap <- readIORef objsIORef
            unless (Map.null objsMap) $ do
                setupRasterContext rCtx
                setupAccumulationContext aCtx
                glUseProgram po
                drawObjs
    print slotName
    print uLoc'
    return $! (renderFun, disposeFun, uLoc', sLoc)
