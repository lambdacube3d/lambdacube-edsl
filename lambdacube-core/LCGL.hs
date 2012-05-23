module LCGL where

import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Data.IORef
import Data.List as L
import Data.Maybe
import Data.Set (Set)
import Data.Trie as T
import Foreign
import qualified Data.ByteString.Char8 as SB
import qualified Data.Set as Set
import qualified Data.Traversable as T
import qualified Data.Vector as V

import Graphics.Rendering.OpenGL.Raw.Core32

import TypeLevel.Number.Nat
import TypeLevel.Number.Nat.Num

import LCType
import LCAPIType hiding (Buffer)
import LCDSLType
import LCConvert
import LCDeBruijn
import qualified LCHOAS as H
import LCGLUtil
import LCGLSLCodeGen

--import qualified Criterion.Measurement as C

data GPOutput where
    ImageOut    :: ByteString
                -> H.GP (Image sh t)
                -> GPOutput

    ScreenOut   :: H.GP (Image N1 t)
                -> GPOutput

---------
-- API --
---------

-- Buffer
compileBuffer   :: [Array] -> IO Buffer
bufferSize      :: Buffer -> Int
arraySize       :: Buffer -> Int -> Int
arrayType       :: Buffer -> Int -> ArrayType

-- Renderer
compileRenderer :: [GPOutput] -> IO Renderer
{-
slotUniforms    :: Renderer -> Trie (Trie InputType)
slotStreams     :: Renderer -> Trie (PrimitiveType, Trie InputType)
uniformSetter   :: Renderer -> Trie InputSetter
render          :: Renderer -> IO ()
dispose        :: Renderer -> IO ()
-}
-- Object
addObject       :: Renderer -> ByteString -> Primitive -> Maybe (IndexStream Buffer) -> Trie (Stream Buffer) -> [ByteString] -> IO Object
removeObject    :: Renderer -> Object -> IO ()
--objectUniformSetter  :: Object -> Trie InputSetter

--------------------
-- Implementation --
--------------------

-- BUFFER
compileBuffer arrs = do
    let calcDesc (offset,setters,descs) (Array arrType cnt setter) =
          let size = cnt * sizeOfArrayType arrType
          in (size + offset, (offset,size,setter):setters, ArrayDesc arrType cnt offset size:descs)
        (bufSize,arrSetters,arrDescs) = foldl' calcDesc (0,[],[]) arrs
    bo <- alloca $! \pbo -> glGenBuffers 1 pbo >> peek pbo
    glBindBuffer gl_ARRAY_BUFFER bo
    glBufferData gl_ARRAY_BUFFER (fromIntegral bufSize) nullPtr gl_STATIC_DRAW
    forM_ arrSetters $! \(offset,size,setter) -> setter $! glBufferSubData gl_ARRAY_BUFFER (fromIntegral offset) (fromIntegral size)
    glBindBuffer gl_ARRAY_BUFFER 0
    return $! Buffer (V.fromList $! reverse arrDescs) bo

bufferSize              = V.length . bufArrays
arraySize buf arrIdx    = arrLength $! bufArrays buf V.! arrIdx
arrayType buf arrIdx    = arrType $! bufArrays buf V.! arrIdx

-- RENDERER
{-
    we should store:
        generic attributer setters (default values in that case when an attribute is missing from a mesh)
        question:
            should we provide default attributes or should we require a full attribute description from the user?
          answer: the latter seems better idea!
        question:
            is a mesh object constant in vertex attributes? e.g. if we'd like to change the value of an attribute buffer or generic attribute
            then we have to swap the old mesh object with a new one.
          answer: seems a good and reasonable idea, because we can customize object rendering using per object uniforms!
        PUBLIC:
            - slotUniforms  :: Trie (Trie InputType)
            - slotStreams   :: Trie (PrimitiveType, Trie InputType)
            - uniformSetter :: Trie InputSetter
            - render        :: IO ()
            - dispose      :: IO ()
        INTERNAL:
            - object sets               :: Trie (TVar ObjectSet)
            - globalUniformSetup        :: Trie (GLint -> IO ())
            - uniform setup actions     :: Trie (STM (GLint -> IO (), InputSetter))
            - attribute setup actions   :: Trie (GLuint -> StreamSetter)

    note: if we'd like to support slot sharing between different sub gfx networks,
           then we have to check that they have the same primitve type and
           there is no type collision if we build the union of they stream input
          otherwise we can not support slot sharing.
          should we use namespaces in stream input names?
           e.g "slot name/atribute name" in this case we can store stream attribute descriptions in a single trie.
           or should we use trie of tries?
    discussion:
        how to handle sub network uniform/attribute naming
            - global all
            - namespace prefix in name
            - trie of tries
        temporary decision:
            i'll use global names

    minimal restricition for (global name) uniforms and attributes:
        a name sould bound to only one type!

    TODO:
        proper handling of attribute and uniform sharing between shader programs
            two alternatives:
                - use same mapping for every program (more efficient, but requires more gl features)
                - use custom mapping per program with custom attribute/uniform setter (less efficient, but more compatible/less restricitive)
-}
type ObjectSet = [IO ()]

data Renderer -- internal type
    = Renderer
    -- public
    { slotUniform           :: Trie (Trie InputType)
    , slotStream            :: Trie (PrimitiveType, Trie InputType)
    , uniformSetter         :: Trie InputSetter         -- global uniform
    , render                :: IO ()
    , dispose              :: IO ()

    -- internal
    , objectSet             :: Trie (IORef ObjectSet)
    , mkUniformSetup        :: Trie (GLint -> IO ())    -- global unifiorm
    , slotUniformLocation   :: Trie (Trie GLint)
    , slotStreamLocation    :: Trie (Trie GLuint)
    }

data ShaderSource
    = VertexShaderSrc   !ByteString
    | GeometryShaderSrc !ByteString
    | FragmentShaderSrc !ByteString

{-
data RasterContext t where
    PointCtx    :: RasterContext Point      -- TODO: PointSize, POINT_FADE_THRESHOLD_SIZE, POINT_SPRITE_COORD_ORIGIN, ProvokingVertex??
    LineCtx     :: RasterContext Line       -- TODO: LineWidth, ProvokingVertex
    TriangleCtx ::
        { ctxCullMode           :: CullMode
        , ctxPolygonMode        :: PolygonMode
        , ctxPolygonOffset      :: PolygonOffset
        , ctxProvokingVertex    :: ProvokingVertex
        } -> RasterContext Triangle

data FrontFace          = CCW | CW deriving (Eq,Ord,Show)
data CullMode           = CullNone | CullFront FrontFace | CullBack FrontFace deriving (Eq,Ord,Show)
data PolygonMode        = PolygonPoint | PolygonLine | PolygonFill deriving (Eq,Ord,Bounded,Enum,Show)
data ProvokingVertex    = FirstVertex | LastVertex deriving (Eq,Ord,Bounded,Enum,Show)
-}
setupRasterContext :: RasterContext t -> IO ()
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

    cvt :: RasterContext t -> IO ()
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

{-
data FragmentOperation ty where
    DepthOp         :: DepthFunction
                    -> Bool
                    -> FragmentOperation (Depth Float)

    StencilOp       :: StencilTests
                    -> StencilOps
                    -> StencilOps
                    -> FragmentOperation (Stencil Int32)

    ColorOp         :: (IsVec s (v Bool) Bool, IsNum c)
                    => Blending c
                    -> v Bool -- mask
                    -> FragmentOperation (Color (v c))
-}
setupAccumulationContext :: AccumulationContext t -> IO ()
setupAccumulationContext = cvt
  where
    cvt :: AccumulationContext t -> IO ()
    cvt (StencilOp a b c :. DepthOp f m :. xs) = do
        -- TODO
        cvtC 0 xs
    cvt (StencilOp a b c :. xs) = do
        -- TODO
        cvtC 0 xs
    cvt (DepthOp df dm :. xs) = do
        -- TODO
        glDisable gl_STENCIL_TEST
        glEnable  gl_DEPTH_TEST
        glDepthMask (cvtBool dm)
        glDepthFunc $! comparisonFunctionToGLType df
        cvtC 0 xs
    cvt xs = do 
        glDisable gl_DEPTH_TEST
        glDisable gl_STENCIL_TEST
        cvtC 0 xs

    cvtC :: Int -> AccumulationContext t -> IO ()
    cvtC i (ColorOp b m :. xs) = do
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
        glColorMask 1 1 1 1
        cvtC (i + 1) xs
    cvtC _ ZT = return ()

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
compileFrameBuffer :: GP (FrameBuffer sh a) -> Renderer -> IO Renderer
compileFrameBuffer (FrameBuffer (V2 w h) fb) rndr = do
    -- create FBO
    bo <- alloca $! \pbo -> glGenFramebuffers 1 pbo >> peek pbo
    glBindFramebuffer gl_DRAW_FRAMEBUFFER bo

    -- we have to handle depth and stencil specially, available configurations:
    --  depth
    --  stencil
    --  depth-stencil
    let cvt :: FrameBuffer sh t -> IO (Int, IO (), IO ()) -- color component count, render, dispose
        cvt (StencilImage sh1 s :. DepthImage sh2 d :. xs) = do
            -- TODO
            cvtC 0 xs
        cvt (StencilImage sh s :. xs) = do
            -- TODO
            cvtC 0 xs
        cvt (DepthImage sh d :. xs) = do
            to <- alloca $! \pto -> glGenTextures 1 pto >> peek pto
            glBindTexture gl_TEXTURE_2D to
            glTexImage2D gl_TEXTURE_2D 0 (fromIntegral gl_DEPTH_COMPONENT32) (fromIntegral w) (fromIntegral h) 0 gl_DEPTH_COMPONENT gl_UNSIGNED_INT nullPtr
            glFramebufferTexture2D gl_DRAW_FRAMEBUFFER gl_DEPTH_ATTACHMENT gl_TEXTURE_2D to 0
            let renderGL3   = with d $ \pd -> glClearBufferfv gl_DEPTH 0 $ castPtr pd
                render      = do
                    glClearDepth $ realToFrac d
                    glClear $ fromIntegral gl_DEPTH_BUFFER_BIT
                dispose    = with to $ \pto -> glDeleteTextures 1 pto
            (i,r,f) <- cvtC 0 xs
            return $! (i,render >> r, dispose >> f)
        cvt xs = cvtC 0 xs

        cvtC :: Int -> FrameBuffer sh t -> IO (Int, IO (), IO ()) -- color component count, render, dispose
        cvtC i (ColorImage sh c :. xs) = do
            to <- alloca $! \pto -> glGenTextures 1 pto >> peek pto
            glBindTexture gl_TEXTURE_2D to
            glTexImage2D gl_TEXTURE_2D 0 (fromIntegral gl_RGBA8) (fromIntegral w) (fromIntegral h) 0 gl_BGRA gl_UNSIGNED_BYTE nullPtr
            glFramebufferTexture2D gl_DRAW_FRAMEBUFFER (gl_COLOR_ATTACHMENT0 + fromIntegral i) gl_TEXTURE_2D to 0
            let render      = do
                    let c' = V4 1 0 0 1 :: V4F-- TODO
                    -- for GL3:
                    --with c' $ \pc -> glClearBufferfv gl_COLOR (fromIntegral $ gl_DRAW_BUFFER0 + fromIntegral i) $ castPtr pc
                    glClearColor 1 0 0 1
                    glClear $ fromIntegral gl_COLOR_BUFFER_BIT
                dispose    = with to $ \pto -> glDeleteTextures 1 pto
            (i',r,f) <- cvtC (i + 1) xs
            return $! (i', render >> r, dispose >> f)
        cvtC i ZT = return $! (i, return (), return ())

    (colorCnt,r,f) <- cvt fb
    withArray [gl_COLOR_ATTACHMENT0 + fromIntegral i | i <- [0..colorCnt-1]] $
        glDrawBuffers (fromIntegral colorCnt)
    let draw    = do
            glBindFramebuffer gl_DRAW_FRAMEBUFFER bo
            glBindFramebuffer gl_READ_FRAMEBUFFER bo
            r
        delete  = with bo $ \pbo -> glDeleteFramebuffers 1 pbo
    return $! rndr {render = render rndr >> draw, dispose = dispose rndr >> f >> delete}

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
compileFrameBuffer (Accumulate fCtx ffilter fsh (Rasterize rCtx gs (Transform vsh (Fetch slotName prim attrs))) fb) rndr' = do
    rndr <- compileFrameBuffer fb rndr'
    po <- glCreateProgram
    let (shl,fragOuts) = case gs of
            NoGeometryShader    -> ([VertexShaderSrc srcV, FragmentShaderSrc srcF], (map fst outF))
            _                   -> ([VertexShaderSrc srcV, GeometryShaderSrc srcG, FragmentShaderSrc srcF], (map fst outF))
        (srcV,outV) = codeGenVertexShader slotInput vsh
        (srcG,outG) = codeGenGeometryShader outV gs
        (srcF,outF) = codeGenFragmentShader outG ffilter fsh
        slotInput   = toInputList attrs
        primType    = toPrimitive prim
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

    -- connect Fragment output to FBO
    forM_ (zip fragOuts [0..]) $ \(n,i) -> SB.useAsCString n $ \pn -> do
        glBindFragDataLocation po (fromIntegral $ gl_DRAW_BUFFER0 + i) $ castPtr pn
    glLinkProgram po
    printProgramLog po

    -- check link status
    status <- glGetProgramiv1 gl_LINK_STATUS po
    when (status /= fromIntegral gl_TRUE) $ fail "link program failed!"

    -- query active uniforms, attributes and samplers
    (uLoc,uType) <- queryUniforms po
    (sLoc,sType) <- queryStreams po

    --  for new uniforms, create setter and setup function
    --  for new slots create object set tvar
    let newUNames = Set.toList $! (Set.fromList $! T.keys uLoc) Set.\\ (Set.fromList $! T.keys $! uniformSetter rndr)
        objSet    = objectSet rndr
    (uSetup,uSetter) <- unzip <$> (sequence [mkUSetter t | n <- newUNames, let Just t = T.lookup n uType])
    objSet' <- if T.member slotName objSet then return objSet else do
        v <- newIORef []
        return $! T.insert slotName v objSet

    let disposeFun = glDeleteProgram po >> mapM_ glDeleteShader (catMaybes [vsh,gsh,fsh])
        Just objsTVar = T.lookup slotName objSet'
        renderFun = do
            objs <- readIORef objsTVar
            unless (L.null objs) $ do
                setupRasterContext rCtx
                setupAccumulationContext fCtx
                glUseProgram po
                sequence_ objs

    return $! Renderer
        -- public
        { slotUniform           = T.insert slotName uType $! slotUniform rndr
        , slotStream            = T.insert slotName (primType,sType) $! slotStream rndr
        , uniformSetter         = uniformSetter rndr `T.unionL` (T.fromList $! zip newUNames uSetter)
        , render                = render rndr >> renderFun
        , dispose              = dispose rndr >> disposeFun

        -- internal
        , objectSet             = objSet'
        , mkUniformSetup        = mkUniformSetup rndr `T.unionL` (T.fromList $! zip newUNames uSetup)
        , slotUniformLocation   = T.insert slotName uLoc $! slotUniformLocation rndr
        , slotStreamLocation    = T.insert slotName sLoc $! slotStreamLocation rndr
        }

-- FIXME: implement properly
compileRenderer [ScreenOut (H.PrjFrameBuffer n idx fb')] = do
    let fb = convertGP fb'
        --si = streamInput fb
        --ui = uniformInput fb

        blit = do   -- FIXME: implement properly
            glBindFramebuffer gl_DRAW_FRAMEBUFFER 0
            glReadBuffer gl_COLOR_ATTACHMENT0
            glDrawBuffer gl_BACK
            glBlitFramebuffer 0 0 640 480 0 0 640 480 (fromIntegral gl_COLOR_BUFFER_BIT) gl_NEAREST
    -- TODO: validate
    --          all slot name should be unique
    --          all uniform with same name have the same type
    --          all stream input with same name have the same type
    rndr <- compileFrameBuffer fb $! Renderer
        -- public
        { slotUniform           = T.empty
        , slotStream            = T.empty
        , uniformSetter         = T.empty
        , render                = return ()
        , dispose              = return ()

        -- internal
        , objectSet             = T.empty
        , mkUniformSetup        = T.empty
        , slotUniformLocation   = T.empty
        , slotStreamLocation    = T.empty
        }

    return $! rndr {render = render rndr >> blit}

-- OBJECT
data Object -- internal type
    = Object
    { objectUniformSetter :: Trie InputSetter
    }

-- question: should we render the full stream?
--  answer: YES

-- WARNING: sub network slot sharing is not supported at the moment!
--addObject :: Renderer -> ByteString -> Primitive -> Maybe (IndexStream Buffer) -> Trie (Stream Buffer) -> [ByteString] -> IO Object
addObject renderer slotName prim objIndices objAttributes objUniforms = do
    let Just (slotType,sType) = T.lookup slotName $ slotStream renderer
        objSType = fmap streamToInputType objAttributes
        primType = case prim of
            TriangleStrip   -> Triangles
            TriangleList    -> Triangles
            TriangleFan     -> Triangles
            LineStrip       -> Lines
            LineList        -> Lines
            PointList       -> Points
        primGL = case prim of
            TriangleStrip   -> gl_TRIANGLE_STRIP
            TriangleList    -> gl_TRIANGLES
            TriangleFan     -> gl_TRIANGLE_FAN
            LineStrip       -> gl_LINE_STRIP
            LineList        -> gl_LINES
            PointList       -> gl_POINTS
        streamCounts = [c | Stream _ _ _ _ c <- T.elems objAttributes]
        count = head streamCounts
        Just uLocs = T.lookup slotName (slotUniformLocation renderer)
        Just uTypes = T.lookup slotName (slotUniform renderer)

    -- validate
    unless (T.member slotName $! slotUniform renderer) $ fail $ "addObject: slot name mismatch: " ++ show slotName
    when (slotType /= primType) $ fail $ "addObject: primitive type mismatch: " ++ show (slotType,primType)
    when (objSType /= sType) $ fail $ unlines
        [ "addObject: attribute mismatch"
        , "expected:"
        , "  " ++ show sType
        , "actual:"
        , "  " ++ show objSType
        ]
    when (L.null streamCounts) $ fail "addObject: missing stream attribute, a least one stream attribute is required!"
    when (L.or [c /= count | c <- streamCounts]) $ fail "addObject: streams should have the same length!"
    unless (and [T.member n uLocs | n <- objUniforms]) $ fail "addObject: unknown slot uniform!"

    -- validate index type if presented and create draw action
    (iSetup,draw) <- case objIndices of
        Nothing -> return (glBindBuffer gl_ELEMENT_ARRAY_BUFFER 0, glDrawArrays primGL 0 (fromIntegral count))
        Just (IndexStream (Buffer arrs bo) arrIdx start idxCount) -> do
            -- setup index buffer
            let ArrayDesc arrType arrLen arrOffs arrSize = arrs V.! arrIdx
                glType = arrayTypeToGLType arrType
                ptr    = intPtrToPtr $! fromIntegral (arrOffs + start * sizeOfArrayType arrType)
            -- validate index type
            when (notElem arrType [ArrWord8, ArrWord16, ArrWord32]) $ fail "addObject: index type should be unsigned integer type"
            return (glBindBuffer gl_ELEMENT_ARRAY_BUFFER bo, glDrawElements primGL (fromIntegral idxCount) glType ptr)

    -- create uniform setup action
    (mkObjUSetup,objUSetters) <- unzip <$> (sequence [mkUSetter t | n <- objUniforms, let Just t = T.lookup n uTypes])
    let objUSetterTrie = T.fromList $! zip objUniforms objUSetters
        objUSetup = zipWithM_ (\n mkOUS -> let Just loc = T.lookup n uLocs in mkOUS loc) objUniforms mkObjUSetup

        globalUNames = Set.toList $! (Set.fromList $! T.keys uLocs) Set.\\ (Set.fromList objUniforms)
        mkUSetup = mkUniformSetup renderer
        globalUSetup = sequence_ [mkUS loc | n <- globalUNames, let Just mkUS = T.lookup n mkUSetup, let Just loc = T.lookup n uLocs]

        -- stream setup action
        Just sLocs = T.lookup slotName (slotStreamLocation renderer)
        sSetup = sequence_ [mkSSetter t loc s | (n,s) <- T.toList objAttributes, let Just t = T.lookup n sType, let Just loc = T.lookup n sLocs]

    -- create Vertex Array Object
    vao <- alloca $! \pvao -> glGenVertexArrays 1 pvao >> peek pvao
    glBindVertexArray vao
    sSetup -- setup vertex attributes
    iSetup -- setup index buffer
    let renderFun = globalUSetup >> objUSetup >> glBindVertexArray vao >> draw
        -- setup uniforms
        -- setup stream input (aka object attributes)
        -- execute draw function
        Just objSet = T.lookup slotName (objectSet renderer)
    modifyIORef objSet (renderFun:)
    return $! Object objUSetterTrie

removeObject gfxNetwork obj = undefined
