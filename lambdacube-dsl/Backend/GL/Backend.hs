{-# LANGUAGE TupleSections, MonadComprehensions, ViewPatterns #-}
module Backend.GL.Backend where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Bits
import Data.ByteString.Char8 (ByteString,pack)
import Data.IORef
import Data.IntMap (IntMap)
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Trie as T
import Data.Vector (Vector,(!),(//))
import qualified Data.ByteString.Char8 as SB
import qualified Data.Foldable as F
import qualified Data.IntMap as IM
import qualified Data.Map as Map
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Graphics.Rendering.OpenGL.Raw.Core33
import Foreign

-- LC IR imports
import IR as IR

import Backend.GL.Type
import Backend.GL.Util

import Backend.GL.Input

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
                glDepthMask 1
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
                glColorMask 1 1 1 1
                glClearColor r g b a
                return (m .|. gl_COLOR_BUFFER_BIT, i+1)
            _ -> error "internal error (clearRenderTarget)"
    (mask,_) <- foldM setClearValue (0,0) values
    glClear $ fromIntegral mask


printGLStatus = checkGL >>= print
printFBOStatus = checkFBO >>= print

compileProgram :: Trie InputType -> Program -> IO GLProgram
compileProgram uniTrie p = do
    po <- glCreateProgram
    putStrLn $ "compile program: " ++ show po
    let createAndAttach src t = do
            o <- glCreateShader t
            compileShader o $ map pack [src]
            glAttachShader po o
            putStr "    + compile shader source: " >> printGLStatus
            return o

    objs <- sequence $ createAndAttach (vertexShader p) gl_VERTEX_SHADER : createAndAttach (fragmentShader p) gl_FRAGMENT_SHADER : case geometryShader p of
        Nothing -> []
        Just s  -> [createAndAttach s gl_GEOMETRY_SHADER]

    forM_ (zip (programOutput p) [0..]) $ \((pack -> n,t),i) -> SB.useAsCString n $ \pn -> do
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
    print uniforms
    print attributes
    when (uniformsType /= (toTrie $ programUniforms p) `unionL` (toTrie $ programInTextures p)) $ fail "shader program uniform input mismatch!"
    when (attributesType /= fmap snd (toTrie $ programStreams p)) $ fail $ "shader program stream input mismatch! " ++ show (attributesType,fmap snd (toTrie $ programStreams p))
    -- the public (user) pipeline and program input is encoded by the slots, therefore the programs does not distinct the render and slot textures input
    let inUniNames = toTrie $ programUniforms p
        (inUniforms,inTextures) = L.partition (\(n,v) -> T.member n inUniNames) $ T.toList $ uniforms
        texUnis = [n | (n,_) <- inTextures, T.member n uniTrie]
    return $ GLProgram
        { shaderObjects         = objs
        , programObject         = po
        , inputUniforms         = T.fromList inUniforms
        , inputTextures         = T.fromList inTextures
        , inputTextureUniforms  = S.fromList $ texUnis
        , inputStreams          = T.fromList [(n,(idx,pack attrName)) | ((n,idx),(_,(attrName,_))) <- zip (T.toList $ attributes) (T.toList $ toTrie $ programStreams p)]
        }

compileSampler :: SamplerDescriptor -> IO GLSampler
compileSampler s = return $ GLSampler {}

{-
data ImageIndex
    = TextureImage  TextureName Int (Maybe Int)  -- Texture name, mip index, array index
    | Framebuffer   ImageSemantic

data ImageSemantic
    = Depth
    | Stencil
    | Color
-}
{-
    = RenderTarget
    { renderTargets :: [(ImageSemantic,Maybe ImageIndex)]   -- render texture or default framebuffer (semantic, render texture for the program output)
    }
-}
{-
  glDrawBuffers
    GL_NONE
    --GL_FRONT_LEFT
    --GL_FRONT_RIGHT
    GL_BACK_LEFT
    --GL_BACK_RIGHT
    GL_COLOR_ATTACHMENTn
-}
compileRenderTarget :: Vector TextureDescriptor -> Vector GLTexture -> RenderTarget -> IO GLRenderTarget
compileRenderTarget texs glTexs (RenderTarget targets) = do
    let isFB (Framebuffer _)    = True
        isFB _                  = False
        images = [img | (_,Just img) <- targets]
    case all isFB images of
        True -> do
            let bufs = [cvt img | (Color,img) <- targets]
                cvt a = case a of
                    Nothing                     -> gl_NONE
                    Just (Framebuffer Color)    -> gl_BACK_LEFT
                    _                           -> error "internal error (compileRenderTarget)!"
            return $ GLRenderTarget
                { framebufferObject         = 0
                , framebufferDrawbuffers    = Just bufs
                }
        False -> do
            when (any isFB images) $ fail "internal error (compileRenderTarget)!"
            fbo <- alloca $! \pbo -> glGenFramebuffers 1 pbo >> peek pbo
            glBindFramebuffer gl_DRAW_FRAMEBUFFER fbo
            {-
                void glFramebufferTexture1D(GLenum target, GLenum attachment, GLenum textarget, GLuint texture, GLint level);
                    GL_TEXTURE_1D
                void glFramebufferTexture2D(GLenum target, GLenum attachment, GLenum textarget, GLuint texture, GLint level);
                    GL_TEXTURE_2D
                    GL_TEXTURE_RECTANGLE
                    GL_TEXTURE_CUBE_MAP_POSITIVE_X
                    GL_TEXTURE_CUBE_MAP_POSITIVE_Y
                    GL_TEXTURE_CUBE_MAP_POSITIVE_Z
                    GL_TEXTURE_CUBE_MAP_NEGATIVE_X
                    GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
                    GL_TEXTURE_CUBE_MAP_NEGATIVE_Z
                    GL_TEXTURE_2D_MULTISAMPLE
                void glFramebufferTextureLayer(GLenum target, GLenum attachment, GLuint texture, GLint level, GLint layer);
                void glFramebufferRenderbuffer(GLenum target, GLenum attachment, GLenum renderbuffertarget, GLuint renderbuffer);
                void glFramebufferTexture(GLenum target, GLenum attachment, GLuint texture, GLint level);
            -}
            let attach attachment (TextureImage texIdx level (Just layer)) =
                    glFramebufferTextureLayer gl_DRAW_FRAMEBUFFER attachment (glTextureTarget $ glTexs ! texIdx) (fromIntegral level) (fromIntegral layer)
                attach attachment (TextureImage texIdx level Nothing) = do
                    let glTex = glTexs ! texIdx
                        tex = texs ! texIdx
                        txLevel = fromIntegral level
                        txTarget = glTextureTarget glTex
                        txObj = glTextureObject glTex
                        attachArray = glFramebufferTexture gl_DRAW_FRAMEBUFFER attachment txObj txLevel
                        attach2D    = glFramebufferTexture2D gl_DRAW_FRAMEBUFFER attachment txTarget txObj txLevel
                    case textureType tex of
                        Texture1D     _ n
                            | n > 1             -> attachArray
                            | otherwise         -> glFramebufferTexture1D gl_DRAW_FRAMEBUFFER attachment txTarget txObj txLevel
                        Texture2D     _ n
                            | n > 1             -> attachArray
                            | otherwise         -> attach2D
                        Texture3D     _         -> attachArray
                        TextureCube   _         -> attachArray
                        TextureRect   _         -> attach2D
                        Texture2DMS   _ n _ _
                            | n > 1             -> attachArray
                            | otherwise         -> attach2D
                        TextureBuffer _         -> fail "internalError (compileRenderTarget/TextureBuffer)!"
            
                go a (Stencil,Just img) = do
                    fail "Stencil support is not implemented yet!"
                    return a
                go a (Depth,Just img) = do
                    attach gl_DEPTH_ATTACHMENT img
                    return a
                go (bufs,colorIdx) (Color,Just img) = do
                    let attachment = gl_COLOR_ATTACHMENT0 + fromIntegral colorIdx
                    attach attachment img
                    return (attachment : bufs, colorIdx + 1)
                go (bufs,colorIdx) (Color,Nothing) = return (gl_NONE : bufs, colorIdx + 1)
                go a _ = return a
            (bufs,_) <- foldM go ([],0) targets
            withArray (reverse bufs) $ glDrawBuffers (fromIntegral $ length bufs)
            return $ GLRenderTarget
                { framebufferObject         = fbo
                , framebufferDrawbuffers    = Nothing
                }

allocPipeline :: Pipeline -> IO GLPipeline
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

disposePipeline :: GLPipeline -> IO ()
disposePipeline p = do
    setPipelineInput p Nothing
    V.forM_ (glPrograms p) $ \prg -> do
        glDeleteProgram $ programObject prg
        mapM_ glDeleteShader $ shaderObjects prg
    let targets = glTargets p
    withArray (map framebufferObject $ V.toList targets) $ (glDeleteFramebuffers $ fromIntegral $ V.length targets)
    let textures = glTextures p
    withArray (map glTextureObject $ V.toList textures) $ (glDeleteTextures $ fromIntegral $ V.length textures)
    with (glVAO p) $ (glDeleteVertexArrays 1)

{-
data SlotSchema
    = SlotSchema
    { primitive     :: FetchPrimitive
    , attributes    :: Trie StreamType
    }
    deriving Show

data PipelineSchema
    = PipelineSchema
    { slots     :: Trie SlotSchema
    , uniforms  :: Trie InputType
    }
    deriving Show
-}
isSubTrie :: (a -> a -> Bool) -> Trie a -> Trie a -> Bool
isSubTrie eqFun universe subset = and [isMember a (T.lookup n universe) | (n,a) <- T.toList subset]
  where
    isMember a Nothing  = False
    isMember a (Just b) = eqFun a b

-- TODO: if there is a mismatch thow detailed error message in the excoeption, containing the missing attributes and uniforms
{-
    let sch = schema input
    forM_ uniformNames $ \n -> case T.lookup n (uniforms sch) of
        Nothing -> throw $ userError $ "Unknown uniform: " ++ show n
        _ -> return ()
    case T.lookup slotName (slots sch) of
        Nothing -> throw $ userError $ "Unknown slot: " ++ show slotName
        Just (SlotSchema sPrim sAttrs) -> do
            when (sPrim /= (primitiveToFetchPrimitive prim)) $ throw $ userError $
                "Primitive mismatch for slot (" ++ show slotName ++ ") expected " ++ show sPrim  ++ " but got " ++ show prim
            let sType = fmap streamToStreamType attribs
            when (sType /= sAttrs) $ throw $ userError $ unlines $ 
                [ "Attribute stream mismatch for slot (" ++ show slotName ++ ") expected "
                , show sAttrs
                , " but got "
                , show sType
                ]
-}
setPipelineInput :: GLPipeline -> Maybe GLPipelineInput -> IO ()
setPipelineInput p input' = do
    -- TODO: check matching input schema
    {-
    case input' of
        Nothing     -> return ()
        Just input  -> schemaFromPipeline p
    -}
    {-
        deletion:
            - remove pipeline's object commands from used slots
            - remove pipeline from attached pipelines vector
    -}
    ic' <- readIORef $ glInput p
    case ic' of
        Nothing -> return ()
        Just ic -> do
            let idx      = icId ic
                oldInput = icInput ic
                slotMask = icSlotMapPipelineToInput ic
                slotRefs = slotVector oldInput
            modifyIORef (pipelines oldInput) $ \v -> v // [(idx,Nothing)]
            V.forM_ slotMask $ \slotIdx -> do
                slot <- readIORef (slotRefs ! slotIdx)
                forM_ (IM.elems $ objectMap slot) $ \obj -> do
                    modifyIORef (objCommands obj) $ \v -> v // [(idx,V.empty)]
    {-
        addition:
            - get an id from pipeline input
            - add to attached pipelines
            - generate slot mappings
            - update used slots, and generate object commands for objects in the related slots
    -}
    case input' of
        Nothing -> writeIORef (glInput p) Nothing
        Just input -> do
            let pipelinesRef = pipelines input
            oldPipelineV <- readIORef pipelinesRef
            (idx,shouldExtend) <- case V.findIndex isNothing oldPipelineV of
                Nothing -> do
                    -- we don't have empty space, hence we double the vector size
                    let len = V.length oldPipelineV
                    modifyIORef pipelinesRef $ \v -> (V.concat [v,V.replicate len Nothing]) // [(len,Just p)]
                    return (len,Just len)
                Just i  -> do
                    modifyIORef pipelinesRef $ \v -> v // [(i,Just p)]
                    return (i,Nothing)
            -- create input connection
            let sm      = slotMap input
                pToI    = [i | n <- glSlotNames p, let Just i = T.lookup n sm]
                iToP    = V.update (V.replicate (T.size sm) Nothing) (V.imap (\i v -> (v, Just i)) pToI)
            writeIORef (glInput p) $ Just $ InputConnection idx input pToI iToP

            -- generate object commands for related slots
            {-
                for each slot in pipeline:
                    map slot name to input slot name
                    for each object:
                        generate command program vector => for each dependent program:
                            generate object commands
            -}
            let slotV = slotVector input
                progV = glPrograms p
                texUnitMap = glTexUnitMapping p
                topUnis = uniformSetup input
                emptyV  = V.replicate (V.length progV) []
                extend v = case shouldExtend of
                    Nothing -> v
                    Just l  -> V.concat [v,V.replicate l V.empty]
            V.forM_ (V.zip pToI (glSlotPrograms p)) $ \(slotIdx,prgs) -> do
                slot <- readIORef $ slotV ! slotIdx
                forM_ (IM.elems $ objectMap slot) $ \obj -> do
                    let cmdV = emptyV // [(prgIdx,createObjectCommands texUnitMap topUnis obj (progV ! prgIdx)) | prgIdx <- prgs]
                    modifyIORef (objCommands obj) $ \v -> extend v // [(idx,cmdV)]
{-
  track state:
    - render target
    - binded textures
-}

{-
  render steps:
    - update uniforms
        - per uniform setup
        - buffer setup (one buffer per object, which has per at least one object uniform)
    - new command: set uniform buffer (binds uniform buffer to program's buffer slot)
    - render slot steps:
        - set uniform buffer or set uniforms separately
        - set vertex and index array
        - call draw command
-}
{-
  storage alternatives:
    - interleaved / separated
    - VAO or VBOs
-}
    {-
      strategy:
        step 1: generate commands for an object
        step 2: sort object merge and do optimization by filtering redundant commands
    -}
{-
  design:
    runtime eleminiation of redundant buffer bind commands and redundant texture bind commands
-}
{-
  track:
    buffer binding on various targets: gl_ARRAY_BUFFER, GL_ELEMENT_ARRAY_BUFFER
    glEnable/DisableVertexAttribArray
-}
renderSlot :: [GLObjectCommand] -> IO ()
renderSlot cmds = forM_ cmds $ \cmd -> do
    case cmd of
        GLSetVertexAttribArray idx buf size typ ptr     -> do
                                                            glBindBuffer gl_ARRAY_BUFFER buf
                                                            glEnableVertexAttribArray idx
                                                            glVertexAttribPointer idx size typ (fromIntegral gl_FALSE) 0 ptr
        GLSetVertexAttribIArray idx buf size typ ptr    -> do
                                                            glBindBuffer gl_ARRAY_BUFFER buf
                                                            glEnableVertexAttribArray idx
                                                            glVertexAttribIPointer idx size typ 0 ptr
        GLDrawArrays mode first count                   -> glDrawArrays mode first count
        GLDrawElements mode count typ buf indicesPtr    -> do
                                                            glBindBuffer gl_ELEMENT_ARRAY_BUFFER buf
                                                            glDrawElements mode count typ indicesPtr
        GLSetUniform idx (GLUniform ty ref)             -> setUniform idx ty ref
        GLBindTexture txTarget tuRef (GLUniform _ ref)  -> do
                                                            txObjVal <- readIORef ref
                                                            -- HINT: ugly and hacky
                                                            with txObjVal $ \txObjPtr -> do
                                                                txObj <- peek $ castPtr txObjPtr :: IO GLuint
                                                                texUnit <- readIORef tuRef
                                                                glActiveTexture $ gl_TEXTURE0 + fromIntegral texUnit
                                                                glBindTexture txTarget txObj
        GLSetVertexAttrib idx val                       -> do
                                                            glDisableVertexAttribArray idx
                                                            setVertexAttrib idx val
    --isOk <- checkGL
    --putStrLn $ SB.unpack isOk ++ " - " ++ show cmd

renderPipeline :: GLPipeline -> IO ()
renderPipeline glp = do
    glBindVertexArray (glVAO glp)
    forM_ (glCommands glp) $ \cmd -> do
        case cmd of
            GLSetRasterContext rCtx         -> setupRasterContext rCtx
            GLSetAccumulationContext aCtx   -> setupAccumulationContext aCtx
            GLSetRenderTarget rt bufs       -> do
                                                -- set target viewport
                                               when (rt == 0) $ do -- screen out
                                                ic' <- readIORef $ glInput glp
                                                case ic' of
                                                    Nothing -> return ()
                                                    Just ic -> do
                                                                let input = icInput ic
                                                                (w,h) <- readIORef $ screenSize input
                                                                glViewport 0 0 (fromIntegral w) (fromIntegral h)
                                                -- TODO: set FBO target viewport
                                                glBindFramebuffer gl_DRAW_FRAMEBUFFER rt
                                                case bufs of
                                                    Nothing -> return ()
                                                    Just bl -> withArray bl $ glDrawBuffers (fromIntegral $ length bl)
            GLSetProgram p                  -> glUseProgram p
            GLSetSamplerUniform i tu ref    -> glUniform1i i tu >> writeIORef ref tu
            GLSetTexture tu target tx       -> glActiveTexture tu >> glBindTexture target tx
            GLClearRenderTarget vals        -> clearRenderTarget vals
            GLGenerateMipMap tu target      -> glActiveTexture tu >> glGenerateMipmap target
            GLRenderSlot slotIdx progIdx    -> do
                                                input <- readIORef (glInput glp)
                                                case input of
                                                    Nothing -> putStrLn "Warning: No pipeline input!" >> return ()
                                                    Just ic -> do
                                                        GLSlot _ objs _ <- readIORef (slotVector (icInput ic) ! (icSlotMapPipelineToInput ic ! slotIdx))
                                                        --putStrLn $ "Rendering " ++ show (V.length objs) ++ " objects"
                                                        V.forM_ objs $ \(_,obj) -> do
                                                            enabled <- readIORef $ objEnabled obj
                                                            when enabled $ do
                                                                cmd <- readIORef $ objCommands obj
                                                                --putStrLn "Render object"
                                                                renderSlot ((cmd ! icId ic) ! progIdx)
            {-
            GLSetSampler
            GLSaveImage
            GLLoadImage
            -}
        --isOk <- checkGL
        --putStrLn $ SB.unpack isOk ++ " - " ++ show cmd

data CGState
    = CGState
    { currentProgram    :: ProgramName
    , textureBinding    :: IntMap GLTexture
    }

initCGState = CGState
    { currentProgram    = error "CGState: empty currentProgram"
    , textureBinding    = IM.empty
    }

type CG a = State CGState a

compileCommand :: Trie (IORef GLint) -> Vector GLSampler -> Vector GLTexture -> Vector GLRenderTarget -> Vector GLProgram -> Command -> CG GLCommand
compileCommand texUnitMap samplers textures targets programs cmd = case cmd of
    SetRasterContext rCtx       -> return $ GLSetRasterContext rCtx
    SetAccumulationContext aCtx -> return $ GLSetAccumulationContext aCtx
    SetRenderTarget rt          -> let GLRenderTarget fbo bufs = targets ! rt in return $ GLSetRenderTarget fbo bufs
    SetProgram p                -> do
                                    modify (\s -> s {currentProgram = p})
                                    return $ GLSetProgram $ programObject $ programs ! p
    SetSamplerUniform n tu      -> do
                                    p <- currentProgram <$> get
                                    case T.lookup (pack n) (inputTextures $ programs ! p) of
                                        Nothing -> fail "internal error (SetSamplerUniform)!"
                                        Just i  -> case T.lookup (pack n) texUnitMap of
                                            Nothing -> fail "internal error (SetSamplerUniform - IORef)!"
                                            Just r  -> return $ GLSetSamplerUniform i (fromIntegral tu) r
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
