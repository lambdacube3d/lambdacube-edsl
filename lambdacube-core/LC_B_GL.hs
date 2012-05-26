module LC_B_GL where

import Debug.Trace

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

import LC_B_GLUtil
import LC_B_GLSLCodeGen
import LC_B_Traversals
import LC_B_GLCompile

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
dispose         :: Renderer -> IO ()
-}
-- Object
addObject       :: Renderer -> ByteString -> Primitive -> Maybe (IndexStream Buffer) -> Trie (Stream Buffer) -> [ByteString] -> IO Object
removeObject    :: Renderer -> Object -> IO ()
--objectUniformSetter  :: Object -> Trie InputSetter

--------------------
-- Implementation --
--------------------

data Renderer -- internal type
    = Renderer
    -- public
    { slotUniform           :: Trie (Trie InputType)
    , slotStream            :: Trie (PrimitiveType, Trie InputType)
    , uniformSetter         :: Trie InputSetter         -- global uniform
    , render                :: IO ()
    , dispose               :: IO ()

    -- internal
    , objectSet             :: Trie (IORef ObjectSet)
    , mkUniformSetup        :: Trie (GLint -> IO ())    -- global unifiorm
    , slotUniformLocation   :: Trie (Trie GLint)
    , slotStreamLocation    :: Trie (Trie GLuint)
{-
    , glFrameBuffer         :: Map GP GLuint    -- frame buffer GLObj for each pass (indexed with FrameBuffer :: GP)

    -- FIXME: for now we don't separate render textures from sampler, because OpenGLRaw does not have support for this
    --        so we use custom textures for every samplers, and we setup the texture's sampler attributes
    , glSampler             :: Map Exp (GLuint,ByteString,Int)  -- (in the future: sampler, but for now: texture) object GLObj, 
                                                                -- glsl name, and used Texture Unit (indexed with Sampler :: Exp)

    -- this is render texture for each (GP Image), at the moment we use to store GPOutput's content
    , glImageTexture        :: Map GP GLuint    -- render texture GLObj for each gfx network output (indexed with PrjFrameBuffer or PrjImage :: GP)
-}
    }

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
            - dispose       :: IO ()
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

orderedFrameBuffersFromGP :: GP -> [GP]
orderedFrameBuffersFromGP orig = order deps
  where 
    deps :: Map GP (Set GP)
    deps = add Map.empty $ findFrameBuffer orig

    add :: Map GP (Set GP) -> GP -> Map GP (Set GP)
    add m fb = Map.unionsWith Set.union $ m' : map (add m') fbl
      where
        m'  = Map.alter fun fb m
        fbl = concat [map findFrameBuffer l | Sampler _ _ _ (Texture _ _ _ l) <- concatMap expUniverse (gpUniverse fb)]
        fbs = Set.fromList fbl
        fun Nothing     = Just fbs
        fun (Just a)    = Just (a `Set.union` fbs)

    order :: Map GP (Set GP) -> [GP]
    order d
        | Map.null d = []
        | otherwise  = leaves ++ order (Map.map (Set.\\ (Set.fromList leaves)) hasDeps)
      where
        leaves = Map.keys noDeps
        (noDeps,hasDeps) = Map.partition Set.null d
{-
    = Renderer
    -- public
    { slotUniform           :: Trie (Trie InputType)
    , slotStream            :: Trie (PrimitiveType, Trie InputType)
    , uniformSetter         :: Trie InputSetter         -- global uniform
    , render                :: IO ()
    , dispose               :: IO ()

    -- internal
    , objectSet             :: Trie (IORef ObjectSet)
    , mkUniformSetup        :: Trie (GLint -> IO ())    -- global unifiorm
    , slotUniformLocation   :: Trie (Trie GLint)
    , slotStreamLocation    :: Trie (Trie GLuint)

    , glFrameBuffer         :: Map GP GLuint    -- frame buffer GLObj for each pass (indexed with FrameBuffer :: GP)

    -- FIXME: for now we don't separate render textures from sampler, because OpenGLRaw does not have support for this
    --        so we use custom textures for every samplers, and we setup the texture's sampler attributes
    , glSampler             :: Map Exp (GLuint,ByteString,Int)  -- (in the future: sampler, but for now: texture) object GLObj, 
                                                                -- glsl name, and used Texture Unit (indexed with Sampler :: Exp)

    -- this is render texture for each (GP Image), at the moment we use to store GPOutput's content
    , glImageTexture        :: Map GP GLuint    -- render texture GLObj for each gfx network output (indexed with PrjFrameBuffer or PrjImage :: GP)
    }
-}
-- FIXME: implement properly
compileRenderer [ScreenOut (PrjFrameBuffer n idx gp)] = do
    let nubS :: Ord a => [a] -> [a]
        nubS = Set.toList . Set.fromList

        findFetch :: GP -> Maybe GP
        findFetch f = listToMaybe [a | a@Fetch {} <- drawOperations f]

        unis :: GP -> [(ByteString,InputType)]
        unis fb = nubS [(name,t) | Uni ty name <- expUniverse fb, let [t] = codeGenType ty]

        ordFBs = orderedFrameBuffersFromGP gp
        allGPs = nubS $ concatMap gpUniverse' ordFBs

        -- collect slot info: name, primitive type, stream input, uniform input
        (slotStreamList, slotUniformList) = unzip 
              [ ((name,(primType,T.fromList inputs)),(name, T.fromList $ unis fb)) 
              | fb <- concatMap renderChain ordFBs
              , Fetch name primType inputs <- maybeToList $ findFetch fb]
        (uniformNames,uniformTypes) = unzip $ nubS $ concatMap (T.toList . snd) slotUniformList
        slotNames = map fst slotStreamList

    (uSetup,uSetter) <- unzip <$> mapM mkUSetter uniformTypes
    let uniformSetterTrie   = T.fromList $! zip uniformNames uSetter
        mkUniformSetupTrie  = T.fromList $! zip uniformNames uSetup

    -- create object sets
    objectSetTrie <- T.fromList <$> (forM slotNames $ \n -> (n,) <$> newIORef [])

    {-
      TODO:
        done - collect passes and sort them into proper render order according dependencies
        done - factor out uniform setter map creation from framebuffer compile function
                it should only create shader objects
        done - compile each FrameBuffer (pass)
        create render textures
        setup each pass's FBO output, attach RenderTarget textures to source FBO
        render operations (one pass, with multiple render calls/slots):
            - bind FBO
            - before each slot's render operation we should setup texture unit mapping
            - call slot's render action
    -}

    {-
data TextureDataType - gl internal representation
    = FloatT        ColorArity
    | IntT          ColorArity
    | WordT         ColorArity
    | ShadowT
    deriving (Show, Eq, Ord)

data TextureType - gl texture target
    = Texture1D     TextureDataType Int
    | Texture2D     TextureDataType Int
    | Texture3D     TextureDataType
    | TextureCube   TextureDataType
    | TextureRect   TextureDataType
    | Texture2DMS   TextureDataType Int
    | TextureBuffer TextureDataType
    deriving (Show, Eq, Ord)
    -}
    -- TODO: create render textures
    -- TODO: create TextureUnit mapping (FBO <---> sampler's render texture)
    --          to do this we have to collect all samplers what uses this pass (FrameBuffer)
    let samplers = nubS [s | s@(Sampler _ _ _ _) <- expUniverse allGPs]
        samplersWithTexture = nubS [s | s@(Sampler _ _ _ (Texture _ _ _ _)) <- samplers]
        samplersWithTextureSlot = nubS [s | s@(Sampler _ _ _ (TextureSlot _ _)) <- samplers]
        -- collect all render textures refers to a FrameBuffer
        isReferred :: GP -> Exp -> Bool
        isReferred f (Sampler _ _ _ (Texture _ _ _ [f'])) = findFrameBuffer f' == f
        isReferred _ _ = False
        dependentSamplers f = filter (isReferred f) samplersWithTexture
    {-
    -- Map (GP :: FrameBuffer) GL_FBO
    fboMap <- fmap Map.fromList $ forM ordFBs $ \fb -> do
        fbo <- alloca $! \pbo -> glGenFramebuffers 1 pbo >> peek pbo
        return (fb,(fbo,dependentSamplers fb))
    -}
        -- texture attributes: GL texture target (1D,2D,etc), arity, float/word/int, size, mipmap
        -- sampler attributes: filter, edge mode
            {-
            to <- alloca $! \pto -> glGenTextures 1 pto >> peek pto
            glBindTexture gl_TEXTURE_2D to
            glTexImage2D gl_TEXTURE_2D 0 (fromIntegral gl_RGBA8) (fromIntegral w) (fromIntegral h) 0 gl_BGRA gl_UNSIGNED_BYTE nullPtr
            glFramebufferTexture2D gl_DRAW_FRAMEBUFFER (gl_COLOR_ATTACHMENT0 + fromIntegral i) gl_TEXTURE_2D to 0
            -}
    -- TODO: also build sampler name map: Map (Exp :: Sampler) (ByteString, GLTexObj)
    renderTextures <- fmap Map.fromList $ forM (zip [0..] samplersWithTexture) $
      \(sIdx,smp@(Sampler ty txFilter txEdgeMode (Texture txType txSize txMipMap txGPList))) -> do
        to <- alloca $! \pto -> glGenTextures 1 pto >> peek pto
        {-
            void glTexImage1D( GLenum target, GLint level, GLint internalformat, GLsizei width, GLint border, GLenum format, GLenum type, void *data );
            void glTexImage2D( GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type, void *data );
            void glTexImage3D( GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLenum format, GLenum type, void *data );
            void glTexImage2DMultisample( GLenum target, GLsizei samples, GLint internalformat, GLsizei width, GLsizei height, GLboolean ﬁxedsamplelocations );
            void glTexImage3DMultisample( GLenum target, GLsizei samples, GLint internalformat, GLsizei width, GLsizei height, GLsizei depth, GLboolean ﬁxedsamplelocations );
        -}
        -- FIXME: for now we support only single 2D texture
        case txType of
            {-
            Texture1D dTy n     -> return ()
            Texture2D dTy n     -> return ()
            Texture3D dTy       -> return ()
            TextureCube dTy     -> return ()
            TextureRect dTy     -> return ()
            Texture2DMS dTy n   -> return ()
            TextureBuffer dTy   -> return ()
            -}
            -- temporary texture support: 2D NoMip Float/Int/Word Red/RG/RGBA
            Texture2D dTy 1     -> if txMipMap /= NoMip then error "FIXME: Only NoMip textures are supported yet!" else 
                                   if length txGPList /= 1 then error "Invalid texture source specification!" else do
                let internalFormat  = fromIntegral $ textureDataTypeToGLType dTy
                    dataFormat      = fromIntegral $ textureDataTypeToGLArityType dTy
                    VV2I (V2 w h)   = txSize
                    --[PrjFrameBuffer name idx gpFB] = txGPList
                    --Just (fbo,)
                glBindTexture gl_TEXTURE_2D to
                glTexImage2D gl_TEXTURE_2D 0 internalFormat (fromIntegral w) (fromIntegral h) 0 dataFormat gl_UNSIGNED_BYTE nullPtr
                return ()
            _ -> error $ "FIXME: This texture format is not yet supported: " ++ show txType
        return (smp,("renderTex_" ++ show sIdx,to))

    -- render and dispose action for each pass
    -- [[(RenderAction, DisposeAction, UniformLocation, StreamLocation)]], [[] :: drawcall] :: pass
    -- TODO create: render, dispose, slotUniformLocation, slotStreamLocation
    passes <- forM ordFBs $ \fb -> do
        -- setup output
        --  setup each pass's FBO output, attach RenderTarget textures to source FBO
        glFBO <- alloca $! \pbo -> glGenFramebuffers 1 pbo >> peek pbo
        glBindFramebuffer gl_DRAW_FRAMEBUFFER glFBO
        let delete      = with glFBO $ \pbo -> glDeleteFramebuffers 1 pbo
            depSamplers = dependentSamplers fb
        fboMapping <- forM (zip [0..] depSamplers) $ \(i,smp) -> do
            let Sampler _ _ _ (Texture _ _ _ [PrjFrameBuffer _ prjIdx _]) = smp
                Just (_,txObj)  = Map.lookup smp renderTextures
            glFramebufferTexture2D gl_DRAW_FRAMEBUFFER (gl_COLOR_ATTACHMENT0 + fromIntegral i) gl_TEXTURE_2D txObj 0
            return $ gl_COLOR_ATTACHMENT0 + fromIntegral (length depSamplers - prjIdx - 1) -- FIXME: calculate FBO attachment index properly, index reffered from right

        withArray fboMapping $ glDrawBuffers (fromIntegral $ length fboMapping)

        -- TODO: setup texture input
        forM (renderChain fb) $ \f -> case f of
            FrameBuffer {}  -> return (compileClearFrameBuffer f, return (), T.empty, T.empty)
            Accumulate {}   -> do
                --FIXME: add support for stream slot sharing across multiple draw actions
                --          for that we should build a map: Trie (Map (GP :: Accumulate) ObjectSet)
                let Just (Fetch name primType inputs) = findFetch f
                    Just objSet = T.lookup name objectSetTrie

                -- TODO: before each slot's render operation we should setup texture unit mapping
                (rA,dA,uT,sT) <- compileRenderFrameBuffer objSet f
                return (rA,dA,T.singleton name uT,T.singleton name sT)
            _ -> error "GP node type error: should be FrameBuffer"

    --putStrLn $ "samplers: " ++ show (length samplers) ++ "  render textures: " ++ show (length textures)
    putStrLn $ "number of passes: " ++ show (length ordFBs) ++ "   is output the last? " ++ show (findFrameBuffer gp == last ordFBs)
    -- TODO: validate
    --          all slot name should be unique
    --          all uniform with same name have the same type
    --          all stream input with same name have the same type

    -- depends on compilation: render, dispose, slotUniformLocation, slotStreamLocation
    let (drawAct,disposeAct,uniLocs,streamLocs) = unzip4 $ concat passes
        [(V2 w h)] = [v | FrameBuffer v _ <- gpUniverse' gp]
        width   = fromIntegral w
        height  = fromIntegral h
        blit = do   -- FIXME: implement properly
            {-
            glBindFramebuffer gl_DRAW_FRAMEBUFFER 0
            glReadBuffer gl_COLOR_ATTACHMENT0
            glDrawBuffer gl_BACK
            glBlitFramebuffer 0 0 width height 0 0 width height (fromIntegral gl_COLOR_BUFFER_BIT) gl_NEAREST
            -}
            glBindFramebuffer gl_DRAW_FRAMEBUFFER 0
            glDrawBuffer gl_BACK_LEFT
            sequence_ drawAct

    return $! Renderer
        -- public
        { slotUniform           = T.fromList slotUniformList
        , slotStream            = T.fromList slotStreamList
        , uniformSetter         = uniformSetterTrie
        , render                = blit -- FIXME: fill after compialtion 
        , dispose               = return () -- FIXME: fill after compialtion

        -- internal
        , objectSet             = objectSetTrie
        , mkUniformSetup        = mkUniformSetupTrie
        , slotUniformLocation   = foldl' T.unionL T.empty uniLocs
        , slotStreamLocation    = foldl' T.unionL T.empty streamLocs

--        , glFrameBuffer         = fboMap
--        , glSampler             = Map.empty
--        , glImageTexture        = Map.empty
        }
{-
    return $! Renderer
        -- public
        { slotUniform           = T.insert slotName uType $! slotUniform rndr
        , slotStream            = T.insert slotName (primType,sType) $! slotStream rndr
        , uniformSetter         = uniformSetter rndr `T.unionL` (T.fromList $! zip newUNames uSetter)
        , render                = render rndr >> renderFun
        , dispose               = dispose rndr >> disposeFun

        -- internal
        , objectSet             = objSet'
        , mkUniformSetup        = mkUniformSetup rndr `T.unionL` (T.fromList $! zip newUNames uSetup)
        , slotUniformLocation   = T.insert slotName uLoc $! slotUniformLocation rndr
        , slotStreamLocation    = T.insert slotName sLoc $! slotStreamLocation rndr

        , glFrameBuffer         = glFrameBuffer rndr
        , glSampler             = glSampler rndr
        , glImageTexture        = glImageTexture rndr
        }
-}

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
            TriangleStrip   -> Triangle
            TriangleList    -> Triangle
            TriangleFan     -> Triangle
            LineStrip       -> Line
            LineList        -> Line
            PointList       -> Point
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
        -- FIXME: 
        --sSetup = sequence_ [mkSSetter t loc s | (n,s) <- T.toList objAttributes, let Just t = T.lookup n sType, let Just loc = T.lookup n sLocs]
        sSetup = sequence_ [mkSSetter t loc s | (n,s) <- T.toList objAttributes, t <- maybeToList $ T.lookup n sType, loc <- maybeToList $ T.lookup n sLocs]

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
