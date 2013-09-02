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
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Traversable as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as MV

import Graphics.Rendering.OpenGL.Raw.Core32
    ( GLuint
    -- * FRAMEBUFFER related *
    -- create
    , glBindFramebuffer
    , glDeleteFramebuffers
    , glGenFramebuffers
    -- content manipulation
    , glActiveTexture
    , glBindRenderbuffer
    , glBindTexture
    , glDeleteTextures
    , glDrawBuffer
    , glDrawBuffers
    , glFramebufferRenderbuffer
    , glFramebufferTexture
    , glFramebufferTexture2D
    , glGenRenderbuffers
    , glRenderbufferStorage
    , glViewport
    , gl_BACK_LEFT
    , gl_COLOR_ATTACHMENT0
    , gl_DEPTH_ATTACHMENT
    , gl_DEPTH_COMPONENT32
    , gl_DRAW_FRAMEBUFFER
    , gl_MAX_COMBINED_TEXTURE_IMAGE_UNITS
    , gl_NONE
    , gl_RENDERBUFFER
    , gl_TEXTURE0
    , gl_TEXTURE_2D
    , gl_UNSIGNED_BYTE
    , glTexImage2D
    , gl_TEXTURE_2D_ARRAY
    , glTexImage3D
    , gl_TEXTURE_MAX_LEVEL
    , glTexParameteri
    , gl_TEXTURE_BASE_LEVEL
    , gl_NEAREST
    , gl_TEXTURE_MIN_FILTER
    , gl_TEXTURE_MAG_FILTER
    , gl_CLAMP_TO_EDGE
    , gl_TEXTURE_WRAP_S
    , gl_TEXTURE_WRAP_T
    , gl_DEPTH_COMPONENT32
    , gl_DEPTH_COMPONENT
    , glGenTextures
    )

import LC_G_Type
import LC_G_APIType
import LC_U_APIType
import LC_U_DeBruijn

import LC_B_GLType
import LC_B_GLUtil
import LC_B_GLSLCodeGen
import LC_B_Traversals
import LC_B_GLCompile

-- Renderer

nubS :: Ord a => [a] -> [a]
nubS = Set.toList . Set.fromList

findFetch :: DAG -> Exp -> Maybe Exp
findFetch dag f = listToMaybe [a | a@Fetch {} <- drawOperations dag f]

-- odered according pass dependency (topology order)
orderedFrameBuffersFromGP :: DAG -> Exp -> [Exp]
orderedFrameBuffersFromGP dag orig = order deps
  where 
    deps :: Map Exp (Set Exp)
    deps = add Map.empty $ findFrameBuffer dag orig

    add :: Map Exp (Set Exp) -> Exp -> Map Exp (Set Exp)
    add m fb = Map.unionsWith Set.union $ m' : map (add m') fbl
      where
        m'  = Map.alter fun fb m
        fbl = concat [map (findFrameBuffer dag . toExp dag) l | Sampler _ _ tx <- concatMap (expUniverse' dag) (gpUniverse' dag fb), Texture _ _ _ l <- [toExp dag tx]]
        fbs = Set.fromList fbl
        fun Nothing     = Just fbs
        fun (Just a)    = Just (a `Set.union` fbs)

    order :: Map Exp (Set Exp) -> [Exp]
    order d
        | Map.null d = []
        | otherwise  = leaves ++ order (Map.map (Set.\\ (Set.fromList leaves)) hasDeps)
      where
        leaves = Map.keys noDeps
        (noDeps,hasDeps) = Map.partition Set.null d

printGLStatus = checkGL >>= print
printFBOStatus = checkFBO >>= print

mkSlotDescriptor :: Set Exp -> IO SlotDescriptor
mkSlotDescriptor gps = SlotDescriptor gps <$> newIORef Set.empty

mkRenderTextures :: DAG -> [Exp] -> IO (Map Exp String, Map Exp String, Map Exp GLuint, IO (), Exp -> [Exp])
mkRenderTextures dag allGPs = do
    let samplers = nubS [s | s@Sampler {} <- expUniverse' dag allGPs]
        samplersWithTexture = nubS [s | s@(Sampler _ _ tx) <- samplers, Texture {} <- [toExp dag tx]]
        -- collect all render textures refers to a FrameBuffer
        isReferred :: Exp -> Exp -> Bool
        isReferred f (Sampler _ _ tx) = findFrameBuffer dag (toExp dag f') == f
          where
            Texture _ _ _ [f'] = toExp dag tx
        isReferred _ _ = False
        dependentSamplers f = filter (isReferred f) samplersWithTexture
        -- texture attributes: GL texture target (1D,2D,etc), arity, float/word/int, size, mipmap
        -- sampler attributes: filter, edge mode
    -- TODO: also build sampler name map: Map (Exp :: Sampler) (ByteString, GLTexObj)

    -- question: how should we handle the Stencil and Depth textures at multipass rendering
    (renderTexNameList,renderTexGLObjList,disposeTex) <- fmap unzip3 $ forM (zip [0..] samplersWithTexture) $ \(sIdx,smp) -> do
        to <- createGLTextureObject dag smp
        putStr (" -- Render Texture " ++ show sIdx ++ ": ") >> printGLStatus
        return ((smp,"renderTex_" ++ show sIdx),(smp,to),with to $ \pto -> glDeleteTextures 1 pto)
    let renderTexName   = Map.fromList renderTexNameList
        renderTexGLObj  = Map.fromList renderTexGLObjList
        texSlotName     = Map.fromList $ nubS [(s,SB.unpack n) | s@(Sampler _ _ txExp) <- samplers, TextureSlot n _ <- [toExp dag txExp]]
    return (texSlotName, renderTexName, renderTexGLObj, sequence_ disposeTex, dependentSamplers)

mkRenderDescriptor :: DAG -> RenderState -> Map Exp String -> Map Exp String -> Map Exp GLuint -> Exp -> IO RenderDescriptor
mkRenderDescriptor dag rendState texSlotName renderTexName renderTexGLObj f = case f of
    FrameBuffer imgs  -> RenderDescriptor T.empty T.empty (compileClearFrameBuffer f) (return ()) <$> newIORef (ObjectSet (return ()) Map.empty) <*> pure (length [() | ColorImage {} <- imgs])
    Accumulate {}   -> do
        {- 
            setup texture input, before each slot's render operation we should setup texture unit mapping
                - we have to create the TextureUnit layout
                - create TextureUnit setter action
                    - the shader should be setup at the creation
                    - we have to setup texture binding before each render action call
        -}
        let usedRenderSamplers  = nubS [s | s@(Sampler _ _ te) <- expUniverse' dag f, Texture {} <- [toExp dag te]]
            usedSlotSamplers    = nubS [s | s@(Sampler _ _ te) <- expUniverse' dag f, TextureSlot {} <- [toExp dag te]]
            usedRenderTexName   = [(s,n) | s <- usedRenderSamplers, let Just n = Map.lookup s renderTexName]
            usedTexSlotName     = [(s,n) | s <- usedSlotSamplers, let Just n = Map.lookup s texSlotName]
            renderTexObjs       = [txObj | s <- usedRenderSamplers, let Just txObj = Map.lookup s renderTexGLObj]
            texUnitState        = textureUnitState rendState
            textureSetup        = forM_ (zip renderTexObjs [0.. MV.length texUnitState-1]) $ \(texObj,texUnitIdx) -> do
                let texObj' = fromIntegral texObj
                curTexObj <- MV.read texUnitState texUnitIdx
                when (curTexObj /= texObj') $ do
                    MV.write texUnitState texUnitIdx texObj'
                    glActiveTexture $ gl_TEXTURE0 + fromIntegral texUnitIdx
                    glBindTexture gl_TEXTURE_2D texObj
                    --putStr (" -- Texture bind (TexUnit " ++ show (texUnitIdx,texObj) ++ " TexObj): ") >> printGLStatus

        drawRef <- newIORef $ ObjectSet (return ()) Map.empty
        (rA,dA,uT,sT,outColorCnt) <- compileRenderFrameBuffer dag usedRenderTexName usedTexSlotName drawRef f
        return $ RenderDescriptor
            { uniformLocation   = uT
            , streamLocation    = sT
            , renderAction      = textureSetup >> rA
            , disposeAction     = dA
            , drawObjectsIORef  = drawRef
            , fragmentOutCount  = outColorCnt
            }
    _ -> error $ "GP node type error: should be FrameBuffer but got: " ++ (head $ words $ show f)

-- FIXME: currently we expect ScreenOut to be the last operation
mkPassSetup :: IORef (Word,Word) -> DAG -> Map Exp GLuint -> (Exp -> [Exp]) -> (Bool,Int,Int) -> Exp -> IO (IO (), IO ())
mkPassSetup screenSizeIORef dag renderTexGLObj dependentSamplers (isLast,outIdx,outCnt) fb = case isLast of
    True    -> do
        putStrLn $ " -- last pass output count: " ++ show outCnt ++ "  outIdx: " ++ show outIdx
        glBindFramebuffer gl_DRAW_FRAMEBUFFER 0
        let fboMapping = [if i == outIdx then gl_BACK_LEFT else gl_NONE | i <- [1..outCnt]]
        withArray fboMapping $ glDrawBuffers (fromIntegral $ length fboMapping)
        let setup = do
                glBindFramebuffer gl_DRAW_FRAMEBUFFER 0
                (screenW,screenH) <- readIORef screenSizeIORef
                glViewport 0 0 (fromIntegral screenW) (fromIntegral screenH)
                --putStr " -- default FB bind: " >> printGLStatus
        return (setup,return ())
    False   -> do
        --  setup each pass's FBO output, attach RenderTarget textures to source FBO
        putStrLn " -- FBO init: "

        glFBO <- alloca $! \pbo -> glGenFramebuffers 1 pbo >> peek pbo
        putStr "    - alloc: " >> printGLStatus
        glBindFramebuffer gl_DRAW_FRAMEBUFFER glFBO
        putStr "    - bind: " >> printGLStatus
        let depSamplers = dependentSamplers fb
            hasDepthOp = case fb of
                Accumulate (AccumulationContext _ ops) _ _ _ _  -> not $ L.null [() | DepthOp {} <- ops]
                FrameBuffer imgs -> not $ L.null [() | DepthImage {} <- imgs]
        ----------
        -- FIXME: samplers must contain the fragment value's output index!
        ----------
        (layerCnts,texSizes,fboMapping) <- fmap unzip3 $ forM (zip [0..] depSamplers) $ \(i,smp) -> do
            let Sampler _ _ txExp  = smp
                Texture txType ts NoMip [prjFBExp] = toExp dag txExp
                PrjFrameBuffer _ prjIdx _               = toExp dag prjFBExp
                Just txObj  = Map.lookup smp renderTexGLObj
                colorNumber = outCnt - prjIdx - 1
                attachSingleLayer = glFramebufferTexture2D gl_DRAW_FRAMEBUFFER (gl_COLOR_ATTACHMENT0 + fromIntegral i) gl_TEXTURE_2D txObj 0
                attachMultiLayer = glFramebufferTexture gl_DRAW_FRAMEBUFFER (gl_COLOR_ATTACHMENT0 + fromIntegral i) txObj 0
            lc <- case txType of
                Texture2D _ ln
                    | ln <= 1   -> attachSingleLayer >> return ln
                    | otherwise -> attachMultiLayer >> return ln
                TextureCube _   -> attachMultiLayer >> return 6
            putStr ("    - attach to color slot #" ++ show i ++ "  texture object #" ++ show txObj ++ " with color number #" ++ show colorNumber ++ ": ") >> printGLStatus
            return (lc, ts, (colorNumber,gl_COLOR_ATTACHMENT0 + fromIntegral i)) -- FIXME: calculate FBO attachment index properly, index reffered from right
        let fboMappingMap   = IntMap.fromList fboMapping
            fboMappingList  = [IntMap.findWithDefault gl_NONE i fboMappingMap | i <- [0..outCnt-1]]
        withArray fboMappingList $ glDrawBuffers $ fromIntegral outCnt
        putStrLn $ "    - FBO mapping: " ++ show [if i == gl_NONE then "gl_NONE" else ("gl_COLOR_ATTACHMENT" ++ (show $ i - gl_COLOR_ATTACHMENT0)) | i <- fboMappingList]
        putStr "    - mappig setup: " >> printGLStatus

        -- check all texture size maches
        unless (all (== head texSizes) texSizes) $ error ("Framebuffer attachment size mismatch! \n" ++ "  - sizes: " ++ show texSizes)
        -- create and attach depth buffer
        let VV2U (V2 depthW depthH) = head texSizes
        when hasDepthOp $ do
            {-
            depthTex <- alloca $! \pto -> glGenRenderbuffers 1 pto >> peek pto
            putStr "    - alloc depth texture: " >> printGLStatus
            glBindRenderbuffer gl_RENDERBUFFER depthTex
            putStr "    - bind depth texture: " >> printGLStatus
            glRenderbufferStorage gl_RENDERBUFFER gl_DEPTH_COMPONENT32 (fromIntegral depthW) (fromIntegral depthH)
            putStr "    - define depth texture: " >> printGLStatus
            glFramebufferRenderbuffer gl_DRAW_FRAMEBUFFER gl_DEPTH_ATTACHMENT gl_RENDERBUFFER depthTex
            putStr "    - attach depth texture: " >> printGLStatus
            -}
            depthTex <- alloca $! \pto -> glGenTextures 1 pto >> peek pto
            putStr "    - alloc depth texture: " >> printGLStatus
            let layerCnt = head layerCnts
                txTarget = if layerCnt > 1 then gl_TEXTURE_2D_ARRAY else gl_TEXTURE_2D
                internalFormat = fromIntegral gl_DEPTH_COMPONENT32
                dataFormat = fromIntegral gl_DEPTH_COMPONENT
            glBindTexture txTarget depthTex
            putStr "    - bind depth texture: " >> printGLStatus
            -- temp
            glTexParameteri txTarget gl_TEXTURE_WRAP_S $ fromIntegral gl_CLAMP_TO_EDGE
            glTexParameteri txTarget gl_TEXTURE_WRAP_T $ fromIntegral gl_CLAMP_TO_EDGE
            glTexParameteri txTarget gl_TEXTURE_MAG_FILTER $ fromIntegral gl_NEAREST
            glTexParameteri txTarget gl_TEXTURE_MIN_FILTER $ fromIntegral gl_NEAREST
            glTexParameteri txTarget gl_TEXTURE_BASE_LEVEL 0
            glTexParameteri txTarget gl_TEXTURE_MAX_LEVEL 0
            -- temp end
            case layerCnt > 1 of
                True    -> glTexImage3D gl_TEXTURE_2D_ARRAY 0 internalFormat (fromIntegral depthW) (fromIntegral depthH) (fromIntegral layerCnt) 0 dataFormat gl_UNSIGNED_BYTE nullPtr
                False   -> glTexImage2D gl_TEXTURE_2D 0 internalFormat (fromIntegral depthW) (fromIntegral depthH) 0 dataFormat gl_UNSIGNED_BYTE nullPtr
            putStr "    - define depth texture: " >> printGLStatus
            case layerCnt > 1 of
                True    -> glFramebufferTexture gl_DRAW_FRAMEBUFFER gl_DEPTH_ATTACHMENT depthTex 0
                False   -> glFramebufferTexture2D gl_DRAW_FRAMEBUFFER gl_DEPTH_ATTACHMENT gl_TEXTURE_2D depthTex 0
            putStr "    - attach depth texture: " >> printGLStatus


        putStr "    - check FBO completeness: " >> printFBOStatus

        let renderAct = do
                glBindFramebuffer gl_DRAW_FRAMEBUFFER glFBO
                glViewport 0 0 (fromIntegral depthW) (fromIntegral depthH)
                --putStr " -- FBO bind: " >> printGLStatus
                --putStr " -- FBO status: " >> printFBOStatus
            disposeAct = do
                with glFBO $ \pbo -> glDeleteFramebuffers 1 pbo
                --with depthTex $ \pto -> glDeleteTextures 1 pto
        return (renderAct,disposeAct)

mkRenderState :: IO RenderState
mkRenderState = do
    maxTextureUnits <- glGetIntegerv1 gl_MAX_COMBINED_TEXTURE_IMAGE_UNITS
    texUnitState <- MV.new $ fromIntegral maxTextureUnits
    MV.set texUnitState (-1)
    return $ RenderState
        { textureUnitState  = texUnitState
        }
{-
  Note: Input mapping problem
    more programs use the same slot    -> minimize vertex attribute mapping collisions (best case: use the same mapping)
    more programs use the same uniform -> minimize uniform mapping collisions (best case: use the same mapping)
-}
-- FIXME: implement properly
compileRenderer :: DAG -> Exp -> IO Renderer
compileRenderer dag (ScreenOut img) = do
    let PrjFrameBuffer n idx gpId = toExp dag img
        gp  = toExp dag gpId
        unis :: Exp -> [(ByteString,InputType)]
        unis fb = nubS [(name,t) | u@(Uni name) <- expUniverse' dag fb, let [t] = codeGenType $ expType dag u] ++
                  nubS [(name,t) | s@(Sampler _ _ ts) <- expUniverse' dag fb
                       , TextureSlot name _ <- [toExp dag ts]
                       , let [t] = codeGenType $ expType dag s]

        ordFBs = orderedFrameBuffersFromGP dag gp
        allGPs = nubS $ concatMap (gpUniverse' dag) ordFBs

        -- collect slot info: name, primitive type, stream input, uniform input
        (slotStreamList, slotUniformList, slotGPList) = unzip3
              [ (T.singleton name (primType,T.fromList inputs)
                ,T.singleton name (T.fromList $ unis fb)
                ,T.singleton name (Set.singleton fb))
              | fb <- concatMap (renderChain dag) ordFBs
              , Fetch name primType inputs <- maybeToList $ findFetch dag fb
              ]
        slotStreamTrie  = foldl' (T.mergeBy (\(a1,a2) (b1,b2) -> Just (a1, T.unionL a2 b2))) T.empty slotStreamList
        slotUniformTrie = foldl' (T.mergeBy (\a b -> Just (T.unionL a b))) T.empty slotUniformList
        (uniformNames,uniformTypes) = unzip $ nubS $ concatMap (T.toList . snd) $ T.toList slotUniformTrie

    putStrLn $ "GP universe size:  " ++ show (length allGPs)
    putStrLn $ "Exp universe size: " ++ show (length (nubS $ expUniverse' dag gp))

    -- create RenderState
    rendState <- mkRenderState

    (uSetup,uSetter) <- unzip <$> mapM (mkUniformSetter rendState) uniformTypes
    let uniformSetterTrie   = T.fromList $! zip uniformNames uSetter
        mkUniformSetupTrie  = T.fromList $! zip uniformNames uSetup

        slotGP :: Trie (Set Exp)
        slotGP = foldl' (T.mergeBy (\a b -> Just $ Set.union a b)) T.empty slotGPList

    -- create SlotDescriptors (input setup)
    slotDescriptors <- T.fromList <$> mapM (\(n,a) -> (n,) <$> mkSlotDescriptor a) (T.toList slotGP)

    -- allocate render textures (output resource initialization)
    (texSlotName,renderTexName,renderTexGLObj,renderTexDispose,dependentSamplers) <- mkRenderTextures dag allGPs

    -- create RenderDescriptors
    renderDescriptors <- Map.fromList <$> mapM (\a -> (a,) <$> mkRenderDescriptor dag rendState texSlotName renderTexName renderTexGLObj a) (nubS $ concatMap (renderChain dag) ordFBs)

    -- create IORef for ScreenOut Size
    screenSizeIORef <- newIORef (0,0)

    putStrLn ("number of passes: " ++ show (length ordFBs))
    -- join compiled graphics network components
    (passRender,passDispose) <- fmap unzip $ forM (zip ordFBs [1..]) $ \(fb,passNo) -> do
        let (drawList, disposeList) = unzip [(renderAction rd, disposeAction rd) | f <- renderChain dag fb, let Just rd = Map.lookup f renderDescriptors]
        let Just rd = Map.lookup fb renderDescriptors
        putStrLn ("pass #" ++ show passNo)
        putStrLn (" - draw count: " ++ show (length drawList))
        (passSetup,passDispose) <- mkPassSetup screenSizeIORef dag renderTexGLObj dependentSamplers (fb == gp,fragmentOutCount rd - idx, fragmentOutCount rd) fb
        return (passSetup >> sequence_ drawList, passDispose >> sequence_ disposeList)

    -- debug
    putStrLn $ "number of passes: " ++ show (length ordFBs) ++ "   is output the last? " ++ show (findFrameBuffer dag gp == last ordFBs)

    -- TODO: validate
    --          all slot name should be unique
    --          all uniform with same name have the same type
    --          all stream input with same name have the same type
    objIDSeed <- newIORef 1
    return $! Renderer
        -- public
        { slotUniform           = slotUniformTrie
        , slotStream            = slotStreamTrie
        , uniformSetter         = uniformSetterTrie
        , render                = do
                                    --print " * Frame Started"
                                    sequence_ passRender
                                    --print " * Frame Ended"
        , dispose               = renderTexDispose >> sequence_ passDispose
        , setScreenSize         = \w h -> writeIORef screenSizeIORef (w,h)

        -- internal
        , mkUniformSetup        = mkUniformSetupTrie
        , slotDescriptor        = slotDescriptors
        , renderDescriptor      = renderDescriptors
        , renderState           = rendState
        , objectIDSeed          = objIDSeed
        }
