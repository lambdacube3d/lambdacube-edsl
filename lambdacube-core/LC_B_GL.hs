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

import LC_B_GLType
import LC_B_GLUtil
import LC_B_GLSLCodeGen
import LC_B_Traversals
import LC_B_GLCompile

-- Renderer

nubS :: Ord a => [a] -> [a]
nubS = Set.toList . Set.fromList

findFetch :: GP -> Maybe GP
findFetch f = listToMaybe [a | a@Fetch {} <- drawOperations f]

-- odered according pass dependency (topology order)
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

printGLStatus = checkGL >>= print
printFBOStatus = checkFBO >>= print

mkSlotDescriptor :: Set GP -> IO SlotDescriptor
mkSlotDescriptor gps = SlotDescriptor gps <$> newIORef Set.empty

mkRenderTextures :: [GP] -> IO (Map Exp String, Map Exp String, Map Exp GLuint, IO (), GP -> [Exp])
mkRenderTextures allGPs = do
    let samplers = nubS [s | s@(Sampler _ _ _ _) <- expUniverse allGPs]
        samplersWithTexture = nubS [s | s@(Sampler _ _ _ (Texture {})) <- samplers]
        -- collect all render textures refers to a FrameBuffer
        isReferred :: GP -> Exp -> Bool
        isReferred f (Sampler _ _ _ (Texture _ _ _ [f'])) = findFrameBuffer f' == f
        isReferred _ _ = False
        dependentSamplers f = filter (isReferred f) samplersWithTexture
        -- texture attributes: GL texture target (1D,2D,etc), arity, float/word/int, size, mipmap
        -- sampler attributes: filter, edge mode
    -- TODO: also build sampler name map: Map (Exp :: Sampler) (ByteString, GLTexObj)

    -- question: how should we handle the Stencil and Depth textures at multipass rendering
    (renderTexNameList,renderTexGLObjList,disposeTex) <- fmap unzip3 $ forM (zip [0..] samplersWithTexture) $ \(sIdx,smp) -> do
        to <- createGLTextureObject smp
        putStr (" -- Render Texture " ++ show sIdx ++ ": ") >> printGLStatus
        return ((smp,"renderTex_" ++ show sIdx),(smp,to),with to $ \pto -> glDeleteTextures 1 pto)
    let renderTexName   = Map.fromList renderTexNameList
        renderTexGLObj  = Map.fromList renderTexGLObjList
        texSlotName     = Map.fromList $ nubS [(s,SB.unpack n) | s@(Sampler _ _ _ (TextureSlot n _)) <- samplers]
    return (texSlotName, renderTexName, renderTexGLObj, sequence_ disposeTex, dependentSamplers)

mkRenderDescriptor :: Map Exp String -> Map Exp String -> Map Exp GLuint -> GP -> IO RenderDescriptor
mkRenderDescriptor texSlotName renderTexName renderTexGLObj f = case f of
    FrameBuffer {}  -> RenderDescriptor T.empty T.empty (compileClearFrameBuffer f) (return ()) <$> newIORef (ObjectSet (return ()) Map.empty)
    Accumulate {}   -> do
        {- 
            setup texture input, before each slot's render operation we should setup texture unit mapping
                - we have to create the TextureUnit layout
                - create TextureUnit setter action
                    - the shader should be setup at the creation
                    - we have to setup texture binding before each render action call
        -}
        maxTextureUnits <- glGetIntegerv1 gl_MAX_COMBINED_TEXTURE_IMAGE_UNITS
        let usedRenderSamplers  = nubS [s | s@(Sampler _ _ _ (Texture {})) <- expUniverse f]
            usedSlotSamplers    = nubS [s | s@(Sampler _ _ _ (TextureSlot {})) <- expUniverse f]
            usedRenderTexName   = [(s,n) | s <- usedRenderSamplers, let Just n = Map.lookup s renderTexName]
            usedTexSlotName     = [(s,n) | s <- usedSlotSamplers, let Just n = Map.lookup s texSlotName]
            renderTexObjs       = [txObj | s <- usedRenderSamplers, let Just txObj = Map.lookup s renderTexGLObj]
            textureSetup        = forM_ (zip renderTexObjs [0..fromIntegral maxTextureUnits-1]) $ \(texObj,texUnitIdx) -> do
                glActiveTexture $ gl_TEXTURE0 + texUnitIdx
                glBindTexture gl_TEXTURE_2D texObj
                --putStr (" -- Texture bind (TexUnit " ++ show (texUnitIdx,texObj) ++ " TexObj): ") >> printGLStatus

        drawRef <- newIORef $ ObjectSet (return ()) Map.empty
        (rA,dA,uT,sT) <- compileRenderFrameBuffer usedRenderTexName usedTexSlotName drawRef f
        return $ RenderDescriptor
            { uniformLocation   = uT
            , streamLocation    = sT
            , renderAction      = textureSetup >> rA
            , disposeAction     = dA
            , drawObjectsIORef  = drawRef
            }
    _ -> error $ "GP node type error: should be FrameBuffer but got: " ++ (head $ words $ show f)

mkPassSetup :: Map Exp GLuint -> (GP -> [Exp]) -> Bool -> GP -> IO (IO (), IO ())
mkPassSetup renderTexGLObj dependentSamplers isLast fb = case isLast of
    True    -> do
        let setup = do
                --glViewport 100 100 312 312 --(fromIntegral depthW) (fromIntegral depthH)
                --glViewport 0 0 512 512 --(fromIntegral depthW) (fromIntegral depthH)
                glBindFramebuffer gl_DRAW_FRAMEBUFFER 0
                glDrawBuffer gl_BACK_LEFT
                --putStr " -- default FB bind: " >> printGLStatus
        return (setup,return ())
    False   -> do
        --  setup each pass's FBO output, attach RenderTarget textures to source FBO
        putStrLn " -- FBO init: "
        -- FIXME: impelement properly
        let depthW = 512
            depthH = 512
        --glViewport 0 0 (fromIntegral depthW) (fromIntegral depthH)
        depthTex <- alloca $! \pto -> glGenRenderbuffers 1 pto >> peek pto
        putStr "    - alloc depth texture: " >> printGLStatus
        glBindRenderbuffer gl_RENDERBUFFER depthTex
        putStr "    - bind depth texture: " >> printGLStatus
        -- temp
        -- temp end
        glRenderbufferStorage gl_RENDERBUFFER gl_DEPTH_COMPONENT32 depthW depthH 
        putStr "    - define depth texture: " >> printGLStatus

        glFBO <- alloca $! \pbo -> glGenFramebuffers 1 pbo >> peek pbo
        putStr "    - alloc: " >> printGLStatus
        glBindFramebuffer gl_DRAW_FRAMEBUFFER glFBO
        putStr "    - bind: " >> printGLStatus
        glFramebufferRenderbuffer gl_DRAW_FRAMEBUFFER gl_DEPTH_ATTACHMENT gl_RENDERBUFFER depthTex
        
        putStr "    - attach depth texture: " >> printGLStatus
        let depSamplers = dependentSamplers fb
        fboMapping <- forM (zip [0..] depSamplers) $ \(i,smp) -> do
            let Sampler _ _ _ (Texture _ _ _ [PrjFrameBuffer _ prjIdx _]) = smp
                Just txObj  = Map.lookup smp renderTexGLObj
            glFramebufferTexture2D gl_DRAW_FRAMEBUFFER (gl_COLOR_ATTACHMENT0 + fromIntegral i) gl_TEXTURE_2D txObj 0
            putStr ("    - attach to color slot" ++ show (i,txObj) ++ ": ") >> printGLStatus
            return $ gl_COLOR_ATTACHMENT0 + fromIntegral (length depSamplers - prjIdx - 1) -- FIXME: calculate FBO attachment index properly, index reffered from right
        withArray fboMapping $ glDrawBuffers (fromIntegral $ length fboMapping)
        putStrLn $ "    - FBO mapping: " ++ show [i - gl_COLOR_ATTACHMENT0 | i <- fboMapping]
        putStr "    - mappig setup: " >> printGLStatus

        putStr "    - check FBO completeness: " >> printFBOStatus

        let renderAct = do
                glBindFramebuffer gl_DRAW_FRAMEBUFFER glFBO
                --putStr " -- FBO bind: " >> printGLStatus
                --putStr " -- FBO status: " >> printFBOStatus
            disposeAct = do
                with glFBO $ \pbo -> glDeleteFramebuffers 1 pbo
                with depthTex $ \pto -> glDeleteTextures 1 pto
        return (renderAct,disposeAct)

{-
  Note: Input mapping problem
    more programs use the same slot    -> minimize vertex attribute mapping collisions (best case: use the same mapping)
    more programs use the same uniform -> minimize uniform mapping collisions (best case: use the same mapping)
-}
-- FIXME: implement properly
compileRenderer :: GPOutput -> IO Renderer
compileRenderer (ScreenOut (PrjFrameBuffer n idx gp)) = do
    let unis :: GP -> [(ByteString,InputType)]
        unis fb = nubS [(name,t) | Uni ty name <- expUniverse fb, let [t] = codeGenType ty] ++
                  nubS [(name,t) | Sampler ty _ _ (TextureSlot name _) <- expUniverse fb, let [t] = codeGenType ty]

        ordFBs = orderedFrameBuffersFromGP gp
        allGPs = nubS $ concatMap gpUniverse' ordFBs

        -- collect slot info: name, primitive type, stream input, uniform input
        (slotStreamList, slotUniformList, slotGPList) = unzip3
              [ (T.singleton name (primType,T.fromList inputs)
                ,T.singleton name (T.fromList $ unis fb)
                ,T.singleton name (Set.singleton fb))
              | fb <- concatMap renderChain ordFBs
              , Fetch name primType inputs <- maybeToList $ findFetch fb
              ]
        slotStreamTrie  = foldl' (T.mergeBy (\(a1,a2) (b1,b2) -> Just (a1, T.unionL a2 b2))) T.empty slotStreamList
        slotUniformTrie = foldl' (T.mergeBy (\a b -> Just (T.unionL a b))) T.empty slotUniformList
        (uniformNames,uniformTypes) = unzip $ nubS $ concatMap (T.toList . snd) $ T.toList slotUniformTrie

    putStrLn $ "GP universe size:  " ++ show (length allGPs)
    putStrLn $ "Exp universe size: " ++ show (length (nubS $ expUniverse gp))
    (uSetup,uSetter) <- unzip <$> mapM mkUniformSetter uniformTypes
    let uniformSetterTrie   = T.fromList $! zip uniformNames uSetter
        mkUniformSetupTrie  = T.fromList $! zip uniformNames uSetup

        slotGP :: Trie (Set GP)
        slotGP = foldl' (T.mergeBy (\a b -> Just $ Set.union a b)) T.empty slotGPList

    -- create SlotDescriptors (input setup)
    slotDescriptors <- T.fromList <$> mapM (\(n,a) -> (n,) <$> mkSlotDescriptor a) (T.toList slotGP)

    -- allocate render textures (output resource initialization)
    (texSlotName,renderTexName,renderTexGLObj,renderTexDispose,dependentSamplers) <- mkRenderTextures allGPs

    -- create RenderDescriptors
    renderDescriptors <- Map.fromList <$> mapM (\a -> (a,) <$> mkRenderDescriptor texSlotName renderTexName renderTexGLObj a) (nubS $ concatMap renderChain ordFBs)

    -- join compiled graphics network components
    (passRender,passDispose) <- fmap unzip $ forM ordFBs $ \fb -> do
        let (drawList, disposeList) = unzip [(renderAction rd, disposeAction rd) | f <- renderChain fb, let Just rd = Map.lookup f renderDescriptors]
        (passSetup,passDispose) <- mkPassSetup renderTexGLObj dependentSamplers (fb == gp) fb
        return (passSetup >> sequence_ drawList, passDispose >> sequence_ disposeList)

    -- debug
    putStrLn $ "number of passes: " ++ show (length ordFBs) ++ "   is output the last? " ++ show (findFrameBuffer gp == last ordFBs)

    -- TODO: validate
    --          all slot name should be unique
    --          all uniform with same name have the same type
    --          all stream input with same name have the same type

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

        -- internal
        , mkUniformSetup        = mkUniformSetupTrie
        , slotDescriptor        = slotDescriptors
        , renderDescriptor      = renderDescriptors
        }
