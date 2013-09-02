module LC_B2_Compile3 (compile) where

import GHC.Exts (groupWith)
import Control.Monad.State
import Data.ByteString.Char8 (ByteString)
import Data.IntMap (IntMap)
import Data.IntMap (IntMap)
import Data.List as L
import Data.Map (Map)
import Data.Set (Set)
import Data.Trie as T
import Data.Trie.Convenience as T
import Data.Vector ((!),Vector,(//))
import qualified Data.ByteString.Char8 as SB
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

import BiMap
import LC_G_Type
import LC_G_APIType
import LC_U_APIType
import LC_U_DeBruijn (Exp(..), DAG(..), toExp, ExpId)
import LC_B2_IR
import LC_B_GLSLCodeGen

expUniverse :: Int -> Exp -> V.Vector IS.IntSet -> IS.IntSet
expUniverse expId exp v = (\l -> IS.unions $ IS.singleton expId : (map (v !) l)) $ case exp of
    Lam a                   -> [a]
    Body a                  -> [a]
    Apply a b               -> [a, b]
    Tup a                   -> a
    Prj _ a                 -> [a]
    Cond a b c              -> [a, b, c]
    PrimApp _ a             -> [a]
    Loop a b c d            -> [a, b, c, d]
    VertexOut a b c d       -> [a, b] ++ c ++ d
    GeometryOut a b c d e   -> [a, b, c] ++ d ++ e
    FragmentOut a           -> a
    FragmentOutDepth a b    -> a : b
    FragmentOutRastDepth a  -> a
    Transform a b           -> [a, b]
    Reassemble a b          -> [a, b]
    Rasterize _ a           -> [a]
    Accumulate _ a b c _    -> [a, b, c]
    PrjFrameBuffer _ _ a    -> [a]
    PrjImage _ _ a          -> [a]
    Filter a                -> [a]
    Flat a                  -> [a]
    Smooth a                -> [a]
    NoPerspective a         -> [a]
    GeometryShader _ _ _ a b c  -> [a, b, c]
    Sampler _ a             -> [a]
    _                       -> []

mkExpUni :: DAG -> V.Vector [(Int,Ty,Exp)]
mkExpUni dag = V.map (\is -> [(i, tys ! i, exps ! i) | i <- IS.elems is]) $ foldl' (\v i -> V.snoc v $ expUniverse i (toExp dag i) v) V.empty [0..s-1]
  where
    s = IM.size im
    exps = V.generate s (im IM.!)
    tys = V.generate s (tim IM.!)
    tim = dagTy dag
    BiMap _ im = dagExp dag

{-
frameBufferChain :: DAG -> Int -> [(Int,Exp)]
frameBufferChain dag expId = reverse $ go expId
      where
    go i = let a = toExp dag i in case a of
        FrameBuffer {}          -> [(i,a)]
        Accumulate _ _ _ _ b    -> (i,a) : go b
        _                       -> []
-}

{-
traverse :: Monad m => DAG -> Int -> (Exp -> m a) -> m a
-}

textureName :: DAG -> Int -> (ByteString,Bool)
textureName dag smpExpId = case toExp dag smpExpId of
    Sampler _ txExpId   -> case toExp dag txExpId of
        TextureSlot name _  -> (name, True)
        _                   -> (SB.pack $ "texture_" ++ show smpExpId, False)
    _                   -> error "internal error (textureName - Sampler)!"

{-
getTextures :: [(Int,Exp)] -> Trie TextureDescriptor
getTextures expL = undefined --T.fromList [(textureName i, TextureDescriptor a b c) | (i, Texture a b c _) <- expL]
-}

codeGenGLSL :: DAG
            -> M.Map Exp String
            -> FetchPrimitive
            -> [(ByteString,InputType)]
            -> Int
            -> Maybe Int
            -> Int
            -> Int
            -> (ByteString, Maybe ByteString, ByteString, [(ByteString,InputType)])
codeGenGLSL dag smpMap prim attrIn vShader gShader fFilter fShader = (vSrc, gSrc, fSrc, fOut)
  where
    (vSrc,vOut) = codeGenVertexShader dag smpMap attrIn (toExp dag vShader)
    (gSrc,fIn)  = case gShader of
        Nothing -> (Nothing,vOut)
        Just g  -> (Just s, o)
          where
            (s,o) = codeGenGeometryShader dag smpMap prim vOut (toExp dag g)
    (fSrc,fOut,_) = codeGenFragmentShader dag smpMap fIn (toExp dag fFilter) (toExp dag fShader)

{-
    program name is derived from accumilate node id
    
    collects slots (Slot) and programs (Program)
-}
{-
  hint:
    we refer and use slots only here (we generate slots, programs and program execution commands)
-}
data ProgramContext
    = ProgramContext
    { pcRasterCtx       :: RasterContext
    , pcAccumulationCtx :: AccumulationContext
    , pcProgramIndex    :: ProgramName
    , pcProgram         :: Program
    , pcSlotIndex       :: SlotName
    }

getSlotsAndPrograms :: DAG -> [(Int,Exp)] -> V.Vector [(Int,Ty,Exp)] -> (Vector Slot, Vector Program, IntMap ProgramContext)
getSlotsAndPrograms dag expL expU = (slotsV,progsV,ctxMap)
  where
    {-
        - produce a Slot and Program and ExecutionContext triple for each accumulation node
        - slot temp ID: string name
        - program temp ID: accumulation exp id
    -}
    (slotList, progList, ctxList) = unzip3 [program prgIdx accIdx | (accIdx, Accumulate {}) <- expL | prgIdx <- [0..]]

    -- create final command map which maps accumilate exp id to list of commnands what setup and runs the program
    ctxMap = IM.fromList [(key,ctxFun slotsMap) | (key,ctxFun) <- ctxList]

    -- join per program slot (refererence) to final result
    slotsL = T.elems $ foldl' (\t s -> T.insertWith' mergeSlots (slotName s) s t) T.empty slotList
    slotsV = V.fromList [s {slotPrograms = map (progsMap IM.!) (slotPrograms s)} | s <- slotsL]

    mergeSlots :: Slot -> Slot -> Slot
    mergeSlots a b =
        Slot
            { slotName      = slotName a
            , slotUniforms  = T.unionL (slotUniforms a) (slotUniforms b)
            , slotStreams   = T.unionL (slotStreams a) (slotStreams b)
            , slotPrimitive = slotPrimitive a
            , slotPrograms  = slotPrograms a ++ slotPrograms b
            }

    -- maps slot name to slot vector index
    slotsMap = T.fromList $ zip (map slotName slotsL) [0..]

    -- maps program name (accumulate exp id) to program vector index
    progsMap = IM.fromList $ zip (map fst ctxList) [0..]

    -- create final program vector
    progsV = V.fromList progList

    program :: Int -> Int -> (Slot,Program,(Int,Trie Int -> ProgramContext))
    program prgIdx accIdx = (slot,prog,(accIdx,ctx))
      where
        ctx m = prgCtx
          where
            prgCtx = ProgramContext
                { pcRasterCtx       = rCtx
                , pcAccumulationCtx = aCtx
                , pcProgramIndex    = prgIdx
                , pcProgram         = prog
                , pcSlotIndex       = slotIdx
                }
            slotIdx = T.lookupWithDefault (error "internal error (getSlotsAndPrograms)") sName m
        exps = expU ! accIdx

        -- extract Exp nodes for program
        Accumulate aCtx fFilter fShader fst _ = toExp dag accIdx
        Rasterize rCtx ps = toExp dag fst
        (gShader,ps') = case toExp dag ps of
            Reassemble gsh ps'' -> (Just gsh, ps'')
            _                   -> (Nothing, ps)
        Transform vShader vst = toExp dag ps'
        Fetch sName primitive attributes = toExp dag vst

        -- query uniforms and attributes from shader expressions
        uniforms = T.fromList $ [(name, ty) | (_,Single ty,Uni name) <- exps]
        textureUniforms = T.fromList $ [(name, ty) | (smpExpId,Single ty,Sampler {}) <- exps, let (name,isTextureSlot) = textureName dag smpExpId, isTextureSlot]

        -- render and slot texture names are generated from sampler exp id
        inTextures = T.fromList [(name, ty) | (j,Single ty,e@(Sampler {})) <- exps, let (name,_) = textureName dag j]
        inTexturesMap = M.fromList [(e, SB.unpack name) | (j,Single _,e@(Sampler {})) <- exps, let (name,_) = textureName dag j]

        (inAttributesMap,inAttributes) = unzip $
            [ ((attrName, (an,at)),(attrName, at))
            | (j,(an,at)) <- zip [0..] attributes
            , let attrName = SB.pack $ "vAttributeIn_" ++ show j]

        -- generate GLSL shader source
        (vSrc,gSrc,fSrc,progOut) = codeGenGLSL dag inTexturesMap primitive inAttributes vShader gShader fFilter fShader

        -- create final program
        prog = Program
            { programUniforms   = uniforms
            , programStreams    = T.fromList inAttributesMap
            , programInTextures = inTextures
            , programOutput     = progOut
            , vertexShader      = vSrc
            , geometryShader    = gSrc
            , fragmentShader    = fSrc
            }

        -- create intermediate slot, this refers to accumulate exp id
        slot = Slot
            { slotName      = sName
            , slotUniforms  = uniforms `T.unionL` textureUniforms
            , slotStreams   = T.fromList attributes
            , slotPrimitive = primitive
            , slotPrograms  = [accIdx]
            }
{-
    | PrjFrameBuffer        ByteString Int !ExpId
    | PrjImage              ByteString Int !ExpId
    | Texture               TextureType Value MipMap [ExpId] -- hint: type, size, mip, data
    | FrameBuffer           [Image]
    | Accumulate            AccumulationContext !ExpId !ExpId !ExpId !ExpId
    | ImageOut              ByteString !ExpId
    | ScreenOut             !ExpId
    | MultiOut              [ExpId]
-}

{-
  compile algorithm:
    multipass compilation
        step 1: collect logical render targets, this determines the image sets related to each accumulation
        step 2: allocate textures to store images and framebuffer objects for render targets

  allocation:
    step 1: allocate storage for FBO components
    step 2: create textures from FBO components:
      options:
        - using SaveImage (TexImageCopy)
        - attach directly to FBO (possible if the image value is not shared between multiple textures with different settings e.g. mipmap)
-}

data TargetState
    = TargetState
    { sFBComponent  :: Maybe Int    -- framebuffer component index of the current image
    -- , sDepExpId     :: Maybe Int    -- exp node id, which uses (depends on) the current image value
    -- , sImageSize    :: Maybe Value
    -- , sIndices      :: [Int]        -- exp values needed for image (like other images)
    , sTarget       :: [ImageTarget]
    }
    deriving Show

{-



for each image in the framebuffer we store a list of textures or renderbuffers who use the image's value

a framebuffer component can be used for:
    texture input
    framebuffer input
    screen out input

for each framebuffer component we must specify:
    size (2D,Array)
    semantic
    type (float,int,word)
    arity
-}
data ImageTarget
    = ScreenOutTarget
    | ImageOutTarget    ExpId
    | TextureTarget     ExpId
    | SamplerTarget     ExpId
    deriving (Eq, Ord, Show)

type ImageTargetMap = IntMap (Set (Int,ImageTarget)) -- accumulation exp id to ImageTargets mapping
{-
type FBO = [Set ImageTarget]

type FBOMap = IntMap FBO -- accumulation exp id to ImageTargets mapping

-- group accumulation's images by size and produce logical FBO's (set of components)
groupFBO :: Set ImageTarget -> Set FBO
groupFBO s = S.fromList [fbo x | x <- S.toList s, then group by lImageSize x using groupWith]
  where
    fbo :: [RenderImage] -> [Maybe RenderImage]
    fbo l = V.toList $ V.replicate (maximum $ map lFBComponent l) Nothing // [(lFBComponent a,Just a) | a <- l]
-}
{-
  collects all logical render targets with their image components
  temporary constraint:
    for now we only support autogenerated mipmapped textures and autogenerated texture arrays
-}

collectImageTargets :: DAG -> V.Vector [(Int,Ty,Exp)] -> ExpId -> ImageTargetMap
collectImageTargets dag expU = {-fmap groupFBO . -}go emptyState IM.empty
  where
    emptyState = TargetState
        { sFBComponent  = Nothing
        , sTarget       = []
        }

    addImage :: TargetState -> ImageTargetMap -> ExpId -> ImageTargetMap
    addImage t imageMap expId = IM.unionWith (S.union) imageMap $ IM.singleton expId $ S.fromList $ case t of
        TargetState (Just fbIdx) targets -> [(fbIdx,t) | t <- targets]
        s   -> error $ "internal error (tagRenderTargets): " ++ show s

    addTarget :: ImageTarget -> TargetState -> TargetState
    addTarget t ts = ts {sTarget = t : sTarget ts}

    go :: TargetState -> ImageTargetMap -> ExpId -> ImageTargetMap
    go s imageMap expId = case toExp dag expId of
        -- change render target tag
        Sampler _ tex       -> go (addTarget (SamplerTarget expId) s) imageMap tex
        Texture _ _ _ [img] -> go (addTarget (TextureTarget expId) s) imageMap img
        Texture _ _ _ _     -> error "FIXME: Texture composition from multiple images is not supported yet!"
        ImageOut _ _ i      -> go (addTarget (ImageOutTarget expId) s) imageMap i
        ScreenOut i         -> go (addTarget ScreenOutTarget s) imageMap i
        MultiOut outs       -> foldl' (go s) imageMap outs

        -- mark framebuffer component usage
        PrjFrameBuffer _ i j    -> go s {sFBComponent = Just i} imageMap j
        PrjImage _ i j          -> error "PrjImage is not supported yet!"--go s imageMap j

        -- tag with render target
        FrameBuffer _           -> addImage s imageMap expId
        -- add Accumulate's expId to tag, because we can free the render texture after that render operation
        Accumulate _ _ _ _ j    -> go s imageMap' j
          where
            isTexture txExpId = case toExp dag txExpId of
                Texture {}  -> True
                _           -> False
            -- filter out TextureSlot nodes
            imageMap' = foldl' (go emptyState) (addImage s imageMap expId) [i | (i,_,Sampler _ txExpId) <- expU ! expId, isTexture txExpId]

data AllocationState
    = AllocationState
    { sCommands         :: [Command]
    , sTextures         :: IntMap TextureDescriptor
    , sTargets          :: IntMap RenderTarget
    }

type Alloc = State AllocationState

{-
  allocates:
    textures
    samplers
    render targets
  constructs render command sequence
-}
{-
  TODO:
    allocates physical resources for logical render targets, like:
        framebuffer objects
        textures
        samplers
-}
allocate :: IntMap ProgramContext -> DAG -> V.Vector [(Int,Ty,Exp)] -> Int -> AllocationState
allocate ctxMap dag expU rootExpId = execState (evalOut rootExpId) emptyState
  where
    fboMap = collectImageTargets dag expU rootExpId
    emptyState  = AllocationState
        { sCommands = []
        , sTextures = IM.empty
        , sTargets  = IM.empty
        }

    updateCache :: ExpId -> Alloc ()
    updateCache expId = do
        -- free render texture if necessary
        return ()
{-
    allocTexture :: ImageDescriptor -> Alloc ByteString
    allocTexture d = do
        s <- get
        let m = sFreeTextures s
        case M.lookup d m of
            Just (i:xs) -> do
                put s {sFreeTextures = M.insert d xs m}
                return i
            _ -> do
                let tm = sTextures s
                    texName = textureName (T.size tm)
                put s {sTextures = T.insert texName d tm}
                return texName
-}

    addRenderTarget :: RenderTarget -> Alloc RenderTargetName
    addRenderTarget rt = do
        s <- get
        let targets = sTargets s
            key = IM.size targets
        put $ s {sTargets = IM.insert key rt targets}
        return key

    emit :: Command -> Alloc ()
    emit cmd = modify (\s -> s {sCommands = cmd : sCommands s})

    ---------------
    -- Traversals
    ---------------
    -- TODO: do we render to the screen or do we need FBOs?
    evalOut :: ExpId -> Alloc ()
    evalOut expId = case toExp dag expId of
        ImageOut _ s i -> do
            -- pre
            evalImage i
            -- post
        ScreenOut i -> do
            -- pre
            evalImage i
            -- post
        MultiOut outs -> mapM_ evalOut outs
        e -> fail $ "internal error (evalOut): " ++ show e

    {-
      TODO:
        step 1. collect FBO attachments
        step 2. allocate FBOs
    -}
    evalImage :: ExpId -> Alloc ()
    evalImage expId = case toExp dag expId of
        PrjFrameBuffer _ idx i -> do
            -- pre
            evalFrameBuffer i
            -- post
        PrjImage _ _ i -> do
            -- pre
            evalImage i
            -- post
        e -> fail $ "internal error (evalImage): " ++ show e

    -- eval framebuffer
    {-
      TODO:
        emit framebuffer commands:
            - clear framebuffer
            - program setup and execution and allocate texture unit mapping
    -}
    {-
        data ImageIndex
            = TextureImage  TextureName Int (Maybe Int)  -- Texture name, mip index, array index
            | Framebuffer   ImageSemantic

        data ImageSemantic
            = Depth
            | Stencil
            | Color

    = RenderTarget
    { renderTargets :: [(ImageSemantic,Maybe ImageIndex)]   -- render texture or default framebuffer (semantic, render texture for the program output)
    }

    = TextureDescriptor
    { textureType       :: TextureType
    , textureSize       :: Value
    , textureSemantic   :: ImageSemantic
    , textureSampler    :: SamplerDescriptor
    , textureBaseLevel  :: Int
    , textureMaxLevel   :: Int
    }
    -}
    evalFrameBuffer :: ExpId -> Alloc ()
    evalFrameBuffer expId = do
        -- TODO: allocate textures and rendertarget
        let isScreenOut (_,ScreenOutTarget) = True
            isScreenOut _ = False

            (screens,targets) = partition isScreenOut $ S.toList $ fboMap IM.! expId
        {-
          TODO:
            group according targets:
                screen
                texture size and type
            add stencil or depth component if necessary
        -}
        {-
        SetProgram
        SetRasterContext
        SetAccumulationContext

        foreach target:
            SetRenderTarget
            ClearRenderTarget / RenderSlot
        -}
        case toExp dag expId of
            FrameBuffer imgs -> do
                let cvtImg img = case img of
                        DepthImage _ v      -> (Depth,VFloat v)
                        StencilImage _ v    -> (Stencil, VInt v)
                        ColorImage _ v      -> (Color,v)
                    imgList = map cvtImg imgs
                    semas   = map fst imgList
                    [(screenIdx,_)] = screens
                    fun _ Stencil   = (Stencil, Just $ Framebuffer Stencil)
                    fun _ Depth     = (Depth, Just $ Framebuffer Depth)
                    fun i Color
                        | i == screenIdx = (Color, Just $ Framebuffer Color)
                        | otherwise = (Color,Nothing)
                    screenTarget = RenderTarget $ zipWith fun [0..] semas
                tg <- addRenderTarget screenTarget
                emit $ SetRenderTarget tg
                emit $ ClearRenderTarget imgList
                return ()
            Accumulate aCtx _ _ _ i -> do
                -- pre
                -- TODO: let imageMap' = foldl' (go emptyState {sDepExpId = Just expId}) (addImage rt imageMap expId) [i | (i,_,Texture {}) <- expU ! expId]
                -- TODO: go rt imageMap' i
                textureBinding <- mapM evalTexture [j | (j,_,Sampler {}) <- expU ! expId]
                -- ask for set render target
                evalFrameBuffer i
                -- post
                -- emit render commands
                let progCtx = ctxMap IM.! expId
                emit $ SetRasterContext $ pcRasterCtx progCtx
                emit $ SetAccumulationContext $ pcAccumulationCtx progCtx
                emit $ SetProgram $ pcProgramIndex progCtx

                -- FIXME: improve texture unit layout calculation
                mapM_ emit [SetSamplerUniform n tuIdx | n <- T.keys $ programInTextures $ pcProgram progCtx | tuIdx <- [0..]]

                emit $ RenderSlot $ pcSlotIndex progCtx
                updateCache expId
            e -> fail $ "internal error (allocate): " ++ show e

    evalTexture :: ExpId -> Alloc (ByteString,ByteString)
    evalTexture expId = case toExp dag expId of
        Sampler _ texId -> case toExp dag texId of
            Texture _ s _ imgs  -> do
                -- pre
                forM_ imgs $ \img -> do
                    evalImage img
                -- post
                return (fst $ textureName dag expId, "")
            TextureSlot _ _ -> do
                -- TODO
                return (fst $ textureName dag expId, "")
            e -> fail $ "internal error (evalTexture): " ++ show e
        e -> fail $ "internal error (evalTexture): " ++ show e
{-
    genRendering :: Bool -> ExpId -> Alloc [(ByteString, Set LogicalRenderTaget)]
    genRendering = undefined
-}
-- HINT:  render texture allocation: liveness analysis of render targets

{-
  share a framebuffer component from the middle of render chain (share partially evaluated render texture)
-}
{-
  HINT:
    - project image (texture array to single texture):
        - attach source image to FBO with the corrseponding layer: glFramebufferTextureLayer
        - glCopyTexSubImage2D
-}

data CompileOptions
    = CompileOptions
    { 
    }

compile :: DAG -> Either String Pipeline
compile dag = if L.null errors then Right pipeline else Left (unlines errors)
  where
    BiMap _ expIds = dagExp dag
    -- expL contains all expressions in a list, useful for global traversal
    expL = IM.toAscList expIds

    -- typed expression list, used for error checking
    expTL = [(i,t,e) | ((i,e),(_,t)) <- zip expL $ IM.toAscList $ dagTy dag]
    errors = validateInput expTL

    -- (U = Universe) expU is a map contains sub nodes for each expression node
    expU = mkExpUni dag

    -- the top exppression node's ID
    rootExpId = IM.size expIds - 1

    -- generate programs and slots
    (slotsV, progsV, progCmds) = getSlotsAndPrograms dag expL expU

    -- generate commands
    allocState = allocate progCmds dag expU rootExpId

    -- the final compiled pipeline
    pipeline = Pipeline
        { textures      = V.empty -- TODO --T.empty -- TODO -- getTextures expL
        , samplers      = V.empty -- TODO
        , targets       = V.fromList $ IM.elems $ sTargets allocState
        , programs      = progsV
        , slots         = slotsV
        , commands      = reverse (sCommands allocState)
        }

{-
  constrains to check:
    - all uniforms with the same name must have same type
    - all slots with the same name must have:
        - same primitive input type
        - slot attributes which share the same name must have the same type
    - only one screen putput is premitted
  Hint:
    it is allowed to fetch only a subset from slot attributes
-}
validateInput :: [(Int,Ty,Exp)] -> [String]
validateInput exps = uniErrors ++ slotPErrors ++ slotAErrors ++ outputErrors
  where
    -- collect not unique elements, those name - value pairs what are ambiguous (e.g "var" - Int and "var" - Bool results: ("var",[Int,Bool]))
    collect         :: Ord a => [T.Trie (Set a)] -> [(String,[a])]
    collect l       = [(SB.unpack n, S.elems s) | (n,s) <- L.filter (\(n,s) -> S.size s > 1) $ T.toList $ foldl' (\t v -> T.unionWith' S.union t v) T.empty l]

    invalidUnis     = collect [T.singleton name (S.singleton ty) | (_,Single ty,Uni name) <- exps]
    invalidSlotPs   = collect slotP
    (slotP,slotA)   = unzip [( T.singleton name (S.singleton primitive)
                             , T.singleton name $ map (\(a,t) -> T.singleton a (S.singleton t)) attributes
                             )
                            | (_,_,Fetch name primitive attributes) <- exps]
    uniErrors       = [ "Uniform '" ++ name ++ "' has ambiguous type: " ++ unwords (map show tys)
                      | (name,tys) <- invalidUnis]
    slotPErrors     = [ "Slot '" ++ name ++ "' has ambiguous primitive type: " ++ unwords (map show prims)
                      | (name,prims) <- invalidSlotPs]
    slotAErrors     = [ unlines $ ("Slot '" ++ SB.unpack name ++ "' has ambiguous attribute types:") : map slotAError attrs
                      | (name,attrs) <- invalidSlotAs]
    invalidSlotAs   = L.filter (\(n,a) -> not $ L.null a) $ T.toList $ fmap collect allAttrs
      where
        allAttrs    = foldl' (\t v -> T.unionWith' (++) t v) T.empty slotA
    slotAError (a,t)= "  '" ++ a ++ "' : " ++ unwords (map show t)
    outputErrors    = if L.length [() | (_,_,ScreenOut _) <- exps] <= 1 then [] else ["Only one screen output is permitted"]
