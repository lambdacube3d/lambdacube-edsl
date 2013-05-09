module LC_B2_Compile3 (compile) where

import Data.ByteString.Char8 (ByteString)
import Data.Trie as T
import Data.Trie.Convenience as T
import qualified Data.ByteString.Char8 as SB
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Vector ((!))
import Data.List as L

import BiMap
import LC_G_Type
import LC_G_APIType
import LC_U_APIType
import LC_U_DeBruijn (Exp(..), DAG(..), toExp)
import LC_B2_IR
import LC_B_GLSLCodeGen

expUniverse :: Int -> Exp -> V.Vector IS.IntSet -> IS.IntSet
expUniverse expId exp v = IS.unions $ IS.singleton expId : case exp of
    Lam a                   -> [v ! a]
    Body a                  -> [v ! a]
    Apply a b               -> map (v !) [a, b]
    Tup a                   -> map (v !) a
    Prj _ a                 -> [v ! a]
    Cond a b c              -> map (v !) [a, b, c]
    PrimApp _ a             -> [v ! a]
    Loop a b c d            -> map (v !) [a, b, c, d]
    VertexOut a b c d       -> map (v !) [a, b] ++ map (v !) c ++ map (v !) d
    GeometryOut a b c d e   -> map (v !) [a, b, c] ++ map (v !) d ++ map (v !) e
    FragmentOut a           -> map (v !) a
    FragmentOutDepth a b    -> v ! a : map (v !) b
    FragmentOutRastDepth a  -> map (v !) a
    Transform a b           -> map (v !) [a, b]
    Reassemble a b          -> map (v !) [a, b]
    Rasterize _ a           -> [v ! a]
    Accumulate _ a b c _    -> map (v !) [a, b, c]
    PrjFrameBuffer _ _ a    -> [v ! a]
    PrjImage _ _ a          -> [v ! a]
    Filter a                -> [v ! a]
    Flat a                  -> [v ! a]
    Smooth a                -> [v ! a]
    NoPerspective a         -> [v ! a]
    GeometryShader _ _ _ a b c  -> map (v !) [a, b, c]
    Sampler _ _ a           -> [v ! a]
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

textureName :: Int -> ByteString
textureName i = SB.pack $ "texture_" ++ show i

programName :: Int -> ByteString
programName i = SB.pack $ "program_" ++ show i

{-
getTextures :: [(Int,Exp)] -> Trie TextureDescriptor
getTextures expL = undefined --T.fromList [(textureName i, TextureDescriptor a b c) | (i, Texture a b c _) <- expL]
-}

getSlots :: Trie (Trie InputType) -> [(Int,Exp)] -> Trie Slot
getSlots slotUnis expL = T.fromList [(name, Slot unis (T.fromList attributes) primitive) | (_, Fetch name primitive attributes) <- expL
                                    , let Just unis = T.lookup name slotUnis]

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

getProgramsAndSlotUniforms :: DAG -> [(Int,Exp)] -> V.Vector [(Int,Ty,Exp)] -> (Trie Program, Trie (Trie InputType), Trie [Command])
getProgramsAndSlotUniforms dag expL expU = (T.fromList progs, slotUnis, T.fromList cmds)
  where
    (slotUnis', progs, cmds) = unzip3 [program i | (i, Accumulate {}) <- expL]
    slotUnis = foldl' (\t v -> T.unionWith' T.unionL t v) T.empty slotUnis'
    program i = (T.singleton slotName uniforms, (progName, prog), (progName, cmd))
      where
        progName = programName i
        cmd = [SetRasterContext rCtx, SetAccumulationContext aCtx, SetProgram progName [], RenderSlot slotName]
        exps = expU ! i

        Accumulate aCtx fFilter fShader fst _ = toExp dag i
        Rasterize rCtx ps = toExp dag fst
        (gShader,ps') = case toExp dag ps of
            Reassemble gsh ps'  -> (Just gsh, ps')
            _                   -> (Nothing, ps)
        Transform vShader vst = toExp dag vst
        Fetch slotName primitive attributes = toExp dag vst

        uniforms = T.fromList $ [(name, ty) | (_,Single ty,Uni name) <- exps] ++ [(name, ty) | (_,Single ty,TextureSlot name _) <- exps]
        inTextures = T.fromList [(textureName j, ty) | (j,Single ty,e@(Texture {})) <- exps]
        inTexturesMap = M.fromList [(e, SB.unpack $ textureName j) | (j,Single _,e@(Texture {})) <- exps]
        (inAttributesMap,inAttributes) = unzip $
            [ ((attrName, (an,at)),(attrName, at))
            | (j,(an,at)) <- zip [0..] attributes
            , let attrName = SB.pack $ "vAttributeIn_" ++ show j]
        (vSrc,gSrc,fSrc,progOut) = codeGenGLSL dag inTexturesMap primitive inAttributes vShader gShader fFilter fShader
        prog = Program
            { programUniforms   = uniforms
            , programStreams    = T.fromList inAttributesMap
            , programInTextures = inTextures
            , programOutput     = progOut
            , vertexShader      = vSrc
            , geometryShader    = gSrc
            , fragmentShader    = fSrc
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

data RTDesc = RTDesc Int (Maybe Int) RenderTaget deriving (Eq, Ord)
data RenderTaget
    = RT_Texture    Int Int -- texture ExpId, accumulate ExpId
    | RT_ImageOut   Int
    | RT_ScreenOut
    deriving (Ord, Eq)
{-
  render target
    Texture     - texture object
    ImageOut    - pixel buffer object
    ScreenOut   - back frame buffer
-}

data WalkState
    = WalkState
    { sFBComponent  :: Maybe Int
    , sIsScreenOut  :: Bool
    , sSrcImageOut  :: Maybe Int
    , sSrcTexture   :: Maybe Int
    , sSrcAccum     :: Maybe Int
    , sImgComponent :: Maybe Int
    }
    deriving Show

tagRenderTargets :: DAG -> V.Vector [(Int,Ty,Exp)] -> Int -> IM.IntMap (S.Set RTDesc)
tagRenderTargets dag expU = go emptyState IM.empty
  where
    emptyState = WalkState
        { sFBComponent  = Nothing
        , sIsScreenOut  = False
        , sSrcImageOut  = Nothing
        , sSrcTexture   = Nothing
        , sSrcAccum     = Nothing
        , sImgComponent = Nothing
        }

    addTag t tagMap expId = IM.unionWith (S.union) tagMap $ IM.singleton expId $ S.singleton $ case t of
        WalkState (Just fbIdx) True Nothing Nothing Nothing imgIdx      -> RTDesc fbIdx imgIdx RT_ScreenOut
        WalkState (Just fbIdx) False (Just i) Nothing Nothing imgIdx    -> RTDesc fbIdx imgIdx (RT_ImageOut i)
        WalkState (Just fbIdx) False Nothing (Just t) (Just a) imgIdx   -> RTDesc fbIdx imgIdx (RT_Texture t a)
        s   -> error $ "internal error: tagRT - " ++ show s

    go rt tagMap expId = case toExp dag expId of
        -- change render target tag
        Texture t s _ imgs  -> foldl' (go rt {sSrcTexture = Just expId}) tagMap imgs
        ImageOut n s i      -> go rt {sSrcImageOut = Just expId} tagMap i
        ScreenOut i         -> go rt {sIsScreenOut = True} tagMap i
        MultiOut outs       -> foldl' (go rt) tagMap outs

        -- mark framebuffer component usage, add render target dependency to framebuffer
        PrjFrameBuffer _ i j    -> go rt {sFBComponent = Just i} tagMap j
        PrjImage _ i j          -> go rt {sImgComponent = Just i} tagMap j

        -- tag with render target
        FrameBuffer _           -> addTag rt tagMap expId
        -- add Accumulate's expId to tag, because we can free the render texture after that render operation
        Accumulate _ _ _ _ j    -> go rt tagMap' j
          where
            tagMap' = foldl' (go emptyState {sSrcAccum = Just expId}) (addTag rt tagMap expId) [i | (i,_,Texture {}) <- expU ! expId]

-- 

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


compile :: DAG -> Either String Pipeline
compile dag = if L.null errors then Right pipeline else Left (unlines errors)
  where
    BiMap _ expIds = dagExp dag
    expL = IM.toAscList expIds
    expTL = [(i,t,e) | ((i,e),(_,t)) <- zip expL $ IM.toAscList $ dagTy dag]
    expU = mkExpUni dag
    errors = validateInput expTL
    (progs, slotUnis, progCmds) = getProgramsAndSlotUniforms dag expL expU
    pipeline = Pipeline
        { textures      = undefined -- TODO -- getTextures expL
        , programs      = progs
        , slots         = getSlots slotUnis expL
        , commands      = undefined -- :: [Command]
        }

validateInput :: [(Int,Ty,Exp)] -> [String]
validateInput exps = uniErrors ++ slotPErrors ++ slotAErrors ++ outputErrors
  where
    collect         :: Ord a => [T.Trie (S.Set a)] -> [(String,[a])]
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
