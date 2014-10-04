module LambdaCube.Core.DeBruijn where

import Debug.Trace
import Control.Monad.State
import Data.ByteString.Char8 (ByteString)

import LambdaCube.Core.Type
import LambdaCube.Core.PrimFun

import LambdaCube.Core.Util.BiMap
import qualified Data.IntMap as IM

import Data.Vector ((!),Vector,(//))
import qualified Data.IntSet as IS
import qualified Data.Vector as V
import Data.List (foldl')
import Data.Maybe (maybeToList)

type ExpId = Int

--newtype DAG = DAG (BiMap Exp) deriving Show
data DAG
    = DAG 
    { dagExp    :: BiMap Exp
    , dagTy     :: IM.IntMap Ty
    , dagCount  :: IM.IntMap Int
    , expUniverseV :: V.Vector [Exp]
    , gpUniverseV :: V.Vector [Exp]
    } deriving (Show,Read)


-- HINT: traveres over one accumulation's sub expressions including samplers covering multiple passes, but does not include the whole accumulation chain
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
    Accumulate a b c d _    -> [a, b, c, d]
    PrjFrameBuffer _ _ a    -> [a]
    PrjImage _ _ a          -> [a]
    Filter a                -> [a]
    Flat a                  -> [a]
    Smooth a                -> [a]
    NoPerspective a         -> [a]
    GeometryShader _ _ _ a b c  -> [a, b, c]
    Sampler _ _ a           -> [a]
    AccumulationContext a _ -> maybeToList a
    _                       -> []

mkExpUni :: DAG -> V.Vector [Exp]
mkExpUni dag = V.map (\is -> [exps ! i | i <- IS.elems is]) $ foldl' (\v i -> V.snoc v $ expUniverse i (toExp dag i) v) V.empty [0..s-1]
  where
    s = IM.size im
    exps = V.generate s (im IM.!)
    BiMap _ im = dagExp dag

gpUniverse :: Int -> Exp -> V.Vector IS.IntSet -> IS.IntSet
gpUniverse expId exp v = (\l -> IS.unions $ IS.singleton expId : (map (v !) l)) $ case exp of
    Transform _ a           -> [a]
    Reassemble _ a          -> [a]
    Rasterize _ a           -> [a]
    Accumulate _ _ _ a b    -> [a, b]
    PrjFrameBuffer _ _ a    -> [a]
    PrjImage _ _ a          -> [a]
    _                       -> []

mkGPUni :: DAG -> V.Vector [Exp]
mkGPUni dag = V.map (\is -> reverse [exps ! i | i <- IS.elems is]) $ foldl' (\v i -> V.snoc v $ gpUniverse i (toExp dag i) v) V.empty [0..s-1]
  where
    s = IM.size im
    exps = V.generate s (im IM.!)
    BiMap _ im = dagExp dag

{-
myNub l = go IS.empty l
  where
    go s [] = []
    go s (x:xs)
        | IS.member x s = go s xs
        | otherwise     = x : go (IS.insert x s) xs

gpUniverse :: Int -> Exp -> V.Vector [ExpId] -> [ExpId]
gpUniverse expId exp v = (\l -> myNub $ concat $ [expId] : (map (v !) l)) $ case exp of
    Transform _ a           -> [a]
    Reassemble _ a          -> [a]
    Rasterize _ a           -> [a]
    Accumulate _ _ _ a b    -> [a, b]
    PrjFrameBuffer _ _ a    -> [a]
    PrjImage _ _ a          -> [a]
    _                       -> []

mkGPUni :: DAG -> V.Vector [Exp]
mkGPUni dag = V.map (\is -> [exps ! i | i <- is]) $ foldl' (\v i -> V.snoc v $ gpUniverse i (toExp dag i) v) V.empty [0..s-1]
  where
    s = IM.size im
    exps = V.generate s (im IM.!)
    BiMap _ im = dagExp dag
-}
emptyDAG :: DAG
emptyDAG = DAG empty IM.empty IM.empty V.empty V.empty

hashcons :: Ty -> Exp -> State DAG ExpId
hashcons !t !e = do
  DAG !m !tm !cm !uv !gv <- get
  case lookup_key e m of
    Nothing -> let (!k,!m') = insert e m
                   !tm'     = IM.insert k t tm
                   !cm'     = IM.insert k 1 cm
               in put (DAG m' tm' cm' uv gv) >> return k
    Just !k  -> do
        {-trace ("sharing : " ++ show k ++ " :: " ++ show (tm IM.! k)) $ -}
        let !cm'    = IM.adjust (1+) k cm
        put (DAG m tm cm' uv gv)
        return k
{-
--hashcons = dontShare
dontShare :: Ty -> Exp -> State DAG ExpId
dontShare t e = do
    DAG m tm <- get
    let (k,m') = insert e m
        tm'    = IM.insert k t tm
    put (DAG m' tm') >> return k
-}

-- Utility functions for CodeGen
toExp :: DAG -> ExpId -> Exp
toExp (DAG !m _ _ _ _) !k = lookup_val k m

toExpId :: DAG -> Exp -> ExpId
toExpId (DAG !m _ _ _ _) !v = let Just k = lookup_key v m in k

expIdType :: DAG -> ExpId -> Ty
expIdType (DAG _ !tm _ _ _) !k = tm IM.! k

expType :: DAG -> Exp -> Ty
expType dag@(DAG !m !tm _ _ _) !e = case lookup_key e m of
    Nothing -> error $ "unknown Exp node: " ++ show e
    Just !k  -> expIdType dag k

expIdCount :: DAG -> ExpId -> Int
expIdCount (DAG _ _ !cm _ _) !k = cm IM.! k

expCount :: DAG -> Exp -> Int
expCount dag@(DAG !m !tm !cm _ _) !e = case lookup_key e m of
    Nothing -> error $ "unknown Exp node: " ++ show e
    Just !k  -> expIdCount dag k

{-
  TODO:
    represent these as tuples from specific types:  VertexOut, GeometryOut, FragmentOut, FragmentOutDepth, FragmentOutRastDepth
-}
data Exp
    -- Fun
    = Lam                   !ExpId
    | Body                  !ExpId
    | Var                   Int String   -- index, layout counter
    | Apply                 !ExpId !ExpId

    -- Exp
    | Const                 !Value
    | PrimVar               !ByteString
    | Uni                   !ByteString
    | Tup                   [ExpId]
    | Prj                   Int !ExpId
    | Cond                  !ExpId !ExpId !ExpId
    | PrimApp               !PrimFun !ExpId
    | Sampler               !Filter !EdgeMode !ExpId
    | Loop                  !ExpId !ExpId !ExpId !ExpId
    -- special tuple expressions
    | VertexOut             !ExpId !ExpId [ExpId] [ExpId]
    | GeometryOut           !ExpId !ExpId !ExpId [ExpId] [ExpId]
    | FragmentOut           [ExpId]
    | FragmentOutDepth      !ExpId [ExpId]
    | FragmentOutRastDepth  [ExpId]

    -- GP
    | Fetch                 ByteString FetchPrimitive [(ByteString,InputType)]
    | Transform             !ExpId !ExpId
    | Reassemble            !ExpId !ExpId
    | Rasterize             RasterContext !ExpId
    | FrameBuffer           [Image]
    | Accumulate            !ExpId !ExpId !ExpId !ExpId !ExpId
    | PrjFrameBuffer        ByteString Int !ExpId
    | PrjImage              ByteString Int !ExpId

    -- Texture
    | TextureSlot           ByteString TextureType
    | Texture               TextureType Value MipMap [ExpId] -- hint: type, size, mip, data

    -- Interpolated
    | Flat                  !ExpId
    | Smooth                !ExpId
    | NoPerspective         !ExpId

    | GeometryShader        Int OutputPrimitive Int !ExpId !ExpId !ExpId

    -- FragmentFilter
    | PassAll
    | Filter                !ExpId

    -- GPOutput
    | ImageOut              ByteString V2U !ExpId
    | ScreenOut             !ExpId
    | MultiOut              [ExpId]

    -- Contexts
    | AccumulationContext   !(Maybe ExpId) [FragmentOperation]
    deriving (Eq, Ord, Show, Read)

class ExpC exp where
    -- exp constructors
    let_        :: exp -> (exp -> exp) -> exp
    lam         :: Ty -> exp -> exp
    body        :: exp -> exp
    var         :: Ty -> Int -> String -> exp -- type, index, layout counter (this needed for proper sharing)
    apply       :: Ty -> exp -> exp -> exp
    const_      :: Ty -> Value -> exp
    primVar     :: Ty -> ByteString -> exp
    uni         :: Ty -> ByteString -> exp
    tup         :: Ty -> [exp] -> exp
    prj         :: Ty -> Int -> exp -> exp
    cond        :: Ty -> exp -> exp -> exp -> exp
    primApp     :: Ty -> PrimFun -> exp -> exp
    sampler     :: Ty -> Filter -> EdgeMode -> exp -> exp
    loop        :: Ty -> exp -> exp -> exp -> exp -> exp
    -- special tuple expressions
    vertexOut               :: exp -> exp -> [exp] -> [exp] -> exp
    geometryOut             :: exp -> exp -> exp -> [exp] -> [exp] -> exp
    fragmentOut             :: [exp] -> exp
    fragmentOutDepth        :: exp -> [exp] -> exp
    fragmentOutRastDepth    :: [exp] -> exp
    -- gp constructors
    fetch           :: ByteString -> FetchPrimitive -> [(ByteString,InputType)] -> exp
    transform       :: exp -> exp -> exp
    reassemble      :: exp -> exp -> exp
    rasterize       :: RasterContext -> exp -> exp
    frameBuffer     :: [Image] -> exp
    accumulationContext :: Maybe exp -> [FragmentOperation] -> exp
    accumulate      :: exp -> exp -> exp -> exp -> exp -> exp
    prjFrameBuffer  :: ByteString -> Int -> exp -> exp
    prjImage        :: ByteString -> Int -> exp -> exp
    -- texture constructors
    textureSlot     :: ByteString -> TextureType -> exp
    texture         :: TextureType -> Value -> MipMap -> [exp] -> exp -- hint: type, size, mip, data
    -- Interpolated constructors
    flat            :: exp -> exp
    smooth          :: exp -> exp
    noPerspective   :: exp -> exp
    -- GeometryShader constructors
    geometryShader  :: Int -> OutputPrimitive -> Int -> exp -> exp -> exp -> exp
    -- FragmentFilter constructors
    passAll         :: exp
    filter_         :: exp -> exp
    -- GPOutput constructors
    imageOut        :: ByteString -> V2U -> exp -> exp
    screenOut       :: exp -> exp
    multiOut        :: [exp] -> exp

newtype N = N {unN :: State DAG ExpId}

instance ExpC N where
    let_ !e !f = N(do
        x <- unN e
        unN $ f (N (return x)))
    lam !t !a = N $ do
        !h1 <- unN a
        hashcons t $ Lam h1
    body !a = N $ do
        !h1 <- unN a
        hashcons (Unknown "Body") $ Body h1
    var !t !a !b     = N $ hashcons t $ Var a b
    apply !t !a !b   = N $ do
        !h1 <- unN a
        !h2 <- unN b
        hashcons t $ Apply h1 h2
    const_ !t !a    = N $ hashcons t $ Const a
    primVar !t !a   = N $ hashcons t $ PrimVar a
    uni !t !a       = N $ hashcons t $ Uni a
    tup !t !a       = N $ do
        !h <- mapM unN a
        hashcons t $ Tup h
    prj !t !a !b     = N $ do
        !h1 <- unN b
        hashcons t $ Prj a h1
    cond !t !a !b !c  = N $ do
        !h1 <- unN a
        !h2 <- unN b
        !h3 <- unN c
        hashcons t $ Cond h1 h2 h3
    primApp !t !a !b = N $ do
        !h1 <- unN b
        hashcons t $ PrimApp a h1
    sampler !t !a !b !c = N $ do
        !h1 <- unN c
        hashcons t $ Sampler a b h1
    loop !t !a !b !c !d = N $ do
        !h1 <- unN a
        !h2 <- unN b
        !h3 <- unN c
        !h4 <- unN d
        hashcons t $ Loop h1 h2 h3 h4

    -- special tuple expressions
    vertexOut !a !b !c !d = N $ do
        !h1 <- unN a
        !h2 <- unN b
        !h3 <- mapM unN c
        !h4 <- mapM unN d
        hashcons (Unknown "VertexOut") $ VertexOut h1 h2 h3 h4
    geometryOut !a !b !c !d !e = N $ do
        !h1 <- unN a
        !h2 <- unN b
        !h3 <- unN c
        !h4 <- mapM unN d
        !h5 <- mapM unN e
        hashcons (Unknown "GeometryOut") $ GeometryOut h1 h2 h3 h4 h5
    fragmentOut !a   = N $ do
        !h <- mapM unN a
        hashcons (Unknown "FragmentOut") $ FragmentOut h
    fragmentOutDepth !a !b    = N $ do
        !h1 <- unN a
        !h2 <- mapM unN b
        hashcons (Unknown "FragmentOutDepth") $ FragmentOutDepth h1 h2
    fragmentOutRastDepth !a  = N $ do
        !h <- mapM unN a
        hashcons (Unknown "FragmentOutRastDepth") $ FragmentOutRastDepth h
    -- gp constructors
    fetch !a !b !c = N $ do
        hashcons (Unknown "VertexStream") $ Fetch a b c
    transform !a !b = N $ do
        !h1 <- unN a
        !h2 <- unN b
        hashcons (Unknown "PrimitiveStream") $ Transform h1 h2
    reassemble !a !b = N $ do
        !h1 <- unN a
        !h2 <- unN b
        hashcons (Unknown "PrimitiveStream") $ Reassemble h1 h2
    rasterize !a !b = N $ do
        !h1 <- unN b
        hashcons (Unknown "FragmentStream") $ Rasterize a h1
    frameBuffer !a = N $ do
        hashcons (Unknown "FrameBuffer") $ FrameBuffer a
    accumulationContext !a !b = N $ do
        !h1 <- case a of
          Nothing -> return Nothing
          Just v -> fmap Just $ unN v
        hashcons (Unknown "AccumulationContext") $ AccumulationContext h1 b
    accumulate !a !b !c !d !e = N $ do
        !h0 <- unN a
        !h1 <- unN b
        !h2 <- unN c
        !h3 <- unN d
        !h4 <- unN e
        hashcons (Unknown "FrameBuffer") $ Accumulate h0 h1 h2 h3 h4
    prjFrameBuffer !a !b !c = N $ do
        !h1 <- unN c
        hashcons (Unknown "Image") $ PrjFrameBuffer a b h1
    prjImage !a !b !c = N $ do
        !h1 <- unN c
        hashcons (Unknown "Image") $ PrjImage a b h1
    -- texture constructors
    textureSlot !a !b = N $ hashcons (Unknown "TextureSlot") $ TextureSlot a b
    texture !a !b !c !d = N $ do
        !h1 <- mapM unN d
        hashcons (Unknown "Texture") $ Texture a b c h1
    -- Interpolated constructors
    flat !a = N $ do
        !h1 <- unN a
        hashcons (Unknown "Flat") $ Flat h1
    smooth !a = N $ do
        !h1 <- unN a
        hashcons (Unknown "Smooth") $ Smooth h1
    noPerspective !a = N $ do
        !h1 <- unN a
        hashcons (Unknown "NoPerspective") $ NoPerspective h1
    -- GeometryShader constructors
    geometryShader !a !b !c !d !e !f = N $ do
        !h1 <- unN d
        !h2 <- unN e
        !h3 <- unN f
        hashcons (Unknown "GeometryShader") $ GeometryShader a b c h1 h2 h3
    -- FragmentFilter constructors
    passAll = N $ hashcons (Unknown "PassAll") PassAll
    filter_ !a = N $ do
        !h1 <- unN a
        hashcons (Unknown "Filter") $ Filter h1
    -- GPOutput constructors
    imageOut !a !b !c = N $ do
        !h1 <- unN c
        hashcons (Unknown "ImageOut") $ ImageOut a b h1
    screenOut !a = N $ do
        !h1 <- unN a
        hashcons (Unknown "ScreenOut") $ ScreenOut h1
    multiOut !a = N $ do
        !h1 <- mapM unN a
        hashcons (Unknown "MultiOut") $ MultiOut h1
