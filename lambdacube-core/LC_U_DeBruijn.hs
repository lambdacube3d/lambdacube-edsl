module LC_U_DeBruijn where

import Debug.Trace
import Control.Monad.State
import Data.ByteString.Char8 (ByteString)
import Data.List (foldl')

import LC_G_Type
import LC_G_APIType
import LC_U_APIType
import LC_U_PrimFun
import LC_B2_IR (SamplerDescriptor)

import BiMap
import qualified Data.IntMap as IM

type ExpId = Int

--newtype DAG = DAG (BiMap Exp) deriving Show
data DAG
    = DAG 
    { dagExp    :: BiMap Exp
    , dagTy     :: IM.IntMap Ty
    , dagCount  :: IM.IntMap Int
    } deriving Show

addCount :: DAG -> DAG
addCount (DAG bm@(BiMap _ m) tm _) = DAG bm tm cm
  where
    cm = IM.foldlWithKey' go IM.empty m
    go c i e = foldl' (\c' i' -> IM.adjust (1+) i' c') (IM.insert i 0 c) $ case e of
        Tup a                   -> a
        Prj _ a                 -> [a]
        Cond a b c              -> [a,b,c]
        PrimApp _ a             -> [a]
        Loop a b c d            -> [a,b,c,d]
        Body a                  -> [a]
        VertexOut a b c d       -> a : b : c ++ d
        GeometryOut a b c d e   -> a : b : c : d ++ e
        FragmentOut a           -> a
        FragmentOutDepth a b    -> a : b
        FragmentOutRastDepth a  -> a
        Flat a                  -> [a]
        Smooth a                -> [a]
        NoPerspective a         -> [a]
        Filter a                -> [a]
        _                       -> []

emptyDAG :: DAG
emptyDAG = DAG empty IM.empty IM.empty

hashcons :: Ty -> Exp -> State DAG ExpId
hashcons !t !e = do
  DAG !m !tm !cm <- get
  case lookup_key e m of
    Nothing -> let (!k,!m') = insert e m
                   !tm'     = IM.insert k t tm
               in put (DAG m' tm' cm) >> return k
    Just !k  -> return k
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
toExp (DAG !m _ _) !k = lookup_val k m

toExpId :: DAG -> Exp -> ExpId
toExpId (DAG !m _ _) !v = let Just k = lookup_key v m in k

expIdType :: DAG -> ExpId -> Ty
expIdType (DAG _ !tm _) !k = tm IM.! k

expType :: DAG -> Exp -> Ty
expType dag@(DAG !m !tm _) !e = case lookup_key e m of
    Nothing -> error $ "unknown Exp node: " ++ show e
    Just !k  -> expIdType dag k

expIdCount :: DAG -> ExpId -> Int
expIdCount (DAG _ _ !cm) !k = cm IM.! k

expCount :: DAG -> Exp -> Int
expCount dag@(DAG !m !tm !cm) !e = case lookup_key e m of
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
    | Sampler               !SamplerDescriptor !ExpId
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
    | Accumulate            AccumulationContext !ExpId !ExpId !ExpId !ExpId
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
    deriving (Eq, Ord, Show)

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
    sampler     :: Ty -> SamplerDescriptor -> exp -> exp
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
    accumulate      :: AccumulationContext -> exp -> exp -> exp -> exp -> exp
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
    sampler !t !a !b = N $ do
        !h1 <- unN b
        hashcons t $ Sampler a h1
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
        hashcons VertexStream' $ Fetch a b c
    transform !a !b = N $ do
        !h1 <- unN a
        !h2 <- unN b
        hashcons PrimitiveStream' $ Transform h1 h2
    reassemble !a !b = N $ do
        !h1 <- unN a
        !h2 <- unN b
        hashcons PrimitiveStream' $ Reassemble h1 h2
    rasterize !a !b = N $ do
        !h1 <- unN b
        hashcons FragmentStream' $ Rasterize a h1
    frameBuffer !a = N $ do
        hashcons FrameBuffer' $ FrameBuffer a
    accumulate !a !b !c !d !e = N $ do
        !h1 <- unN b
        !h2 <- unN c
        !h3 <- unN d
        !h4 <- unN e
        hashcons FrameBuffer' $ Accumulate a h1 h2 h3 h4
    prjFrameBuffer !a !b !c = N $ do
        !h1 <- unN c
        hashcons Image' $ PrjFrameBuffer a b h1
    prjImage !a !b !c = N $ do
        !h1 <- unN c
        hashcons Image' $ PrjImage a b h1
    -- texture constructors
    textureSlot !a !b = N $ hashcons Texture' $ TextureSlot a b
    texture !a !b !c !d = N $ do
        !h1 <- mapM unN d
        hashcons Texture' $ Texture a b c h1
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
