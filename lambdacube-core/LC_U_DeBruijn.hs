module LC_U_DeBruijn where

import Data.Typeable
import Debug.Trace
import Control.Applicative hiding (empty,Const)
import Control.Monad.State
import Data.ByteString.Char8 (ByteString)
import Data.Int

import LC_G_Type

import LC_G_APIType
import LC_U_APIType
import LC_U_PrimFun

import BiMap
import qualified Data.IntMap as IM

type ExpId = Int

--newtype DAG = DAG (BiMap Exp) deriving Show
data DAG
    = DAG 
    { dagExp    :: BiMap Exp
    , dagTy     :: IM.IntMap Ty
    } deriving Show

emptyDAG :: DAG
emptyDAG = DAG empty IM.empty

hashcons :: Ty -> Exp -> State DAG ExpId
hashcons !t !e = do
  DAG !m !tm <- get
  case lookup_key e m of
    Nothing -> let (!k,!m') = insert e m
                   !tm'    = IM.insert k t tm
               in put (DAG m' tm') >> return k
    Just !k  -> {-trace ("sharing : " ++ show k ++ " :: " ++ show (tm IM.! k)) $ -} return k

--hashcons = dontShare
dontShare :: Ty -> Exp -> State DAG ExpId
dontShare t e = do
    DAG m tm <- get
    let (k,m') = insert e m
        tm'    = IM.insert k t tm
    put (DAG m' tm') >> return k

toExp :: DAG -> ExpId -> Exp
toExp (DAG !m _) !k = lookup_val k m

toExpId :: DAG -> Exp -> ExpId
toExpId (DAG !m _) !v = let Just k = lookup_key v m in k

expIdType :: DAG -> ExpId -> Ty
expIdType (DAG _ !tm) !k = tm IM.! k

expType :: DAG -> Exp -> Ty
expType dag@(DAG !m !tm) !e = case lookup_key e m of
    Nothing -> error $ "unknown Exp node: " ++ show e
    Just !k  -> expIdType dag k
{-
  TODO:
    represent these as tuples from specific types:  VertexOut, GeometryOut, FragmentOut, FragmentOutDepth, FragmentOutRastDepth
-}

data Exp
    -- Fun
    = Lam                   !ExpId
    | Body                  !ExpId
    | Let                   !ExpId !ExpId
    | Var                   Int TypeRep   -- index, layout counter
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
    | VertexOut             !ExpId !ExpId [ExpId]
    | GeometryOut           !ExpId !ExpId !ExpId !ExpId !ExpId [ExpId]
    | FragmentOut           [ExpId]
    | FragmentOutDepth      !ExpId [ExpId]
    | FragmentOutRastDepth  [ExpId]

    -- GP
    | Fetch                 ByteString PrimitiveType [(ByteString,InputType)]
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

    | GeometryShader        Int PrimitiveType Int !ExpId !ExpId !ExpId

    -- FragmentFilter
    | PassAll
    | Filter                !ExpId

    -- GPOutput
    | ImageOut              ByteString !ExpId
    | ScreenOut             !ExpId
    deriving (Show, Eq, Ord)

class ExpC exp where
    -- exp constructors
    lam         :: exp -> exp
    body        :: exp -> exp
    let_        :: Ty -> exp -> exp -> exp
    --let_        :: Ty -> exp -> (exp -> exp) -> exp
    var         :: Ty -> Int -> TypeRep -> exp -- type, index, layout counter (this needed for proper sharing)
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
    vertexOut               :: exp -> exp -> [exp] -> exp
    geometryOut             :: exp -> exp -> exp -> exp -> exp -> [exp] -> exp
    fragmentOut             :: [exp] -> exp
    fragmentOutDepth        :: exp -> [exp] -> exp
    fragmentOutRastDepth    :: [exp] -> exp
    -- gp constructors
    fetch           :: ByteString -> PrimitiveType -> [(ByteString,InputType)] -> exp
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
    geometryShader  :: Int -> PrimitiveType -> Int -> exp -> exp -> exp -> exp
    -- FragmentFilter constructors
    passAll         :: exp
    filter_         :: exp -> exp
    -- GPOutput constructors
    imageOut        :: ByteString -> exp -> exp
    screenOut       :: exp -> exp

newtype N = N {unN :: State DAG ExpId}

instance ExpC N where
    lam !a = N $ do
        !h1 <- unN a
        hashcons Unknown $ Lam h1
    body !a = N $ do
        !h1 <- unN a
        hashcons Unknown $ Body h1
    let_ !t !a !b    = N $ do
        !h1 <- unN a
        !h2 <- unN b
        hashcons t $ Let h1 h2
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
    vertexOut !a !b !c = N $ do
        !h1 <- unN a
        !h2 <- unN b
        !h3 <- mapM unN c
        hashcons Unknown $ VertexOut h1 h2 h3
    geometryOut !a !b !c !d !e !f = N $ do
        !h1 <- unN a
        !h2 <- unN b
        !h3 <- unN c
        !h4 <- unN d
        !h5 <- unN e
        !h6 <- mapM unN f
        hashcons Unknown $ GeometryOut h1 h2 h3 h4 h5 h6
    fragmentOut !a   = N $ do
        !h <- mapM unN a
        hashcons Unknown $ FragmentOut h
    fragmentOutDepth !a !b    = N $ do
        !h1 <- unN a
        !h2 <- mapM unN b
        hashcons Unknown $ FragmentOutDepth h1 h2
    fragmentOutRastDepth !a  = N $ do
        !h <- mapM unN a
        hashcons Unknown $ FragmentOutRastDepth h
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
    textureSlot !a !b = N $ hashcons Unknown $ TextureSlot a b
    texture !a !b !c !d = N $ do
        !h1 <- mapM unN d
        hashcons Unknown $ Texture a b c h1
    -- Interpolated constructors
    flat !a = N $ do
        !h1 <- unN a
        hashcons Unknown $ Flat h1
    smooth !a = N $ do
        !h1 <- unN a
        hashcons Unknown $ Smooth h1
    noPerspective !a = N $ do
        !h1 <- unN a
        hashcons Unknown $ NoPerspective h1
    -- GeometryShader constructors
    geometryShader !a !b !c !d !e !f = N $ do
        !h1 <- unN d
        !h2 <- unN e
        !h3 <- unN f
        hashcons Unknown $ GeometryShader a b c h1 h2 h3
    -- FragmentFilter constructors
    passAll = N $ hashcons Unknown PassAll
    filter_ !a = N $ do
        !h1 <- unN a
        hashcons Unknown $ Filter h1
    -- GPOutput constructors
    imageOut !a !b = N $ do
        !h1 <- unN b
        hashcons Unknown $ ImageOut a h1
    screenOut !a = N $ do
        !h1 <- unN a
        hashcons Unknown $ ScreenOut h1
