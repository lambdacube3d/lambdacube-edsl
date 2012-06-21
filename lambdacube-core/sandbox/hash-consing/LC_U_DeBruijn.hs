module LC_U_DeBruijn where

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

hashcons :: Ty -> Exp -> State DAG ExpId
hashcons t e = do
  DAG m tm <- get
  case lookup_key e m of
    Nothing -> let (k,m') = insert e m
                   tm'    = IM.insert k t tm
               in put (DAG m' tm') >> return k
    Just k  -> return k

exp :: DAG -> ExpId -> Exp
exp (DAG m _) k = lookup_val k m

expType :: DAG -> ExpId -> Ty
expType (DAG _ tm) k = tm IM.! k

{-
  TODO:
    represent these as tuples from specific types:  VertexOut, GeometryOut, FragmentOut, FragmentOutDepth, FragmentOutRastDepth
-}

data Exp
    -- Fun
    = Lam                   ExpId
    | Let                   ExpId ExpId
    | Var                   Int
    | Apply                 ExpId ExpId

    -- Exp
    | Const                 Value
    | PrimVar               ByteString
    | Uni                   ByteString
    | Tup                   [ExpId]
    | Prj                   Int ExpId
    | Cond                  ExpId ExpId ExpId
    | PrimApp               PrimFun ExpId
    | Sampler               Filter EdgeMode (Texture ExpId)
    -- special tuple expressions
    | VertexOut             ExpId ExpId [Interpolated ExpId]
    | GeometryOut           ExpId ExpId ExpId ExpId ExpId [Interpolated ExpId]
    | FragmentOut           [ExpId]
    | FragmentOutDepth      ExpId [ExpId]
    | FragmentOutRastDepth  [ExpId]

    -- GP
    | Fetch                 ByteString PrimitiveType [(ByteString,InputType)]
    | Transform             ExpId ExpId
    | Reassemble            (GeometryShader ExpId) ExpId
    | Rasterize             RasterContext ExpId
    | FrameBuffer           [Image]
    | Accumulate            [FragmentOperation] (FragmentFilter ExpId) ExpId ExpId ExpId
    | PrjFrameBuffer        ByteString Int ExpId
    | PrjImage              ByteString Int ExpId
    deriving (Show, Eq, Ord)

class ExpC exp where
    -- exp constructors
    lam         :: exp -> exp
    let_        :: Ty -> exp -> exp -> exp
    var         :: Ty -> Int -> exp
    apply       :: Ty -> exp -> exp -> exp
    const_      :: Ty -> Value -> exp
    primVar     :: Ty -> ByteString -> exp
    uni         :: Ty -> ByteString -> exp
    tup         :: Ty -> [exp] -> exp
    prj         :: Ty -> Int -> exp -> exp
    cond        :: Ty -> exp -> exp -> exp -> exp
    primApp     :: Ty -> PrimFun -> exp -> exp
    sampler     :: Ty -> Filter -> EdgeMode -> (Texture exp) -> exp
    -- special tuple expressions
    vertexOut               :: exp -> exp -> [Interpolated exp] -> exp
    geometryOut             :: exp -> exp -> exp -> exp -> exp -> [Interpolated exp] -> exp
    fragmentOut             :: [exp] -> exp
    fragmentOutDepth        :: exp -> [exp] -> exp
    fragmentOutRastDepth    :: [exp] -> exp
    -- gp constructors
    fetch           :: ByteString -> PrimitiveType -> [(ByteString,InputType)] -> exp
    transform       :: exp -> exp -> exp
    reassemble      :: GeometryShader exp -> exp -> exp
    rasterize       :: RasterContext -> exp -> exp
    frameBuffer     :: [Image] -> exp
    accumulate      :: [FragmentOperation] -> FragmentFilter exp -> exp -> exp -> exp -> exp
    prjFrameBuffer  :: ByteString -> Int -> exp -> exp
    prjImage        :: ByteString -> Int -> exp -> exp

newtype N = N {unN :: State DAG ExpId}

unNTexture :: ExpC exp => Texture exp -> State DAG (Texture ExpId)
unNTexture = undefined

unNInterpolated :: ExpC exp => Interpolated exp -> State DAG (Interpolated ExpId)
unNInterpolated = undefined

unNGeometryShader :: ExpC exp => GeometryShader exp -> State DAG (GeometryShader ExpId)
unNGeometryShader = undefined

unNFragmentFilter :: ExpC exp => FragmentFilter exp -> State DAG (FragmentFilter ExpId)
unNFragmentFilter = undefined

instance ExpC N where
    lam a = N $ do
        h1 <- unN a
        hashcons Unknown $ Lam h1
    let_ t a b    = N $ do
        h1 <- unN a
        h2 <- unN b
        hashcons t $ Let h1 h2
    var t a       = N $ hashcons t $ Var a
    apply t a b   = N $ do
        h1 <- unN a
        h2 <- unN b
        hashcons t $ Apply h1 h2
    const_ t a    = N $ hashcons t $ Const a
    primVar t a   = N $ hashcons t $ PrimVar a
    uni t a       = N $ hashcons t $ Uni a
    tup t a       = N $ do
        h <- mapM unN a
        hashcons t $ Tup h
    prj t a b     = N $ do
        h1 <- unN b
        hashcons t $ Prj a h1
    cond t a b c  = N $ do
        h1 <- unN a
        h2 <- unN b
        h3 <- unN c
        hashcons t $ Cond h1 h2 h3
    primApp t a b = N $ do
        h1 <- unN b
        hashcons t $ PrimApp a h1
    sampler t a b c = N $ do
        h1 <- unNTexture c
        hashcons t $ Sampler a b h1

    -- special tuple expressions
    vertexOut a b c = N $ do
        h1 <- unN a
        h2 <- unN b
        h3 <- mapM unNInterpolated c
        hashcons Unknown $ VertexOut h1 h2 h3
    geometryOut a b c d e f = N $ do
        h1 <- unN a
        h2 <- unN b
        h3 <- unN c
        h4 <- unN d
        h5 <- unN e
        h6 <- mapM unNInterpolated f
        hashcons Unknown $ GeometryOut h1 h2 h3 h4 h5 h6
    fragmentOut a   = N $ do
        h <- mapM unN a
        hashcons Unknown $ FragmentOut h
    fragmentOutDepth a b    = N $ do
        h1 <- unN a
        h2 <- mapM unN b
        hashcons Unknown $ FragmentOutDepth h1 h2
    fragmentOutRastDepth a  = N $ do
        h <- mapM unN a
        hashcons Unknown $ FragmentOutRastDepth h

    -- gp constructors
    fetch a b c = N $ do
        hashcons Unknown $ Fetch a b c
    transform a b = N $ do
        h1 <- unN a
        h2 <- unN b
        hashcons Unknown $ Transform h1 h2
    reassemble a b = N $ do
        h1 <- unNGeometryShader a
        h2 <- unN b
        hashcons Unknown $ Reassemble h1 h2
    rasterize a b = N $ do
        h1 <- unN b
        hashcons Unknown $ Rasterize a h1
    frameBuffer a = N $ do
        hashcons Unknown $ FrameBuffer a
    accumulate a b c d e = N $ do
        h1 <- unNFragmentFilter b
        h2 <- unN c
        h3 <- unN d
        h4 <- unN e
        hashcons Unknown $ Accumulate a h1 h2 h3 h4
    prjFrameBuffer a b c = N $ do
        h1 <- unN c
        hashcons Unknown $ PrjFrameBuffer a b h1
    prjImage a b c = N $ do
        h1 <- unN c
        hashcons Unknown $ PrjImage a b h1

data FragmentFilter exp
    = PassAll
    | Filter    exp
    deriving (Show, Eq, Ord)

data GeometryShader exp
    = GeometryShader    Int PrimitiveType Int exp exp exp
    deriving (Show, Eq, Ord)

data GPOutput exp
    = ImageOut  ByteString exp
    | ScreenOut exp
    deriving (Show, Eq, Ord)
