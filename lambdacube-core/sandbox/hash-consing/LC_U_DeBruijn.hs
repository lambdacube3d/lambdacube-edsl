module LC_U_DeBruijn where

import Control.Monad.State
import Data.ByteString.Char8 (ByteString)
import Data.Int

import LC_G_Type

import LC_G_APIType
import LC_U_APIType
import LC_U_PrimFun

import BiMap

type ExpId = Int

newtype DAG = DAG (BiMap Exp) deriving Show

hashcons :: Exp -> State DAG ExpId
hashcons e = do
  DAG m <- get
  case lookup_key e m of
    Nothing -> let (k,m') = insert e m
               in put (DAG m') >> return k
    Just k  -> return k

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
{-
  these will be expressed by tuples
    -- special tuple expressions
    | VertexOut             ExpId ExpId [Interpolated ExpId]
    | GeometryOut           ExpId ExpId ExpId ExpId ExpId [Interpolated ExpId]
    | FragmentOut           [ExpId]
    | FragmentOutDepth      ExpId [ExpId]
    | FragmentOutRastDepth  [ExpId]
-}
    -- GP
    | Fetch                 ByteString PrimitiveType [(ByteString,InputType)]
    | Transform             ExpId ExpId
    | Reassemble            GeometryShader ExpId
    | Rasterize             RasterContext ExpId
    | FrameBuffer           [Image]
    | Accumulate            [FragmentOperation] FragmentFilter ExpId ExpId ExpId
    | PrjFrameBuffer        ByteString Int ExpId
    | PrjImage              ByteString Int ExpId
    deriving (Show, Eq, Ord)

class ExpC exp where
    let_        :: exp -> exp -> exp
    var         :: Int -> exp
    apply       :: exp -> exp -> exp
    const_      :: Value -> exp
    uni         :: ByteString -> exp
    tup         :: [exp] -> exp
    prj         :: Int -> exp -> exp
    cond        :: exp -> exp -> exp -> exp
    primApp     :: PrimFun -> exp -> exp
    sampler     :: Filter -> EdgeMode -> (Texture ExpId) -> exp
{-
    -- special tuple expressions
    vertexOut               :: exp -> exp -> [Interpolated exp] -> exp
    geometryOut             :: exp -> exp -> exp -> exp -> exp -> [Interpolated exp] -> exp
    fragmentOut             :: [exp] -> exp
    fragmentOutDepth        :: exp -> [exp] -> exp
    fragmentOutRastDepth    :: [exp] -> exp
-}

newtype N = N{unN :: State DAG ExpId}

instance ExpC N where
    let_ a b    = N(do
        h1 <- unN a
        h2 <- unN b
        hashcons $ Let h1 h2)
    var a       = N(hashcons $ Var a)
    apply a b   = N(do
        h1 <- unN a
        h2 <- unN b
        hashcons $ Apply h1 h2)
    const_ a    = N(hashcons $ Const a)
    uni a       = N(hashcons $ Uni a)
    tup a       = N(do
        h <- mapM unN a
        hashcons $ Tup h)
    prj a b     = N(do
        h1 <- unN b
        hashcons $ Prj a h1)
    cond a b c  = N(do
        h1 <- unN a
        h2 <- unN b
        h3 <- unN c
        hashcons $ Cond h1 h2 h3)
    primApp a b = N(do
        h1 <- unN b
        hashcons $ PrimApp a h1)
    {-
    sampler     :: Ty -> Filter -> EdgeMode -> (Texture GP) -> exp
    vertexOut               :: exp -> exp -> [Interpolated exp] -> exp
    geometryOut             :: exp -> exp -> exp -> exp -> exp -> [Interpolated exp] -> exp
    -}
{-
    -- sapecial tuple expressions
    fragmentOut a   = N(do
        h <- mapM unN a
        hashcons $ FragmentOut h)
    fragmentOutDepth a b    = N(do
        h1 <- unN a
        h2 <- mapM unN b
        hashcons $ FragmentOutDepth h1 h2)
    fragmentOutRastDepth a  = N(do
        h <- mapM unN a
        hashcons $ FragmentOutRastDepth h)
-}
data FragmentFilter
    = PassAll
    | Filter    ExpId
    deriving (Show, Eq, Ord)

data GeometryShader
    = GeometryShader    Int PrimitiveType Int ExpId ExpId ExpId
    deriving (Show, Eq, Ord)

data GPOutput
    = ImageOut  ByteString ExpId
    | ScreenOut ExpId
    deriving (Show, Eq, Ord)
