module LC_U_DeBruijn where

import Control.Applicative hiding (empty,Const)
import Data.Typeable
import Debug.Trace
import Control.Monad.State
import Data.ByteString.Char8 (ByteString)

import LC_G_APIType
import LC_U_APIType
import LC_U_PrimFun

import BiMap
import qualified Data.IntMap as IM

type ExpId = Int
data Ty = Ty
    deriving (Show,Eq,Ord)-- TODO

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
    | Const                 !InputValue
    | Input                 !ByteString
    | Use                   !ExpId
    | Cond                  !ExpId !ExpId !ExpId
    | PrimApp               !PrimFun !ExpId
    | Tup                   [ExpId]
    | Prj                   Int !ExpId
    | Loop                  !ExpId !ExpId !ExpId !ExpId

    -- Array operations
    | ArrayFromList         [ExpId]
    | ArrayReplicate        !ExpId !ExpId
    | ArrayGenerate         !ExpId !ExpId
    | ArrayIterateN         !ExpId !ExpId !ExpId
    | ArrayIndex            !ExpId !ExpId
    | ArrayFilter           !ExpId !ExpId
    | ArrayMap              !ExpId !ExpId
    | ArrayZipWith          !ExpId !ExpId !ExpId
    | ArrayAccumulate       !ExpId !ExpId !ExpId

    -- GPU pipeline model
    | Fetch                 !FetchPrimitive !ExpId !(Maybe ExpId)
    | Transform             !ExpId !ExpId
    | Reassemble            !ExpId !ExpId
    | Rasterize             RasterContext !ExpId
    | FrameBuffer           [Image]
    | Accumulate            AccumulationContext !ExpId !ExpId !ExpId !ExpId

    -- Transform feedback support
    | ArrayFromStream       !ExpId

    -- FrameBuffer and Image helpers
    | PrjFrameBuffer        Int !ExpId
    | PrjImage              Int !ExpId

    -- special tuple expressions
    | VertexOut             !ExpId !ExpId [ExpId]
    | GeometryOut           !ExpId !ExpId !ExpId !ExpId !ExpId [ExpId]
    | FragmentOut           [ExpId]
    | FragmentOutDepth      !ExpId [ExpId]
    | FragmentOutRastDepth  [ExpId]

    -- Interpolated
    | Flat                  !ExpId
    | Smooth                !ExpId
    | NoPerspective         !ExpId

    | GeometryShader        Int OutputPrimitive Int !ExpId !ExpId !ExpId

    -- FragmentFilter
    | PassAll
    | Filter                !ExpId

    -- Output
    | ImageOut              ByteString !ExpId
    | ScreenOut             !ExpId
    deriving (Eq, Ord, Show)

class ExpC exp where
    -- Fun
    lam             :: Ty -> exp -> exp
    body            :: Ty -> exp -> exp
    let_            :: Ty -> exp -> exp -> exp
    --let_            :: Ty -> exp -> (exp -> exp) -> exp
    var             :: Ty -> Int -> TypeRep -> exp -- type, index, layout counter (this needed for proper sharing)
    apply           :: Ty -> exp -> exp -> exp

    -- Exp
    const_          :: Ty -> InputValue -> exp
    input           :: Ty -> ByteString -> exp
    use             :: Ty -> exp -> exp
    cond            :: Ty -> exp -> exp -> exp -> exp
    primApp         :: Ty -> PrimFun -> exp -> exp
    tup             :: Ty -> [exp] -> exp
    prj             :: Ty -> Int -> exp -> exp
    loop            :: Ty -> exp -> exp -> exp -> exp -> exp

    -- Array operations
    arrayFromList   :: Ty -> [exp] -> exp
    arrayReplicate  :: Ty -> exp -> exp -> exp
    arrayGenerate   :: Ty -> exp -> exp -> exp
    arrayIterateN   :: Ty -> exp -> exp -> exp -> exp
    arrayIndex      :: Ty -> exp -> exp -> exp
    arrayFilter     :: Ty -> exp -> exp -> exp
    arrayMap        :: Ty -> exp -> exp -> exp
    arrayZipWith    :: Ty -> exp -> exp -> exp -> exp
    arrayAccumulate :: Ty -> exp -> exp -> exp -> exp

    -- GPU pipeline model
    fetch           :: Ty -> FetchPrimitive -> exp -> Maybe exp -> exp
    transform       :: Ty -> exp -> exp -> exp
    reassemble      :: Ty -> exp -> exp -> exp
    rasterize       :: Ty -> RasterContext -> exp -> exp
    frameBuffer     :: Ty -> [Image] -> exp
    accumulate      :: Ty -> AccumulationContext -> exp -> exp -> exp -> exp -> exp

    -- Transform feedback support
    arrayFromStream :: Ty -> exp -> exp

    -- FrameBuffer and Image helpers
    prjFrameBuffer  :: Ty -> Int -> exp -> exp
    prjImage        :: Ty -> Int -> exp -> exp

    -- Special tuple expressions
    vertexOut               :: Ty -> exp -> exp -> [exp] -> exp
    geometryOut             :: Ty -> exp -> exp -> exp -> exp -> exp -> [exp] -> exp
    fragmentOut             :: Ty -> [exp] -> exp
    fragmentOutDepth        :: Ty -> exp -> [exp] -> exp
    fragmentOutRastDepth    :: Ty -> [exp] -> exp

    -- Interpolated constructors
    flat            :: Ty -> exp -> exp
    smooth          :: Ty -> exp -> exp
    noPerspective   :: Ty -> exp -> exp

    -- GeometryShader constructors
    geometryShader  :: Ty -> Int -> OutputPrimitive -> Int -> exp -> exp -> exp -> exp

    -- FragmentFilter constructors
    passAll         :: Ty -> exp
    filter_         :: Ty -> exp -> exp

    -- Output constructors
    imageOut        :: Ty -> ByteString -> exp -> exp
    screenOut       :: Ty -> exp -> exp

newtype N = N {unN :: State DAG ExpId}

--mkN :: Ty -> StateT DAG Data.Functor.Identity.Identity Exp -> N
mkN t m = N (hashcons t =<< m)

instance ExpC N where
    -- Fun
    lam                     !t !a               = mkN t (Lam    <$> unN a)
    body                    !t !a               = mkN t (Body   <$> unN a)
    let_                    !t !a !b            = mkN t (Let    <$> unN a <*> unN b)
    var                     !t !a !b            = mkN t $ return $ Var a b
    apply                   !t !a !b            = mkN t (Apply  <$> unN a <*> unN b)

    -- Exp
    const_                  !t !a               = mkN t $ return $ Const a
    input                   !t !a               = mkN t $ return $ Input a
    use                     !t !a               = mkN t (Use        <$> unN a)
    cond                    !t !a !b !c         = mkN t (Cond       <$> unN a <*> unN b <*> unN c)
    primApp                 !t !a !b            = mkN t (PrimApp a  <$> unN b)
    tup                     !t !a               = mkN t (Tup        <$> mapM unN a)
    prj                     !t !a !b            = mkN t (Prj a      <$> unN b)
    loop                    !t !a !b !c !d      = mkN t (Loop       <$> unN a <*> unN b <*> unN c <*> unN d)

    -- Array operations
    arrayFromList           !t !a               = mkN t (ArrayFromList      <$> mapM unN a)
    arrayReplicate          !t !a !b            = mkN t (ArrayReplicate     <$> unN a <*> unN b)
    arrayGenerate           !t !a !b            = mkN t (ArrayGenerate      <$> unN a <*> unN b)
    arrayIterateN           !t !a !b !c         = mkN t (ArrayIterateN      <$> unN a <*> unN b <*> unN c)
    arrayIndex              !t !a !b            = mkN t (ArrayIndex         <$> unN a <*> unN b)
    arrayFilter             !t !a !b            = mkN t (ArrayFilter        <$> unN a <*> unN b)
    arrayMap                !t !a !b            = mkN t (ArrayMap           <$> unN a <*> unN b)
    arrayZipWith            !t !a !b !c         = mkN t (ArrayZipWith       <$> unN a <*> unN b <*> unN c)
    arrayAccumulate         !t !a !b !c         = mkN t (ArrayAccumulate    <$> unN a <*> unN b <*> unN c)

    -- GPU pipeline model
    fetch                   !t !a !b !c         = mkN t (Fetch a        <$> unN b <*> maybe (return Nothing) (\v -> return . Just =<< unN v) c)
    transform               !t !a !b            = mkN t (Transform      <$> unN a <*> unN b)
    reassemble              !t !a !b            = mkN t (Reassemble     <$> unN a <*> unN b)
    rasterize               !t !a !b            = mkN t (Rasterize a    <$> unN b)
    frameBuffer             !t !a               = mkN t (return $ FrameBuffer a)
    accumulate              !t !a !b !c !d !e   = mkN t (Accumulate a   <$> unN b <*> unN c <*> unN d <*> unN e)

    -- Transform feedback support
    arrayFromStream         !t !a               = mkN t (ArrayFromStream    <$> unN a)

    -- FrameBuffer and Image helpers
    prjFrameBuffer          !t !a !b            = mkN t (PrjFrameBuffer a   <$> unN b)
    prjImage                !t !a !b            = mkN t (PrjImage a         <$> unN b)

    -- Special tuple expressions
    vertexOut               !t !a !b !c         = mkN t (VertexOut              <$> unN a <*> unN b <*> mapM unN c)
    geometryOut             !t !a !b !c !d !e !f= mkN t (GeometryOut            <$> unN a <*> unN b <*> unN c <*> unN d <*> unN e <*> mapM unN f)
    fragmentOut             !t !a               = mkN t (FragmentOut            <$> mapM unN a)
    fragmentOutDepth        !t !a !b            = mkN t (FragmentOutDepth       <$> unN a <*> mapM unN b)
    fragmentOutRastDepth    !t !a               = mkN t (FragmentOutRastDepth   <$> mapM unN a)

    -- Interpolated constructors
    flat                    !t !a               = mkN t (Flat           <$> unN a)
    smooth                  !t !a               = mkN t (Smooth         <$> unN a)
    noPerspective           !t !a               = mkN t (NoPerspective  <$> unN a)

    -- GeometryShader constructors
    geometryShader          !t !a !b !c !d !e !f= mkN t (GeometryShader a b c <$> unN d <*> unN e <*> unN f)

    -- FragmentFilter constructors
    passAll                 !t                  = N $ hashcons t PassAll
    filter_                 !t !a               = mkN t (Filter     <$> unN a)

    -- Output constructors
    imageOut                !t !a !b            = mkN t (ImageOut a <$> unN b)
    screenOut               !t !a               = mkN t (ScreenOut  <$> unN a)
