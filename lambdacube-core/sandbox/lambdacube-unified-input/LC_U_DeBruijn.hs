module LC_U_DeBruijn where

import Data.ByteString.Char8 (ByteString)
import qualified Data.IntMap as IM

import BiMap

import LC_U_APIType
import LC_U_PrimFun

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
    | Var                   !Int !ExpType   -- index, layout counter
    | Apply                 !ExpId !ExpId

    -- Exp
    | Const                 !ExpValue
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

    -- Special tuple expressions
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
