module LC_U_DeBruijn where

import Data.ByteString.Char8
import Data.Set (Set)
import Data.Typeable
import qualified Data.Set as Set
import Data.Int

import LCType

import LC_APIType
import LC_U_APIType
import LC_U_PrimFun

data Fun
    = Body  Exp
    | Lam   Fun
    deriving (Show, Eq, Ord)

data Exp
    = Var       Int
    | Const     Value
    | PrimVar   ByteString InputType
    | Uni       ByteString InputType
    | Tup       [Exp]
    | Prj       Int Exp
    | Cond      Exp Exp Exp
    | PrimApp   PrimFun Exp
    | VertexOut             Exp Exp [Interpolated Exp]
    | GeometryOut           Exp Exp Exp Exp Exp [Interpolated Exp]
    | FragmentOut           [Exp]
    | FragmentOutDepth      Exp [Exp]
    | FragmentOutRastDepth  [Exp]
    | Sampler               Filter EdgeMode (Texture GP)
    deriving (Show, Eq, Ord)

data FragmentFilter
    = PassAll
    | Filter    Fun
    deriving (Show, Eq, Ord)

data GeometryShader
    = NoGeometryShader 
    | GeometryShader    Int PrimitiveType Int Fun Fun Fun
    deriving (Show, Eq, Ord)

data GPfun
    = GPBody    GP
    | GPLam     GPfun
    deriving (Show, Eq, Ord)

data GP
    = GPLet             GP GP
    | GPVar             Int
    | Apply             GPfun GP
    | Fetch             ByteString PrimitiveType [(ByteString,InputType)]
    | Transform         Fun GP
    | Rasterize         RasterContext GeometryShader GP
    | FrameBuffer       V2U [Image]
    | Accumulate        [FragmentOperation] FragmentFilter Fun GP GP
    | PrjFrameBuffer    ByteString Int GP
    | PrjImage          ByteString Int GP
    deriving (Show, Eq, Ord)
