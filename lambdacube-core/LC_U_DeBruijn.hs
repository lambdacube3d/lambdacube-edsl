module LC_U_DeBruijn where

import Data.Generics.Uniplate.Data

import Data.ByteString.Char8 (ByteString)
import Data.Data
import Data.Int

import LCType

import LC_APIType
import LC_U_APIType
import LC_U_PrimFun

data Fun a
    = Body  a
    | Lam   (Fun a)
    deriving (Show, Eq, Ord, Data,Typeable)

type ExpFun = Fun Exp
type GPFun = Fun GP

data Exp
    = Let       Exp Exp
    | Var       Int
    | Apply     ExpFun Exp
    | Const     Value
    | PrimVar   ByteString InputType
    | Uni       ByteString InputType
    | Tup       [Exp]
    | Prj       Int Exp
    | Cond      Exp Exp Exp
    | PrimApp   PrimFun Exp
    -- special expressions
    | VertexOut             Exp Exp [Interpolated Exp]
    | GeometryOut           Exp Exp Exp Exp Exp [Interpolated Exp]
    | FragmentOut           [Exp]
    | FragmentOutDepth      Exp [Exp]
    | FragmentOutRastDepth  [Exp]
    | Sampler               Filter EdgeMode (Texture GP)
    deriving (Show, Eq, Ord, Data,Typeable)

data FragmentFilter
    = PassAll
    | Filter    ExpFun
    deriving (Show, Eq, Ord, Data,Typeable)

data GeometryShader
    = NoGeometryShader 
    | GeometryShader    Int PrimitiveType Int ExpFun ExpFun ExpFun
    deriving (Show, Eq, Ord, Data,Typeable)

data GP
    = GPLet             GP GP
    | GPVar             Int
    | GPApply           GPFun GP
    | Fetch             ByteString PrimitiveType [(ByteString,InputType)]
    | Transform         ExpFun GP
    | Rasterize         RasterContext GeometryShader GP
    | FrameBuffer       V2U [Image]
    | Accumulate        [FragmentOperation] FragmentFilter ExpFun GP GP
    | PrjFrameBuffer    ByteString Int GP
    | PrjImage          ByteString Int GP
    deriving (Show, Eq, Ord, Data,Typeable)

unis :: GP -> [(ByteString,InputType)]
unis x = [(n,t) | (Uni n t) <- universeBi x]

passes :: GP -> [Texture GP]
passes x = [t | t@(Texture _ _ _) <- universeBi x]

countSin :: GP -> Int
countSin x = length [() | PrimSin <- universeBi x]
