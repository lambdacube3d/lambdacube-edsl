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
    = Let       Ty Exp Exp
    | Var       Ty Int
    | Apply     Ty ExpFun Exp
    | Const     Ty Value
    | PrimVar   Ty ByteString
    | Uni       Ty ByteString
    | Tup       Ty [Exp]
    | Prj       Ty Int Exp
    | Cond      Ty Exp Exp Exp
    | PrimApp   Ty PrimFun Exp
    | Sampler               Ty Filter EdgeMode (Texture GP)
    -- special tuple expressions
    | VertexOut             Ty Exp Exp [Interpolated Exp]
    | GeometryOut           Ty Exp Exp Exp Exp Exp [Interpolated Exp]
    | FragmentOut           Ty [Exp]
    | FragmentOutDepth      Ty Exp [Exp]
    | FragmentOutRastDepth  Ty [Exp]
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

unis :: GP -> [(ByteString,Ty)]
unis x = [(n,t) | (Uni t n) <- universeBi x]

passes :: GP -> [Texture GP]
passes x = [t | t@(Texture _ _ _) <- universeBi x]

countSin :: GP -> Int
countSin x = length [() | PrimSin <- universeBi x]
