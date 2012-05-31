module LC_U_DeBruijn where

import Data.ByteString.Char8 (ByteString)
import Data.Int

import LC_G_Type

import LC_G_APIType
import LC_U_APIType
import LC_U_PrimFun

data Fun a
    = Body  a
    | Lam   (Fun a)
    deriving (Show, Eq, Ord)

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
    | Sampler   Ty Filter EdgeMode (Texture GP)
    -- special tuple expressions
    | VertexOut             Exp Exp [Interpolated Exp]
    | GeometryOut           Exp Exp Exp Exp Exp [Interpolated Exp]
    | FragmentOut           [Exp]
    | FragmentOutDepth      Exp [Exp]
    | FragmentOutRastDepth  [Exp]
    deriving (Show, Eq, Ord)

data FragmentFilter
    = PassAll
    | Filter    ExpFun
    deriving (Show, Eq, Ord)

data GeometryShader
    = NoGeometryShader 
    | GeometryShader    Int PrimitiveType Int ExpFun ExpFun ExpFun
    deriving (Show, Eq, Ord)

data GP
    = GPLet             GP GP
    | GPVar             Int
    | GPApply           GPFun GP
    | Fetch             ByteString PrimitiveType [(ByteString,InputType)]
    | Transform         ExpFun GP
    | Rasterize         RasterContext GeometryShader GP
    | FrameBuffer       [Image]
    | Accumulate        [FragmentOperation] FragmentFilter ExpFun GP GP
    | PrjFrameBuffer    ByteString Int GP
    | PrjImage          ByteString Int GP
    deriving (Show, Eq, Ord)

data GPOutput
    = ImageOut  ByteString GP
    | ScreenOut GP
    deriving (Show, Eq, Ord)
