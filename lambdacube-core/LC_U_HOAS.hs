module LC_U_HOAS where

import Data.ByteString.Char8

import LCType

import LC_APIType
import LC_U_APIType
import LC_U_PrimFun

data Exp
    = Tag       Int
    | Lam       (Exp -> Exp)
    | Const     Value
    | PrimVar   ByteString InputType
    | Uni       ByteString InputType
    | Cond      Exp Exp Exp
    | PrimApp   PrimFun Exp
    | Tup       [Exp]
    | Prj       Int Exp
    -- special expressions
    | VertexOut             Exp Exp [Interpolated Exp]
    | GeometryOut           Exp Exp Exp Exp Exp [Interpolated Exp]
    | FragmentOut           [Exp]
    | FragmentOutDepth      Exp [Exp]
    | FragmentOutRastDepth  [Exp]
    | Sampler               Filter EdgeMode (Texture GP)

data GeometryShader
    = NoGeometryShader
    | GeometryShader    Int PrimitiveType Int Exp Exp Exp

data FragmentFilter
    = PassAll
    | Filter    Exp

data GP
    = GPTag             Int
    | Fetch             ByteString PrimitiveType [(ByteString,InputType)]
    | Transform         Exp GP
    | Rasterize         RasterContext GeometryShader GP
    | FrameBuffer       V2U [Image]
    | Accumulate        [FragmentOperation] FragmentFilter Exp GP GP
    | PrjFrameBuffer    ByteString Int GP
    | PrjImage          ByteString Int GP
