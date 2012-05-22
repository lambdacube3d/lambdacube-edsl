module LC_U_HOAS where

import Data.ByteString.Char8
import Data.Typeable
import Data.Int

import LCType

import LC_APIType
import LC_U_APIType
import LC_U_PrimFun

data Exp
    = Tag       Int
    | Const     Value
    | PrimVar   ByteString InputType
    | Uni       ByteString InputType
    | Cond      Exp Exp Exp
    | PrimApp   PrimFun Exp
    | Tup       [Exp]
    | Prj       Int Exp
    | Sampler   Filter EdgeMode (Texture GP)

data VertexOut
    = VertexOut Exp Exp [Interpolated Exp]

data GeometryShader
    = NoGeometryShader
    | GeometryShader Int PrimitiveType Int (Exp -> Exp) (Exp -> Exp) (Exp -> GeometryOut)

data GeometryOut
    = GeometryOut Exp Exp Exp Exp Exp [Interpolated Exp]

data FragmentOut
    = FragmentOut           [Exp]
    | FragmentOutDepth      Exp [Exp]
    | FragmentOutRastDepth  [Exp]

data FragmentFilter
    = PassAll
    | Filter    (Exp -> Exp)

data GP
    = GPtag             Int
    | Fetch             ByteString PrimitiveType [(ByteString,InputType)]
    | Transform         (Exp -> VertexOut) GP
    | Rasterize         RasterContext GeometryShader GP
    | FrameBuffer       V2U [Image]
    | Accumulate        [FragmentOperation] FragmentFilter (Exp -> FragmentOut) GP GP
    | PrjFrameBuffer    ByteString Int GP
    | PrjImage          ByteString Int GP
