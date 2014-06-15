{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module LC_U_DeBruijn2 where

import Data.ByteString.Char8 (ByteString)
import Data.Traversable
import Data.Foldable

import LC_G_Type
import LC_G_APIType
import LC_U_APIType
import LC_U_PrimFun

{-
  TODO:
    represent these as tuples from specific types:  VertexOut, GeometryOut, FragmentOut, FragmentOutDepth, FragmentOutRastDepth
-}
data Exp e
    -- Fun
    = Lam                   e e
--    | Body                  e
    | Var
--    | Apply                 e e
    -- list
    | Nil
    | Cons                  e e

    -- Exp
    | Const                 !Value
    | PrimVar               !ByteString
    | Uni                   !ByteString
    | Tup                   e
    | Prj                   Int e
    | Cond                  e e e
    | PrimApp               !PrimFun e
    | Sampler               !Filter !EdgeMode e
    | Loop                  e e e e
    -- special tuple expressions
    | VertexOut             e e e e
    | GeometryOut           e e e e e
    | FragmentOut           e
    | FragmentOutDepth      e e
    | FragmentOutRastDepth  e

    -- GP
    | Fetch                 ByteString FetchPrimitive [(ByteString,InputType)]
    | Transform             e e
    | Reassemble            e e
    | Rasterize             RasterContext e
    | FrameBuffer           [Image]
    | Accumulate            AccumulationContext e e e e
    | PrjFrameBuffer        ByteString Int e
    | PrjImage              ByteString Int e

    -- Texture
    | TextureSlot           ByteString TextureType
    | Texture               TextureType Value MipMap e -- hint: type, size, mip, data

    -- Interpolated
    | Flat                  e
    | Smooth                e
    | NoPerspective         e

    | GeometryShader        Int OutputPrimitive Int e e e

    -- FragmentFilter
    | PassAll
    | Filter                e

    -- GPOutput
    | ImageOut              ByteString V2U e
    | ScreenOut             e
    | MultiOut              e
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
