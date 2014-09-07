module LC_U_DeBruijn where

import Data.ByteString.Char8 (ByteString)

import Data.Generics.Fixplate

import LC_U_APIType
import LC_U_PrimFun

{-
  TODO:
    represent these as tuples from specific types:  Vertex, Fragment, FragmentDepth, FragmentRastDepth
-}

data Exp e
    -- Fun
    = Lam                   !e
    | Body                  !e
    | Let                   !e !e
    | Var                   !Int    -- De Bruijn index
    | Apply                 !e !e

    -- Exp
    | Const                 !ExpValue
    | Input                 !ByteString
    | Use                   !e
    | Cond                  !e !e !e
    | PrimApp               !PrimFun !e
    | Tup                   [e]
    | Prj                   Int !e
    | Loop                  !e !e !e !e

    -- Array operations
    | ArrayFromList         [e]
    | ArrayReplicate        !e !e
    | ArrayGenerate         !e !e
    | ArrayIterateN         !e !e !e
    | ArrayIndex            !e !e
    | ArrayFilter           !e !e
    | ArrayMap              !e !e
    | ArrayZipWith          !e !e !e
    | ArrayAccumulate       !e !e !e

    -- GPU pipeline model
    | Fetch                 !FetchPrimitive !e !(Maybe e)
    | Transform             !e !e
    | Reassemble            !e !e
    | Rasterize             RasterContext !e !(Maybe e) !e
    | FrameBuffer           [Image]
    | Accumulate            AccumulationContext !(Maybe e) !(Maybe e) !e !e !e !e

    -- Transform feedback support
    | ArrayFromStream       !e

    -- FrameBuffer and Image helpers
    | PrjFrameBuffer        Int !e
    | PrjImage              Int !e

    -- Special tuple expressions
    | Vertex                !e !e [e] [e]
    | Fragment              [e]
    | FragmentDepth         !e [e]
    | FragmentRastDepth     [e]

    -- Interpolated
    | Flat                  !e
    | Smooth                !e
    | NoPerspective         !e

    | GeometryShader        Int OutputPrimitive Int !e !e !e

    -- Output
    | Output                ByteString !e
    | ScreenOutput          !e
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance ShowF Exp where showsPrecF = showsPrec
instance EqF   Exp where equalF     = (==)
instance OrdF  Exp where compareF   = compare
