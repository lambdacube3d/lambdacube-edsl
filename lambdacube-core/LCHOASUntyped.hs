module LCHOASUntyped where

import Data.ByteString.Char8
import Data.Typeable
import Data.Int

import TypeLevel.Number.Nat
import TypeLevel.Number.Nat.Num

import LCType
import LCAPITypeTypeclass
import LCDSLTypeTypeclass
import LCPrimFunUntyped

data Exp
    = Tag       Int
    | Const     Value
    | PrimVar   ByteString InputType
    | Uni       ByteString InputType
    | Cond      Exp Exp Exp
    | PrimApp   PrimFun Exp
    deriving (Show, Ord, Eq)
--    | Tup

{-
-- Common Exp, describes shader functions
data Exp stage t where
    -- Needed for conversion to de Bruijn form
    Tag     :: GPU t
            => Int
            -> Exp stage t
                 -- environment size at defining occurrence

    -- constant value
    Const   :: GPU t
            => t
            -> Exp stage t

    -- builtin variable
    PrimVar :: GPU t
            => Input t
            -> Exp stage t

    -- uniform value
    Uni     :: (InputTuple t, GPU (InputTupleRepr t))
            => t
            -> Exp stage (InputTupleRepr t)

    -- conditional expression
    Cond    :: GPU t
            => Exp stage Bool
            -> Exp stage t
            -> Exp stage t
            -> Exp stage t

    PrimApp :: (GPU a, GPU r)
            => PrimFun stage (a -> r)
            -> Exp stage a
            -> Exp stage r

    -- tuple support
    Tup     :: (GPU t, IsTuple t)
            => Tuple (Exp stage) (TupleRepr t)
            -> Exp stage t

    Prj     :: (GPU e, GPU t, IsTuple t)
            => TupleIdx (TupleRepr t) e
            -> Exp stage t
            -> Exp stage e

    -- sampler support
    Sampler :: GPU (Sampler dim arr t ar)
            => Filter
            -> EdgeMode
            -> Texture GP dim arr t ar
            -> Exp stage (Sampler dim arr t ar)
-}
