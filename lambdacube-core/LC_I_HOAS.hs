module LC_I_HOAS where

import Data.ByteString.Char8
import Data.Typeable
import Data.Int

import TypeLevel.Number.Nat
import TypeLevel.Number.Nat.Num

import LCType

import LC_APIType
import LC_T_APIType
import LC_T_DSLType
import LC_T_PrimFun

import LC_T_HOAS
import qualified LC_U_HOAS as U -- HOAS Untyped
import qualified LC_U_PrimFun as U -- HOAS Untyped

newtype ExpI stage t = ExpI (U.Exp)

instance Exp ExpI where
    type Exp_PrimFun ExpI = PrimFunI
    tag     = ExpI . U.Tag
    cnst    = ExpI . U.Const . toValue
    primVar = ExpI . uncurry U.PrimVar . toInput
    uni     = ExpI . uncurry U.Uni . toInput
    cond (ExpI c) (ExpI t) (ExpI e) = ExpI (U.Cond c t e)
    primApp (PrimFunI f) (ExpI a) = ExpI $ U.PrimApp f a

{-
    primApp :: (GPU a, GPU r, PrimFun reprPF)
            => reprPF stage (a -> r)
            -> repr stage a
            -> repr stage r
-}
val :: ExpI V Float
val = primApp primSin (cnst (0::Float))

val2 :: U.Exp
val2 = let ExpI v = val in v
{-
iseqI :: ExpI a b -> ExpI c d -> Bool
iseqI (ExpI a) (ExpI b) = a == b

iseq :: Bool
iseq = val2 == val2
-}