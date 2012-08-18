{-# LANGUAGE UndecidableInstances, OverlappingInstances #-}
module LC_T_Language where

import Data.ByteString.Char8
import Data.Int
import Data.Typeable
import Data.Word

import LC_G_Type
import LC_T_APIType
import LC_T_DSLType
import LC_T_PrimFun
import LC_T_HOAS
{-
    all operatiors with @<>?,./\';""|:}{+_)0-=§±!@#$%^&*()_+
    <+> <+.> <.+>   -- good, conflict: <*>
    [+] [+.] [.+]   -- good, conflict with lists
    |+| |+.| |.+|   -- medium
    @+ @+. @.+      -- bad
    :+: :+.: :.+:   -- bad
    [*]
-}
{-
infixl 7  .*., ./., .%.
infixl 6  .+., .-.
infix  4  .==., ./=., .<., .<=., .>=., .>.

infixr 3  .&&.
infixr 2  .||.

infixl 8 .>>., .<<.
infixl 7 .&.
infixl 6 .^.
infixl 5 .|.
-}

infixl 7  @*, @/, @%
infixl 6  @+, @-
infix  4  @==, @/=, @<, @<=, @>=, @>

infixr 3  @&&
infixr 2  @||

infixl 8 @>>, @<<
infixl 7 @&
infixl 6 @^
infixl 5 @|

infix 7 @.      -- dot
infix 7 @#      -- cross
infixr 7 @*.    -- mulmv
infixl 7 @.*    -- mulvm
infixl 7 @.*.   -- mulmm

-- TODO: we should use template haskell or a preprocessor to generate the instances
class OperatorArithmetic a b where
    (@+) :: a -> b -> a
    (@-) :: a -> b -> a
    (@*) :: a -> b -> a

instance (GPU (V2 t), IsNumComponent t, IsMatVec (V2 t) c, IsNum c) => OperatorArithmetic (Exp stage (V2 t)) (Exp stage (V2 t)) where
    a @+ b = PrimApp PrimAdd $! tup2 (a,b)
    a @- b = PrimApp PrimSub $! tup2 (a,b)
    a @* b = PrimApp PrimMul $! tup2 (a,b)

instance (GPU (V3 t), IsNumComponent t, IsMatVec (V3 t) c, IsNum c) => OperatorArithmetic (Exp stage (V3 t)) (Exp stage (V3 t)) where
    a @+ b = PrimApp PrimAdd $! tup2 (a,b)
    a @- b = PrimApp PrimSub $! tup2 (a,b)
    a @* b = PrimApp PrimMul $! tup2 (a,b)

instance (GPU (V4 t), IsNumComponent t, IsMatVec (V4 t) c, IsNum c) => OperatorArithmetic (Exp stage (V4 t)) (Exp stage (V4 t)) where
    a @+ b = PrimApp PrimAdd $! tup2 (a,b)
    a @- b = PrimApp PrimSub $! tup2 (a,b)
    a @* b = PrimApp PrimMul $! tup2 (a,b)

instance (GPU c, GPU (V2 t), IsNumComponent t, IsMatVecScalar (V2 t) c, IsNum c) => OperatorArithmetic (Exp stage (V2 t)) (Exp stage c) where
    a @+ b = PrimApp PrimAddS $! tup2 (a,b)
    a @- b = PrimApp PrimSubS $! tup2 (a,b)
    a @* b = PrimApp PrimMulS $! tup2 (a,b)

instance (GPU c, GPU (V3 t), IsNumComponent t, IsMatVecScalar (V3 t) c, IsNum c) => OperatorArithmetic (Exp stage (V3 t)) (Exp stage c) where
    a @+ b = PrimApp PrimAddS $! tup2 (a,b)
    a @- b = PrimApp PrimSubS $! tup2 (a,b)
    a @* b = PrimApp PrimMulS $! tup2 (a,b)

instance (GPU c, GPU (V4 t), IsNumComponent t, IsMatVecScalar (V4 t) c, IsNum c) => OperatorArithmetic (Exp stage (V4 t)) (Exp stage c) where
    a @+ b = PrimApp PrimAddS $! tup2 (a,b)
    a @- b = PrimApp PrimSubS $! tup2 (a,b)
    a @* b = PrimApp PrimMulS $! tup2 (a,b)

instance (GPU a, GPU t, IsNum t, IsMatVecScalar a t) => OperatorArithmetic (Exp stage a) (Exp stage t) where
    a @+ b = PrimApp PrimAddS $! tup2 (a,b)
    a @- b = PrimApp PrimSubS $! tup2 (a,b)
    a @* b = PrimApp PrimMulS $! tup2 (a,b)

{-
instance (GPU a, IsNum t, IsMatVec a t) => OperatorArithmetic (Exp stage a) (Exp stage a) (Exp stage a) where
    a @+ b = PrimApp PrimAdd $! tup2 (a,b)
    a @- b = PrimApp PrimSub $! tup2 (a,b)
    a @* b = PrimApp PrimMul $! tup2 (a,b)

instance (GPU a, GPU t, IsNum t, IsMatVecScalar a t) => OperatorArithmetic (Exp stage a) (Exp stage t) (Exp stage a) where
    a @+ b = PrimApp PrimAddS $! tup2 (a,b)
    a @- b = PrimApp PrimSubS $! tup2 (a,b)
    a @* b = PrimApp PrimMulS $! tup2 (a,b)
-}
{-
instance OperatorArithmetic (Exp stage V4F) (Exp stage V4F) (Exp stage V4F) where
    a @+ b = PrimApp PrimAdd $! tup2 (a,b)
    a @- b = PrimApp PrimSub $! tup2 (a,b)
    a @* b = PrimApp PrimMul $! tup2 (a,b)

instance OperatorArithmetic (Exp stage V4F) (Exp stage Float) (Exp stage V4F) where
    a @+ b = PrimApp PrimAddS $! tup2 (a,b)
    a @- b = PrimApp PrimSubS $! tup2 (a,b)
    a @* b = PrimApp PrimMulS $! tup2 (a,b)
-}
class OperatorDivide a b where
    (@/) :: a -> b -> a
    (@%) :: a -> b -> a

instance (GPU a, IsNum t, IsVecScalar d a t) => OperatorDivide (Exp stage a) (Exp stage a) where
    a @/ b = PrimApp PrimDiv $! tup2 (a,b)
    a @% b = PrimApp PrimMod $! tup2 (a,b)

instance (GPU a, GPU t, IsNum t, IsVecScalar d a t) => OperatorDivide (Exp stage a) (Exp stage t) where
    a @/ b = PrimApp PrimDivS $! tup2 (a,b)
    a @% b = PrimApp PrimModS $! tup2 (a,b)

class OperatorBit a b where
    (@&) :: a -> b -> a
    (@|) :: a -> b -> a
    (@^) :: a -> b -> a

instance (GPU a, IsIntegral t, IsVecScalar d a t) => OperatorBit (Exp stage a) (Exp stage a) where
    a @& b = PrimApp PrimBAnd $! tup2 (a,b)
    a @| b = PrimApp PrimBOr  $! tup2 (a,b)
    a @^ b = PrimApp PrimBXor $! tup2 (a,b)

instance (GPU a, GPU t, IsIntegral t, IsVecScalar d a t) => OperatorBit (Exp stage a) (Exp stage t) where
    a @& b = PrimApp PrimBAndS $! tup2 (a,b)
    a @| b = PrimApp PrimBOrS  $! tup2 (a,b)
    a @^ b = PrimApp PrimBXorS $! tup2 (a,b)

class OperatorShift a b where
    (@>>) :: a -> b -> a
    (@<<) :: a -> b -> a

instance (GPU a, GPU b, IsIntegral t, IsVecScalar d a t, IsVecScalar d b Word32) => OperatorShift (Exp stage a) (Exp stage b) where
    a @>> b = PrimApp PrimBShiftR $! tup2 (a,b)
    a @<< b = PrimApp PrimBShiftL $! tup2 (a,b)

instance (GPU a, IsIntegral t, IsVecScalar d a t) => OperatorShift (Exp stage a) (Exp stage Word32) where
    a @>> b = PrimApp PrimBShiftRS $! tup2 (a,b)
    a @<< b = PrimApp PrimBShiftLS $! tup2 (a,b)

class OperatorEq a b where
    (@==) :: a -> a -> b
    (@/=) :: a -> a -> b

instance (GPU a, GPU b, IsNum t, IsVecScalar d a t, IsVecScalar d b Bool) => OperatorEq (Exp stage a) (Exp stage b) where
    a @== b = PrimApp PrimEqualV $! tup2 (a,b)
    a @/= b = PrimApp PrimNotEqualV $! tup2 (a,b)

instance (GPU a, IsMatVecScalar a t) => OperatorEq (Exp stage a) (Exp stage Bool) where
    a @== b = PrimApp PrimEqual $! tup2 (a,b)
    a @/= b = PrimApp PrimNotEqual $! tup2 (a,b)

class OperatorRelational a b where
    (@<=) :: a -> a -> b
    (@>=) :: a -> a -> b
    (@<)  :: a -> a -> b
    (@>)  :: a -> a -> b

instance (GPU a, GPU b, IsNum t, IsVecScalar d a t, IsVecScalar d b Bool) => OperatorRelational (Exp stage a) (Exp stage b) where
    a @<= b = PrimApp PrimLessThanEqual $! tup2 (a,b)
    a @>= b = PrimApp PrimGreaterThanEqual $! tup2 (a,b)
    a @< b  = PrimApp PrimLessThan $! tup2 (a,b)
    a @> b  = PrimApp PrimGreaterThan $! tup2 (a,b)

a @&& b = PrimApp PrimAnd $! tup2 (a,b)
a @|| b = PrimApp PrimOr $! tup2 (a,b)

xor'               a b = PrimApp PrimXor     $! tup2 (a,b)
not'                 a = PrimApp PrimNot     a
any'                 a = PrimApp PrimAny     a
all'                 a = PrimApp PrimAll     a

acos'                a = PrimApp PrimACos    a
acosh'               a = PrimApp PrimACosH   a
asin'                a = PrimApp PrimASin    a
asinh'               a = PrimApp PrimASinH   a
atan'                a = PrimApp PrimATan    a
atan2'             a b = PrimApp PrimATan2   $! tup2 (a,b)
atanh'               a = PrimApp PrimATanH   a
cos'                 a = PrimApp PrimCos     a
cosh'                a = PrimApp PrimCosH    a
degrees'             a = PrimApp PrimDegrees a
radians'             a = PrimApp PrimRadians a
sin'                 a = PrimApp PrimSin     a
sinh'                a = PrimApp PrimSinH    a
tan'                 a = PrimApp PrimTan     a
tanh'                a = PrimApp PrimTanH    a

pow'               a b = PrimApp PrimPow     $! tup2 (a,b)
exp'                 a = PrimApp PrimExp     a
log'                 a = PrimApp PrimLog     a
exp2'                a = PrimApp PrimExp2    a
log2'                a = PrimApp PrimLog2    a
sqrt'                a = PrimApp PrimSqrt    a
invsqrt'             a = PrimApp PrimInvSqrt a

isnan'               a = PrimApp PrimIsNan   a
isinf'               a = PrimApp PrimIsInf   a
abs'                 a = PrimApp PrimAbs     a
sign'                a = PrimApp PrimSign    a
floor'               a = PrimApp PrimFloor   a
trunc'               a = PrimApp PrimTrunc   a
round'               a = PrimApp PrimRound   a
roundEven'           a = PrimApp PrimRoundEven a
ceil'                a = PrimApp PrimCeil    a
fract'               a = PrimApp PrimFract   a

floatBitsToInt'      a = PrimApp PrimFloatBitsToInt  a
floatBitsToUint'     a = PrimApp PrimFloatBitsToUInt a
intBitsToFloat'      a = PrimApp PrimIntBitsToFloat  a
uintBitsToFloat'     a = PrimApp PrimUIntBitsToFloat a

length'              a = PrimApp PrimLength      a
distance'          a b = PrimApp PrimDistance    $! tup2 (a,b)
dot'               a b = PrimApp PrimDot         $! tup2 (a,b)
cross'             a b = PrimApp PrimCross       $! tup2 (a,b)
normalize'           a = PrimApp PrimNormalize   a
faceforward'     a b c = PrimApp PrimFaceForward $! tup3 (a,b,c)
reflect'           a b = PrimApp PrimReflect     $! tup2 (a,b)
refract'         a b c = PrimApp PrimRefract     $! tup3 (a,b,c)

transpose'           a = PrimApp PrimTranspose   a
determinant'         a = PrimApp PrimDeterminant a
inverse'             a = PrimApp PrimInverse     a
outerProduct'      a b = PrimApp PrimOuterProduct $! tup2 (a,b)

dFdx'                a = PrimApp PrimDFdx    a
dFdy'                a = PrimApp PrimDFdy    a
fwidth'              a = PrimApp PrimFWidth  a

noise1'              a = PrimApp PrimNoise1  a
noise2'              a = PrimApp PrimNoise2  a
noise3'              a = PrimApp PrimNoise3  a
noise4'              a = PrimApp PrimNoise4  a

textureSize'                    a b = PrimApp PrimTextureSize           $! tup2 (a,b)
texture'                        a b = PrimApp PrimTexture               $! tup2 (a,b)
textureB'                     a b c = PrimApp PrimTextureB              $! tup3 (a,b,c)
textureProj'                    a b = PrimApp PrimTextureProj           $! tup2 (a,b)
textureProjB'                 a b c = PrimApp PrimTextureProjB          $! tup3 (a,b,c)
textureLod'                   a b c = PrimApp PrimTextureLod            $! tup3 (a,b,c)
textureOffset'                a b c = PrimApp PrimTextureOffset         $! tup3 (a,b,c)
textureOffsetB'             a b c d = PrimApp PrimTextureOffsetB        $! tup4 (a,b,c,d)
texelFetch'                   a b c = PrimApp PrimTexelFetch            $! tup3 (a,b,c)
texelFetchOffset'           a b c d = PrimApp PrimTexelFetchOffset      $! tup4 (a,b,c,d)
textureProjOffset'            a b c = PrimApp PrimTextureProjOffset     $! tup3 (a,b,c)
textureProjOffsetB'         a b c d = PrimApp PrimTextureProjOffsetB    $! tup4 (a,b,c,d)
textureLodOffset'           a b c d = PrimApp PrimTextureLodOffset      $! tup4 (a,b,c,d)
textureProjLod'               a b c = PrimApp PrimTextureProjLod        $! tup3 (a,b,c)
textureProjLodOffset'       a b c d = PrimApp PrimTextureProjLodOffset  $! tup4 (a,b,c,d)
textureGrad'                a b c d = PrimApp PrimTextureGrad           $! tup4 (a,b,c,d)
textureGradOffset'        a b c d e = PrimApp PrimTextureGradOffset     $! tup5 (a,b,c,d,e)
textureProjGrad'            a b c d = PrimApp PrimTextureProjGrad       $! tup4 (a,b,c,d)
textureProjGradOffset'    a b c d e = PrimApp PrimTextureProjGradOffset $! tup5 (a,b,c,d,e)

class BuiltinCommon a b where
    min'     :: a -> b -> a
    max'     :: a -> b -> a
    clamp'   :: a -> b -> b -> a

instance (GPU a, IsNum t, IsVecScalar d a t) => BuiltinCommon (Exp stage a) (Exp stage a) where
    min'     a b = PrimApp PrimMin   $! tup2 (a,b)
    max'     a b = PrimApp PrimMax   $! tup2 (a,b)
    clamp' a b c = PrimApp PrimClamp $! tup3 (a,b,c)

instance (GPU a, GPU t, IsNum t, IsVecScalar d a t) => BuiltinCommon (Exp stage a) (Exp stage t) where
    min'     a b = PrimApp PrimMinS   $! tup2 (a,b)
    max'     a b = PrimApp PrimMaxS   $! tup2 (a,b)
    clamp' a b c = PrimApp PrimClampS $! tup3 (a,b,c)

class BuiltinMix a b where
    mix' :: a -> a -> b -> a

instance (GPU a, IsVecScalar d a Float) => BuiltinMix (Exp stage a) (Exp stage a) where
    mix' a b c = PrimApp PrimMix $! tup3 (a,b,c)

instance (GPU a, IsVecScalar d a Float) => BuiltinMix (Exp stage a) (Exp stage Float) where
    mix' a b c = PrimApp PrimMixS $! tup3 (a,b,c)

instance (GPU a, GPU b, IsVecScalar d a Float, IsVecScalar d b Bool) => BuiltinMix (Exp stage a) (Exp stage b) where
    mix' a b c = PrimApp PrimMixB $! tup3 (a,b,c)

class BuiltinStep a b where
    step'        :: b -> a -> a
    smoothstep'  :: b -> b -> a -> a

instance (GPU a, IsVecScalar d a Float) => BuiltinStep (Exp stage a) (Exp stage a) where
    step'          a b = PrimApp PrimStep        $! tup2 (a,b)
    smoothstep'  a b c = PrimApp PrimSmoothStep  $! tup3 (a,b,c)

instance (GPU a, IsVecScalar d a Float) => BuiltinStep (Exp stage a) (Exp stage Float) where
    step'          a b = PrimApp PrimStepS       $! tup2 (a,b)
    smoothstep'  a b c = PrimApp PrimSmoothStepS $! tup3 (a,b,c)

a @. b = dot' a b
a @# b = cross' a b
a @*. b = PrimApp PrimMulMatVec $! tup2 (a,b)
a @.* b = PrimApp PrimMulVecMat $! tup2 (a,b)
a @.*. b = PrimApp PrimMulMatMat $! tup2 (a,b)

complement' a    = PrimApp PrimBNot a
neg'  a = PrimApp PrimNeg a
modf' a = PrimApp PrimModF a

{-
data PrimFun sig where

    -- Vec/Mat (de)construction
    PrimTupToV2             :: IsComponent a                            => PrimFun ((a,a)     -> V2 a)
    PrimTupToV3             :: IsComponent a                            => PrimFun ((a,a,a)   -> V3 a)
    PrimTupToV4             :: IsComponent a                            => PrimFun ((a,a,a,a) -> V4 a)
    PrimV2ToTup             :: IsComponent a                            => PrimFun (V2 a     -> (a,a))
    PrimV3ToTup             :: IsComponent a                            => PrimFun (V3 a   -> (a,a,a))
    PrimV4ToTup             :: IsComponent a                            => PrimFun (V4 a -> (a,a,a,a))
-}
--mkV2 a b = 
--mkV3 
--mkV4

class SpecialConstant a where
    zero'    :: a
    one'     :: a

instance SpecialConstant Bool where
    zero' = False
    one'  = True

instance SpecialConstant Int32 where
    zero' = 0
    one'  = 1

instance SpecialConstant Word32 where
    zero' = 0
    one'  = 1

instance SpecialConstant Float where
    zero' = 0
    one'  = 1

instance (SpecialConstant a) => SpecialConstant (V2 a) where
    zero' = V2 zero' zero'
    one'  = V2 one' one'

instance (SpecialConstant a) => SpecialConstant (V3 a) where
    zero' = V3 zero' zero' zero'
    one'  = V3 one' one' one'

instance (SpecialConstant a) => SpecialConstant (V4 a) where
    zero' = V4 zero' zero' zero' zero'
    one'  = V4 one' one' one' one'

class IdentityMatrix a where
    idmtx' :: a

instance IdentityMatrix M22F where
    idmtx' = V2 (V2 1 0) (V2 0 1)

instance IdentityMatrix M33F where
    idmtx' = V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)

instance IdentityMatrix M44F where
    idmtx' = V4 (V4 1 0 0 0) (V4 0 1 0 0) (V4 0 0 1 0) (V4 0 0 0 1)
{-
  TODO: 
    extendZero
    extendWith
    trim
-}

class PkgVec v where
    unpack' :: (GPU a, GPU (v a), IsComponent a, Typeable (EltRepr (v a)), Typeable (EltRepr' (v a)))
            => Exp stage (v a) -> v (Exp stage a)

    pack'   :: (GPU a, GPU (v a), IsComponent a, Typeable (EltRepr (v a)), Typeable (EltRepr' (v a)))
            => v (Exp stage a) -> Exp stage (v a)

instance PkgVec V2 where
    unpack' v        = let (x,y) = untup2 $! PrimApp PrimV2ToTup v in V2 x y
    pack' (V2 x y)   = PrimApp PrimTupToV2 $! tup2 (x,y)

instance PkgVec V3 where
    unpack' v        = let (x,y,z) = untup3 $! PrimApp PrimV3ToTup v in V3 x y z
    pack' (V3 x y z) = PrimApp PrimTupToV3 $! tup3 (x,y,z)

instance PkgVec V4 where
    unpack' v          = let (x,y,z,w) = untup4 $! PrimApp PrimV4ToTup v in V4 x y z w
    pack' (V4 x y z w) = PrimApp PrimTupToV4 $! tup4 (x,y,z,w)

-- Smart constructor and destructors for tuples
--

tup2 :: (GPU a, GPU b)
     => (Exp stage a, Exp stage b) -> Exp stage (a, b)
tup2 (x1, x2) = Tup (NilTup `SnocTup` x1 `SnocTup` x2)

tup3 :: (GPU a, GPU b, GPU c)
     => (Exp stage a, Exp stage b, Exp stage c) -> Exp stage (a, b, c)
tup3 (x1, x2, x3) = Tup (NilTup `SnocTup` x1 `SnocTup` x2 `SnocTup` x3)

tup4 :: (GPU a, GPU b, GPU c, GPU d)
     => (Exp stage a, Exp stage b, Exp stage c, Exp stage d) -> Exp stage (a, b, c, d)
tup4 (x1, x2, x3, x4) = Tup (NilTup `SnocTup` x1 `SnocTup` x2 `SnocTup` x3 `SnocTup` x4)

tup5 :: (GPU a, GPU b, GPU c, GPU d, GPU e)
     => (Exp stage a, Exp stage b, Exp stage c, Exp stage d, Exp stage e) -> Exp stage (a, b, c, d, e)
tup5 (x1, x2, x3, x4, x5) = Tup $! NilTup `SnocTup` x1 `SnocTup` x2 `SnocTup` x3 `SnocTup` x4 `SnocTup` x5

tup6 :: (GPU a, GPU b, GPU c, GPU d, GPU e, GPU f)
     => (Exp stage a, Exp stage b, Exp stage c, Exp stage d, Exp stage e, Exp stage f) -> Exp stage (a, b, c, d, e, f)
tup6 (x1, x2, x3, x4, x5, x6) = Tup $! NilTup `SnocTup` x1 `SnocTup` x2 `SnocTup` x3 `SnocTup` x4 `SnocTup` x5 `SnocTup` x6

tup7 :: (GPU a, GPU b, GPU c, GPU d, GPU e, GPU f, GPU g)
     => (Exp stage a, Exp stage b, Exp stage c, Exp stage d, Exp stage e, Exp stage f, Exp stage g) -> Exp stage (a, b, c, d, e, f, g)
tup7 (x1, x2, x3, x4, x5, x6, x7)
  = Tup $! NilTup `SnocTup` x1 `SnocTup` x2 `SnocTup` x3 `SnocTup` x4 `SnocTup` x5 `SnocTup` x6 `SnocTup` x7

tup8 :: (GPU a, GPU b, GPU c, GPU d, GPU e, GPU f, GPU g, GPU h)
     => (Exp stage a, Exp stage b, Exp stage c, Exp stage d, Exp stage e, Exp stage f, Exp stage g, Exp stage h) -> Exp stage (a, b, c, d, e, f, g, h)
tup8 (x1, x2, x3, x4, x5, x6, x7, x8)
  = Tup $! NilTup `SnocTup` x1 `SnocTup` x2 `SnocTup` x3 `SnocTup` x4 `SnocTup` x5 `SnocTup` x6 `SnocTup` x7 `SnocTup` x8

tup9 :: (GPU a, GPU b, GPU c, GPU d, GPU e, GPU f, GPU g, GPU h, GPU i)
     => (Exp stage a, Exp stage b, Exp stage c, Exp stage d, Exp stage e, Exp stage f, Exp stage g, Exp stage h, Exp stage i) -> Exp stage (a, b, c, d, e, f, g, h, i)
tup9 (x1, x2, x3, x4, x5, x6, x7, x8, x9)
  = Tup $! NilTup `SnocTup` x1 `SnocTup` x2 `SnocTup` x3 `SnocTup` x4 `SnocTup` x5 `SnocTup` x6 `SnocTup` x7 `SnocTup` x8 `SnocTup` x9

untup2 :: (GPU a, GPU b)
       => Exp stage (a, b) -> (Exp stage a, Exp stage b)
untup2 e = (tix1 `Prj` e, tix0 `Prj` e)

untup3 :: (GPU a, GPU b, GPU c)
       => Exp stage (a, b, c) -> (Exp stage a, Exp stage b, Exp stage c)
untup3 e = (tix2 `Prj` e, tix1 `Prj` e, tix0 `Prj` e)

untup4 :: (GPU a, GPU b, GPU c, GPU d)
       => Exp stage (a, b, c, d) -> (Exp stage a, Exp stage b, Exp stage c, Exp stage d)
untup4 e = (tix3 `Prj` e, tix2 `Prj` e, tix1 `Prj` e, tix0 `Prj` e)

untup5 :: (GPU a, GPU b, GPU c, GPU d, GPU e)
       => Exp stage (a, b, c, d, e) -> (Exp stage a, Exp stage b, Exp stage c, Exp stage d, Exp stage e)
untup5 e = (tix4 `Prj` e, tix3 `Prj` e, tix2 `Prj` e, tix1 `Prj` e, tix0 `Prj` e)

untup6 :: (GPU a, GPU b, GPU c, GPU d, GPU e, GPU f)
       => Exp stage (a, b, c, d, e, f) -> (Exp stage a, Exp stage b, Exp stage c, Exp stage d, Exp stage e, Exp stage f)
untup6 e = (tix5 `Prj` e, tix4 `Prj` e, tix3 `Prj` e, tix2 `Prj` e, tix1 `Prj` e, tix0 `Prj` e)

untup7 :: (GPU a, GPU b, GPU c, GPU d, GPU e, GPU f, GPU g)
       => Exp stage (a, b, c, d, e, f, g) -> (Exp stage a, Exp stage b, Exp stage c, Exp stage d, Exp stage e, Exp stage f, Exp stage g)
untup7 e = (tix6 `Prj` e, tix5 `Prj` e, tix4 `Prj` e, tix3 `Prj` e, tix2 `Prj` e, tix1 `Prj` e, tix0 `Prj` e)

untup8 :: (GPU a, GPU b, GPU c, GPU d, GPU e, GPU f, GPU g, GPU h)
       => Exp stage (a, b, c, d, e, f, g, h) -> (Exp stage a, Exp stage b, Exp stage c, Exp stage d, Exp stage e, Exp stage f, Exp stage g, Exp stage h)
untup8 e = (tix7 `Prj` e, tix6 `Prj` e, tix5 `Prj` e, tix4 `Prj` e, tix3 `Prj` e, tix2 `Prj` e, tix1 `Prj` e, tix0 `Prj` e)

untup9 :: (GPU a, GPU b, GPU c, GPU d, GPU e, GPU f, GPU g, GPU h, GPU i)
       => Exp stage (a, b, c, d, e, f, g, h, i) -> (Exp stage a, Exp stage b, Exp stage c, Exp stage d, Exp stage e, Exp stage f, Exp stage g, Exp stage h, Exp stage i)
untup9 e = (tix8 `Prj` e, tix7 `Prj` e, tix6 `Prj` e, tix5 `Prj` e, tix4 `Prj` e, tix3 `Prj` e, tix2 `Prj` e, tix1 `Prj` e, tix0 `Prj` e)

-- builtin variables
-- vertex shader
vertexID :: Exp V Int32
vertexID = PrimVar $ IInt "gl_VertexID"

instanceID :: Exp V Int32
instanceID = PrimVar $ IInt "gl_InstanceID"

-- geometry shader
primitiveIDIn :: Exp G Int32
primitiveIDIn = PrimVar $ IInt "gl_PrimitiveIDIn"

-- fragment shader
fragCoord :: Exp F V4F
fragCoord = PrimVar $ IV4F "gl_FragCoord"

frontFacing :: Exp F Bool
frontFacing = PrimVar $ IBool "gl_FrontFacing"

pointCoord :: Exp F V2F
pointCoord = PrimVar $ IV2F "gl_PointCoord"

primitiveID :: Exp F Int32
primitiveID = PrimVar $ IInt "gl_PrimitiveID"
