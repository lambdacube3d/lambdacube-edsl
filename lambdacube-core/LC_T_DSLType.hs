module LC_T_DSLType where

import Data.Int
import Data.Word
import Data.Typeable

import LCType

import LC_APIType

data Sampler dim layerCount t ar

-- IsScalar means here that the related type is not a tuple, but a GPU primitive type
class GPU a => IsScalar a where
    toValue     :: a -> Value
    toType      :: a -> InputType
{-
singletonScalarType :: IsScalar a => a -> TupleType ((), a)
singletonScalarType a = PairTuple UnitTuple (SingleTuple a)
-}
-- GPU type restriction, the functions are used in shader codegen
class (Show a, Typeable a, Typeable (EltRepr a), Typeable (EltRepr' a)) => GPU a where
    tupleType   :: TupleType reprTT => a -> reprTT (EltRepr a)
    tupleType'  :: TupleType reprTT => a -> reprTT (EltRepr' a)

-- stream type restriction, these types can be used in vertex shader input
class GPU a => SGPU a

-- uniform type restriction
-- hint: EltRepr stands for Elementary Type Representation
type family EltRepr a :: *
type instance EltRepr (Sampler dim sh t ar) = ((), Sampler dim sh t ar)
type instance EltRepr () = ()
type instance EltRepr Int32 = ((), Int32)
type instance EltRepr Word32 = ((), Word32)
type instance EltRepr Float = ((), Float)
type instance EltRepr Bool = ((), Bool)
type instance EltRepr V2F = ((), V2F)
type instance EltRepr V2I = ((), V2I)
type instance EltRepr V2U = ((), V2U)
type instance EltRepr V2B = ((), V2B)
type instance EltRepr M22F = ((), M22F)
type instance EltRepr M23F = ((), M23F)
type instance EltRepr M24F = ((), M24F)
type instance EltRepr V3F = ((), V3F)
type instance EltRepr V3I = ((), V3I)
type instance EltRepr V3U = ((), V3U)
type instance EltRepr V3B = ((), V3B)
type instance EltRepr M32F = ((), M32F)
type instance EltRepr M33F = ((), M33F)
type instance EltRepr M34F = ((), M34F)
type instance EltRepr V4F = ((), V4F)
type instance EltRepr V4I = ((), V4I)
type instance EltRepr V4U = ((), V4U)
type instance EltRepr V4B = ((), V4B)
type instance EltRepr M42F = ((), M42F)
type instance EltRepr M43F = ((), M43F)
type instance EltRepr M44F = ((), M44F)
type instance EltRepr (a, b) = (EltRepr a, EltRepr' b)
type instance EltRepr (a, b, c) = (EltRepr (a, b), EltRepr' c)
type instance EltRepr (a, b, c, d) = (EltRepr (a, b, c), EltRepr' d)
type instance EltRepr (a, b, c, d, e) = (EltRepr (a, b, c, d), EltRepr' e)
type instance EltRepr (a, b, c, d, e, f) = (EltRepr (a, b, c, d, e), EltRepr' f)
type instance EltRepr (a, b, c, d, e, f, g) = (EltRepr (a, b, c, d, e, f), EltRepr' g)
type instance EltRepr (a, b, c, d, e, f, g, h) = (EltRepr (a, b, c, d, e, f, g), EltRepr' h)
type instance EltRepr (a, b, c, d, e, f, g, h, i) = (EltRepr (a, b, c, d, e, f, g, h), EltRepr' i)

type family EltRepr' a :: *
type instance EltRepr' (Sampler dim sh t ar) = Sampler dim sh t ar
type instance EltRepr' () = ()
type instance EltRepr' Int32 = Int32
type instance EltRepr' Word32 = Word32
type instance EltRepr' Float = Float
type instance EltRepr' Bool = Bool
type instance EltRepr' V2F = V2F
type instance EltRepr' V2I = V2I
type instance EltRepr' V2U = V2U
type instance EltRepr' V2B = V2B
type instance EltRepr' M22F = M22F
type instance EltRepr' M23F = M23F
type instance EltRepr' M24F = M24F
type instance EltRepr' V3F = V3F
type instance EltRepr' V3I = V3I
type instance EltRepr' V3U = V3U
type instance EltRepr' V3B = V3B
type instance EltRepr' M32F = M32F
type instance EltRepr' M33F = M33F
type instance EltRepr' M34F = M34F
type instance EltRepr' V4F = V4F
type instance EltRepr' V4I = V4I
type instance EltRepr' V4U = V4U
type instance EltRepr' V4B = V4B
type instance EltRepr' M42F = M42F
type instance EltRepr' M43F = M43F
type instance EltRepr' M44F = M44F
type instance EltRepr' (a, b) = (EltRepr a, EltRepr' b)
type instance EltRepr' (a, b, c) = (EltRepr (a, b), EltRepr' c)
type instance EltRepr' (a, b, c, d) = (EltRepr (a, b, c), EltRepr' d)
type instance EltRepr' (a, b, c, d, e) = (EltRepr (a, b, c, d), EltRepr' e)
type instance EltRepr' (a, b, c, d, e, f) = (EltRepr (a, b, c, d, e), EltRepr' f)
type instance EltRepr' (a, b, c, d, e, f, g) = (EltRepr (a, b, c, d, e, f), EltRepr' g)
type instance EltRepr' (a, b, c, d, e, f, g, h) = (EltRepr (a, b, c, d, e, f, g), EltRepr' h)
type instance EltRepr' (a, b, c, d, e, f, g, h, i) = (EltRepr (a, b, c, d, e, f, g, h), EltRepr' i)

-- |Conversion between surface n-tuples and our tuple representation.
--
-- our language uses nested tuple representation
class IsTuple tup where
    type TupleRepr tup
    fromTuple :: tup -> TupleRepr tup
    toTuple   :: TupleRepr tup -> tup

instance IsTuple () where
    type TupleRepr () = ()
    fromTuple         = id
    toTuple           = id
            
instance IsTuple (a, b) where
    type TupleRepr (a, b) = (((), a), b)
    fromTuple (x, y)      = (((), x), y)
    toTuple (((), x), y)  = (x, y)
            
instance IsTuple (a, b, c) where
    type TupleRepr (a, b, c)  = (TupleRepr (a, b), c)
    fromTuple (x, y, z)       = ((((), x), y), z)
    toTuple ((((), x), y), z) = (x, y, z)

instance IsTuple (a, b, c, d) where
    type TupleRepr (a, b, c, d)    = (TupleRepr (a, b, c), d)
    fromTuple (x, y, z, v)         = (((((), x), y), z), v)
    toTuple (((((), x), y), z), v) = (x, y, z, v)

instance IsTuple (a, b, c, d, e) where
    type TupleRepr (a, b, c, d, e)      = (TupleRepr (a, b, c, d), e)
    fromTuple (x, y, z, v, w)           = ((((((), x), y), z), v), w)
    toTuple ((((((), x), y), z), v), w) = (x, y, z, v, w)

instance IsTuple (a, b, c, d, e, f) where
    type TupleRepr (a, b, c, d, e, f)        = (TupleRepr (a, b, c, d, e), f)
    fromTuple (x, y, z, v, w, r)             = (((((((), x), y), z), v), w), r)
    toTuple (((((((), x), y), z), v), w), r) = (x, y, z, v, w, r)

instance IsTuple (a, b, c, d, e, f, g) where
    type TupleRepr (a, b, c, d, e, f, g)          = (TupleRepr (a, b, c, d, e, f), g)
    fromTuple (x, y, z, v, w, r, s)               = ((((((((), x), y), z), v), w), r), s)
    toTuple ((((((((), x), y), z), v), w), r), s) = (x, y, z, v, w, r, s)

instance IsTuple (a, b, c, d, e, f, g, h) where
    type TupleRepr (a, b, c, d, e, f, g, h)            = (TupleRepr (a, b, c, d, e, f, g), h)
    fromTuple (x, y, z, v, w, r, s, t)                 = (((((((((), x), y), z), v), w), r), s), t)
    toTuple (((((((((), x), y), z), v), w), r), s), t) = (x, y, z, v, w, r, s, t)

instance IsTuple (a, b, c, d, e, f, g, h, i) where
    type TupleRepr (a, b, c, d, e, f, g, h, i)              = (TupleRepr (a, b, c, d, e, f, g, h), i)
    fromTuple (x, y, z, v, w, r, s, t, u)                   = ((((((((((), x), y), z), v), w), r), s), t), u)
    toTuple ((((((((((), x), y), z), v), w), r), s), t), u) = (x, y, z, v, w, r, s, t, u)

-- Tuple representation
-- --------------------

-- |We represent tuples as heterogenous lists, typed by a type list.
--
class Tuple tuple where
    nilTup  ::                              tuple c ()
    snocTup :: GPU t => tuple c s -> c t -> tuple c (s, t)

{-
-- Auxiliary tuple index constants
--
tix0 :: GPU s => TupleIdx (t, s) s
tix0 = ZeroTupIdx
tix1 :: GPU s => TupleIdx ((t, s), s1) s
tix1 = SuccTupIdx tix0
tix2 :: GPU s => TupleIdx (((t, s), s1), s2) s
tix2 = SuccTupIdx tix1
tix3 :: GPU s => TupleIdx ((((t, s), s1), s2), s3) s
tix3 = SuccTupIdx tix2
tix4 :: GPU s => TupleIdx (((((t, s), s1), s2), s3), s4) s
tix4 = SuccTupIdx tix3
tix5 :: GPU s => TupleIdx ((((((t, s), s1), s2), s3), s4), s5) s
tix5 = SuccTupIdx tix4
tix6 :: GPU s => TupleIdx (((((((t, s), s1), s2), s3), s4), s5), s6) s
tix6 = SuccTupIdx tix5
tix7 :: GPU s => TupleIdx ((((((((t, s), s1), s2), s3), s4), s5), s6), s7) s
tix7 = SuccTupIdx tix6
tix8 :: GPU s => TupleIdx (((((((((t, s), s1), s2), s3), s4), s5), s6), s7), s8) s
tix8 = SuccTupIdx tix7
-}
-- used in shader codegen
class TupleType tupleType where
  unitTuple   ::                               tupleType ()
  singleTuple :: IsScalar a =>            a -> tupleType a
  pairTuple   :: tupleType a -> tupleType b -> tupleType (a, b)

-- Extend Typeable support for 8- and 9-tuple
-- ------------------------------------------

myMkTyCon :: String -> TyCon
myMkTyCon = mkTyCon

class Typeable8 t where
    typeOf8 :: t a b c d e f g h -> TypeRep

instance Typeable8 (,,,,,,,) where
    typeOf8 _ = myMkTyCon "(,,,,,,,)" `mkTyConApp` []

typeOf7Default :: (Typeable8 t, Typeable a) => t a b c d e f g h -> TypeRep
typeOf7Default x = typeOf7 x `mkAppTy` typeOf (argType x)
  where
    argType :: t a b c d e f g h -> a
    argType =  undefined

instance (Typeable8 s, Typeable a) => Typeable7 (s a) where
    typeOf7 = typeOf7Default
  
class Typeable9 t where
    typeOf9 :: t a b c d e f g h i -> TypeRep

instance Typeable9 (,,,,,,,,) where
    typeOf9 _ = myMkTyCon "(,,,,,,,,)" `mkTyConApp` []

typeOf8Default :: (Typeable9 t, Typeable a) => t a b c d e f g h i -> TypeRep
typeOf8Default x = typeOf8 x `mkAppTy` typeOf (argType x)
  where
    argType :: t a b c d e f g h i -> a
    argType =  undefined

instance (Typeable9 s, Typeable a) => Typeable8 (s a) where
    typeOf8 = typeOf8Default
