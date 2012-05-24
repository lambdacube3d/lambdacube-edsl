module LC_GADT_DSLType where

import Data.Int
import Data.Word
import Data.Typeable

import LCType
import LC_GADT_APIType

-- IsScalar means here that the related type is not a tuple, but a GPU primitive type
class GPU a => IsScalar a where
    toValue     :: a -> Value
    toType      :: a -> InputType
instance (Typeable dim, Typeable sh, Typeable t, Typeable ar) => IsScalar (Sampler dim sh t ar) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = error "toType Sampler is not implemented yet" -- TODO
instance IsScalar Int32 where
    toValue v    = VInt v
    toType _     = ITInt
instance IsScalar Word32 where
    toValue v    = VWord v
    toType _     = ITWord
instance IsScalar Float where
    toValue v    = VFloat v
    toType _     = ITFloat
instance IsScalar Bool where
    toValue v    = VBool v
    toType _     = ITBool
instance IsScalar M22F where
    toValue v    = VM22F v
    toType _     = ITM22F
instance IsScalar M23F where
    toValue v    = VM23F v
    toType _     = ITM23F
instance IsScalar M24F where
    toValue v    = VM24F v
    toType _     = ITM24F
instance IsScalar M32F where
    toValue v    = VM32F v
    toType _     = ITM32F
instance IsScalar M33F where
    toValue v    = VM33F v
    toType _     = ITM33F
instance IsScalar M34F where
    toValue v    = VM34F v
    toType _     = ITM34F
instance IsScalar M42F where
    toValue v    = VM42F v
    toType _     = ITM42F
instance IsScalar M43F where
    toValue v    = VM43F v
    toType _     = ITM43F
instance IsScalar M44F where
    toValue v    = VM44F v
    toType _     = ITM44F
instance IsScalar V2F where
    toValue v    = VV2F v
    toType _     = ITV2F
instance IsScalar V3F where
    toValue v    = VV3F v
    toType _     = ITV3F
instance IsScalar V4F where
    toValue v    = VV4F v
    toType _     = ITV4F
instance IsScalar V2I where
    toValue v    = VV2I v
    toType _     = ITV2I
instance IsScalar V3I where
    toValue v    = VV3I v
    toType _     = ITV3I
instance IsScalar V4I where
    toValue v    = VV4I v
    toType _     = ITV4I
instance IsScalar V2U where
    toValue v    = VV2U v
    toType _     = ITV2U
instance IsScalar V3U where
    toValue v    = VV3U v
    toType _     = ITV3U
instance IsScalar V4U where
    toValue v    = VV4U v
    toType _     = ITV4U
instance IsScalar V2B where
    toValue v    = VV2B v
    toType _     = ITV2B
instance IsScalar V3B where
    toValue v    = VV3B v
    toType _     = ITV3B
instance IsScalar V4B where
    toValue v    = VV4B v
    toType _     = ITV4B

-- GPU type value reification, needed for shader codegen
data Value
    = VBool     !Bool
    | VV2B      !V2B
    | VV3B      !V3B
    | VV4B      !V4B
    | VWord     !Word32
    | VV2U      !V2U
    | VV3U      !V3U
    | VV4U      !V4U
    | VInt      !Int32
    | VV2I      !V2I
    | VV3I      !V3I
    | VV4I      !V4I
    | VFloat    !Float
    | VV2F      !V2F
    | VV3F      !V3F
    | VV4F      !V4F
    | VM22F     !M22F
    | VM23F     !M23F
    | VM24F     !M24F
    | VM32F     !M32F
    | VM33F     !M33F
    | VM34F     !M34F
    | VM42F     !M42F
    | VM43F     !M43F
    | VM44F     !M44F
    deriving (Show,Eq,Ord)

singletonScalarType :: IsScalar a => a -> TupleType ((), a)
singletonScalarType a = PairTuple UnitTuple (SingleTuple a)

-- GPU type restriction, the functions are used in shader codegen
class (Show a, Typeable a, Typeable (EltRepr a), Typeable (EltRepr' a)) => GPU a where
    tupleType   :: a -> TupleType (EltRepr a)
    tupleType'  :: a -> TupleType (EltRepr' a)
instance (Typeable dim, Typeable sh, Typeable t, Typeable ar) => GPU (Sampler dim sh t ar) where
    tupleType v  = singletonScalarType v
    tupleType' v = SingleTuple v
instance GPU () where
    tupleType _  = UnitTuple
    tupleType' _ = UnitTuple
instance GPU Bool where
    tupleType v  = singletonScalarType v
    tupleType' v = SingleTuple v
instance GPU Float where
    tupleType v  = singletonScalarType v
    tupleType' v = SingleTuple v
instance GPU Int32 where
    tupleType v  = singletonScalarType v
    tupleType' v = SingleTuple v
instance GPU Word32 where
    tupleType v  = singletonScalarType v
    tupleType' v = SingleTuple v
instance GPU V2B where
    tupleType v  = singletonScalarType v
    tupleType' v = SingleTuple v
instance GPU V2F where
    tupleType v  = singletonScalarType v
    tupleType' v = SingleTuple v
instance GPU V2I where
    tupleType v  = singletonScalarType v
    tupleType' v = SingleTuple v
instance GPU V2U where
    tupleType v  = singletonScalarType v
    tupleType' v = SingleTuple v
instance GPU V3B where
    tupleType v  = singletonScalarType v
    tupleType' v = SingleTuple v
instance GPU V3F where
    tupleType v  = singletonScalarType v
    tupleType' v = SingleTuple v
instance GPU V3I where
    tupleType v  = singletonScalarType v
    tupleType' v = SingleTuple v
instance GPU V3U where
    tupleType v  = singletonScalarType v
    tupleType' v = SingleTuple v
instance GPU V4B where
    tupleType v  = singletonScalarType v
    tupleType' v = SingleTuple v
instance GPU V4F where
    tupleType v  = singletonScalarType v
    tupleType' v = SingleTuple v
instance GPU V4I where
    tupleType v  = singletonScalarType v
    tupleType' v = SingleTuple v
instance GPU V4U where
    tupleType v  = singletonScalarType v
    tupleType' v = SingleTuple v
instance GPU M22F where
    tupleType v  = singletonScalarType v
    tupleType' v = SingleTuple v
instance GPU M23F where
    tupleType v  = singletonScalarType v
    tupleType' v = SingleTuple v
instance GPU M24F where
    tupleType v  = singletonScalarType v
    tupleType' v = SingleTuple v
instance GPU M32F where
    tupleType v  = singletonScalarType v
    tupleType' v = SingleTuple v
instance GPU M33F where
    tupleType v  = singletonScalarType v
    tupleType' v = SingleTuple v
instance GPU M34F where
    tupleType v  = singletonScalarType v
    tupleType' v = SingleTuple v
instance GPU M42F where
    tupleType v  = singletonScalarType v
    tupleType' v = SingleTuple v
instance GPU M43F where
    tupleType v  = singletonScalarType v
    tupleType' v = SingleTuple v
instance GPU M44F where
    tupleType v  = singletonScalarType v
    tupleType' v = SingleTuple v
instance (GPU a, GPU b) => GPU (a, b) where
    tupleType (_::(a, b)) 
        = PairTuple (tupleType (undefined :: a)) (tupleType' (undefined :: b))
    tupleType' (_::(a, b)) 
        = PairTuple (tupleType (undefined :: a)) (tupleType' (undefined :: b))
instance (GPU a, GPU b, GPU c) => GPU (a, b, c) where
    tupleType (_::(a, b, c)) 
        = PairTuple (tupleType (undefined :: (a, b))) (tupleType' (undefined :: c))
    tupleType' (_::(a, b, c)) 
        = PairTuple (tupleType (undefined :: (a, b))) (tupleType' (undefined :: c))
instance (GPU a, GPU b, GPU c, GPU d) => GPU (a, b, c, d) where
    tupleType (_::(a, b, c, d)) 
        = PairTuple (tupleType (undefined :: (a, b, c))) (tupleType' (undefined :: d))
    tupleType' (_::(a, b, c, d)) 
        = PairTuple (tupleType (undefined :: (a, b, c))) (tupleType' (undefined :: d))
instance (GPU a, GPU b, GPU c, GPU d, GPU e) => GPU (a, b, c, d, e) where
    tupleType (_::(a, b, c, d, e)) 
        = PairTuple (tupleType (undefined :: (a, b, c, d))) 
                (tupleType' (undefined :: e))
    tupleType' (_::(a, b, c, d, e)) 
        = PairTuple (tupleType (undefined :: (a, b, c, d))) 
                (tupleType' (undefined :: e))
instance (GPU a, GPU b, GPU c, GPU d, GPU e, GPU f) => GPU (a, b, c, d, e, f) where
    tupleType (_::(a, b, c, d, e, f)) 
        = PairTuple (tupleType (undefined :: (a, b, c, d, e))) 
                (tupleType' (undefined :: f))
    tupleType' (_::(a, b, c, d, e, f)) 
        = PairTuple (tupleType (undefined :: (a, b, c, d, e))) 
                (tupleType' (undefined :: f))
instance (GPU a, GPU b, GPU c, GPU d, GPU e, GPU f, GPU g) => GPU (a, b, c, d, e, f, g) where
    tupleType (_::(a, b, c, d, e, f, g)) 
        = PairTuple (tupleType (undefined :: (a, b, c, d, e, f))) 
                (tupleType' (undefined :: g))
    tupleType' (_::(a, b, c, d, e, f, g)) 
        = PairTuple (tupleType (undefined :: (a, b, c, d, e, f))) 
                (tupleType' (undefined :: g))
instance (GPU a, GPU b, GPU c, GPU d, GPU e, GPU f, GPU g, GPU h) => GPU (a, b, c, d, e, f, g, h) where
    tupleType (_::(a, b, c, d, e, f, g, h)) 
        = PairTuple (tupleType (undefined :: (a, b, c, d, e, f, g))) 
                (tupleType' (undefined :: h))
    tupleType' (_::(a, b, c, d, e, f, g, h)) 
        = PairTuple (tupleType (undefined :: (a, b, c, d, e, f, g))) 
                (tupleType' (undefined :: h))
instance (GPU a, GPU b, GPU c, GPU d, GPU e, GPU f, GPU g, GPU h, GPU i) => GPU (a, b, c, d, e, f, g, h, i) where
    tupleType (_::(a, b, c, d, e, f, g, h, i)) 
        = PairTuple (tupleType (undefined :: (a, b, c, d, e, f, g, h))) 
                (tupleType' (undefined :: i))
    tupleType' (_::(a, b, c, d, e, f, g, h, i)) 
        = PairTuple (tupleType (undefined :: (a, b, c, d, e, f, g, h))) 
                (tupleType' (undefined :: i))

-- stream type restriction, these types can be used in vertex shader input
class GPU a => SGPU a
instance SGPU Int32
instance SGPU Word32
instance SGPU Float
instance SGPU M22F
instance SGPU M23F
instance SGPU M24F
instance SGPU M32F
instance SGPU M33F
instance SGPU M34F
instance SGPU M42F
instance SGPU M43F
instance SGPU M44F
instance SGPU V2F
instance SGPU V3F
instance SGPU V4F
instance SGPU V2I
instance SGPU V3I
instance SGPU V4I
instance SGPU V2U
instance SGPU V3U
instance SGPU V4U
instance (SGPU a, SGPU b) => SGPU (a, b)
instance (SGPU a, SGPU b, SGPU c) => SGPU (a, b, c)
instance (SGPU a, SGPU b, SGPU c, SGPU d) => SGPU (a, b, c, d)
instance (SGPU a, SGPU b, SGPU c, SGPU d, SGPU e) => SGPU (a, b, c, d, e)
instance (SGPU a, SGPU b, SGPU c, SGPU d, SGPU e, SGPU f) => SGPU (a, b, c, d, e, f)
instance (SGPU a, SGPU b, SGPU c, SGPU d, SGPU e, SGPU f, SGPU g) => SGPU (a, b, c, d, e, f, g)
instance (SGPU a, SGPU b, SGPU c, SGPU d, SGPU e, SGPU f, SGPU g, SGPU h) => SGPU (a, b, c, d, e, f, g, h)
instance (SGPU a, SGPU b, SGPU c, SGPU d, SGPU e, SGPU f, SGPU g, SGPU h, SGPU i) => SGPU (a, b, c, d, e, f, g, h, i)

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
data Tuple c t where
    NilTup  ::                     Tuple c ()
    SnocTup :: GPU t => Tuple c s -> c t -> Tuple c (s, t)

-- |Type-safe projection indicies for tuples.
--
-- NB: We index tuples by starting to count from the *right*!
--
data TupleIdx t e where
    ZeroTupIdx :: GPU s =>        TupleIdx (t, s) s
    SuccTupIdx :: TupleIdx t e -> TupleIdx (t, s) e

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

-- used in shader codegen
data TupleType a where
  UnitTuple   ::                               TupleType ()
  SingleTuple :: IsScalar a =>            a -> TupleType a
  PairTuple   :: TupleType a -> TupleType b -> TupleType (a, b)

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
