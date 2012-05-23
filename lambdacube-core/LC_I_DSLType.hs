module LC_I_DSLType where

import Data.ByteString.Char8
import Data.Int
import Data.Word

import LC_APIType

import LC_T_DSLType

{-
instance (Typeable dim, Typeable sh, Typeable t, Typeable ar) => GPU (Sampler dim sh t ar) where
    tupleType v  = singletonScalarType v
    tupleType' v = SingleTuple v
instance GPU () where
    tupleType _  = UnitTuple
    tupleType' _ = UnitTuple
instance GPU Bool where
    tupleType v  = singletonScalarType v
    tupleType' v = SingleTuple v
-}
instance GPU Float where
{-
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
-}
{-
instance IsScalar (Sampler dim sh t ar) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = error "toType Sampler is not implemented yet" -- TODO
instance IsScalar Int32 where
    toValue v    = VInt v
    toType _     = ITInt
instance IsScalar Word32 where
    toValue v    = VWord v
    toType _     = ITWord
-}
instance IsScalar Float where
    toValue v    = VFloat v
    toType _     = Float
{-
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
-}

{-
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
-}


{-
class Tuple tuple where
    nilTup  ::                              tuple c ()
    snocTup :: GPU t => tuple c s -> c t -> tuple c (s, t)
-}
