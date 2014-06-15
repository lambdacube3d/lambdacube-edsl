{-# LANGUAGE        BangPatterns #-}
{-# LANGUAGE        ConstraintKinds #-}
{-# LANGUAGE        DataKinds #-}
{-# LANGUAGE        DeriveDataTypeable #-}
{-# LANGUAGE        EmptyDataDecls #-}
{-# LANGUAGE        FlexibleContexts #-}
{-# LANGUAGE        FlexibleInstances #-}
{-# LANGUAGE        FunctionalDependencies #-}
{-# LANGUAGE        GADTs #-}
{-# LANGUAGE        ImpredicativeTypes #-}
{-# LANGUAGE        KindSignatures #-}
{-# LANGUAGE        MultiParamTypeClasses #-}
{-# LANGUAGE        OverloadedStrings #-}
{-# LANGUAGE        ParallelListComp #-}
{-# LANGUAGE        Rank2Types #-}
{-# LANGUAGE        ScopedTypeVariables #-}
{-# LANGUAGE        StandaloneDeriving #-}
{-# LANGUAGE        TupleSections #-}
{-# LANGUAGE        TypeFamilies #-}
{-# LANGUAGE        TypeOperators #-}
{-# LANGUAGE        TypeSynonymInstances #-}
module LC_G_Type where

import GHC.TypeLits
import Data.Typeable

import Data.Int
import Data.Word
import Foreign.Storable
import Foreign.Ptr

data V2 a = V2 !a !a deriving (Eq,Ord,Show,Typeable)
data V3 a = V3 !a !a !a deriving (Eq,Ord,Show,Typeable)
data V4 a = V4 !a !a !a !a deriving (Eq,Ord,Show,Typeable)

-- matrices are stored in column major order
type M22F = V2 V2F
type M23F = V3 V2F
type M24F = V4 V2F
type M32F = V2 V3F
type M33F = V3 V3F
type M34F = V4 V3F
type M42F = V2 V4F
type M43F = V3 V4F
type M44F = V4 V4F

type V2F = V2 Float
type V3F = V3 Float
type V4F = V4 Float
type V2I = V2 Int32
type V3I = V3 Int32
type V4I = V4 Int32
type V2U = V2 Word32
type V3U = V3 Word32
type V4U = V4 Word32
type V2B = V2 Bool
type V3B = V3 Bool
type V4B = V4 Bool

-- vector types: V2, V3, V4
class IsVec (dim :: Nat) vec component | vec -> dim component, dim component -> vec
instance IsVec 2 (V2 Float) Float
instance IsVec 3 (V3 Float) Float
instance IsVec 4 (V4 Float) Float
instance IsVec 2 (V2 Int32) Int32
instance IsVec 3 (V3 Int32) Int32
instance IsVec 4 (V4 Int32) Int32
instance IsVec 2 (V2 Word32) Word32
instance IsVec 3 (V3 Word32) Word32
instance IsVec 4 (V4 Word32) Word32
instance IsVec 2 (V2 Bool) Bool
instance IsVec 3 (V3 Bool) Bool
instance IsVec 4 (V4 Bool) Bool

-- scalar and vector types: scalar, V2, V3, V4
class IsVecScalar (dim :: Nat) vec component | vec -> dim component, dim component -> vec
instance IsVecScalar 1 Float Float
instance IsVecScalar 2 (V2 Float) Float
instance IsVecScalar 3 (V3 Float) Float
instance IsVecScalar 4 (V4 Float) Float
instance IsVecScalar 1 Int32 Int32
instance IsVecScalar 2 (V2 Int32) Int32
instance IsVecScalar 3 (V3 Int32) Int32
instance IsVecScalar 4 (V4 Int32) Int32
instance IsVecScalar 1 Word32 Word32
instance IsVecScalar 2 (V2 Word32) Word32
instance IsVecScalar 3 (V3 Word32) Word32
instance IsVecScalar 4 (V4 Word32) Word32
instance IsVecScalar 1 Bool Bool
instance IsVecScalar 2 (V2 Bool) Bool
instance IsVecScalar 3 (V3 Bool) Bool
instance IsVecScalar 4 (V4 Bool) Bool

-- matrix types of dimension [2..4] x [2..4]
class IsMat mat h w | mat -> h w
instance IsMat M22F V2F V2F
instance IsMat M23F V2F V3F
instance IsMat M24F V2F V4F
instance IsMat M32F V3F V2F
instance IsMat M33F V3F V3F
instance IsMat M34F V3F V4F
instance IsMat M42F V4F V2F
instance IsMat M43F V4F V3F
instance IsMat M44F V4F V4F

-- matrix, vector and scalar types
class IsMatVecScalar a t | a -> t
instance IsMatVecScalar Float Float
instance IsMatVecScalar (V2 Float) Float
instance IsMatVecScalar (V3 Float) Float
instance IsMatVecScalar (V4 Float) Float
instance IsMatVecScalar Int32 Int32
instance IsMatVecScalar (V2 Int32) Int32
instance IsMatVecScalar (V3 Int32) Int32
instance IsMatVecScalar (V4 Int32) Int32
instance IsMatVecScalar Word32 Word32
instance IsMatVecScalar (V2 Word32) Word32
instance IsMatVecScalar (V3 Word32) Word32
instance IsMatVecScalar (V4 Word32) Word32
instance IsMatVecScalar Bool Bool
instance IsMatVecScalar (V2 Bool) Bool
instance IsMatVecScalar (V3 Bool) Bool
instance IsMatVecScalar (V4 Bool) Bool
instance IsMatVecScalar M22F Float
instance IsMatVecScalar M23F Float
instance IsMatVecScalar M24F Float
instance IsMatVecScalar M32F Float
instance IsMatVecScalar M33F Float
instance IsMatVecScalar M34F Float
instance IsMatVecScalar M42F Float
instance IsMatVecScalar M43F Float
instance IsMatVecScalar M44F Float

-- matrix and vector types
class IsMatVec a t | a -> t
instance IsMatVec (V2 Float) Float
instance IsMatVec (V3 Float) Float
instance IsMatVec (V4 Float) Float
instance IsMatVec (V2 Int32) Int32
instance IsMatVec (V3 Int32) Int32
instance IsMatVec (V4 Int32) Int32
instance IsMatVec (V2 Word32) Word32
instance IsMatVec (V3 Word32) Word32
instance IsMatVec (V4 Word32) Word32
instance IsMatVec (V2 Bool) Bool
instance IsMatVec (V3 Bool) Bool
instance IsMatVec (V4 Bool) Bool
instance IsMatVec M22F Float
instance IsMatVec M23F Float
instance IsMatVec M24F Float
instance IsMatVec M32F Float
instance IsMatVec M33F Float
instance IsMatVec M34F Float
instance IsMatVec M42F Float
instance IsMatVec M43F Float
instance IsMatVec M44F Float

-- matrix or vector component type
class IsComponent a
instance IsComponent Float
instance IsComponent Int32
instance IsComponent Word32
instance IsComponent Bool
instance IsComponent V2F
instance IsComponent V3F
instance IsComponent V4F

-- matrix or vector number component type
class IsNumComponent a
instance IsNumComponent Float
instance IsNumComponent Int32
instance IsNumComponent Word32
instance IsNumComponent V2F
instance IsNumComponent V3F
instance IsNumComponent V4F

class IsSigned a
instance IsSigned Float
instance IsSigned Int

class Real a => IsNum a
instance IsNum Float
instance IsNum Int32
instance IsNum Word32

class IsIntegral a
instance IsIntegral Int32
instance IsIntegral Word32

class IsFloating a
instance IsFloating Float
instance IsFloating V2F
instance IsFloating V3F
instance IsFloating V4F
instance IsFloating M22F
instance IsFloating M23F
instance IsFloating M24F
instance IsFloating M32F
instance IsFloating M33F
instance IsFloating M34F
instance IsFloating M42F
instance IsFloating M43F
instance IsFloating M44F


-- storable instances
instance Storable a => Storable (V2 a) where
    sizeOf    _ = 2 * sizeOf (undefined :: a)
    alignment _ = sizeOf (undefined :: a)

    peek q = do
        let p = castPtr q :: Ptr a
            k = sizeOf (undefined :: a)
        x <- peek        p 
        y <- peekByteOff p k
        return $! (V2 x y)

    poke q (V2 x y) = do
        let p = castPtr q :: Ptr a
            k = sizeOf (undefined :: a)
        poke        p   x
        pokeByteOff p k y

instance Storable a => Storable (V3 a) where
    sizeOf    _ = 3 * sizeOf (undefined :: a)
    alignment _ = sizeOf (undefined :: a)

    peek q = do
        let p = castPtr q :: Ptr a
            k = sizeOf (undefined :: a)
        x <- peek        p 
        y <- peekByteOff p k
        z <- peekByteOff p (k*2)
        return $! (V3 x y z)

    poke q (V3 x y z) = do
        let p = castPtr q :: Ptr a
            k = sizeOf (undefined :: a)
        poke        p   x
        pokeByteOff p k y
        pokeByteOff p (k*2) z

instance Storable a => Storable (V4 a) where
    sizeOf    _ = 4 * sizeOf (undefined :: a)
    alignment _ = sizeOf (undefined :: a)

    peek q = do
        let p = castPtr q :: Ptr a
            k = sizeOf (undefined :: a)
        x <- peek        p 
        y <- peekByteOff p k
        z <- peekByteOff p (k*2)
        w <- peekByteOff p (k*3)
        return $! (V4 x y z w)

    poke q (V4 x y z w) = do
        let p = castPtr q :: Ptr a
            k = sizeOf (undefined :: a)
        poke        p   x
        pokeByteOff p k y
        pokeByteOff p (k*2) z
        pokeByteOff p (k*3) w
