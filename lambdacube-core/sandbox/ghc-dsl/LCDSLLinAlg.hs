module LCDSLLinAlg where

import Data.Word

---------------------
-- LINEAR ALGEBRA
---------------------

-- constructors are required for texture specification
data DIM1 = DIM1
data DIM2 = DIM2
data DIM3 = DIM3
data DIM4

data V2 a = V2 a a
data V3 a = V3 a a a
data V4 a = V4 a a a a

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
type V2I = V2 Int
type V3I = V3 Int
type V4I = V4 Int
type V2U = V2 Word
type V3U = V3 Word
type V4U = V4 Word
type V2B = V2 Bool
type V3B = V3 Bool
type V4B = V4 Bool

-- vector types: V2, V3, V4
class IsVec dim vec component | vec -> dim component
instance IsVec DIM2 (V2 Float) Float
instance IsVec DIM3 (V3 Float) Float
instance IsVec DIM4 (V4 Float) Float
instance IsVec DIM2 (V2 Int) Int
instance IsVec DIM3 (V3 Int) Int
instance IsVec DIM4 (V4 Int) Int
instance IsVec DIM2 (V2 Word) Word
instance IsVec DIM3 (V3 Word) Word
instance IsVec DIM4 (V4 Word) Word
instance IsVec DIM2 (V2 Bool) Bool
instance IsVec DIM3 (V3 Bool) Bool
instance IsVec DIM4 (V4 Bool) Bool

-- scalar and vector types: scalar, V2, V3, V4
class IsVecScalar dim vec component | vec -> dim component
instance IsVecScalar DIM1 Float Float
instance IsVecScalar DIM2 (V2 Float) Float
instance IsVecScalar DIM3 (V3 Float) Float
instance IsVecScalar DIM4 (V4 Float) Float
instance IsVecScalar DIM1 Int Int
instance IsVecScalar DIM2 (V2 Int) Int
instance IsVecScalar DIM3 (V3 Int) Int
instance IsVecScalar DIM4 (V4 Int) Int
instance IsVecScalar DIM1 Word Word
instance IsVecScalar DIM2 (V2 Word) Word
instance IsVecScalar DIM3 (V3 Word) Word
instance IsVecScalar DIM4 (V4 Word) Word
instance IsVecScalar DIM1 Bool Bool
instance IsVecScalar DIM2 (V2 Bool) Bool
instance IsVecScalar DIM3 (V3 Bool) Bool
instance IsVecScalar DIM4 (V4 Bool) Bool

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
instance IsMatVecScalar Int Int
instance IsMatVecScalar (V2 Int) Int
instance IsMatVecScalar (V3 Int) Int
instance IsMatVecScalar (V4 Int) Int
instance IsMatVecScalar Word Word
instance IsMatVecScalar (V2 Word) Word
instance IsMatVecScalar (V3 Word) Word
instance IsMatVecScalar (V4 Word) Word
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
instance IsMatVec (V2 Int) Int
instance IsMatVec (V3 Int) Int
instance IsMatVec (V4 Int) Int
instance IsMatVec (V2 Word) Word
instance IsMatVec (V3 Word) Word
instance IsMatVec (V4 Word) Word
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
instance IsComponent Int
instance IsComponent Word
instance IsComponent Bool
instance IsComponent V2F
instance IsComponent V3F
instance IsComponent V4F

-- matrix or vector number component type
class IsNumComponent a
instance IsNumComponent Float
instance IsNumComponent Int
instance IsNumComponent Word
instance IsNumComponent V2F
instance IsNumComponent V3F
instance IsNumComponent V4F

class IsSigned a
instance IsSigned Float
instance IsSigned Int

class Real a => IsNum a
instance IsNum Float
instance IsNum Int
instance IsNum Word

class IsIntegral a
instance IsIntegral Int
instance IsIntegral Word

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
