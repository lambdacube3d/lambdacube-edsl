module LC_T_DSLType where

import Data.Int
import Data.Word
import Data.Typeable

import LC_G_Type
import LC_G_APIType
import qualified LC_G_APIType as U

import TypeLevel.Number.Nat
import TypeLevel.Number.Nat.Num

data Rect deriving Typeable

data Red
data RG
data RGB
data RGBA

data Regular a      deriving Typeable
data Shadow a       deriving Typeable
data MultiSample a  deriving Typeable
data Buffer a       deriving Typeable

data SingleTex deriving Typeable    -- singleton texture
data ArrayTex  deriving Typeable    -- array texture
data CubeTex   deriving Typeable    -- cube texture = array with size 6

data Sampler dim layerCount t ar deriving Typeable

instance Show (Sampler dim layerCount t ar) where
    show _ = "Sampler dim layerCount t ar"

-- GPU type restriction, the functions are used in shader codegen
class (Show a) => GPU a where

-- Float
instance Typeable a => GPU (Sampler DIM1 SingleTex (Regular Float) a) where
instance Typeable a => GPU (Sampler DIM2 SingleTex (Regular Float) a) where
instance Typeable a => GPU (Sampler DIM3 SingleTex (Regular Float) a) where
instance Typeable a => GPU (Sampler DIM2 CubeTex (Regular Float) a) where
instance Typeable a => GPU (Sampler DIM1 ArrayTex (Regular Float) a) where
instance Typeable a => GPU (Sampler DIM2 ArrayTex (Regular Float) a) where
instance Typeable a => GPU (Sampler DIM2 SingleTex (MultiSample Float) a) where
instance Typeable a => GPU (Sampler DIM2 ArrayTex (MultiSample Float) a) where
instance Typeable a => GPU (Sampler DIM1 SingleTex (Buffer Float) a) where
instance Typeable a => GPU (Sampler Rect SingleTex (Regular Float) a) where

-- Int
instance Typeable a => GPU (Sampler DIM1 SingleTex (Regular Int) a) where
instance Typeable a => GPU (Sampler DIM2 SingleTex (Regular Int) a) where
instance Typeable a => GPU (Sampler DIM3 SingleTex (Regular Int) a) where
instance Typeable a => GPU (Sampler DIM2 CubeTex (Regular Int) a) where
instance Typeable a => GPU (Sampler DIM1 ArrayTex (Regular Int) a) where
instance Typeable a => GPU (Sampler DIM2 ArrayTex (Regular Int) a) where
instance Typeable a => GPU (Sampler DIM2 SingleTex (MultiSample Int) a) where
instance Typeable a => GPU (Sampler DIM2 ArrayTex (MultiSample Int) a) where
instance Typeable a => GPU (Sampler DIM1 SingleTex (Buffer Int) a) where
instance Typeable a => GPU (Sampler Rect SingleTex (Regular Int) a) where

-- Word
instance Typeable a => GPU (Sampler DIM1 SingleTex (Regular Word) a) where
instance Typeable a => GPU (Sampler DIM2 SingleTex (Regular Word) a) where
instance Typeable a => GPU (Sampler DIM3 SingleTex (Regular Word) a) where
instance Typeable a => GPU (Sampler DIM2 CubeTex (Regular Word) a) where
instance Typeable a => GPU (Sampler DIM1 ArrayTex (Regular Word) a) where
instance Typeable a => GPU (Sampler DIM2 ArrayTex (Regular Word) a) where
instance Typeable a => GPU (Sampler DIM2 SingleTex (MultiSample Word) a) where
instance Typeable a => GPU (Sampler DIM2 ArrayTex (MultiSample Word) a) where
instance Typeable a => GPU (Sampler DIM1 SingleTex (Buffer Word) a) where
instance Typeable a => GPU (Sampler Rect SingleTex (Regular Word) a) where

-- Shadow
instance Typeable a => GPU (Sampler DIM1 SingleTex (Shadow Float) a) where
instance Typeable a => GPU (Sampler DIM2 SingleTex (Shadow Float) a) where
instance Typeable a => GPU (Sampler DIM2 CubeTex (Shadow Float) a) where
instance Typeable a => GPU (Sampler DIM1 ArrayTex (Shadow Float) a) where
instance Typeable a => GPU (Sampler DIM2 ArrayTex (Shadow Float) a) where
instance Typeable a => GPU (Sampler Rect SingleTex (Shadow Float) a) where

instance GPU () where
instance GPU Bool where
instance GPU Float where
instance GPU Int32 where
instance GPU Word32 where
instance GPU V2B where
instance GPU V2F where
instance GPU V2I where
instance GPU V2U where
instance GPU V3B where
instance GPU V3F where
instance GPU V3I where
instance GPU V3U where
instance GPU V4B where
instance GPU V4F where
instance GPU V4I where
instance GPU V4U where
instance GPU M22F where
instance GPU M23F where
instance GPU M24F where
instance GPU M32F where
instance GPU M33F where
instance GPU M34F where
instance GPU M42F where
instance GPU M43F where
instance GPU M44F where
instance (GPU a, GPU b) => GPU (a, b) where
instance (GPU a, GPU b, GPU c) => GPU (a, b, c) where
instance (GPU a, GPU b, GPU c, GPU d) => GPU (a, b, c, d) where
instance (GPU a, GPU b, GPU c, GPU d, GPU e) => GPU (a, b, c, d, e) where
instance (GPU a, GPU b, GPU c, GPU d, GPU e, GPU f) => GPU (a, b, c, d, e, f) where
instance (GPU a, GPU b, GPU c, GPU d, GPU e, GPU f, GPU g) => GPU (a, b, c, d, e, f, g) where
instance (GPU a, GPU b, GPU c, GPU d, GPU e, GPU f, GPU g, GPU h) => GPU (a, b, c, d, e, f, g, h) where
instance (GPU a, GPU b, GPU c, GPU d, GPU e, GPU f, GPU g, GPU h, GPU i) => GPU (a, b, c, d, e, f, g, h, i) where

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
