module LC_T_Instances where

import Data.Int
import Data.Word

import LC_G_LinearAlgebraTypes
import LC_T_APIType hiding (InputType(..))
import LC_U_APIType

instance Freq Obj where
    reifyFreq _ = Obj'
instance Freq V where
    reifyFreq _ = V'
instance Freq G where
    reifyFreq _ = G'
instance Freq F where
    reifyFreq _ = F'

-- Float
instance Freq freq => LCType freq (Sampler DIM1 SingleTex (Regular Float) a) where
instance Freq freq => LCType freq (Sampler DIM2 SingleTex (Regular Float) a) where
instance Freq freq => LCType freq (Sampler DIM3 SingleTex (Regular Float) a) where
instance Freq freq => LCType freq (Sampler DIM2 CubeTex   (Regular Float) a) where
instance Freq freq => LCType freq (Sampler DIM1 ArrayTex  (Regular Float) a) where
instance Freq freq => LCType freq (Sampler DIM2 ArrayTex  (Regular Float) a) where
instance Freq freq => LCType freq (Sampler DIM2 SingleTex (MultiSample Float) a) where
instance Freq freq => LCType freq (Sampler DIM2 ArrayTex  (MultiSample Float) a) where
instance Freq freq => LCType freq (Sampler DIM1 SingleTex (Buffer Float) a) where
instance Freq freq => LCType freq (Sampler Rect SingleTex (Regular Float) a) where

-- Int
instance Freq freq => LCType freq (Sampler DIM1 SingleTex (Regular Int) a) where
instance Freq freq => LCType freq (Sampler DIM2 SingleTex (Regular Int) a) where
instance Freq freq => LCType freq (Sampler DIM3 SingleTex (Regular Int) a) where
instance Freq freq => LCType freq (Sampler DIM2 CubeTex   (Regular Int) a) where
instance Freq freq => LCType freq (Sampler DIM1 ArrayTex  (Regular Int) a) where
instance Freq freq => LCType freq (Sampler DIM2 ArrayTex  (Regular Int) a) where
instance Freq freq => LCType freq (Sampler DIM2 SingleTex (MultiSample Int) a) where
instance Freq freq => LCType freq (Sampler DIM2 ArrayTex  (MultiSample Int) a) where
instance Freq freq => LCType freq (Sampler DIM1 SingleTex (Buffer Int) a) where
instance Freq freq => LCType freq (Sampler Rect SingleTex (Regular Int) a) where

-- Word
instance Freq freq => LCType freq (Sampler DIM1 SingleTex (Regular Word) a) where
instance Freq freq => LCType freq (Sampler DIM2 SingleTex (Regular Word) a) where
instance Freq freq => LCType freq (Sampler DIM3 SingleTex (Regular Word) a) where
instance Freq freq => LCType freq (Sampler DIM2 CubeTex   (Regular Word) a) where
instance Freq freq => LCType freq (Sampler DIM1 ArrayTex  (Regular Word) a) where
instance Freq freq => LCType freq (Sampler DIM2 ArrayTex  (Regular Word) a) where
instance Freq freq => LCType freq (Sampler DIM2 SingleTex (MultiSample Word) a) where
instance Freq freq => LCType freq (Sampler DIM2 ArrayTex  (MultiSample Word) a) where
instance Freq freq => LCType freq (Sampler DIM1 SingleTex (Buffer Word) a) where
instance Freq freq => LCType freq (Sampler Rect SingleTex (Regular Word) a) where

-- Shadow
instance Freq freq => LCType freq (Sampler DIM1 SingleTex (Shadow Float) a) where
instance Freq freq => LCType freq (Sampler DIM2 SingleTex (Shadow Float) a) where
instance Freq freq => LCType freq (Sampler DIM2 CubeTex   (Shadow Float) a) where
instance Freq freq => LCType freq (Sampler DIM1 ArrayTex  (Shadow Float) a) where
instance Freq freq => LCType freq (Sampler DIM2 ArrayTex  (Shadow Float) a) where
instance Freq freq => LCType freq (Sampler Rect SingleTex (Shadow Float) a) where

instance Freq freq => LCType freq Bool      where reifyType _ _ = Bool'
instance Freq freq => LCType freq Float     where reifyType _ _ = Float'
instance Freq freq => LCType freq Int32     where reifyType _ _ = Int32'
instance Freq freq => LCType freq Word32    where reifyType _ _ = Word32'
instance Freq freq => LCType freq V2B       where reifyType _ _ = V2B'
instance Freq freq => LCType freq V2F       where reifyType _ _ = V2F'
instance Freq freq => LCType freq V2I       where reifyType _ _ = V2I'
instance Freq freq => LCType freq V2U       where reifyType _ _ = V2U'
instance Freq freq => LCType freq V3B       where reifyType _ _ = V3B'
instance Freq freq => LCType freq V3F       where reifyType _ _ = V3F'
instance Freq freq => LCType freq V3I       where reifyType _ _ = V3I'
instance Freq freq => LCType freq V3U       where reifyType _ _ = V3U'
instance Freq freq => LCType freq V4B       where reifyType _ _ = V4B'
instance Freq freq => LCType freq V4F       where reifyType _ _ = V4F'
instance Freq freq => LCType freq V4I       where reifyType _ _ = V4I'
instance Freq freq => LCType freq V4U       where reifyType _ _ = V4U'
instance Freq freq => LCType freq M22F      where reifyType _ _ = M22F'
instance Freq freq => LCType freq M23F      where reifyType _ _ = M23F'
instance Freq freq => LCType freq M24F      where reifyType _ _ = M24F'
instance Freq freq => LCType freq M32F      where reifyType _ _ = M32F'
instance Freq freq => LCType freq M33F      where reifyType _ _ = M33F'
instance Freq freq => LCType freq M34F      where reifyType _ _ = M34F'
instance Freq freq => LCType freq M42F      where reifyType _ _ = M42F'
instance Freq freq => LCType freq M43F      where reifyType _ _ = M43F'
instance Freq freq => LCType freq M44F      where reifyType _ _ = M44F'
instance Freq freq => LCType freq ZZ        where reifyType _ _ = ZZ'
instance (LCType freq a, LCType freq b, Freq freq) => LCType freq (a :+: b) where
    reifyType _ _ = case reifyType (undefined :: freq) (undefined :: b) of
        ZZ'         -> Tuple' [ty,ZZ']
        Tuple' l    -> Tuple' $ ty:l
      where
        ty = reifyType (undefined :: freq) (undefined :: a)
