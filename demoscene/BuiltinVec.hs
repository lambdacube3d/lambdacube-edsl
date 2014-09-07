{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module BuiltinVec where

import LambdaCube.GL

class BuiltinVec4 v r stage s where
  vec4' :: (GPU v,
            IsVecScalar dim v s,
            GPU (V4 s),
            IsComponent s,
            GPU s) =>
           Exp stage v -> r

class BuiltinVec3 v r stage s where
  vec3' :: (GPU v,
            IsVecScalar dim v s,
            GPU (V3 s),
            IsComponent s,
            GPU s) =>
           Exp stage v -> r

class BuiltinVec2 v r stage s where
  vec2' :: (GPU v,
            IsVecScalar dim v s,
            GPU (V2 s),
            IsComponent s,
            GPU s) =>
           Exp stage v -> r

-- 3 1
instance BuiltinVec4 (V3 s) (Exp stage s -> Exp stage (V4 s)) stage s where
  vec4' xyz w = pack' $ V4 x y z w
    where V3 x y z = unpack' xyz

-- 2 2
instance BuiltinVec4 (V2 s) (Exp stage (V2 s) -> Exp stage (V4 s)) stage s where
  vec4' xy zw = pack' $ V4 x y z w
    where V2 x y = unpack' xy
          V2 z w = unpack' zw

-- 2 1 1
instance BuiltinVec4 (V2 s) (Exp stage s -> Exp stage s -> Exp stage (V4 s)) stage s where
  vec4' xy z w = pack' $ V4 x y z w
    where V2 x y = unpack' xy

-- 1 3
instance (GPU (V3 s)) => BuiltinVec4 s (Exp stage (V3 s) -> Exp stage (V4 s)) stage s where
  vec4' x yzw = pack' $ V4 x y z w
    where V3 y z w = unpack' yzw

-- 1 2 1
instance (GPU (V2 s)) => BuiltinVec4 s (Exp stage (V2 s) -> Exp stage s -> Exp stage (V4 s)) stage s where
  vec4' x yz w = pack' $ V4 x y z w
    where V2 y z = unpack' yz

-- 1 1 2
instance (GPU (V2 s)) => BuiltinVec4 s (Exp stage s -> Exp stage (V2 s) -> Exp stage (V4 s)) stage s where
  vec4' x y zw = pack' $ V4 x y z w
    where V2 z w = unpack' zw

-- 1 1 1 1
instance (GPU (V2 s)) => BuiltinVec4 s (Exp stage s -> Exp stage s -> Exp stage s -> Exp stage (V4 s)) stage s where
  vec4' x y z w = pack' $ V4 x y z w

-- 2 1
instance BuiltinVec3 (V2 s) (Exp stage s -> Exp stage (V3 s)) stage s where
  vec3' xy z = pack' $ V3 x y z
    where V2 x y = unpack' xy

-- 1 2
instance (GPU (V2 s)) => BuiltinVec3 s (Exp stage (V2 s) -> Exp stage (V3 s)) stage s where
  vec3' x yz = pack' $ V3 x y z
    where V2 y z = unpack' yz

-- 1 1 1
instance BuiltinVec3 s (Exp stage s -> Exp stage s -> Exp stage (V3 s)) stage s where
  vec3' x y z = pack' $ V3 x y z

-- 1 1
instance BuiltinVec2 s (Exp stage s -> Exp stage (V2 s)) stage s where
  vec2' x y = pack' $ V2 x y
