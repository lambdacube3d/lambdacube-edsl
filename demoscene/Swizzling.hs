{-# LANGUAGE OverloadedStrings, PackageImports, TypeOperators, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts #-}
module Swizzling where

import LC_API

-- Some basic swizzling

class PrjXY a b | a -> b where
    x_ :: a -> b
    y_ :: a -> b

class PrjXYZ a b c | a -> b, a -> c where
    xy_ :: a -> b
    z_  :: a -> c

class PrjXYZW a b c | a -> b, a -> c where
    xyz_ :: a -> b
    w_   :: a -> c

instance (GPU (V2 t), GPU t, IsComponent t) => PrjXY (Exp f (V2 t)) (Exp f t) where
    x_ a = let V2 x _ = unpack' a in x
    y_ a = let V2 _ y = unpack' a in y

instance (GPU (V3 t), GPU t, IsComponent t) => PrjXY (Exp f (V3 t)) (Exp f t) where
    x_ a = let V3 x _ _ = unpack' a in x
    y_ a = let V3 _ y _ = unpack' a in y

instance (GPU (V4 t), GPU t, IsComponent t) => PrjXY (Exp f (V4 t)) (Exp f t) where
    x_ a = let V4 x _ _ _ = unpack' a in x
    y_ a = let V4 _ y _ _ = unpack' a in y

instance (GPU (V3 t), GPU (V2 t), GPU t, IsComponent t) => PrjXYZ (Exp f (V3 t)) (Exp f (V2 t)) (Exp f t) where
    xy_ a   = let V3 x y _ = unpack' a in pack' $ V2 x y
    z_ a    = let V3 _ _ z = unpack' a in z

instance (GPU (V4 t), GPU (V2 t), GPU t, IsComponent t) => PrjXYZ (Exp f (V4 t)) (Exp f (V2 t)) (Exp f t) where
    xy_ a   = let V4 x y _ _ = unpack' a in pack' $ V2 x y
    z_ a    = let V4 _ _ z _ = unpack' a in z

instance (GPU (V4 t), GPU (V3 t), GPU t, IsComponent t) => PrjXYZW (Exp f (V4 t)) (Exp f (V3 t)) (Exp f t) where
    xyz_ a  = let V4 x y z _ = unpack' a in pack' $ V3 x y z
    w_ a    = let V4 _ _ _ w = unpack' a in w

