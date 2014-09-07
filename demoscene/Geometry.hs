{-# LANGUAGE OverloadedStrings #-}
module Geometry where

import Data.ByteString.Char8 (ByteString)
import qualified Data.Trie as T
import qualified Data.Vector.Storable as SV

import LambdaCube.GL
import LambdaCube.GL.Mesh

-- Geometry for effects
quad :: Mesh
quad = Mesh
    { mAttributes   = T.singleton "position" $ A_V2F $ SV.fromList [V2 a b, V2 a a, V2 b a, V2 b a, V2 b b, V2 a b]
    , mPrimitive    = P_Triangles
    , mGPUData      = Nothing
    }
  where
    a = -1
    b = 1
