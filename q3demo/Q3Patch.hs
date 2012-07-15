-- http://graphics.cs.brown.edu/games/quake/quake3.html#RenderPatch

module Q3Patch where

import Control.Monad
import Data.Vect.Float hiding (Vector)
import Data.Vect.Float.Instances
import Data.Vector (Vector,(!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import BSP

tessellate :: Vector DrawVertex -> Int -> (Vector DrawVertex,Vector Int)
tessellate controls level = (v,stripsI)
  where
    plus (DrawVertex p1 d1 l1 n1 c1) (DrawVertex p2 d2 l2 n2 c2) = DrawVertex (p1 + p2) (d1 + d2) (l1 + l2) (n1 + n2) (c1 + c2)
    mult (DrawVertex p d l n c) f = DrawVertex (p &* f) (d &* f) (l &* f) (n &* f) (c &* f)
    mix a c0 c1 c2 = let b = 1 - a in (c0 `mult` (b * b)) `plus` (c1 `mult` (2 * b * a)) `plus` (c2 `mult` (a * a))
    l1 = level + 1
    v = V.create $ do
        vertex <- MV.new (l1*l1)
        forM_ [0..level] $ \i -> let a = fromIntegral i / fromIntegral level in MV.write vertex i $ mix a (controls ! 0) (controls ! 3) (controls ! 6)
        forM_ [1..level] $ \i -> do
            let a = fromIntegral i / fromIntegral level
                c0 = mix a (controls ! 0) (controls ! 1) (controls ! 2)
                c1 = mix a (controls ! 3) (controls ! 4) (controls ! 5)
                c2 = mix a (controls ! 6) (controls ! 7) (controls ! 8)
            forM_ [0..level] $ \j -> let a' = fromIntegral j / fromIntegral level in MV.write vertex (i * l1 + j) $ mix a' c0 c1 c2
        return vertex
    -- merge triangle strips using degenerate triangles
    idx row col2 | col2 `mod` 2 == 1 = (row + 1) * l1 + col2 `div` 2
                 | otherwise         = row * l1 + col2 `div` 2
    strips = [V.generate (l1*2) (idx row) | row <- [0..level-1]]
    separate (a:b:c:xs) = a:b:c:separate (b:c:xs)
    separate [] = []
    trisI   = V.concat [V.fromList $ separate $ V.toList s | s <- strips]
    stripsI = V.concat [V.concat [h,s,l] | s <- strips  -- concatenated triangle strips using degenerated triangles
                       , let h = V.singleton $ V.head s -- degenerate triangles will be shown in line polygon mode
                       , let l = V.singleton $ V.last s
                       ]
{-
tess c l = [f u v | v <- [0..l], u <- [0..l]]

mo = Mat3 (Vec3   1 (-2) 1)
          (Vec3 (-2)  2  0)
          (Vec3   1   0  0)
------------------------
m = Mat3 (Vec3   1   0  0)
         (Vec3 (-2)  2  0)
         (Vec3   1 (-2) 1)

m' = transpose m

cm c f = Mat3 (Vec3 (a 0) (a 1) (a 2))
              (Vec3 (a 3) (a 4) (a 5))
              (Vec3 (a 6) (a 7) (a 8))
  where  a n = f $ c ! n

fn u v c f = Vec3 1 u u^2 *. m .*. cm c f .*. m' .* Vec3 1 v v^2
p u v c = Vec3 (fn u v c _1) (fn u v c _2) (fn u v c _3)
tess c l = [p u v c | u <- [0..l], v <- [0..l]]
-}
