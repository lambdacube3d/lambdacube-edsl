{-# LANGUAGE OverloadedStrings #-}

module GraphicsUtils where

import Data.Bits
import qualified Data.ByteString.Char8 as SB
import qualified Data.Trie as T
import Data.Vect
import qualified Data.Vector.Storable as V
import FRP.Elerea.Param

import LC_API
import LC_Mesh

quad :: Mesh
quad = Mesh
    { mAttributes = T.singleton "position" $ A_V2F $ V.fromList [-1 ^ 1, -1 ^ -1, 1 ^ -1, 1 ^ -1, 1 ^ 1, -1 ^ 1]
    , mPrimitive = P_Triangles
    , mGPUData = Nothing
    }
  where
    infixr 0 ^
    (^) = V2

cube :: Mesh
cube = Mesh
    { mAttributes = T.fromList [("position", A_V3F vertices), ("normal", A_V3F normals)]
    , mPrimitive = P_Triangles
    , mGPUData = Nothing
    }
  where
    quads = [[6, 2, 3, 7], [5, 1, 0, 4], [7, 3, 1, 5], [4, 0, 2, 6], [3, 2, 0, 1], [6, 7, 5, 4]]
    indices = V.fromList $ concat [[a, b, c, c, d, a] | [d, c, b, a] <- quads]
    vertices = V.backpermute (V.generate 8 mkVertex) indices
    normals = V.concatMap (V.replicate 6) (V.generate 6 mkNormal)
    
    mkVertex n = V3 x y z
      where
        x = if testBit n 2 then 1 else -1
        y = if testBit n 1 then 1 else -1
        z = if testBit n 0 then 1 else -1
        
    mkNormal n = fromVec3 (normalize ((v3 &- v2) &^ (v2 &- v1)))
      where
        i = n * 6
        v1 = toVec3 (vertices V.! i)
        v2 = toVec3 (vertices V.! (i + 1))
        v3 = toVec3 (vertices V.! (i + 2))

toVec3 :: V3F -> Vec3
toVec3 (V3 x y z) = Vec3 x y z

fromVec3 :: Vec3 -> V3F
fromVec3 (Vec3 x y z) = V3 x y z

fromVec4 :: Vec4 -> V4F
fromVec4 (Vec4 x y z w) = V4 x y z w

fromMat4 :: Mat4 -> M44F
fromMat4 (Mat4 a b c d) = V4 (fromVec4 a) (fromVec4 b) (fromVec4 c) (fromVec4 d)

v3v4 :: Exp s V3F -> Exp s V4F
v3v4 v = pack' (V4 x y z (Const 1))
  where 
    V3 x y z = unpack' v

n3v4 :: Exp s V3F -> Exp s V4F
n3v4 v = pack' (V4 x y z (Const 0))
  where 
    V3 x y z = unpack' v

v4v3 :: Exp s V4F -> Exp s V3F
v4v3 v = pack' (V3 x y z)
  where
    V4 x y z _ = unpack' v

floatV :: Float -> Exp V Float
floatV = Const

floatF :: Float -> Exp F Float
floatF = Const

intF :: Int32 -> Exp F Int32
intF = Const

-- | Perspective transformation matrix in row major order.
perspective :: Float  -- ^ Near plane clipping distance (always positive).
            -> Float  -- ^ Far plane clipping distance (always positive).
            -> Float  -- ^ Field of view of the y axis, in radians.
            -> Float  -- ^ Aspect ratio, i.e. screen's width\/height.
            -> Mat4
perspective n f fovy aspect = transpose $
    Mat4 (Vec4 (2*n/(r-l))       0       (-(r+l)/(r-l))        0)
         (Vec4     0        (2*n/(t-b))  ((t+b)/(t-b))         0)
         (Vec4     0             0       (-(f+n)/(f-n))  (-2*f*n/(f-n)))
         (Vec4     0             0            (-1)             0)
  where
    t = n*tan(fovy/2)
    b = -t
    r = aspect*t
    l = -r

-- | Pure orientation matrix defined by Euler angles.
rotationEuler :: Vec3 -> Proj4
rotationEuler (Vec3 a b c) = orthogonal $ toOrthoUnsafe $ rotMatrixY a .*. rotMatrixX b .*. rotMatrixZ c

-- | Camera transformation matrix.
lookat :: Vec3   -- ^ Camera position.
       -> Vec3   -- ^ Target position.
       -> Vec3   -- ^ Upward direction.
       -> Proj4
lookat pos target up = translateBefore4 (neg pos) (orthogonal $ toOrthoUnsafe r)
  where
    w = normalize $ pos &- target
    u = normalize $ up &^ w
    v = w &^ u
    r = transpose $ Mat3 u v w

-- | Continuous camera state (rotated with mouse, moved with arrows)
userCamera :: Vec3 -> Signal Vec2 -> Signal (Bool, Bool, Bool, Bool, Bool) -> SignalGen Float (Signal (Vec3, Vec3, Vec3, Vec2))
userCamera startPosition mouseDelta directionKeys = transfer2 (startPosition, zero, zero, zero) calcCam mouseDelta directionKeys
  where
    d0 = Vec4 0 0 (-1) 1
    u0 = Vec4 0 1 0 1
    calcCam dt dm (ka, kw, ks, kd, turbo) (p0, _, _, m) = (p', d, u, m')
      where
        f0 c v = if c then v else zero
        p' = p0 &+ (f0 kw d &- f0 ks d &+ f0 kd v &- f0 ka v) &* (realToFrac dt * if turbo then 5 else 1)
        m' = dm &+ m
        rm = fromProjective $ rotationEuler $ extendZero (m' &* 0.01)
        d  = trim (rm *. d0) :: Vec3
        u  = trim (rm *. u0) :: Vec3
        v  = normalize (d &^ u)
