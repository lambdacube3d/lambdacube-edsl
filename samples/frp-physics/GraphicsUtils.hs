{-# LANGUAGE OverloadedStrings, NamedFieldPuns, ParallelListComp, DataKinds #-}

module GraphicsUtils where

import Data.Bits
import qualified Data.ByteString.Char8 as SB
import qualified Data.Trie as T
import Data.Vect
import qualified Data.Vector.Storable as V
import FRP.Elerea.Param

import LC_API
import LC_Mesh

complexMesh :: [(Proj4, Mesh)] -> Mesh
complexMesh parts = Mesh
    { mAttributes = T.fromList [("position", A_V3F vertices), ("normal", A_V3F normals)]
    , mPrimitive = P_Triangles
    , mGPUData = Nothing
    }
  where
    vertices = V.concat partVertices
    normals = V.concat partNormals
    
    (partVertices, partNormals) = unzip [(getV3F trans "position" attr 1, getV3F (transpose (inverse trans)) "normal" attr 0) |
                                         (trans, mesh) <- parts, let attr = mAttributes (unrollIndices mesh)]
    getV3F trans name attributes w = V.map transform vector
      where
        transform v = fromVec3 (trim ((extendWith w (toVec3 v) :: Vec4) .* fromProjective trans))
        Just (A_V3F vector) = T.lookup name attributes

quad :: Mesh
quad = Mesh
    { mAttributes = T.singleton "position" $ A_V2F $ V.fromList [-1 ^ 1, -1 ^ -1, 1 ^ -1, 1 ^ -1, 1 ^ 1, -1 ^ 1]
    , mPrimitive = P_Triangles
    , mGPUData = Nothing
    }
  where
    infixr 0 ^
    (^) = V2

cube :: Float -> Mesh
cube size = box (Vec3 size size size) 

box :: Vec3 -> Mesh
box (Vec3 scaleX scaleY scaleZ) = addFlatNormals $ Mesh
    { mAttributes = T.singleton "position" (A_V3F vertices)
    , mPrimitive = P_Triangles
    , mGPUData = Nothing
    }
  where
    quads = [[6, 2, 3, 7], [5, 1, 0, 4], [7, 3, 1, 5], [4, 0, 2, 6], [3, 2, 0, 1], [6, 7, 5, 4]]
    indices = V.fromList $ concat [[a, b, c, c, d, a] | [d, c, b, a] <- quads]
    vertices = V.backpermute (V.generate 8 mkVertex) indices
    
    mkVertex n = V3 x y z
      where
        x = if testBit n 2 then scaleX else -scaleX
        y = if testBit n 1 then scaleY else -scaleY
        z = if testBit n 0 then scaleZ else -scaleZ

capsule :: Float -> Float -> Int -> Mesh
capsule radius height n = complexMesh
                          [ (idmtx, cylinderLateralArea height' radius (n * 2))
                          , (translation (Vec3 0 (-height') 0), halfSphere radius n)
                          , (scaling (Vec3 (-1) (-1) 1) .*. translation (Vec3 0 height' 0), halfSphere radius n)
                          ]
  where
    height' = height / 2

halfSphere :: Float -> Int -> Mesh
halfSphere radius n = Mesh
    { mAttributes = T.fromList [("position", A_V3F vertices), ("normal", A_V3F normals)]
    , mPrimitive = P_TrianglesI indices
    , mGPUData = Nothing
    }
  where
    m = pi / fromIntegral n
    vertices = V.map (\(V3 x y z) -> V3 (radius * x) (radius * y) (radius * z)) normals
    normals = V.fromList [V3 (sin a * cos b) (cos a) (sin a * sin b) | i <- [0..n], j <- [0..2 * n - 1],
                          let a = fromIntegral i * m * 0.5 + 0.5 * pi, let b = fromIntegral j * m]
    indices = V.fromList $ concat [[ix i j, ix i' j, ix i' j', ix i' j', ix i j', ix i j] | i <- [0..n - 1], j <- [0..2 * n - 1],
                                   let i' = i + 1, let j' = (j + 1) `mod` (2 * n)]
    ix i j = fromIntegral (i * 2 * n + j)

sphere :: Float -> Int -> Mesh
sphere radius n = Mesh
    { mAttributes = T.fromList [("position", A_V3F vertices), ("normal", A_V3F normals)]
    , mPrimitive = P_TrianglesI indices
    , mGPUData = Nothing
    }
  where
    m = pi / fromIntegral n
    vertices = V.map (\(V3 x y z) -> V3 (radius * x) (radius * y) (radius * z)) normals
    normals = V.fromList [V3 (sin a * cos b) (cos a) (sin a * sin b) | i <- [0..n], j <- [0..2 * n - 1],
                          let a = fromIntegral i * m, let b = fromIntegral j * m]
    indices = V.fromList $ concat [[ix i j, ix i' j, ix i' j', ix i' j', ix i j', ix i j] | i <- [0..n - 1], j <- [0..2 * n - 1],
                                   let i' = i + 1, let j' = (j + 1) `mod` (2 * n)]
    ix i j = fromIntegral (i * 2 * n + j)

cylinder :: Float -> Float -> Int -> Mesh
cylinder height radius n = complexMesh
                           [ (idmtx, cylinderLateralArea height radius n)
                           , (translation (Vec3 0 height 0), regularPolygon radius n)
                           , (scaling (Vec3 1 (-1) 1) .*. translation (Vec3 0 (-height) 0), regularPolygon radius n)
                           ]

regularPolygon :: Float -> Int -> Mesh
regularPolygon radius n = Mesh
    { mAttributes = T.fromList [("position", A_V3F vertices), ("normal", A_V3F normals)]
    , mPrimitive = P_TrianglesI indices
    , mGPUData = Nothing
    }
  where
    vertices = V.cons (V3 0 0 0) (V.generate n mkVertex)
    normals = V.replicate (n + 1) (V3 0 1 0)
    indices = V.map fromIntegral . V.fromList $ concat [[0, i, i `mod` n + 1] | i <- [1..n]]
    mkVertex i = V3 (radius * cos t) 0 (radius * sin t)
      where
        t = fromIntegral i * 2 * pi / fromIntegral n

cylinderLateralArea :: Float -> Float -> Int -> Mesh
cylinderLateralArea height radius n = Mesh
    { mAttributes = T.fromList [("position", A_V3F vertices), ("normal", A_V3F normals)]
    , mPrimitive = P_TrianglesI indices
    , mGPUData = Nothing
    }
  where
    ts = V.generate n (\t -> fromIntegral t * 2 * pi / fromIntegral n)
    ts' = ts V.++ ts
    xs = V.map cos ts'
    ys = V.replicate n height V.++ V.replicate n (-height)
    zs = V.map sin ts'
    is = [t `mod` n | t <- [0..n]]
    vertices = V.zipWith3 (\x y z -> V3 (radius*x) y (radius*z)) xs ys zs
    normals = V.zipWith3 V3 xs (V.replicate (n*2) 0) zs
    indices = V.fromList (map fromIntegral (concat [[i,i+n,i'+n,i'+n,i',i] | i <- is | i' <- tail is]))

addFlatNormals :: Mesh -> Mesh
addFlatNormals mesh@Mesh { mAttributes, mPrimitive = P_Triangles } =
    mesh { mAttributes = T.insert "normal" (A_V3F normals) mAttributes } 
  where
    Just (A_V3F positions) = T.lookup "position" mAttributes
    normals = V.concatMap mkNormal (V.generate (V.length positions `div` 3) id)
    mkNormal i = V.replicate 3 (fromVec3 (normalize ((p3 &- p2) &^ (p2 &- p1))))
      where
        p1 = toVec3 (positions V.! (i*3))
        p2 = toVec3 (positions V.! (i*3 + 1))
        p3 = toVec3 (positions V.! (i*3 + 2))
addFlatNormals mesh@Mesh { mPrimitive = P_TrianglesI indices } = addFlatNormals (unrollIndices mesh)
addFlatNormals _ = error "addFlatNormals: unsupported primitive type"

unrollIndices :: Mesh -> Mesh
unrollIndices mesh@Mesh { mAttributes, mPrimitive = P_Triangles } = mesh
unrollIndices mesh@Mesh { mAttributes, mPrimitive = P_TrianglesI indices } =
    mesh { mAttributes = fmap (unrollAttribute indices') mAttributes, mPrimitive = P_Triangles }
  where
    indices' = V.map fromIntegral indices
unrollIndices _ = error "unrollIndices: unsupported primitive type"

unrollAttribute :: V.Vector Int -> MeshAttribute -> MeshAttribute
unrollAttribute indices attribute = case attribute of
    A_V3F vs -> A_V3F (V.backpermute vs indices)
    _        -> error "unrollAttribute: unsupported attribute type"

toVec3 :: V3F -> Vec3
toVec3 (V3 x y z) = Vec3 x y z

fromVec3 :: Vec3 -> V3F
fromVec3 (Vec3 x y z) = V3 x y z

fromVec4 :: Vec4 -> V4F
fromVec4 (Vec4 x y z w) = V4 x y z w

fromMat4 :: Mat4 -> M44F
fromMat4 (Mat4 a b c d) = V4 (fromVec4 a) (fromVec4 b) (fromVec4 c) (fromVec4 d)

snoc :: Exp s V3F -> Exp s Float -> Exp s V4F
snoc v w = pack' (V4 x y z w)
  where 
    V3 x y z = unpack' v

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
