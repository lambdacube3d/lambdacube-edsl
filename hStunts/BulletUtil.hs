{-# LANGUAGE OverloadedStrings #-}
module BulletUtil where

import Data.ByteString.Char8 (ByteString)
import Physics.Bullet.Raw
import Physics.Bullet.Raw.Class
import qualified Data.Trie as T
import LambdaCube.GL
import LambdaCube.GL.Mesh
import qualified Data.Vector.Storable as V
import Data.List
import Data.Vector.Storable ((!))
import Control.Monad
import Data.Vect.Float

-- FIXME: add support for non indexed mesh type
mkTriangleMeshInterface :: [Mesh] -> IO BtTriangleMesh
mkTriangleMeshInterface ml = do
    mi <- btTriangleMesh True True
    forM_ ml $ \(Mesh attrs prim _) -> do
        let pos = case T.lookup "position" attrs of
                Just (A_V3F v) -> v
                _              -> error "Mesh attribute semantic mismatch!"
            f n                 = let V3 x y z = pos ! (fromIntegral n) in Vec3 x y z
            addStrip (a,b) i    = btTriangleMesh_addTriangle mi (f a) (f b) (f i) False >> return (b,i)
            addFan a b i        = btTriangleMesh_addTriangle mi (f a) (f b) (f i) False >> return i
            dummyIndex          = V.generate (V.length pos) id
            triangles idx       = do
                let split3 v | V.length v < 3 = Nothing
                             | otherwise      = Just (V.take 3 v, V.drop 3 v)
                forM_ (unfoldr split3 idx) $ \iv -> btTriangleMesh_addTriangle mi (f (iv ! 0)) (f (iv ! 1)) (f (iv ! 2)) False
            triangleStrip idx   = void $ let ab = V.take 2 idx in V.foldM' addStrip (ab ! 0, ab ! 1) $ V.drop 2 idx
        case prim of
            P_TriangleStrip     -> triangleStrip dummyIndex
            P_TriangleStripI i  -> triangleStrip i
            P_Triangles         -> triangles dummyIndex
            P_TrianglesI i      -> triangles i
            _ -> return ()
    return mi

mkStaticTriangleMeshShape :: [Mesh] -> IO BtBvhTriangleMeshShape
mkStaticTriangleMeshShape mesh = do
    mi <- mkTriangleMeshInterface mesh
    btBvhTriangleMeshShape0 mi True True

mkConvexTriangleMeshShape :: [Mesh] -> IO BtConvexTriangleMeshShape
mkConvexTriangleMeshShape mesh = do
    mi <- mkTriangleMeshInterface mesh
    btConvexTriangleMeshShape mi True
