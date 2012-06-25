{-# LANGUAGE OverloadedStrings #-}
{-|

Functions for combining meshes.

-}

module MeshUtil (transformMesh,transformMesh',joinMesh) where

import qualified Data.Vector.Storable as SV

import qualified Data.Trie as T
import LC_API
import LC_Mesh

import Data.List
import Data.ByteString.Char8 (ByteString)
import Data.Vect.Float
import Data.Vect.Float.Util.Quaternion

-- make non indexed joinded mesh
{-
    validate: all mesh should have same primitive type
    transform attributes
    join primitives
-}
-- idea: separate mesh transform and mesh join operations, the later will be used as an optimization is LC

{-
  mapping:
    validate: all mesh should have same primitive type
    indexed and non indexed meshes can be mixed
    cases:
        indexed only     - keep indexed
        non indexed only - keep non indexed
        mixed   - can be convert all mesh to indexed or non indexed depends on how can we save more memory
-}

-- join a set of meshes with same primitive types and attribute set
joinMesh :: [Mesh] -> Mesh
joinMesh [] = error "joinMesh: no mesh to join!"
joinMesh ml@(Mesh a0 p0 _:_) = Mesh joinedAttrs joinedPrims Nothing
  where
    (attrs,prims)   = foldl' (\(a,p) (Mesh at pr _) -> (addAttr a at, pr:p)) (T.fromList $ zip (T.keys a0) (repeat []),[]) ml
    addAttr a at    = foldl' (\b (k,bt) -> T.adjust (bt:) k b) a $ T.toList at
    offset          = scanl (+) 0 [attrSize a | a <- head $ T.elems attrs]
    attrSize :: MeshAttribute -> Int32
    attrSize a = fromIntegral $ case a of
        A_Float v -> SV.length v
        A_V2F   v -> SV.length v
        A_V3F   v -> SV.length v
        A_V4F   v -> SV.length v
        A_M22F  v -> SV.length v
        A_M33F  v -> SV.length v
        A_M44F  v -> SV.length v
        A_Int   v -> SV.length v
        A_Word  v -> SV.length v
    joinedPrims = case p0 of
        P_TriangleStripI _  -> P_TriangleStripI $ SV.concat $ zipWith (\o v -> SV.map (+o) v) offset [v | P_TriangleStripI v <- prims]
        P_TrianglesI _      -> P_TrianglesI $ SV.concat $ zipWith (\o v -> SV.map (+o) v) offset [v | P_TrianglesI v <- prims]
        p                   -> p
    joinedAttrs = T.mapBy joinAttrs attrs
    joinAttrs n l = case head l of
        A_Float _ -> Just $ A_Float $ SV.concat [v | A_Float v <- l]
        A_V2F   _ -> Just $ A_V2F   $ SV.concat [v | A_V2F   v <- l]
        A_V3F   _ -> Just $ A_V3F   $ SV.concat [v | A_V3F   v <- l]
        A_V4F   _ -> Just $ A_V4F   $ SV.concat [v | A_V4F   v <- l]
        A_M22F  _ -> Just $ A_M22F  $ SV.concat [v | A_M22F  v <- l]
        A_M33F  _ -> Just $ A_M33F  $ SV.concat [v | A_M33F  v <- l]
        A_M44F  _ -> Just $ A_M44F  $ SV.concat [v | A_M44F  v <- l]
        A_Int   _ -> Just $ A_Int   $ SV.concat [v | A_Int   v <- l]
        A_Word  _ -> Just $ A_Word  $ SV.concat [v | A_Word  v <- l]

transformMesh :: U -> Vec3 -> Mesh -> Mesh
transformMesh o p = transformMesh' ((orthogonal $ rightOrthoU o) .*. translation p)

transformMesh' :: Proj4 -> Mesh -> Mesh
transformMesh' proj (Mesh attrs prim Nothing) = Mesh (T.mapBy go attrs) prim Nothing
  where
    go aName a
        | aName == "binormal"   = mapV3 rot a
        | aName == "normal"     = mapV3 rot a
        | aName == "position"   = mapV3 rotTrans a
        | aName == "tangent"    = mapV3 rot a
        | otherwise = Just a

    mapV3 f (A_V3F v) = Just $ A_V3F $ SV.map f v
    mapV3 _ _ = error " transformAttr: unknown attribute semantic!"

    mulProj4 :: Proj4 -> V3F -> V3F
    mulProj4 p (V3 x y z) = let Vec4 a b c _ = (Vec4 x y z 1) .* fromProjective p in V3 a b c
    rot      = mulProj4 (linear $ trim $ fromProjective proj :: Proj4)
    rotTrans = mulProj4 proj
