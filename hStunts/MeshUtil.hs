{-# LANGUAGE OverloadedStrings #-}
{-|

Functions for combining meshes.

-}

module MeshUtil (transformMesh,transformMesh') where

import qualified Data.Vector.Storable as SV

import qualified Data.Trie as T
import LCAPI
import LCMesh

import Data.ByteString.Char8 (ByteString)
import Data.Vect.Float
import Data.Vect.Float.Util.Quaternion
{-
type MeshGroup = (String,VVB,Maybe VIB,Proj4,OperationType)

-- | Build a single mesh that represents the union of a list of
-- transformed meshes (given the orientation and translation for each
-- constituent).  The resulting mesh is optimised with respect to
-- context switches during rendering.
mkMesh :: [(U,Vec3,Mesh)] -> Mesh
mkMesh vml = mkVMesh' [((orthogonal $ rightOrthoU o) .*. translation p,m) | (o,p,m) <- vml]

-- | Build a single mesh that represents the union of a list of
-- transformed meshes (given the transformation matrix for each
-- constituent).  The resulting mesh is optimised with respect to
-- context switches during rendering.

-- FIXME: problem shared geom vs private geom
vertexData :: [(Proj4,VMesh)] -> [MeshGroup]
vertexData l =
    [ (materialName, sortedVData gData vData, iData, proj, opType)
    | (proj, VMesh subMeshList gData) <- l
    , VSubMesh materialName opType vData iData <- subMeshList
    ]
  where
    sortedVData local global = V.modify (V.sortBy (comparing vectorVertexType)) $ case (local,global) of
        (Just a, _) -> a
        (Nothing, Just a) -> a
        _ -> error "illegal mesh format"

groupByMaterial :: [MeshGroup] -> [[MeshGroup]]
groupByMaterial = groupSetBy (\(a,_,_,_,_) (b,_,_,_,_) -> compare a b)

groupByGeometry :: [[MeshGroup]] -> [[MeshGroup]]
groupByGeometry l = groupSetBy compareMeshItem =<< l
  where
    compareMeshItem (_,_,Just _,_,_) (_,_,Nothing,_,_) = GT
    compareMeshItem (_,_,Nothing,_,_) (_,_,Just _,_,_) = LT
    compareMeshItem (_,a1,_,_,a2) (_,b1,_,_,b2) = compare (V.map vectorVertexType a1, a2) (V.map vectorVertexType b1, b2)

joinGroup :: [MeshGroup] -> VSubMesh
joinGroup groupMeshList = VSubMesh materialName operationType joinedVertexData joinedIndexData
  where
    (materialName,_,indexData,_,operationType) = head groupMeshList
    vertexDataList :: [[(VectorVertexData,Proj4)]]
    vertexDataList = [[(v,proj) | v <- V.toList vd] | (_,vd,_,proj,_) <- groupMeshList]

    joinedIndexData = case indexData of
        Nothing -> Nothing
        Just _  -> let indexDataList = [fromJust id | (_,_,id,_,_) <- groupMeshList]
                       offsets = scanl (+) 0 [V.length v | a <- vertexDataList, (VVD_POSITION v,_) <- a]
                   in Just $ V.concat $ zipWith (\o v -> V.map (+o) v) offsets indexDataList

    joinedVertexData :: Maybe VVB
    joinedVertexData = Just $ V.fromList $ map mergeAttribs $ transpose vertexDataList

mergeAttribs :: [(VectorVertexData, Proj4)] -> VectorVertexData
mergeAttribs ca = case vectorVertexType $ (fst (head ca)) of
    VVT_BINORMAL ->             VVD_BINORMAL $             V.concat [rot proj v      | (VVD_BINORMAL v,proj) <- ca]
    VVT_BLEND_INDICES ->        VVD_BLEND_INDICES $        V.concat [v               | (VVD_BLEND_INDICES v,_proj) <- ca]
    VVT_BLEND_WEIGHTS ->        VVD_BLEND_WEIGHTS $        V.concat [v               | (VVD_BLEND_WEIGHTS v,_proj) <- ca]
    VVT_DIFFUSE ->              VVD_DIFFUSE $              V.concat [v               | (VVD_DIFFUSE v,_proj) <- ca]
    VVT_NORMAL ->               VVD_NORMAL $               V.concat [rot proj v      | (VVD_NORMAL v,proj) <- ca]
    VVT_POSITION ->             VVD_POSITION $             V.concat [rotTrans proj v | (VVD_POSITION v,proj) <- ca]
    VVT_SPECULAR ->             VVD_SPECULAR $             V.concat [v               | (VVD_SPECULAR v,_proj) <- ca]
    VVT_TANGENT ->              VVD_TANGENT $              V.concat [rot proj v      | (VVD_TANGENT v,proj) <- ca]
    VVT_TEXTURE_COORDINATES1 -> VVD_TEXTURE_COORDINATES1 $ V.concat [v               | (VVD_TEXTURE_COORDINATES1 v,_proj) <- ca]
    VVT_TEXTURE_COORDINATES2 -> VVD_TEXTURE_COORDINATES2 $ V.concat [v               | (VVD_TEXTURE_COORDINATES2 v,_proj) <- ca]
    VVT_TEXTURE_COORDINATES3 -> VVD_TEXTURE_COORDINATES3 $ V.concat [v               | (VVD_TEXTURE_COORDINATES3 v,_proj) <- ca]
  where
    mulProj4 :: Proj4 -> Vec3 -> Vec3
    mulProj4 p v = trim ((extendWith 1 v :: Vec4) .* fromProjective p)
    rot proj v = V.map (mulProj4 proj') v
      where
        proj' = linear $ trim $ fromProjective proj :: Proj4
    rotTrans proj v = V.map (mulProj4 proj) v
-}
{-
data MeshAttribute
    = A_Float   (V.Vector Float)
    | A_V2F     (V.Vector V2F)
    | A_V3F     (V.Vector V3F)
    | A_V4F     (V.Vector V4F)
    | A_M22F    (V.Vector M22F)
    | A_M33F    (V.Vector M33F)
    | A_M44F    (V.Vector M44F)
    | A_Int     (V.Vector Int32)
    | A_Word    (V.Vector Word32)

data MeshPrimitive
    = P_Points
    | P_TriangleStrip
    | P_Triangles
    | P_TriangleStripI  (V.Vector Int)
    | P_TrianglesI      (V.Vector Int)

data Mesh
    = Mesh
    { mAttributes   :: T.Trie MeshAttribute
    , mPrimitive    :: MeshPrimitive
    , mGPUData      :: Maybe GPUData
    }
-}
{-
    group by primitive type
    handle both indexed and non indexed meshes
-}
{-
mkMesh' :: [(Proj4,Mesh)] -> [(Primitive,Mesh)]
mkMesh' vml = Mesh [joinGroup g | g <- groupByGeometry $ groupByMaterial $ vertexData vml] Nothing
-}
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
joinMesh :: [Mesh] -> Mesh
joinMesh = undefined

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
