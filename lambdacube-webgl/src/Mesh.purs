module Mesh where

import Control.Monad.Eff
import Control.Monad.Eff.Exception

import qualified Data.StrMap as StrMap
import Data.Maybe
import Data.Array
import Data.Tuple

import IR
import Type
import Data
import Input

data MeshAttribute
    = A_Float   [Float]
    | A_V2F     [V2F]
    | A_V3F     [V3F]
    | A_V4F     [V4F]
    | A_M22F    [M22F]
    | A_M33F    [M33F]
    | A_M44F    [M44F]

data MeshPrimitive
    = P_Points
    | P_TriangleStrip
    | P_Triangles
    | P_TriangleStripI  [Word16]
    | P_TrianglesI      [Word16]

type Mesh =
    { attributes  :: StrMap.StrMap MeshAttribute
    , primitive   :: MeshPrimitive
    , gpuData     :: Maybe GPUData
    }

type GPUData =
    { primitive :: Primitive
    , streams   :: StrMap.StrMap (Stream Buffer)
    , indices   :: Maybe (IndexStream Buffer)
    }

addMesh :: WebGLPipelineInput -> String -> Mesh -> [String] -> GFX GLObject
addMesh input slotName mesh objUniNames = case mesh.gpuData of
  Nothing -> throwException $ error "addMesh: only compiled mesh with GPUData is supported"
  Just g -> case StrMap.lookup slotName input.schema.slots of
    Nothing -> throwException $ error "addMesh: slot not found"
    Just slotSchema -> do
      -- select proper attributes
      let filterStream (Tuple n s) = StrMap.member n slotSchema.attributes
      addObject input slotName g.primitive g.indices (StrMap.fromList $ filter filterStream $ StrMap.toList g.streams) objUniNames

compileMesh :: Mesh -> GFX Mesh
compileMesh mesh = case mesh.gpuData of
  Just _ -> return mesh
  Nothing -> do
    let mkIndexBuf v = do
            iBuf <- compileBuffer [Array ArrWord16 (length v) {}]
            return $ Just {buffer: iBuf, arrIdx: 0, start: 0, length: length v}
    vBuf <- compileBuffer $ map meshAttrToArray (StrMap.values mesh.attributes)
    Tuple prim indices <- case mesh.primitive of
        P_Points            -> return $ Tuple PointList     Nothing
        P_TriangleStrip     -> return $ Tuple TriangleStrip Nothing
        P_Triangles         -> return $ Tuple TriangleList  Nothing
        P_TriangleStripI v  -> Tuple TriangleStrip <$> mkIndexBuf v
        P_TrianglesI v      -> Tuple TriangleList <$> mkIndexBuf v
    let streams = StrMap.fromList $ zipWith (\i (Tuple n a) -> Tuple n (meshAttrToStream vBuf i a)) (0..StrMap.size mesh.attributes) (StrMap.toList mesh.attributes)
        gpuData = {primitive: prim, streams: streams, indices: indices}
    return $ mesh {gpuData = Just gpuData}

meshAttrToArray :: MeshAttribute -> LCArray
meshAttrToArray a = case a of
  A_Float v -> Array ArrFloat  (1 *  length v) {}
  A_V2F   v -> Array ArrFloat  (2 *  length v) {}
  A_V3F   v -> Array ArrFloat  (3 *  length v) {}
  A_V4F   v -> Array ArrFloat  (4 *  length v) {}
  A_M22F  v -> Array ArrFloat  (4 *  length v) {}
  A_M33F  v -> Array ArrFloat  (9 *  length v) {}
  A_M44F  v -> Array ArrFloat  (16 * length v) {}

meshAttrToStream :: Buffer -> Int -> MeshAttribute -> Stream Buffer
meshAttrToStream b i a = Stream $ case a of
  A_Float v -> {sType: TFloat, buffer: b, arrIdx: i , start: 0, length: length v}
  A_V2F   v -> {sType: TV2F  , buffer: b, arrIdx: i , start: 0, length: length v}
  A_V3F   v -> {sType: TV3F  , buffer: b, arrIdx: i , start: 0, length: length v}
  A_V4F   v -> {sType: TV4F  , buffer: b, arrIdx: i , start: 0, length: length v}
  A_M22F  v -> {sType: TM22F , buffer: b, arrIdx: i , start: 0, length: length v}
  A_M33F  v -> {sType: TM33F , buffer: b, arrIdx: i , start: 0, length: length v}
  A_M44F  v -> {sType: TM44F , buffer: b, arrIdx: i , start: 0, length: length v}
