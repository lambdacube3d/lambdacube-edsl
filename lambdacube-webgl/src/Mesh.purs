module Mesh where

import Control.Monad.Eff
import Control.Monad.Eff.Exception

import Data.StrMap (StrMap(..))
import Data.Maybe

import IR
import Type

data MeshAttribute
    = A_Float   [Float]
    | A_V2F     [V2F]
    | A_V3F     [V3F]
    | A_V4F     [V4F]
    | A_M22F    [M22F]
    | A_M33F    [M33F]
    | A_M44F    [M44F]
    | A_Int     [Int32]
    | A_Word    [Word32]

data MeshPrimitive
    = P_Points
    | P_TriangleStrip
    | P_Triangles
    | P_TriangleStripI  [Int32]
    | P_TrianglesI      [Int32]

type Mesh =
    { attributes  :: StrMap MeshAttribute
    , primitive   :: MeshPrimitive
    , gpuData     :: Maybe GPUData
    }

type GPUData =
    { primitive :: Primitive
    , streams   :: StrMap (Stream Buffer)
    , indices   :: Maybe (IndexStream Buffer)
    }

addMesh :: WebGLPipelineInput -> String -> Mesh -> [String] -> GFX GLObject
addMesh _ _ _ _ = throwException $ error "not implemented"

compileMesh :: Mesh -> GFX Mesh
compileMesh _ = throwException $ error "not implemented"
