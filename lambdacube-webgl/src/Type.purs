module Type where

import Data.StrMap (StrMap(..))
import qualified Graphics.WebGLRaw as GL

import IR

type Buffer = -- internal type
    { bufArrays :: [ArrayDesc]
    , bufGLObj  :: GL.WebGLBuffer
    }

type ArrayDesc =
    { arrType   :: ArrayType
    , arrLength :: Int  -- item count
    , arrOffset :: Int  -- byte position in buffer
    , arrSize   :: Int  -- size in bytes
    }

data ArrayType
    = ArrWord8
    | ArrWord16
    | ArrInt8
    | ArrInt16
    | ArrFloat
    | ArrFixed

sizeOfArrayType :: ArrayType -> Int
sizeOfArrayType ArrWord8  = 1
sizeOfArrayType ArrWord16 = 2
sizeOfArrayType ArrInt8   = 1
sizeOfArrayType ArrInt16  = 2
sizeOfArrayType ArrFloat  = 4
sizeOfArrayType ArrFixed  = 4

{-
-- describes an array in a buffer
data Array  -- array type, element count (NOT byte size!), setter
    = Array ArrayType Int BufferSetter
-}
type BufferSetter = {} --(Ptr () -> IO ()) -> IO ()

data StreamType
    = TFloat
    | TV2F
    | TV3F
    | TV4F
    | TM22F
    | TM33F
    | TM44F

data Stream b
    = ConstFloat Float
    | ConstV2F   V2F
    | ConstV3F   V3F
    | ConstV4F   V4F
    | ConstM22F  M22F
    | ConstM33F  M33F
    | ConstM44F  M44F
    | Stream 
        { streamType    :: StreamType
        , streamBuffer  :: b
        , streamArrIdx  :: Int
        , streamStart   :: Int
        , streamLength  :: Int
        }

type IndexStream b =
    { indexBuffer   :: b
    , indexArrIdx   :: Int
    , indexStart    :: Int
    , indexLength   :: Int
    }

data Primitive
    = TriangleStrip
    | TriangleList
    | TriangleFan
    | LineStrip
    | LineList
    | LineLoop
    | PointList

type SlotSchema =
    { primitive     :: FetchPrimitive
    , attributes    :: StrMap StreamType
    }

type PipelineSchema =
    { slots     :: StrMap SlotSchema
    , uniforms  :: StrMap InputType
    }
