module Type where

import Debug.Trace
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Exception
import Control.Monad.Eff.WebGL
import qualified Data.StrMap as StrMap
import qualified Graphics.WebGLRaw as GL

import qualified Data.Map as Map
import Data.Tuple
import Data.Maybe

import IR

type GFX a = forall e . Eff (webgl :: WebGl, trace :: Trace, err :: Exception, ref :: Ref | e) a

type IntMap a = Map.Map Int a

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

-- describes an array in a buffer
data LCArray  -- array type, element count (NOT byte size!), setter
    = Array ArrayType Int BufferSetter

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
        { sType   :: StreamType
        , buffer  :: b
        , arrIdx  :: Int
        , start   :: Int
        , length  :: Int
        }

type IndexStream b =
    { buffer   :: b
    , arrIdx   :: Int
    , start    :: Int
    , length   :: Int
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
    , attributes    :: StrMap.StrMap StreamType
    }

type PipelineSchema =
    { slots     :: StrMap.StrMap SlotSchema
    , uniforms  :: StrMap.StrMap InputType
    }

data OrderJob
    = Generate
    | Reorder
    | Ordered

type GLSlot =
    { objectMap     :: IntMap GLObject
    , sortedObjects :: [Tuple Int GLObject]
    , orderJob      :: OrderJob
    }

data GLUniform = GLUniform InputType (forall a . RefVal a)

type WebGLPipelineInput =
    { schema        :: PipelineSchema
    , slotMap       :: StrMap.StrMap Int
    , slotVector    :: [RefVal GLSlot]
    , objSeed       :: RefVal Int
    , uniformSetter :: StrMap.StrMap InputSetter
    , uniformSetup  :: StrMap.StrMap GLUniform
    , screenSize    :: RefVal V2U
    , pipelines     :: RefVal [(Maybe WebGLPipeline)] -- attached pipelines
    }

type GLObject = -- internal type
    { slot       :: Int
    , primitive  :: Primitive
    , indices    :: Maybe (IndexStream Buffer)
    , attributes :: StrMap.StrMap (Stream Buffer)
    , uniSetter  :: StrMap.StrMap InputSetter
    , uniSetup   :: StrMap.StrMap GLUniform
    , order      :: RefVal Int
    , enabled    :: RefVal Bool
    , id         :: Int
    , commands   :: RefVal [[[GLObjectCommand]]]  -- pipeline id, program name, commands
    }

data InputConnection = InputConnection
  { id                      :: Int                -- identifier (vector index) for attached pipeline
  , input                   :: WebGLPipelineInput
  , slotMapPipelineToInput  :: [SlotName]         -- GLPipeline to GLPipelineInput slot name mapping
  , slotMapInputToPipeline  :: [Maybe SlotName]   -- GLPipelineInput to GLPipeline slot name mapping
  }

type WebGLPipeline =
  { targets       :: [RenderTarget]
  , programs      :: [GLProgram]
  , commands      :: [Command]
  , input         :: RefVal (Maybe InputConnection)
  , slotNames     :: [String]
  , slotPrograms  :: [[ProgramName]] -- program list for every slot (programs depend on a slot)
  }

type GLProgram =
  { program       :: GL.WebGLProgram
  , shaders       :: [GL.WebGLShader]
  , inputUniforms :: StrMap.StrMap GL.WebGLUniformLocation
  , inputStreams  :: StrMap.StrMap {location :: GL.GLint, slotAttribute :: String}
  }

data GLObjectCommand = GLObjectCommand
{-
    = GLSetUniform              !GLint !GLUniform
    | GLBindTexture             !GLenum !(IORef GLint) !GLUniform               -- binds the texture from the gluniform to the specified texture unit and target
    | GLSetVertexAttribArray    !GLuint !GLuint !GLint !GLenum !(Ptr ())        -- index buffer size type pointer
    | GLSetVertexAttrib         !GLuint !(Stream Buffer)                        -- index value
    | GLDrawArrays              !GLenum !GLint !GLsizei                         -- mode first count
    | GLDrawElements            !GLenum !GLsizei !GLenum !GLuint !(Ptr ())      -- mode count type buffer indicesPtr
-}

type SetterFun a = a -> GFX Unit

-- user will provide scalar input data via this type
data InputSetter
    = SBool  (SetterFun Bool)
    | SV2B   (SetterFun V2B)
    | SV3B   (SetterFun V3B)
    | SV4B   (SetterFun V4B)
    | SInt   (SetterFun Int32)
    | SV2I   (SetterFun V2I)
    | SV3I   (SetterFun V3I)
    | SV4I   (SetterFun V4I)
    | SFloat (SetterFun Float)
    | SV2F   (SetterFun V2F)
    | SV3F   (SetterFun V3F)
    | SV4F   (SetterFun V4F)
    | SM22F  (SetterFun M22F)
    | SM33F  (SetterFun M33F)
    | SM44F  (SetterFun M44F)
