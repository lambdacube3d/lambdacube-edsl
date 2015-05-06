module Input where

import Control.Monad.Eff
import Control.Monad.Eff.Exception
import qualified Data.StrMap as StrMap
import Data.Maybe
import Type
import IR

-- temp
foreign import undefined :: forall a. a

-- API
schemaFromPipeline :: IR.Pipeline -> PipelineSchema
schemaFromPipeline _ = undefined

mkWebGLPipelineInput :: PipelineSchema -> GFX WebGLPipelineInput
mkWebGLPipelineInput _ = throwException $ error "not implemented"

addObject :: WebGLPipelineInput -> String -> Primitive -> Maybe (IndexStream Buffer) -> StrMap.StrMap (Stream Buffer) -> [String] -> GFX GLObject
addObject _ _ _ _ _ _ = throwException $ error "not implemented"

removeObject :: WebGLPipelineInput -> GLObject -> GFX Unit
removeObject _ _ = throwException $ error "not implemented"

enableObject :: GLObject -> Bool -> GFX Unit
enableObject _ _ = throwException $ error "not implemented"

setObjectOrder :: WebGLPipelineInput -> GLObject -> Int -> GFX Unit
setObjectOrder _ _ _ = throwException $ error "not implemented"

objectUniformSetter :: GLObject -> StrMap.StrMap InputSetter
objectUniformSetter _ = undefined

setScreenSize :: WebGLPipelineInput -> Word -> Word -> GFX Unit
setScreenSize _ _ _ = throwException $ error "not implemented"

sortSlotObjects :: WebGLPipelineInput -> GFX Unit
sortSlotObjects _ = throwException $ error "not implemented"

uniformBool   :: String -> StrMap.StrMap InputSetter -> SetterFun Bool
uniformBool   _ _ _ = throwException $ error "not implemented"

uniformV2B    :: String -> StrMap.StrMap InputSetter -> SetterFun V2B
uniformV2B    _ _ _ = throwException $ error "not implemented"

uniformV3B    :: String -> StrMap.StrMap InputSetter -> SetterFun V3B
uniformV3B    _ _ _ = throwException $ error "not implemented"

uniformV4B    :: String -> StrMap.StrMap InputSetter -> SetterFun V4B
uniformV4B    _ _ _ = throwException $ error "not implemented"

uniformInt    :: String -> StrMap.StrMap InputSetter -> SetterFun Int32
uniformInt    _ _ _ = throwException $ error "not implemented"

uniformV2I    :: String -> StrMap.StrMap InputSetter -> SetterFun V2I
uniformV2I    _ _ _ = throwException $ error "not implemented"

uniformV3I    :: String -> StrMap.StrMap InputSetter -> SetterFun V3I
uniformV3I    _ _ _ = throwException $ error "not implemented"

uniformV4I    :: String -> StrMap.StrMap InputSetter -> SetterFun V4I
uniformV4I    _ _ _ = throwException $ error "not implemented"

uniformFloat  :: String -> StrMap.StrMap InputSetter -> SetterFun Float
uniformFloat  _ _ _ = throwException $ error "not implemented"

uniformV2F    :: String -> StrMap.StrMap InputSetter -> SetterFun V2F
uniformV2F    _ _ _ = throwException $ error "not implemented"

uniformV3F    :: String -> StrMap.StrMap InputSetter -> SetterFun V3F
uniformV3F    _ _ _ = throwException $ error "not implemented"

uniformV4F    :: String -> StrMap.StrMap InputSetter -> SetterFun V4F
uniformV4F    _ _ _ = throwException $ error "not implemented"

uniformM22F   :: String -> StrMap.StrMap InputSetter -> SetterFun M22F
uniformM22F   _ _ _ = throwException $ error "not implemented"

uniformM33F   :: String -> StrMap.StrMap InputSetter -> SetterFun M33F
uniformM33F   _ _ _ = throwException $ error "not implemented"

uniformM44F   :: String -> StrMap.StrMap InputSetter -> SetterFun M44F
uniformM44F   _ _ _ = throwException $ error "not implemented"

{-
  TODO
    input schema data structure
    uniform
    stream input
      buffer definition
-}
{-
import qualified Data.ArrayBuffer.Types as T
import qualified Data.TypedArray as T

  createBuffer_
  bindBuffer_
  bufferData

bufferData_ :: forall eff. GLenum -> Float32Array -> GLenum -> (Eff (webgl :: WebGl | eff) Unit)
asFloat32Array :: [Number] -> Float32Array


uniforms:
  | uni.uType == _FLOAT         = uniform1f_ uni.uLocation (head value)
  | uni.uType == _FLOAT_MAT4    = uniformMatrix4fv_ uni.uLocation false (asArrayBuffer value)
  | uni.uType == _FLOAT_MAT3    = uniformMatrix3fv_ uni.uLocation false (asArrayBuffer value)
  | uni.uType == _FLOAT_MAT2    = uniformMatrix2fv_ uni.uLocation false (asArrayBuffer value)
  | uni.uType == _FLOAT_VEC4    = uniform4fv_ uni.uLocation (asArrayBuffer value)
  | uni.uType == _FLOAT_VEC3    = uniform3fv_ uni.uLocation (asArrayBuffer value)
  | uni.uType == _FLOAT_VEC2    = uniform2fv_ uni.uLocation (asArrayBuffer value)
-}

{-
type Buffer a = {
    webGLBuffer :: WebGLBuffer,
    bufferType  :: Number,
    bufferSize  :: Number
  }

makeBufferFloat :: forall eff. [Number] ->  Eff (webgl :: WebGl | eff) (Buffer T.Float32)
makeBufferFloat vertices = do
  buffer <- createBuffer_
  bindBuffer_ _ARRAY_BUFFER buffer
  let typedArray = T.asFloat32Array vertices
  bufferData _ARRAY_BUFFER typedArray _STATIC_DRAW
  return {
      webGLBuffer : buffer,
      bufferType  : _ARRAY_BUFFER,
      bufferSize  : length vertices
    }

makeBuffer :: forall a eff. BufferTarget -> ([Number] -> T.ArrayView a) -> [Number]
                  ->  Eff (webgl :: WebGl | eff) (Buffer a)
makeBuffer bufferTarget conversion vertices = do
  let targetConst = bufferTargetToConst bufferTarget
  buffer <- createBuffer_
  bindBuffer_ targetConst buffer
  let typedArray = conversion vertices
  bufferData targetConst typedArray _STATIC_DRAW
  return {
      webGLBuffer : buffer,
      bufferType  : targetConst,
      bufferSize  : length vertices
    }

setUniformFloats :: forall eff typ. Uniform typ -> [Number] -> EffWebGL eff Unit
setUniformFloats (Uniform uni) value
  | uni.uType == _FLOAT         = uniform1f_ uni.uLocation (head value)
  | uni.uType == _FLOAT_MAT4    = uniformMatrix4fv_ uni.uLocation false (asArrayBuffer value)
  | uni.uType == _FLOAT_MAT3    = uniformMatrix3fv_ uni.uLocation false (asArrayBuffer value)
  | uni.uType == _FLOAT_MAT2    = uniformMatrix2fv_ uni.uLocation false (asArrayBuffer value)
  | uni.uType == _FLOAT_VEC4    = uniform4fv_ uni.uLocation (asArrayBuffer value)
  | uni.uType == _FLOAT_VEC3    = uniform3fv_ uni.uLocation (asArrayBuffer value)
  | uni.uType == _FLOAT_VEC2    = uniform2fv_ uni.uLocation (asArrayBuffer value)

setUniformBoolean :: forall eff typ. Uniform typ -> Boolean -> EffWebGL eff Unit
setUniformBoolean (Uniform uni) value
  | uni.uType == _BOOL         = uniform1i_ uni.uLocation (toNumber value)
    where
      toNumber true = 1
      toNumber false = 0

asArrayBuffer ::[Number] -> T.Float32Array
asArrayBuffer = T.asFloat32Array

bindBufAndSetVertexAttr :: forall a eff typ. Buffer a -> Attribute typ -> Eff (webgl :: WebGl | eff) Unit
bindBufAndSetVertexAttr buffer bind = do
  bindBuffer_ buffer.bufferType buffer.webGLBuffer
  vertexPointer bind

vertexPointer ::  forall eff typ. Attribute typ -> EffWebGL eff Unit
vertexPointer (Attribute attrLoc) =
  vertexAttribPointer_ attrLoc.aLocation attrLoc.aItemSize _FLOAT false 0 0
-}