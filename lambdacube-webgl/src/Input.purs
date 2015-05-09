module Input where

import Prelude.Unsafe (unsafeIndex)
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Exception
import qualified Data.Map as Map
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
mkWebGLPipelineInput sch = do
  seed <- newRef 0
  size <- newRef (V2 0 0)
  ppls <- newRef []
  return $
    { schema        : sch
    , slotMap       : StrMap.empty :: StrMap.StrMap Int-- TODO
    , slotVector    : [] -- TODO
    , objSeed       : seed
    , uniformSetter : StrMap.empty :: StrMap.StrMap InputSetter-- TODO
    , uniformSetup  : StrMap.empty :: StrMap.StrMap GLUniform-- TODO
    , screenSize    : size
    , pipelines     : ppls -- TODO
    }
{-
type WebGLPipelineInput =
    { schema        :: PipelineSchema
    , slotMap       :: StrMap.StrMap String
    , slotVector    :: [RefVal GLSlot]
    , objSeed       :: RefVal Int
    , uniformSetter :: StrMap.StrMap InputSetter
    , uniformSetup  :: StrMap.StrMap GLUniform
    , screenSize    :: RefVal V2U
    , pipelines     :: RefVal [(Maybe WebGLPipeline)] -- attached pipelines
    }
-}
{-
mkGLPipelineInput sch = do
    let sm  = T.fromList $ zip (T.keys $ T.slots sch) [0..]
        len = T.size sm
    (setters,unis) <- mkUniform $ T.toList $ uniforms sch
    seed <- newIORef 0
    slotV <- V.replicateM len $ newIORef (GLSlot IM.empty V.empty Ordered)
    size <- newIORef (0,0)
    ppls <- newIORef $ V.singleton Nothing
    return $ GLPipelineInput
        { schema        = sch
        , slotMap       = sm
        , slotVector    = slotV
        , objSeed       = seed
        , uniformSetter = setters
        , uniformSetup  = unis
        , screenSize    = size
        , pipelines     = ppls
        }
-}

addObject :: WebGLPipelineInput -> String -> Primitive -> Maybe (IndexStream Buffer) -> StrMap.StrMap (Stream Buffer) -> [String] -> GFX GLObject
addObject _ _ _ _ _ _ = throwException $ error "not implemented"

removeObject :: WebGLPipelineInput -> GLObject -> GFX Unit
removeObject p obj = modifyRef (p.slotVector `unsafeIndex` obj.slot) $ \s -> {objectMap:Map.delete obj.id s.objectMap, sortedObjects:[], orderJob:Generate}

enableObject :: GLObject -> Bool -> GFX Unit
enableObject obj b = writeRef obj.enabled b

setObjectOrder :: WebGLPipelineInput -> GLObject -> Int -> GFX Unit
setObjectOrder p obj i = do
    writeRef obj.order i
    modifyRef (p.slotVector `unsafeIndex` obj.slot) $ \s -> s {orderJob = Reorder}

objectUniformSetter :: GLObject -> StrMap.StrMap InputSetter
objectUniformSetter o = o.uniSetter

setScreenSize :: WebGLPipelineInput -> V2U -> GFX Unit
setScreenSize p s = writeRef p.screenSize s

sortSlotObjects :: WebGLPipelineInput -> GFX Unit
sortSlotObjects _ = throwException $ error "not implemented"

nullSetter :: forall a . String -> String -> a -> GFX Unit
nullSetter n t _ = return unit -- Prelude.putStrLn $ "WARNING: unknown uniform: " ++ n ++ " :: " ++ t

uniformBool :: String -> StrMap.StrMap InputSetter -> SetterFun Bool
uniformBool n is =
  case StrMap.lookup n is of
  Just (SBool fun) -> fun
  _ -> nullSetter n "Bool"

uniformV2B :: String -> StrMap.StrMap InputSetter -> SetterFun V2B
uniformV2B n is = case StrMap.lookup n is of
  Just (SV2B fun) -> fun
  _ -> nullSetter n "V2B"

uniformV3B :: String -> StrMap.StrMap InputSetter -> SetterFun V3B
uniformV3B n is = case StrMap.lookup n is of
  Just (SV3B fun) -> fun
  _ -> nullSetter n "V3B"

uniformV4B :: String -> StrMap.StrMap InputSetter -> SetterFun V4B
uniformV4B n is =case StrMap.lookup n is of
  Just (SV4B fun) -> fun
  _ -> nullSetter n "V4B"

uniformInt :: String -> StrMap.StrMap InputSetter -> SetterFun Int32
uniformInt n is = case StrMap.lookup n is of
  Just (SInt fun) -> fun
  _ -> nullSetter n "Int"

uniformV2I :: String -> StrMap.StrMap InputSetter -> SetterFun V2I
uniformV2I n is = case StrMap.lookup n is of
  Just (SV2I fun) -> fun
  _ -> nullSetter n "V2I"

uniformV3I :: String -> StrMap.StrMap InputSetter -> SetterFun V3I
uniformV3I n is = case StrMap.lookup n is of
  Just (SV3I fun) -> fun
  _ -> nullSetter n "V3I"

uniformV4I :: String -> StrMap.StrMap InputSetter -> SetterFun V4I
uniformV4I n is = case StrMap.lookup n is of
  Just (SV4I fun) -> fun
  _ -> nullSetter n "V4I"

uniformFloat :: String -> StrMap.StrMap InputSetter -> SetterFun Float
uniformFloat n is = case StrMap.lookup n is of
  Just (SFloat fun) -> fun
  _ -> nullSetter n "Float"

uniformV2F :: String -> StrMap.StrMap InputSetter -> SetterFun V2F
uniformV2F n is = case StrMap.lookup n is of
  Just (SV2F fun) -> fun
  _ -> nullSetter n "V2F"

uniformV3F :: String -> StrMap.StrMap InputSetter -> SetterFun V3F
uniformV3F n is = case StrMap.lookup n is of
  Just (SV3F fun) -> fun
  _ -> nullSetter n "V3F"

uniformV4F :: String -> StrMap.StrMap InputSetter -> SetterFun V4F
uniformV4F n is = case StrMap.lookup n is of
  Just (SV4F fun) -> fun
  _ -> nullSetter n "V4F"

uniformM22F :: String -> StrMap.StrMap InputSetter -> SetterFun M22F
uniformM22F n is = case StrMap.lookup n is of
  Just (SM22F fun) -> fun
  _ -> nullSetter n "M22F"

uniformM33F :: String -> StrMap.StrMap InputSetter -> SetterFun M33F
uniformM33F n is = case StrMap.lookup n is of
  Just (SM33F fun) -> fun
  _ -> nullSetter n "M33F"

uniformM44F :: String -> StrMap.StrMap InputSetter -> SetterFun M44F
uniformM44F n is = case StrMap.lookup n is of
  Just (SM44F fun) -> fun
  _ -> nullSetter n "M44F"

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