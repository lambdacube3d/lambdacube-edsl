module Util where

import Debug.Trace

import qualified Graphics.WebGLRaw as GL
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Ref
import Control.Monad.Eff.WebGL
import Data.Tuple
import Data.Array
import qualified Data.ArrayBuffer.Types as AB
import qualified Data.TypedArray as TA

import IR
import Type

comparisonFunctionToGLType :: ComparisonFunction -> GL.GLenum
comparisonFunctionToGLType a = case a of
    Always      -> GL._ALWAYS
    Equal       -> GL._EQUAL
    Gequal      -> GL._GEQUAL
    Greater     -> GL._GREATER
    Lequal      -> GL._LEQUAL
    Less        -> GL._LESS
    Never       -> GL._NEVER
    Notequal    -> GL._NOTEQUAL

blendEquationToGLType :: BlendEquation -> GL.GLenum
blendEquationToGLType a = case a of
    FuncAdd             -> GL._FUNC_ADD
    FuncReverseSubtract -> GL._FUNC_REVERSE_SUBTRACT
    FuncSubtract        -> GL._FUNC_SUBTRACT
    -- not presented: Max                 -> GL._MAX
    -- not presented: Min                 -> GL._MIN

blendingFactorToGLType :: BlendingFactor -> GL.GLenum
blendingFactorToGLType a = case a of
    ConstantAlpha           -> GL._CONSTANT_ALPHA
    ConstantColor           -> GL._CONSTANT_COLOR
    DstAlpha                -> GL._DST_ALPHA
    DstColor                -> GL._DST_COLOR
    One                     -> GL._ONE
    OneMinusConstantAlpha   -> GL._ONE_MINUS_CONSTANT_ALPHA
    OneMinusConstantColor   -> GL._ONE_MINUS_CONSTANT_COLOR
    OneMinusDstAlpha        -> GL._ONE_MINUS_DST_ALPHA
    OneMinusDstColor        -> GL._ONE_MINUS_DST_COLOR
    OneMinusSrcAlpha        -> GL._ONE_MINUS_SRC_ALPHA
    OneMinusSrcColor        -> GL._ONE_MINUS_SRC_COLOR
    SrcAlpha                -> GL._SRC_ALPHA
    SrcAlphaSaturate        -> GL._SRC_ALPHA_SATURATE
    SrcColor                -> GL._SRC_COLOR
    Zero                    -> GL._ZERO

toStreamType :: InputType -> GFX StreamType
toStreamType a = case a of
  Float -> return TFloat
  V2F   -> return TV2F
  V3F   -> return TV3F
  V4F   -> return TV4F
  M22F  -> return TM22F
  M33F  -> return TM33F
  M44F  -> return TM44F
  _     -> throwException $ error "invalid Stream Type"

foreign import setFloatArray
  """function setFloatArray(ta) {
      return function(a) {
        return function(){ta.set(a);};
      };
     }""" :: AB.Float32Array -> [Float] -> GFX Unit

foreign import setIntArray
  """function setIntArray(ta) {
      return function(a) {
        return function(){ta.set(a);};
      };
     }""" :: AB.Int32Array -> [Int] -> GFX Unit

mkUniformSetter :: InputType -> GFX (Tuple GLUniform InputSetter)
mkUniformSetter t@Bool  = let r = TA.asInt32Array [0]                in return $ Tuple (UniBool  r) (SBool  $ setIntArray r <<< toArray)
mkUniformSetter t@V2B   = let r = TA.asInt32Array (replicate 2 0)    in return $ Tuple (UniV2B   r) (SV2B   $ setIntArray r <<< toArray)
mkUniformSetter t@V3B   = let r = TA.asInt32Array (replicate 3 0)    in return $ Tuple (UniV3B   r) (SV3B   $ setIntArray r <<< toArray)
mkUniformSetter t@V4B   = let r = TA.asInt32Array (replicate 4 0)    in return $ Tuple (UniV4B   r) (SV4B   $ setIntArray r <<< toArray)
mkUniformSetter t@Int   = let r = TA.asInt32Array [0]                in return $ Tuple (UniInt   r) (SInt   $ setIntArray r <<< toArray)
mkUniformSetter t@V2I   = let r = TA.asInt32Array (replicate 2 0)    in return $ Tuple (UniV2I   r) (SV2I   $ setIntArray r <<< toArray)
mkUniformSetter t@V3I   = let r = TA.asInt32Array (replicate 3 0)    in return $ Tuple (UniV3I   r) (SV3I   $ setIntArray r <<< toArray)
mkUniformSetter t@V4I   = let r = TA.asInt32Array (replicate 4 0)    in return $ Tuple (UniV4I   r) (SV4I   $ setIntArray r <<< toArray)
mkUniformSetter t@Float = let r = TA.asFloat32Array [0]              in return $ Tuple (UniFloat r) (SFloat $ setFloatArray r <<< toArray)
mkUniformSetter t@V2F   = let r = TA.asFloat32Array (replicate 2 0)  in return $ Tuple (UniV2F   r) (SV2F   $ setFloatArray r <<< toArray)
mkUniformSetter t@V3F   = let r = TA.asFloat32Array (replicate 3 0)  in return $ Tuple (UniV3F   r) (SV3F   $ setFloatArray r <<< toArray)
mkUniformSetter t@V4F   = let r = TA.asFloat32Array (replicate 4 0)  in return $ Tuple (UniV4F   r) (SV4F   $ setFloatArray r <<< toArray)
mkUniformSetter t@M22F  = let r = TA.asFloat32Array (replicate 4 0)  in return $ Tuple (UniM22F  r) (SM22F  $ setFloatArray r <<< toArray)
mkUniformSetter t@M33F  = let r = TA.asFloat32Array (replicate 9 0)  in return $ Tuple (UniM33F  r) (SM33F  $ setFloatArray r <<< toArray)
mkUniformSetter t@M44F  = let r = TA.asFloat32Array (replicate 16 0) in return $ Tuple (UniM44F  r) (SM44F  $ setFloatArray r <<< toArray)

primitiveToFetchPrimitive :: Primitive -> FetchPrimitive
primitiveToFetchPrimitive prim = case prim of
  TriangleStrip           -> Triangles
  TriangleList            -> Triangles
  TriangleFan             -> Triangles
  LineStrip               -> Lines
  LineLoop                -> Lines
  LineList                -> Lines
  PointList               -> Points

unlines :: [String] -> String
unlines [] = ""
unlines [x] = x
unlines (x:xs) = x ++ "\n" ++ unlines xs

setVertexAttrib :: GL.GLuint -> Stream Buffer -> GFX Unit
setVertexAttrib i val = case val of
  ConstFloat v -> setAFloat i v
  ConstV2F v   -> setAV2F i v
  ConstV3F v   -> setAV3F i v
  ConstV4F v   -> setAV4F i v
  ConstM22F (V2 x y) -> do
    setAV2F i x
    setAV2F (i+1) y
  ConstM33F (V3 x y z) -> do
    setAV3F i x
    setAV3F (i+1) y
    setAV3F (i+2) z
  ConstM44F (V4 x y z w) -> do
    setAV4F i x
    setAV4F (i+1) y
    setAV4F (i+2) z
    setAV4F (i+3) w
  _ -> throwException $ error "internal error (setVertexAttrib)!"

setAFloat :: GL.GLuint -> Float -> GFX Unit
setAFloat i v = GL.vertexAttrib1f_ i v

setAV2F :: GL.GLuint -> V2F -> GFX Unit
setAV2F i (V2 x y) = GL.vertexAttrib2f_ i x y

setAV3F :: GL.GLuint -> V3F -> GFX Unit
setAV3F i (V3 x y z) = GL.vertexAttrib3f_ i x y z

setAV4F :: GL.GLuint -> V4F -> GFX Unit
setAV4F i (V4 x y z w) = GL.vertexAttrib4f_ i x y z w

-- sets value based uniforms only (does not handle textures)
setUniform :: forall a . GL.WebGLUniformLocation -> GLUniform -> GFX Unit
setUniform i uni = case uni of
  UniBool  r -> GL.uniform1iv_ i r
  UniV2B   r -> GL.uniform2iv_ i r
  UniV3B   r -> GL.uniform3iv_ i r
  UniV4B   r -> GL.uniform4iv_ i r
  UniInt   r -> GL.uniform1iv_ i r
  UniV2I   r -> GL.uniform2iv_ i r
  UniV3I   r -> GL.uniform3iv_ i r
  UniV4I   r -> GL.uniform4iv_ i r
  UniFloat r -> GL.uniform1fv_ i r
  UniV2F   r -> GL.uniform2fv_ i r
  UniV3F   r -> GL.uniform3fv_ i r
  UniV4F   r -> GL.uniform4fv_ i r
  UniM22F  r -> GL.uniformMatrix2fv_ i false r
  UniM33F  r -> GL.uniformMatrix3fv_ i false r
  UniM44F  r -> GL.uniformMatrix4fv_ i false r
  _ -> throwException $ error "internal error (setUniform)!"

primitiveToGLType :: Primitive -> GL.GLenum
primitiveToGLType p = case p of
  TriangleStrip -> GL._TRIANGLE_STRIP
  TriangleList  -> GL._TRIANGLES
  TriangleFan   -> GL._TRIANGLE_FAN
  LineStrip     -> GL._LINE_STRIP
  LineLoop      -> GL._LINE_LOOP
  LineList      -> GL._LINES
  PointList     -> GL._POINTS

arrayTypeToGLType :: ArrayType -> GL.GLenum
arrayTypeToGLType a = case a of
  ArrWord8    -> GL._UNSIGNED_BYTE
  ArrWord16   -> GL._UNSIGNED_SHORT
  ArrInt8     -> GL._BYTE
  ArrInt16    -> GL._SHORT
  ArrFloat    -> GL._FLOAT

-- custom typed array buffer and view functions
foreign import newArrayBuffer
  """function newArrayBuffer(s) {
      return function() {return new ArrayBuffer(s);};
     }""" :: Int -> GFX ArrayBuffer

foreign import newWord8View
  """function newWord8View(b) {
      return function(o) {
        return function(l) {
          return function(){new Uint8Array(b,o,l);};
        };
      };
     }""" :: ArrayBuffer -> Int -> Int -> GFX ArrayView

foreign import newWord16View
  """function newWord16View(b) {
      return function(o) {
        return function(l) {
          return function(){new Uint16Array(b,o,l);};
        };
      };
     }""" :: ArrayBuffer -> Int -> Int -> GFX ArrayView

foreign import newInt8View
  """function newInt8View(b) {
      return function(o) {
        return function(l) {
          return function(){new Int8Array(b,o,l);};
        };
      };
     }""" :: ArrayBuffer -> Int -> Int -> GFX ArrayView

foreign import newInt16View
  """function newInt16View(b) {
      return function(o) {
        return function(l) {
          return function(){new Int16Array(b,o,l);};
        };
      };
     }""" :: ArrayBuffer -> Int -> Int -> GFX ArrayView

foreign import newFloatView
  """function newFloatView(b) {
      return function(o) {
        return function(l) {
          return function(){new Float32Array(b,o,l);};
        };
      };
     }""" :: ArrayBuffer -> Int -> Int -> GFX ArrayView

foreign import setArrayView
  """function setArrayView(av) {
      return function(a) {
        return function(){av.set(a);};
      };
     }""" :: ArrayView -> [Number] -> GFX Unit

foreign import nullWebGLBuffer "var nullWebGLBuffer = null" :: GL.WebGLBuffer

foreign import bufferDataAlloc
  """function bufferDataAlloc(target)
   {return function(size)
    {return function(usage)
     {return function()
      {gl.bufferData(target,size,usage);};};};};"""
    :: forall eff. GL.GLenum -> Int -> GL.GLenum -> GFX Unit

foreign import bufferSubDataArrayBuffer
  """function bufferSubDataArrayBuffer(target)
   {return function(offset)
    {return function(data)
     {return function()
      {gl.bufferSubData(target,offset,data);};};};};"""
    :: forall eff. GL.GLenum -> GL.GLintptr -> ArrayBuffer -> GFX Unit

foreign import bufferSubDataArrayView
  """function bufferSubDataArrayView(target)
   {return function(offset)
    {return function(data)
     {return function()
      {gl.bufferSubData(target,offset,data);};};};};"""
    :: forall eff. GL.GLenum -> GL.GLintptr -> ArrayView -> GFX Unit
