module Data where

import Debug.Trace
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Exception

import qualified Graphics.WebGLRaw as GL
import Data.Maybe
import Data.Tuple
import Data.Array
import Data.Traversable

import IR
import Type
import Util

compileBuffer :: [LCArray] -> GFX Buffer
compileBuffer arrs = do
    let offsets = [0] `append` scanl (\s (Array t a) -> s + sizeOfArrayType t * length a) 0 arrs -- BUG:  scanl (+) 0 [1,1] == [1,2] =!= [0,1,2]
        size = case last offsets of
          Just s  -> s
          Nothing -> 0
    b <- newArrayBuffer size
    descs <- flip traverse (zip arrs $ take (length arrs) offsets) $ \(Tuple (Array t a) o) -> do
      let len     = length a
          bytes   = len * sizeOfArrayType t
          newView = case t of
            ArrWord8  -> newWord8View
            ArrWord16 -> newWord16View
            ArrInt8   -> newInt8View
            ArrInt16  -> newInt16View
            ArrFloat  -> newFloatView
      view <- newView b o len
      return {arrType: t, arrLength: len, arrOffset: o, arrSize: bytes, arrView: view}

    bo <- GL.createBuffer_
    GL.bindBuffer_ GL._ARRAY_BUFFER bo
    bufferDataAlloc GL._ARRAY_BUFFER size GL._STATIC_DRAW
    bufferSubDataArrayBuffer GL._ARRAY_BUFFER 0 b
    GL.bindBuffer_ GL._ARRAY_BUFFER nullWebGLBuffer
    return {arrays: descs, glBuffer: bo, buffer: b}

updateBuffer :: Buffer -> [Tuple Int LCArray] -> GFX Unit
updateBuffer b arrs = do
  flip traverse arrs $ \(Tuple i (Array t a)) -> case b.arrays !! i of
    Nothing -> throwException $ error "wrong index"
    Just d  -> do
      when (arrayTypeToGLType t /= arrayTypeToGLType d.arrType) $ throwException $ error "type mismatch"
      when (length a /= d.arrLength) $ throwException $ error "size mismatch"
      setArrayView d.arrView a
      bufferSubDataArrayView GL._ARRAY_BUFFER d.arrOffset d.arrView
  return unit
