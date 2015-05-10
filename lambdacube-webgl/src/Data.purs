module Data where

import Control.Monad.Eff
import Control.Monad.Eff.Exception

import Type
{-
asInt8Array :: [Number] -> ArrayBuffer Int8
asUint8Array :: [Number] -> ArrayBuffer Uint8
asUint8ClampedArray :: [Number] -> ArrayBuffer Uint8ClampedArray
asInt16Array :: [Number] -> ArrayBuffer Int16
asUint16Array :: [Number] -> ArrayBuffer Uint16
asInt32Array :: [Number] -> ArrayBuffer Int32
asUint32Array :: [Number] -> ArrayBuffer Uint32
asFloat32Array :: [Number] -> ArrayBuffer Float32
asFloat64Array :: [Number] -> ArrayBuffer Float64

asArray :: forall a. ArrayBuffer a -> [Number]
length :: forall a. ArrayBuffer a -> Number
byteLength :: forall a. ArrayBuffer a -> Number
-}

compileBuffer :: [LCArray] -> GFX Buffer
compileBuffer _ = throwException $ error "not implemented"
{-
updateBuffer :: Buffer -> [(Int,Array)] -> IO ()

bufferSize :: Buffer -> Int

arraySize :: Buffer -> Int -> Int

arrayType :: Buffer -> Int -> ArrayType
-}