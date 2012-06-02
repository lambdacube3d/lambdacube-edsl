module LC_API (
    -- language
    module LC_G_Type,
    module LC_G_APIType,
    module LC_T_APIType,
    module LC_T_DSLType,
    module LC_T_HOAS,
    module LC_T_Language,
    module TypeLevel.Number.Nat,
    module TypeLevel.Number.Nat.Num,
    Int32,
    Word32,
    UniformSetter(..),

    -- backend
    Buffer,
    compileBuffer,
    bufferSize,
    arraySize,
    arrayType,

    Renderer,
    compileRenderer,
    slotUniform,
    slotStream,
    uniformSetter,
    render,
    dispose,

    Object,
    addObject,
    removeObject,
    objectUniformSetter
) where

import Data.Int
import Data.Word

import TypeLevel.Number.Nat
import TypeLevel.Number.Nat.Num

import LC_G_APIType hiding (InputType(..))
import LC_G_Type

import LC_T_APIType
import LC_T_DSLType hiding (Buffer)
import LC_T_HOAS
import LC_T_Language
import qualified LC_T_APIType as H
import qualified LC_T_HOAS as H

import LC_C_Convert

import LC_B_GL hiding (compileRenderer)
import LC_B_GLCompile
import LC_B_GLData
import LC_B_GLType
import LC_B_GLUtil (Buffer)
import qualified LC_B_GL as GL

import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 as SB
import Data.Trie as T

compileRenderer :: GPOutput -> IO Renderer
compileRenderer l = GL.compileRenderer $ convertGPOutput l

nullSetter :: ByteString -> a -> IO ()
nullSetter n _ = Prelude.putStrLn $ "WARNING: unknown uniform: " ++ SB.unpack n

class UniformSetter t where
    uniform   :: Trie InputSetter -> Input t -> SetterFun t

instance UniformSetter Bool where
    uniform is (IBool n) = case T.lookup n is of
        Just (SBool fun)    -> fun
        _   -> nullSetter n

instance UniformSetter V2B where
    uniform is (IV2B n) = case T.lookup n is of
        Just (SV2B fun)    -> fun
        _   -> nullSetter n

instance UniformSetter V3B where
    uniform is (IV3B n) = case T.lookup n is of
        Just (SV3B fun)    -> fun
        _   -> nullSetter n

instance UniformSetter V4B where
    uniform is (IV4B n) = case T.lookup n is of
        Just (SV4B fun)    -> fun
        _   -> nullSetter n

instance UniformSetter Word32 where
    uniform is (IWord n) = case T.lookup n is of
        Just (SWord fun)    -> fun
        _   -> nullSetter n

instance UniformSetter V2U where
    uniform is (IV2U n) = case T.lookup n is of
        Just (SV2U fun)    -> fun
        _   -> nullSetter n

instance UniformSetter V3U where
    uniform is (IV3U n) = case T.lookup n is of
        Just (SV3U fun)    -> fun
        _   -> nullSetter n

instance UniformSetter V4U where
    uniform is (IV4U n) = case T.lookup n is of
        Just (SV4U fun)    -> fun
        _   -> nullSetter n

instance UniformSetter Int32 where
    uniform is (IInt n) = case T.lookup n is of
        Just (SInt fun)    -> fun
        _   -> nullSetter n

instance UniformSetter V2I where
    uniform is (IV2I n) = case T.lookup n is of
        Just (SV2I fun)    -> fun
        _   -> nullSetter n

instance UniformSetter V3I where
    uniform is (IV3I n) = case T.lookup n is of
        Just (SV3I fun)    -> fun
        _   -> nullSetter n

instance UniformSetter V4I where
    uniform is (IV4I n) = case T.lookup n is of
        Just (SV4I fun)    -> fun
        _   -> nullSetter n

instance UniformSetter Float where
    uniform is (IFloat n) = case T.lookup n is of
        Just (SFloat fun)    -> fun
        _   -> nullSetter n

instance UniformSetter V2F where
    uniform is (IV2F n) = case T.lookup n is of
        Just (SV2F fun)    -> fun
        _   -> nullSetter n

instance UniformSetter V3F where
    uniform is (IV3F n) = case T.lookup n is of
        Just (SV3F fun)    -> fun
        _   -> nullSetter n

instance UniformSetter V4F where
    uniform is (IV4F n) = case T.lookup n is of
        Just (SV4F fun)    -> fun
        _   -> nullSetter n

instance UniformSetter M22F where
    uniform is (IM22F n) = case T.lookup n is of
        Just (SM22F fun)    -> fun
        _   -> nullSetter n

instance UniformSetter M23F where
    uniform is (IM23F n) = case T.lookup n is of
        Just (SM23F fun)    -> fun
        _   -> nullSetter n

instance UniformSetter M24F where
    uniform is (IM24F n) = case T.lookup n is of
        Just (SM24F fun)    -> fun
        _   -> nullSetter n

instance UniformSetter M32F where
    uniform is (IM32F n) = case T.lookup n is of
        Just (SM32F fun)    -> fun
        _   -> nullSetter n

instance UniformSetter M33F where
    uniform is (IM33F n) = case T.lookup n is of
        Just (SM33F fun)    -> fun
        _   -> nullSetter n

instance UniformSetter M34F where
    uniform is (IM34F n) = case T.lookup n is of
        Just (SM34F fun)    -> fun
        _   -> nullSetter n

instance UniformSetter M42F where
    uniform is (IM42F n) = case T.lookup n is of
        Just (SM42F fun)    -> fun
        _   -> nullSetter n

instance UniformSetter M43F where
    uniform is (IM43F n) = case T.lookup n is of
        Just (SM43F fun)    -> fun
        _   -> nullSetter n

instance UniformSetter M44F where
    uniform is (IM44F n) = case T.lookup n is of
        Just (SM44F fun)    -> fun
        _   -> nullSetter n
