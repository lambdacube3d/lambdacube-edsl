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
    uniformBool,
    uniformV2B,
    uniformV3B,
    uniformV4B,

    uniformWord,
    uniformV2U,
    uniformV3U,
    uniformV4U,

    uniformInt,
    uniformV2I,
    uniformV3I,
    uniformV4I,

    uniformFloat,
    uniformV2F,
    uniformV3F,
    uniformV4F,

    uniformM22F,
    uniformM23F,
    uniformM24F,
    uniformM32F,
    uniformM33F,
    uniformM34F,
    uniformM42F,
    uniformM43F,
    uniformM44F,

    uniformFTexture2D,

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
    objectUniformSetter,

    -- texture (temporary)
    compileTexture2DNoMipRGBAF
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

nullSetter :: ByteString -> String -> a -> IO ()
nullSetter n t _ = Prelude.putStrLn $ "WARNING: unknown uniform: " ++ SB.unpack n ++ " :: " ++ t

uniformBool  :: ByteString -> Trie InputSetter -> SetterFun Bool
uniformV2B   :: ByteString -> Trie InputSetter -> SetterFun V2B
uniformV3B   :: ByteString -> Trie InputSetter -> SetterFun V3B
uniformV4B   :: ByteString -> Trie InputSetter -> SetterFun V4B

uniformWord  :: ByteString -> Trie InputSetter -> SetterFun Word32
uniformV2U   :: ByteString -> Trie InputSetter -> SetterFun V2U
uniformV3U   :: ByteString -> Trie InputSetter -> SetterFun V3U
uniformV4U   :: ByteString -> Trie InputSetter -> SetterFun V4U

uniformInt   :: ByteString -> Trie InputSetter -> SetterFun Int32
uniformV2I   :: ByteString -> Trie InputSetter -> SetterFun V2I
uniformV3I   :: ByteString -> Trie InputSetter -> SetterFun V3I
uniformV4I   :: ByteString -> Trie InputSetter -> SetterFun V4I

uniformFloat :: ByteString -> Trie InputSetter -> SetterFun Float
uniformV2F   :: ByteString -> Trie InputSetter -> SetterFun V2F
uniformV3F   :: ByteString -> Trie InputSetter -> SetterFun V3F
uniformV4F   :: ByteString -> Trie InputSetter -> SetterFun V4F

uniformM22F   :: ByteString -> Trie InputSetter -> SetterFun M22F
uniformM23F   :: ByteString -> Trie InputSetter -> SetterFun M23F
uniformM24F   :: ByteString -> Trie InputSetter -> SetterFun M24F
uniformM32F   :: ByteString -> Trie InputSetter -> SetterFun M32F
uniformM33F   :: ByteString -> Trie InputSetter -> SetterFun M33F
uniformM34F   :: ByteString -> Trie InputSetter -> SetterFun M34F
uniformM42F   :: ByteString -> Trie InputSetter -> SetterFun M42F
uniformM43F   :: ByteString -> Trie InputSetter -> SetterFun M43F
uniformM44F   :: ByteString -> Trie InputSetter -> SetterFun M44F

uniformFTexture2D   :: ByteString -> Trie InputSetter -> SetterFun TextureData

uniformBool n is = case T.lookup n is of
    Just (SBool fun)    -> fun
    _   -> nullSetter n "Bool"

uniformV2B n is = case T.lookup n is of
    Just (SV2B fun)    -> fun
    _   -> nullSetter n "V2B"

uniformV3B n is = case T.lookup n is of
    Just (SV3B fun)    -> fun
    _   -> nullSetter n "V3B"

uniformV4B n is = case T.lookup n is of
    Just (SV4B fun)    -> fun
    _   -> nullSetter n "V4B"

uniformWord n is = case T.lookup n is of
    Just (SWord fun)    -> fun
    _   -> nullSetter n "Word"

uniformV2U n is = case T.lookup n is of
    Just (SV2U fun)    -> fun
    _   -> nullSetter n "V2U"

uniformV3U n is = case T.lookup n is of
    Just (SV3U fun)    -> fun
    _   -> nullSetter n "V3U"

uniformV4U n is = case T.lookup n is of
    Just (SV4U fun)    -> fun
    _   -> nullSetter n "V4U"

uniformInt n is = case T.lookup n is of
    Just (SInt fun)    -> fun
    _   -> nullSetter n "Int"

uniformV2I n is = case T.lookup n is of
    Just (SV2I fun)    -> fun
    _   -> nullSetter n "V2I"

uniformV3I n is = case T.lookup n is of
    Just (SV3I fun)    -> fun
    _   -> nullSetter n "V3I"

uniformV4I n is = case T.lookup n is of
    Just (SV4I fun)    -> fun
    _   -> nullSetter n "V4I"

uniformFloat n is = case T.lookup n is of
    Just (SFloat fun)    -> fun
    _   -> nullSetter n "Float"

uniformV2F n is = case T.lookup n is of
    Just (SV2F fun)    -> fun
    _   -> nullSetter n "V2F"

uniformV3F n is = case T.lookup n is of
    Just (SV3F fun)    -> fun
    _   -> nullSetter n "V3F"

uniformV4F n is = case T.lookup n is of
    Just (SV4F fun)    -> fun
    _   -> nullSetter n "V4F"

uniformM22F n is = case T.lookup n is of
    Just (SM22F fun)    -> fun
    _   -> nullSetter n "M22F"

uniformM23F n is = case T.lookup n is of
    Just (SM23F fun)    -> fun
    _   -> nullSetter n "M23F"

uniformM24F n is = case T.lookup n is of
    Just (SM24F fun)    -> fun
    _   -> nullSetter n "M24F"

uniformM32F n is = case T.lookup n is of
    Just (SM32F fun)    -> fun
    _   -> nullSetter n "M32F"

uniformM33F n is = case T.lookup n is of
    Just (SM33F fun)    -> fun
    _   -> nullSetter n "M33F"

uniformM34F n is = case T.lookup n is of
    Just (SM34F fun)    -> fun
    _   -> nullSetter n "M34F"

uniformM42F n is = case T.lookup n is of
    Just (SM42F fun)    -> fun
    _   -> nullSetter n "M42F"

uniformM43F n is = case T.lookup n is of
    Just (SM43F fun)    -> fun
    _   -> nullSetter n "M43F"

uniformM44F n is = case T.lookup n is of
    Just (SM44F fun)    -> fun
    _   -> nullSetter n "M44F"

uniformFTexture2D n is = case T.lookup n is of
    Just (SFTexture2D fun)    -> fun
    _   -> nullSetter n "FTexture2D"
