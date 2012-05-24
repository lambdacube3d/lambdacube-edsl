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
import LC_B_GL hiding (compileRenderer)
import qualified LC_B_GL as GL
import LC_B_GLUtil (Buffer)
import LC_G_APIType hiding (InputType(..))
import LC_G_Type
import LC_T_APIType
import LC_T_DSLType hiding (Buffer)
import LC_T_HOAS
import LC_T_Language
import TypeLevel.Number.Nat
import TypeLevel.Number.Nat.Num

import qualified LC_T_APIType as H
import qualified LC_T_HOAS as H

import LC_C_Convert

compileRenderer :: [GPOutput] -> IO Renderer
compileRenderer l = GL.compileRenderer $ map convertGPOutput l