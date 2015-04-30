module Util where

import Debug.Trace

import qualified Graphics.WebGLRaw as GL
import Control.Monad.Eff.WebGL
import Control.Monad.Eff

import IR

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
