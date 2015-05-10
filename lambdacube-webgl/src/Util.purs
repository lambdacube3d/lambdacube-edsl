module Util where

import Debug.Trace

import qualified Graphics.WebGLRaw as GL
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Ref
import Control.Monad.Eff.WebGL
import Data.Tuple

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

foreign import unsafeCoerce "function unsafeCoerce(a) {return a;}" :: forall a b. a -> b

z2 = V2 0 0 :: V2F
z3 = V3 0 0 0 :: V3F
z4 = V4 0 0 0 0 :: V4F

mkUniformSetter :: InputType -> GFX (Tuple GLUniform InputSetter)
mkUniformSetter t@Bool  = newRef false                        >>= \r -> return $ Tuple (GLUniform t (unsafeCoerce r)) (SBool  $ writeRef r)
mkUniformSetter t@V2B   = newRef (V2 false false)             >>= \r -> return $ Tuple (GLUniform t (unsafeCoerce r)) (SV2B   $ writeRef r)
mkUniformSetter t@V3B   = newRef (V3 false false false)       >>= \r -> return $ Tuple (GLUniform t (unsafeCoerce r)) (SV3B   $ writeRef r)
mkUniformSetter t@V4B   = newRef (V4 false false false false) >>= \r -> return $ Tuple (GLUniform t (unsafeCoerce r)) (SV4B   $ writeRef r)
mkUniformSetter t@Int   = newRef 0                            >>= \r -> return $ Tuple (GLUniform t (unsafeCoerce r)) (SInt   $ writeRef r)
mkUniformSetter t@V2I   = newRef (V2 0 0)                     >>= \r -> return $ Tuple (GLUniform t (unsafeCoerce r)) (SV2I   $ writeRef r)
mkUniformSetter t@V3I   = newRef (V3 0 0 0)                   >>= \r -> return $ Tuple (GLUniform t (unsafeCoerce r)) (SV3I   $ writeRef r)
mkUniformSetter t@V4I   = newRef (V4 0 0 0 0)                 >>= \r -> return $ Tuple (GLUniform t (unsafeCoerce r)) (SV4I   $ writeRef r)
mkUniformSetter t@Float = newRef 0                            >>= \r -> return $ Tuple (GLUniform t (unsafeCoerce r)) (SFloat $ writeRef r)
mkUniformSetter t@V2F   = newRef (V2 0 0)                     >>= \r -> return $ Tuple (GLUniform t (unsafeCoerce r)) (SV2F   $ writeRef r)
mkUniformSetter t@V3F   = newRef (V3 0 0 0)                   >>= \r -> return $ Tuple (GLUniform t (unsafeCoerce r)) (SV3F   $ writeRef r)
mkUniformSetter t@V4F   = newRef (V4 0 0 0 0)                 >>= \r -> return $ Tuple (GLUniform t (unsafeCoerce r)) (SV4F   $ writeRef r)
mkUniformSetter t@M22F  = newRef (V2 z2 z2)                   >>= \r -> return $ Tuple (GLUniform t (unsafeCoerce r)) (SM22F  $ writeRef r)
mkUniformSetter t@M33F  = newRef (V3 z3 z3 z3)                >>= \r -> return $ Tuple (GLUniform t (unsafeCoerce r)) (SM33F  $ writeRef r)
mkUniformSetter t@M44F  = newRef (V4 z4 z4 z4 z4)             >>= \r -> return $ Tuple (GLUniform t (unsafeCoerce r)) (SM44F  $ writeRef r)
