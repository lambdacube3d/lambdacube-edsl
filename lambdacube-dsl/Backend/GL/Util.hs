{-# LANGUAGE OverloadedStrings #-}
module Backend.GL.Util (
    queryUniforms,
    queryStreams,
    mkUniformSetter,
    setUniform,
    setVertexAttrib,
    compileShader,
    printProgramLog,
    glGetShaderiv1,
    glGetProgramiv1,
    Buffer(..),
    ArrayDesc(..),
    StreamSetter,
    streamToInputType,
    arrayTypeToGLType,
    comparisonFunctionToGLType,
    logicOperationToGLType,
    blendEquationToGLType,
    blendingFactorToGLType,
    checkGL,
    textureDataTypeToGLType,
    textureDataTypeToGLArityType,
    glGetIntegerv1,
    setSampler,
    checkFBO,
    compileTexture,
    primitiveToFetchPrimitive,
    primitiveToGLType,
    inputTypeToTextureTarget,
    toTrie
) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.ByteString.Char8 (ByteString,pack,unpack)
import Data.IORef
import Data.List as L
import Data.Trie as T
import Foreign
import qualified Data.ByteString.Char8 as SB
import qualified Data.Vector as V
import Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Map (Map)
import qualified Data.Map as Map

import Graphics.Rendering.OpenGL.Raw.Core33
    ( GLchar
    , GLenum
    , GLint
    , GLsizei
    , GLuint
    , gl_FALSE
    , gl_TRUE
    , glGetIntegerv

    -- * ERROR CHECKING related *
    -- error handling
    , glGetError
    , glCheckFramebufferStatus
    -- error checking
    , gl_COMPILE_STATUS
    , gl_DRAW_FRAMEBUFFER
    , gl_FRAMEBUFFER_COMPLETE
    , gl_FRAMEBUFFER_INCOMPLETE_ATTACHMENT
    , gl_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER
    , gl_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS
    , gl_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE
    , gl_FRAMEBUFFER_INCOMPLETE_READ_BUFFER
    , gl_FRAMEBUFFER_UNDEFINED
    , gl_FRAMEBUFFER_UNSUPPORTED
    , gl_INFO_LOG_LENGTH
    , gl_INVALID_ENUM
    , gl_INVALID_FRAMEBUFFER_OPERATION
    , gl_INVALID_OPERATION
    , gl_INVALID_VALUE
    , gl_NO_ERROR
    , gl_OUT_OF_MEMORY

    -- * TEXTURE related *
    -- texture data
    , glActiveTexture
    , glBindTexture
    , glGenTextures
    , glTexImage1D
    , glTexImage2D
    , glTexImage3D
    , glTexImage2DMultisample
    , glTexImage3DMultisample
    , glTexParameteri
    , glTexParameterf
    , glTexParameterfv
    , glTexParameterIiv
    , glTexParameterIuiv
    , gl_TEXTURE0

    -- texture parameters
    , gl_CLAMP_TO_BORDER
    , gl_CLAMP_TO_EDGE
    , gl_MIRRORED_REPEAT
    , gl_REPEAT
    , gl_LINEAR
    , gl_NEAREST
    , gl_NEAREST_MIPMAP_NEAREST
    , gl_NEAREST_MIPMAP_LINEAR
    , gl_LINEAR_MIPMAP_NEAREST
    , gl_LINEAR_MIPMAP_LINEAR

    , gl_TEXTURE_1D
    , gl_TEXTURE_1D_ARRAY
    , gl_TEXTURE_2D
    , gl_TEXTURE_2D_ARRAY
    , gl_TEXTURE_2D_MULTISAMPLE
    , gl_TEXTURE_2D_MULTISAMPLE_ARRAY
    , gl_TEXTURE_3D
    , gl_TEXTURE_BUFFER
    , gl_TEXTURE_CUBE_MAP
    , gl_TEXTURE_CUBE_MAP_NEGATIVE_X
    , gl_TEXTURE_CUBE_MAP_NEGATIVE_Y
    , gl_TEXTURE_CUBE_MAP_NEGATIVE_Z
    , gl_TEXTURE_CUBE_MAP_POSITIVE_X
    , gl_TEXTURE_CUBE_MAP_POSITIVE_Y
    , gl_TEXTURE_CUBE_MAP_POSITIVE_Z
    , gl_TEXTURE_RECTANGLE
    , gl_TEXTURE_MAG_FILTER
    , gl_TEXTURE_MIN_FILTER
    , gl_TEXTURE_WRAP_S
    , gl_TEXTURE_WRAP_T
    , gl_TEXTURE_WRAP_R
    , gl_TEXTURE_BASE_LEVEL
    , gl_TEXTURE_MAX_LEVEL
    , gl_TEXTURE_MAX_LOD
    , gl_TEXTURE_MIN_LOD
    , gl_TEXTURE_LOD_BIAS
    , gl_NONE
    , gl_TEXTURE_COMPARE_MODE
    , gl_TEXTURE_COMPARE_FUNC
    , gl_COMPARE_REF_TO_TEXTURE
    , gl_TEXTURE_BORDER_COLOR

    -- texture format
    , gl_R32F
    , gl_R32I
    , gl_R32UI
    , gl_RED
    , gl_RG
    , gl_RG32F
    , gl_RG32I
    , gl_RG32UI
    , gl_RGBA
    , gl_RGBA32F
    , gl_RGBA32I
    , gl_RGBA32UI
    , gl_DEPTH_COMPONENT32F
    , gl_DEPTH_COMPONENT32
    , gl_DEPTH_COMPONENT

    -- * SHADER related *
    -- shader program
    , glCompileShader
    , glGetActiveAttrib
    , glGetActiveUniform
    , glGetAttribLocation
    , glGetProgramInfoLog
    , glGetProgramiv
    , glGetShaderInfoLog
    , glGetShaderiv
    , glGetUniformLocation
    , glShaderSource

    -- stream data (stream parameter)
    , glBindBuffer
    , glDisableVertexAttribArray
    , glEnableVertexAttribArray
    , glVertexAttrib1fv
    , glVertexAttrib2fv
    , glVertexAttrib3fv
    , glVertexAttrib4fv
    , glVertexAttribI1iv
    , glVertexAttribI1uiv
    , glVertexAttribI2iv
    , glVertexAttribI2uiv
    , glVertexAttribI3iv
    , glVertexAttribI3uiv
    , glVertexAttribI4iv
    , glVertexAttribI4uiv
    , glVertexAttribIPointer
    , glVertexAttribPointer
    , gl_ACTIVE_ATTRIBUTES
    , gl_ACTIVE_ATTRIBUTE_MAX_LENGTH
    , gl_ARRAY_BUFFER

    -- stream value representation
    , gl_BYTE
    , gl_HALF_FLOAT
    , gl_SHORT
    , gl_UNSIGNED_BYTE
    , gl_UNSIGNED_SHORT

    -- uniform data (constant parameter)
    , glUniform1fv
    , glUniform1i
    , glUniform1iv
    , glUniform1uiv
    , glUniform2fv
    , glUniform2iv
    , glUniform2uiv
    , glUniform3fv
    , glUniform3iv
    , glUniform3uiv
    , glUniform4fv
    , glUniform4iv
    , glUniform4uiv
    , glUniformMatrix2fv
    , glUniformMatrix2x3fv
    , glUniformMatrix2x4fv
    , glUniformMatrix3fv
    , glUniformMatrix3x2fv
    , glUniformMatrix3x4fv
    , glUniformMatrix4fv
    , glUniformMatrix4x2fv
    , glUniformMatrix4x3fv
    , gl_ACTIVE_UNIFORMS
    , gl_ACTIVE_UNIFORM_MAX_LENGTH

    -- uniform types (constant value types)
    , gl_BOOL
    , gl_BOOL_VEC2
    , gl_BOOL_VEC3
    , gl_BOOL_VEC4
    , gl_FLOAT
    , gl_FLOAT_MAT2
    , gl_FLOAT_MAT2x3
    , gl_FLOAT_MAT2x4
    , gl_FLOAT_MAT3
    , gl_FLOAT_MAT3x2
    , gl_FLOAT_MAT3x4
    , gl_FLOAT_MAT4
    , gl_FLOAT_MAT4x2
    , gl_FLOAT_MAT4x3
    , gl_FLOAT_VEC2
    , gl_FLOAT_VEC3
    , gl_FLOAT_VEC4
    , gl_INT
    , gl_INT_SAMPLER_1D
    , gl_INT_SAMPLER_1D_ARRAY
    , gl_INT_SAMPLER_2D
    , gl_INT_SAMPLER_2D_ARRAY
    , gl_INT_SAMPLER_2D_MULTISAMPLE
    , gl_INT_SAMPLER_2D_MULTISAMPLE_ARRAY
    , gl_INT_SAMPLER_2D_RECT
    , gl_INT_SAMPLER_3D
    , gl_INT_SAMPLER_BUFFER
    , gl_INT_SAMPLER_CUBE
    , gl_INT_VEC2
    , gl_INT_VEC3
    , gl_INT_VEC4
    , gl_SAMPLER_1D
    , gl_SAMPLER_1D_ARRAY
    , gl_SAMPLER_1D_ARRAY_SHADOW
    , gl_SAMPLER_1D_SHADOW
    , gl_SAMPLER_2D
    , gl_SAMPLER_2D_ARRAY
    , gl_SAMPLER_2D_ARRAY_SHADOW
    , gl_SAMPLER_2D_MULTISAMPLE
    , gl_SAMPLER_2D_MULTISAMPLE_ARRAY
    , gl_SAMPLER_2D_RECT
    , gl_SAMPLER_2D_RECT_SHADOW
    , gl_SAMPLER_2D_SHADOW
    , gl_SAMPLER_3D
    , gl_SAMPLER_BUFFER
    , gl_SAMPLER_CUBE
    , gl_SAMPLER_CUBE_SHADOW
    , gl_UNSIGNED_INT
    , gl_UNSIGNED_INT_SAMPLER_1D
    , gl_UNSIGNED_INT_SAMPLER_1D_ARRAY
    , gl_UNSIGNED_INT_SAMPLER_2D
    , gl_UNSIGNED_INT_SAMPLER_2D_ARRAY
    , gl_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE
    , gl_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY
    , gl_UNSIGNED_INT_SAMPLER_2D_RECT
    , gl_UNSIGNED_INT_SAMPLER_3D
    , gl_UNSIGNED_INT_SAMPLER_BUFFER
    , gl_UNSIGNED_INT_SAMPLER_CUBE
    , gl_UNSIGNED_INT_VEC2
    , gl_UNSIGNED_INT_VEC3
    , gl_UNSIGNED_INT_VEC4

    -- * CONTEXT PARAMETER realted *
    -- depth and stencil operation
    , gl_ALWAYS
    , gl_EQUAL
    , gl_GEQUAL
    , gl_GREATER
    , gl_LEQUAL
    , gl_LESS
    , gl_NEVER
    , gl_NOTEQUAL

    -- blending function
    , gl_FUNC_ADD
    , gl_FUNC_REVERSE_SUBTRACT
    , gl_FUNC_SUBTRACT
    , gl_MAX
    , gl_MIN

    -- blending
    , gl_CONSTANT_ALPHA
    , gl_CONSTANT_COLOR
    , gl_DST_ALPHA
    , gl_DST_COLOR
    , gl_ONE
    , gl_ONE_MINUS_CONSTANT_ALPHA
    , gl_ONE_MINUS_CONSTANT_COLOR
    , gl_ONE_MINUS_DST_ALPHA
    , gl_ONE_MINUS_DST_COLOR
    , gl_ONE_MINUS_SRC_ALPHA
    , gl_ONE_MINUS_SRC_COLOR
    , gl_SRC_ALPHA
    , gl_SRC_ALPHA_SATURATE
    , gl_SRC_COLOR
    , gl_ZERO

    -- logic operation
    , gl_AND
    , gl_AND_INVERTED
    , gl_AND_REVERSE
    , gl_CLEAR
    , gl_COPY
    , gl_COPY_INVERTED
    , gl_EQUIV
    , gl_INVERT
    , gl_NAND
    , gl_NOOP
    , gl_NOR
    , gl_OR
    , gl_OR_INVERTED
    , gl_OR_REVERSE
    , gl_SET
    , gl_XOR

    -- primitives
    , gl_LINES
    , gl_LINES_ADJACENCY
    , gl_LINE_STRIP
    , gl_LINE_STRIP_ADJACENCY
    , gl_POINTS
    , gl_TRIANGLES
    , gl_TRIANGLES_ADJACENCY
    , gl_TRIANGLE_FAN
    , gl_TRIANGLE_STRIP
    , gl_TRIANGLE_STRIP_ADJACENCY
    )

import IR
import Backend.GL.Type

toTrie :: Map String a -> Trie a
toTrie m = T.fromList [(pack k,v) | (k,v) <- Map.toList m]

setSampler :: GLint -> Int32 -> IO ()
setSampler i v = glUniform1i i $ fromIntegral v

z2 = V2 0 0 :: V2F
z3 = V3 0 0 0 :: V3F
z4 = V4 0 0 0 0 :: V4F

-- uniform functions
queryUniforms :: GLuint -> IO (Trie GLint, Trie InputType)
queryUniforms po = do
    ul <- getNameTypeSize po glGetActiveUniform glGetUniformLocation gl_ACTIVE_UNIFORMS gl_ACTIVE_UNIFORM_MAX_LENGTH
    let uNames = [n | (n,_,_,_) <- ul]
        uTypes = [fromGLType (e,s) | (_,_,e,s) <- ul]
        uLocation = [i | (_,i,_,_) <- ul]
    return $! (T.fromList $! zip uNames uLocation, T.fromList $! zip uNames uTypes)

b2w :: Bool -> GLuint
b2w True = 1
b2w False = 0

mkUniformSetter :: InputType -> IO (GLUniform, InputSetter)
mkUniformSetter t@Bool  = do {r <- newIORef 0;                            return $! (GLUniform t r, SBool $!  writeIORef r . b2w)}
mkUniformSetter t@V2B   = do {r <- newIORef (V2 0 0);                     return $! (GLUniform t r, SV2B $!   writeIORef r . fmap b2w)}
mkUniformSetter t@V3B   = do {r <- newIORef (V3 0 0 0);                   return $! (GLUniform t r, SV3B $!   writeIORef r . fmap b2w)}
mkUniformSetter t@V4B   = do {r <- newIORef (V4 0 0 0 0);                 return $! (GLUniform t r, SV4B $!   writeIORef r . fmap b2w)}
mkUniformSetter t@Word  = do {r <- newIORef 0;                            return $! (GLUniform t r, SWord $!  writeIORef r)}
mkUniformSetter t@V2U   = do {r <- newIORef (V2 0 0);                     return $! (GLUniform t r, SV2U $!   writeIORef r)}
mkUniformSetter t@V3U   = do {r <- newIORef (V3 0 0 0);                   return $! (GLUniform t r, SV3U $!   writeIORef r)}
mkUniformSetter t@V4U   = do {r <- newIORef (V4 0 0 0 0);                 return $! (GLUniform t r, SV4U $!   writeIORef r)}
mkUniformSetter t@Int   = do {r <- newIORef 0;                            return $! (GLUniform t r, SInt $!   writeIORef r)}
mkUniformSetter t@V2I   = do {r <- newIORef (V2 0 0);                     return $! (GLUniform t r, SV2I $!   writeIORef r)}
mkUniformSetter t@V3I   = do {r <- newIORef (V3 0 0 0);                   return $! (GLUniform t r, SV3I $!   writeIORef r)}
mkUniformSetter t@V4I   = do {r <- newIORef (V4 0 0 0 0);                 return $! (GLUniform t r, SV4I $!   writeIORef r)}
mkUniformSetter t@Float = do {r <- newIORef 0;                            return $! (GLUniform t r, SFloat $! writeIORef r)}
mkUniformSetter t@V2F   = do {r <- newIORef (V2 0 0);                     return $! (GLUniform t r, SV2F $!   writeIORef r)}
mkUniformSetter t@V3F   = do {r <- newIORef (V3 0 0 0);                   return $! (GLUniform t r, SV3F $!   writeIORef r)}
mkUniformSetter t@V4F   = do {r <- newIORef (V4 0 0 0 0);                 return $! (GLUniform t r, SV4F $!   writeIORef r)}
mkUniformSetter t@M22F  = do {r <- newIORef (V2 z2 z2);                   return $! (GLUniform t r, SM22F $!  writeIORef r)}
mkUniformSetter t@M23F  = do {r <- newIORef (V3 z2 z2 z2);                return $! (GLUniform t r, SM23F $!  writeIORef r)}
mkUniformSetter t@M24F  = do {r <- newIORef (V4 z2 z2 z2 z2);             return $! (GLUniform t r, SM24F $!  writeIORef r)}
mkUniformSetter t@M32F  = do {r <- newIORef (V2 z3 z3);                   return $! (GLUniform t r, SM32F $!  writeIORef r)}
mkUniformSetter t@M33F  = do {r <- newIORef (V3 z3 z3 z3);                return $! (GLUniform t r, SM33F $!  writeIORef r)}
mkUniformSetter t@M34F  = do {r <- newIORef (V4 z3 z3 z3 z3);             return $! (GLUniform t r, SM34F $!  writeIORef r)}
mkUniformSetter t@M42F  = do {r <- newIORef (V2 z4 z4);                   return $! (GLUniform t r, SM42F $!  writeIORef r)}
mkUniformSetter t@M43F  = do {r <- newIORef (V3 z4 z4 z4);                return $! (GLUniform t r, SM43F $!  writeIORef r)}
mkUniformSetter t@M44F  = do {r <- newIORef (V4 z4 z4 z4 z4);             return $! (GLUniform t r, SM44F $!  writeIORef r)}
mkUniformSetter t@FTexture2D = do {r <- newIORef (TextureData 0);         return $! (GLUniform t r, SFTexture2D $! writeIORef r)}

-- sets value based uniforms only (does not handle textures)
setUniform :: Storable a => GLint -> InputType -> IORef a -> IO ()
setUniform i ty ref = do
    v <- readIORef ref
    let false = fromIntegral gl_FALSE
    with v $ \p -> case ty of
        Bool        -> glUniform1uiv i 1 (castPtr p)
        V2B         -> glUniform2uiv i 1 (castPtr p)
        V3B         -> glUniform3uiv i 1 (castPtr p)
        V4B         -> glUniform4uiv i 1 (castPtr p)
        Word        -> glUniform1uiv i 1 (castPtr p)
        V2U         -> glUniform2uiv i 1 (castPtr p)
        V3U         -> glUniform3uiv i 1 (castPtr p)
        V4U         -> glUniform4uiv i 1 (castPtr p)
        Int         -> glUniform1iv i 1 (castPtr p)
        V2I         -> glUniform2iv i 1 (castPtr p)
        V3I         -> glUniform3iv i 1 (castPtr p)
        V4I         -> glUniform4iv i 1 (castPtr p)
        Float       -> glUniform1fv i 1 (castPtr p)
        V2F         -> glUniform2fv i 1 (castPtr p)
        V3F         -> glUniform3fv i 1 (castPtr p)
        V4F         -> glUniform4fv i 1 (castPtr p)
        M22F        -> glUniformMatrix2fv   i 1 false (castPtr p)
        M23F        -> glUniformMatrix2x3fv i 1 false (castPtr p)
        M24F        -> glUniformMatrix2x4fv i 1 false (castPtr p)
        M32F        -> glUniformMatrix3x2fv i 1 false (castPtr p)
        M33F        -> glUniformMatrix3fv   i 1 false (castPtr p)
        M34F        -> glUniformMatrix3x4fv i 1 false (castPtr p)
        M42F        -> glUniformMatrix4x2fv i 1 false (castPtr p)
        M43F        -> glUniformMatrix4x3fv i 1 false (castPtr p)
        M44F        -> glUniformMatrix4fv   i 1 false (castPtr p)
        _   -> fail "internal error (setUniform)!"

-- attribute functions
queryStreams :: GLuint -> IO (Trie GLuint, Trie InputType)
queryStreams po = do
    al <- getNameTypeSize po glGetActiveAttrib glGetAttribLocation gl_ACTIVE_ATTRIBUTES gl_ACTIVE_ATTRIBUTE_MAX_LENGTH
    let aNames = [n | (n,_,_,_) <- al]
        aTypes = [fromGLType (e,s) | (_,_,e,s) <- al]
        aLocation = [fromIntegral i | (_,i,_,_) <- al]
    return $! (T.fromList $! zip aNames aLocation, T.fromList $! zip aNames aTypes)

arrayTypeToGLType :: ArrayType -> GLenum
arrayTypeToGLType a = case a of
    ArrWord8    -> gl_UNSIGNED_BYTE
    ArrWord16   -> gl_UNSIGNED_SHORT
    ArrWord32   -> gl_UNSIGNED_INT
    ArrInt8     -> gl_BYTE
    ArrInt16    -> gl_SHORT
    ArrInt32    -> gl_INT
    ArrFloat    -> gl_FLOAT
    ArrHalf     -> gl_HALF_FLOAT

setVertexAttrib :: GLuint -> Stream Buffer -> IO ()
setVertexAttrib i val = case val of
    ConstWord v     -> with v $! \p -> glVertexAttribI1uiv i $! castPtr p
    ConstV2U v      -> with v $! \p -> glVertexAttribI2uiv i $! castPtr p
    ConstV3U v      -> with v $! \p -> glVertexAttribI3uiv i $! castPtr p
    ConstV4U v      -> with v $! \p -> glVertexAttribI4uiv i $! castPtr p
    ConstInt v      -> with v $! \p -> glVertexAttribI1iv i $! castPtr p
    ConstV2I v      -> with v $! \p -> glVertexAttribI2iv i $! castPtr p
    ConstV3I v      -> with v $! \p -> glVertexAttribI3iv i $! castPtr p
    ConstV4I v      -> with v $! \p -> glVertexAttribI4iv i $! castPtr p
    ConstFloat v    -> setAFloat i v
    ConstV2F v      -> setAV2F i v
    ConstV3F v      -> setAV3F i v
    ConstV4F v      -> setAV4F i v
    ConstM22F (V2 x y)      -> setAV2F i x >> setAV2F (i+1) y
    ConstM23F (V3 x y z)    -> setAV2F i x >> setAV2F (i+1) y >> setAV2F (i+2) z
    ConstM24F (V4 x y z w)  -> setAV2F i x >> setAV2F (i+1) y >> setAV2F (i+2) z >> setAV2F (i+3) w
    ConstM32F (V2 x y)      -> setAV3F i x >> setAV3F (i+1) y
    ConstM33F (V3 x y z)    -> setAV3F i x >> setAV3F (i+1) y >> setAV3F (i+2) z
    ConstM34F (V4 x y z w)  -> setAV3F i x >> setAV3F (i+1) y >> setAV3F (i+2) z >> setAV3F (i+3) w
    ConstM42F (V2 x y)      -> setAV4F i x >> setAV4F (i+1) y
    ConstM43F (V3 x y z)    -> setAV4F i x >> setAV4F (i+1) y >> setAV4F (i+2) z
    ConstM44F (V4 x y z w)  -> setAV4F i x >> setAV4F (i+1) y >> setAV4F (i+2) z >> setAV4F (i+3) w
    _ -> fail "internal error (setVertexAttrib)!"

setAFloat :: GLuint -> Float -> IO ()
setAV2F   :: GLuint -> V2F -> IO ()
setAV3F   :: GLuint -> V3F -> IO ()
setAV4F   :: GLuint -> V4F -> IO ()
setAFloat i v = with v $! \p -> glVertexAttrib1fv i $! castPtr p
setAV2F i v   = with v $! \p -> glVertexAttrib2fv i $! castPtr p
setAV3F i v   = with v $! \p -> glVertexAttrib3fv i $! castPtr p
setAV4F i v   = with v $! \p -> glVertexAttrib4fv i $! castPtr p

-- result list: [(name string,location,gl type,component count)]
getNameTypeSize :: GLuint -> (GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> Ptr GLchar -> IO ())
                   -> (GLuint -> Ptr GLchar -> IO GLint) -> GLenum -> GLenum -> IO [(ByteString,GLint,GLenum,GLint)]
getNameTypeSize o f g enum enumLen = do
    nameLen <- glGetProgramiv1 enumLen o
    allocaArray (fromIntegral nameLen) $! \namep -> alloca $! \sizep -> alloca $! \typep -> do
        n <- glGetProgramiv1 enum o
        forM [0..n-1] $! \i -> f o (fromIntegral i) (fromIntegral nameLen) nullPtr sizep typep namep >>
            (,,,) <$> SB.packCString (castPtr namep) <*> g o namep <*> peek typep <*> peek sizep

fromGLType :: (GLenum,GLint) -> InputType
fromGLType (t,1)
    | t == gl_BOOL              = Bool
    | t == gl_BOOL_VEC2         = V2B
    | t == gl_BOOL_VEC3         = V3B
    | t == gl_BOOL_VEC4         = V4B
    | t == gl_UNSIGNED_INT      = Word
    | t == gl_UNSIGNED_INT_VEC2 = V2U
    | t == gl_UNSIGNED_INT_VEC3 = V3U
    | t == gl_UNSIGNED_INT_VEC4 = V4U
    | t == gl_INT               = Int
    | t == gl_INT_VEC2          = V2I
    | t == gl_INT_VEC3          = V3I
    | t == gl_INT_VEC4          = V4I
    | t == gl_FLOAT             = Float
    | t == gl_FLOAT_VEC2        = V2F
    | t == gl_FLOAT_VEC3        = V3F
    | t == gl_FLOAT_VEC4        = V4F
    | t == gl_FLOAT_MAT2        = M22F
    | t == gl_FLOAT_MAT2x3      = M23F
    | t == gl_FLOAT_MAT2x4      = M24F
    | t == gl_FLOAT_MAT3x2      = M32F
    | t == gl_FLOAT_MAT3        = M33F
    | t == gl_FLOAT_MAT3x4      = M34F
    | t == gl_FLOAT_MAT4x2      = M42F
    | t == gl_FLOAT_MAT4x3      = M43F
    | t == gl_FLOAT_MAT4        = M44F
    | t == gl_SAMPLER_1D_ARRAY_SHADOW                   = STexture1DArray
    | t == gl_SAMPLER_1D_SHADOW                         = STexture1D
    | t == gl_SAMPLER_2D_ARRAY_SHADOW                   = STexture2DArray
    | t == gl_SAMPLER_2D_RECT_SHADOW                    = STexture2DRect
    | t == gl_SAMPLER_2D_SHADOW                         = STexture2D
    | t == gl_SAMPLER_CUBE_SHADOW                       = STextureCube
    | t == gl_INT_SAMPLER_1D                            = ITexture1D
    | t == gl_INT_SAMPLER_1D_ARRAY                      = ITexture1DArray
    | t == gl_INT_SAMPLER_2D                            = ITexture2D
    | t == gl_INT_SAMPLER_2D_ARRAY                      = ITexture2DArray
    | t == gl_INT_SAMPLER_2D_MULTISAMPLE                = ITexture2DMS
    | t == gl_INT_SAMPLER_2D_MULTISAMPLE_ARRAY          = ITexture2DMSArray
    | t == gl_INT_SAMPLER_2D_RECT                       = ITexture2DRect
    | t == gl_INT_SAMPLER_3D                            = ITexture3D
    | t == gl_INT_SAMPLER_BUFFER                        = ITextureBuffer
    | t == gl_INT_SAMPLER_CUBE                          = ITextureCube
    | t == gl_SAMPLER_1D                                = FTexture1D
    | t == gl_SAMPLER_1D_ARRAY                          = FTexture1DArray
    | t == gl_SAMPLER_2D                                = FTexture2D
    | t == gl_SAMPLER_2D_ARRAY                          = FTexture2DArray
    | t == gl_SAMPLER_2D_MULTISAMPLE                    = FTexture2DMS
    | t == gl_SAMPLER_2D_MULTISAMPLE_ARRAY              = FTexture2DMSArray
    | t == gl_SAMPLER_2D_RECT                           = FTexture2DRect
    | t == gl_SAMPLER_3D                                = FTexture3D
    | t == gl_SAMPLER_BUFFER                            = FTextureBuffer
    | t == gl_SAMPLER_CUBE                              = FTextureCube
    | t == gl_UNSIGNED_INT_SAMPLER_1D                   = UTexture1D
    | t == gl_UNSIGNED_INT_SAMPLER_1D_ARRAY             = UTexture1DArray
    | t == gl_UNSIGNED_INT_SAMPLER_2D                   = UTexture2D
    | t == gl_UNSIGNED_INT_SAMPLER_2D_ARRAY             = UTexture2DArray
    | t == gl_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE       = UTexture2DMS
    | t == gl_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY = UTexture2DMSArray
    | t == gl_UNSIGNED_INT_SAMPLER_2D_RECT              = UTexture2DRect
    | t == gl_UNSIGNED_INT_SAMPLER_3D                   = UTexture3D
    | t == gl_UNSIGNED_INT_SAMPLER_BUFFER               = UTextureBuffer
    | t == gl_UNSIGNED_INT_SAMPLER_CUBE                 = UTextureCube
    | otherwise = error "Failed fromGLType"
fromGLUniformType _ = error "Failed fromGLType"

printShaderLog :: GLuint -> IO ()
printShaderLog o = do
    i <- glGetShaderiv1 gl_INFO_LOG_LENGTH o
    when (i > 0) $
      alloca $ \sizePtr -> allocaArray (fromIntegral i) $! \ps -> do
        glGetShaderInfoLog o (fromIntegral i) sizePtr ps
        size <- peek sizePtr
        log <- SB.packCStringLen (castPtr ps, fromIntegral size)
        SB.putStrLn log

glGetShaderiv1 :: GLenum -> GLuint -> IO GLint
glGetShaderiv1 pname o = alloca $! \pi -> glGetShaderiv o pname pi >> peek pi

glGetProgramiv1 :: GLenum -> GLuint -> IO GLint
glGetProgramiv1 pname o = alloca $! \pi -> glGetProgramiv o pname pi >> peek pi

printProgramLog :: GLuint -> IO ()
printProgramLog o = do
    i <- glGetProgramiv1 gl_INFO_LOG_LENGTH o
    when (i > 0) $
      alloca $ \sizePtr -> allocaArray (fromIntegral i) $! \ps -> do
        glGetProgramInfoLog o (fromIntegral i) sizePtr ps
        size <- peek sizePtr
        log <- SB.packCStringLen (castPtr ps, fromIntegral size)
        SB.putStrLn log

compileShader :: GLuint -> [ByteString] -> IO ()
compileShader o srcl = withMany SB.useAsCString srcl $! \l -> withArray l $! \p -> do
    glShaderSource o (fromIntegral $! length srcl) (castPtr p) nullPtr
    glCompileShader o
    printShaderLog o
    status <- glGetShaderiv1 gl_COMPILE_STATUS o
    when (status /= fromIntegral gl_TRUE) $ fail "compileShader failed!"

checkGL :: IO ByteString
checkGL = do
    let f e | e == gl_INVALID_ENUM                  = "INVALID_ENUM"
            | e == gl_INVALID_VALUE                 = "INVALID_VALUE"
            | e == gl_INVALID_OPERATION             = "INVALID_OPERATION"
            | e == gl_INVALID_FRAMEBUFFER_OPERATION = "INVALID_FRAMEBUFFER_OPERATION"
            | e == gl_OUT_OF_MEMORY                 = "OUT_OF_MEMORY"
            | e == gl_NO_ERROR                      = "OK"
            | otherwise                             = "Unknown error"
    e <- glGetError
    return $ f e

streamToInputType :: Stream Buffer -> InputType
streamToInputType s = case s of
    ConstWord  _    -> Word
    ConstV2U   _    -> V2U
    ConstV3U   _    -> V3U
    ConstV4U   _    -> V4U
    ConstInt   _    -> Int
    ConstV2I   _    -> V2I
    ConstV3I   _    -> V3I
    ConstV4I   _    -> V4I
    ConstFloat _    -> Float
    ConstV2F   _    -> V2F
    ConstV3F   _    -> V3F
    ConstV4F   _    -> V4F
    ConstM22F  _    -> M22F
    ConstM23F  _    -> M23F
    ConstM24F  _    -> M24F
    ConstM32F  _    -> M32F
    ConstM33F  _    -> M33F
    ConstM34F  _    -> M34F
    ConstM42F  _    -> M42F
    ConstM43F  _    -> M43F
    ConstM44F  _    -> M44F
    Stream t (Buffer a _) i _ _
        | 0 <= i && i < V.length a &&
          if elem t integralTypes then elem at integralArrTypes else True
        -> fromStreamType t
        | otherwise -> throw $ userError "streamToInputType failed"
      where
        at = arrType $! (a V.! i)
        integralTypes    = [TWord, TV2U, TV3U, TV4U, TInt, TV2I, TV3I, TV4I]
        integralArrTypes = [ArrWord8, ArrWord16, ArrWord32, ArrInt8, ArrInt16, ArrInt32]

comparisonFunctionToGLType :: ComparisonFunction -> GLenum
comparisonFunctionToGLType a = case a of
    Always      -> gl_ALWAYS
    Equal       -> gl_EQUAL
    Gequal      -> gl_GEQUAL
    Greater     -> gl_GREATER
    Lequal      -> gl_LEQUAL
    Less        -> gl_LESS
    Never       -> gl_NEVER
    Notequal    -> gl_NOTEQUAL

logicOperationToGLType :: LogicOperation -> GLenum
logicOperationToGLType a = case a of
    And             -> gl_AND
    AndInverted     -> gl_AND_INVERTED
    AndReverse      -> gl_AND_REVERSE
    Clear           -> gl_CLEAR
    Copy            -> gl_COPY
    CopyInverted    -> gl_COPY_INVERTED
    Equiv           -> gl_EQUIV
    Invert          -> gl_INVERT
    Nand            -> gl_NAND
    Noop            -> gl_NOOP
    Nor             -> gl_NOR
    Or              -> gl_OR
    OrInverted      -> gl_OR_INVERTED
    OrReverse       -> gl_OR_REVERSE
    Set             -> gl_SET
    Xor             -> gl_XOR

blendEquationToGLType :: BlendEquation -> GLenum
blendEquationToGLType a = case a of
    FuncAdd             -> gl_FUNC_ADD
    FuncReverseSubtract -> gl_FUNC_REVERSE_SUBTRACT
    FuncSubtract        -> gl_FUNC_SUBTRACT
    Max                 -> gl_MAX
    Min                 -> gl_MIN

blendingFactorToGLType :: BlendingFactor -> GLenum
blendingFactorToGLType a = case a of
    ConstantAlpha           -> gl_CONSTANT_ALPHA
    ConstantColor           -> gl_CONSTANT_COLOR
    DstAlpha                -> gl_DST_ALPHA
    DstColor                -> gl_DST_COLOR
    One                     -> gl_ONE
    OneMinusConstantAlpha   -> gl_ONE_MINUS_CONSTANT_ALPHA
    OneMinusConstantColor   -> gl_ONE_MINUS_CONSTANT_COLOR
    OneMinusDstAlpha        -> gl_ONE_MINUS_DST_ALPHA
    OneMinusDstColor        -> gl_ONE_MINUS_DST_COLOR
    OneMinusSrcAlpha        -> gl_ONE_MINUS_SRC_ALPHA
    OneMinusSrcColor        -> gl_ONE_MINUS_SRC_COLOR
    SrcAlpha                -> gl_SRC_ALPHA
    SrcAlphaSaturate        -> gl_SRC_ALPHA_SATURATE
    SrcColor                -> gl_SRC_COLOR
    Zero                    -> gl_ZERO

{-
data ColorArity = Red | RG | RGB | RGBA deriving (Show,Eq,Ord)
data TextureDataType
    = FloatT        ColorArity
    | IntT          ColorArity
    | WordT         ColorArity
    | ShadowT
    deriving (Show, Eq, Ord)
-}
textureDataTypeToGLType :: ImageSemantic -> TextureDataType -> GLenum
textureDataTypeToGLType Color a = case a of
    FloatT Red  -> gl_R32F
    IntT   Red  -> gl_R32I
    WordT  Red  -> gl_R32UI
    FloatT RG   -> gl_RG32F
    IntT   RG   -> gl_RG32I
    WordT  RG   -> gl_RG32UI
    FloatT RGBA -> gl_RGBA32F
    IntT   RGBA -> gl_RGBA32I
    WordT  RGBA -> gl_RGBA32UI
    a           -> error $ "FIXME: This texture format is not yet supported" ++ show a
textureDataTypeToGLType Depth a = case a of
    FloatT Red  -> gl_DEPTH_COMPONENT32F
    WordT  Red  -> gl_DEPTH_COMPONENT32
    a           -> error $ "FIXME: This texture format is not yet supported" ++ show a
textureDataTypeToGLType Stencil a = case a of
    a           -> error $ "FIXME: This texture format is not yet supported" ++ show a

textureDataTypeToGLArityType :: ImageSemantic -> TextureDataType -> GLenum
textureDataTypeToGLArityType Color a = case a of
    FloatT Red  -> gl_RED
    IntT   Red  -> gl_RED
    WordT  Red  -> gl_RED
    FloatT RG   -> gl_RG
    IntT   RG   -> gl_RG
    WordT  RG   -> gl_RG
    FloatT RGBA -> gl_RGBA
    IntT   RGBA -> gl_RGBA
    WordT  RGBA -> gl_RGBA
    a           -> error $ "FIXME: This texture format is not yet supported" ++ show a
textureDataTypeToGLArityType Depth a = case a of
    FloatT Red  -> gl_DEPTH_COMPONENT
    WordT  Red  -> gl_DEPTH_COMPONENT
    a           -> error $ "FIXME: This texture format is not yet supported" ++ show a
textureDataTypeToGLArityType Stencil a = case a of
    a           -> error $ "FIXME: This texture format is not yet supported" ++ show a
{-
Texture and renderbuffer color formats (R):
    R11F_G11F_B10F
    R16
    R16F
    R16I
    R16UI
    R32F
    R32I
    R32UI
    R8
    R8I
    R8UI
    RG16
    RG16F
    RG16I
    RG16UI
    RG32F
    RG32I
    RG32UI
    RG8
    RG8I
    RG8UI
    RGB10_A2
    RGB10_A2UI
    RGBA16
    RGBA16F
    RGBA16I
    RGBA16UI
    RGBA32F
    RGBA32I
    RGBA32UI
    RGBA8
    RGBA8I
    RGBA8UI
    SRGB8_ALPHA8
-}

glGetIntegerv1 :: GLenum -> IO GLint
glGetIntegerv1 e = alloca $ \pi -> glGetIntegerv e pi >> peek pi

checkFBO :: IO ByteString
checkFBO = do
    let f e | e == gl_FRAMEBUFFER_UNDEFINED                 = "FRAMEBUFFER_UNDEFINED"
            | e == gl_FRAMEBUFFER_INCOMPLETE_ATTACHMENT     = "FRAMEBUFFER_INCOMPLETE_ATTACHMENT"
            | e == gl_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER    = "FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER"
            | e == gl_FRAMEBUFFER_INCOMPLETE_READ_BUFFER    = "FRAMEBUFFER_INCOMPLETE_READ_BUFFER"
            | e == gl_FRAMEBUFFER_UNSUPPORTED               = "FRAMEBUFFER_UNSUPPORTED"
            | e == gl_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE    = "FRAMEBUFFER_INCOMPLETE_MULTISAMPLE"
            | e == gl_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS  = "FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS"
            | e == gl_FRAMEBUFFER_COMPLETE                  = "FRAMEBUFFER_COMPLETE"
            | otherwise                                     = "Unknown error"
    e <- glCheckFramebufferStatus gl_DRAW_FRAMEBUFFER
    return $ f e

filterToGLType :: Filter -> GLenum
filterToGLType a = case a of
    Nearest                 -> gl_NEAREST
    Linear                  -> gl_LINEAR
    NearestMipmapNearest    -> gl_NEAREST_MIPMAP_NEAREST
    NearestMipmapLinear     -> gl_NEAREST_MIPMAP_LINEAR
    LinearMipmapNearest     -> gl_LINEAR_MIPMAP_NEAREST
    LinearMipmapLinear      -> gl_LINEAR_MIPMAP_LINEAR

edgeModeToGLType :: EdgeMode -> GLenum
edgeModeToGLType a = case a of
    Repeat          -> gl_REPEAT
    MirroredRepeat  -> gl_MIRRORED_REPEAT
    ClampToEdge     -> gl_CLAMP_TO_EDGE
    ClampToBorder   -> gl_CLAMP_TO_BORDER

setTextureSamplerParameters :: GLenum -> SamplerDescriptor -> IO ()
setTextureSamplerParameters t s = do
    glTexParameteri t gl_TEXTURE_WRAP_S $ fromIntegral $ edgeModeToGLType $ samplerWrapS s
    case samplerWrapT s of
        Nothing -> return ()
        Just a  -> glTexParameteri t gl_TEXTURE_WRAP_T $ fromIntegral $ edgeModeToGLType a
    case samplerWrapR s of
        Nothing -> return ()
        Just a  -> glTexParameteri t gl_TEXTURE_WRAP_R $ fromIntegral $ edgeModeToGLType a
    glTexParameteri t gl_TEXTURE_MIN_FILTER $ fromIntegral $ filterToGLType $ samplerMinFilter s
    glTexParameteri t gl_TEXTURE_MAG_FILTER $ fromIntegral $ filterToGLType $ samplerMagFilter s

    let setBColorV4F a = with a $ \p -> glTexParameterfv t gl_TEXTURE_BORDER_COLOR $ castPtr p
        setBColorV4I a = with a $ \p -> glTexParameterIiv t gl_TEXTURE_BORDER_COLOR $ castPtr p
        setBColorV4U a = with a $ \p -> glTexParameterIuiv t gl_TEXTURE_BORDER_COLOR $ castPtr p
    case samplerBorderColor s of
        -- float, word, int, red, rg, rgb, rgba
        VFloat a        -> setBColorV4F $ V4 a 0 0 0
        VV2F (V2 a b)   -> setBColorV4F $ V4 a b 0 0
        VV3F (V3 a b c) -> setBColorV4F $ V4 a b c 0
        VV4F a          -> setBColorV4F a

        VInt a          -> setBColorV4I $ V4 a 0 0 0
        VV2I (V2 a b)   -> setBColorV4I $ V4 a b 0 0
        VV3I (V3 a b c) -> setBColorV4I $ V4 a b c 0
        VV4I a          -> setBColorV4I a

        VWord a         -> setBColorV4U $ V4 a 0 0 0
        VV2U (V2 a b)   -> setBColorV4U $ V4 a b 0 0
        VV3U (V3 a b c) -> setBColorV4U $ V4 a b c 0
        VV4U a          -> setBColorV4U a
        _ -> fail "internal error (setTextureSamplerParameters)!"

    case samplerMinLod s of
        Nothing -> return ()
        Just a  -> glTexParameterf t gl_TEXTURE_MIN_LOD $ realToFrac a
    case samplerMaxLod s of
        Nothing -> return ()
        Just a  -> glTexParameterf t gl_TEXTURE_MAX_LOD $ realToFrac a
    glTexParameterf t gl_TEXTURE_LOD_BIAS $ realToFrac $ samplerLodBias s
    case samplerCompareFunc s of
        Nothing -> glTexParameteri t gl_TEXTURE_COMPARE_MODE $ fromIntegral gl_NONE
        Just a  -> do
            glTexParameteri t gl_TEXTURE_COMPARE_MODE $ fromIntegral gl_COMPARE_REF_TO_TEXTURE
            glTexParameteri t gl_TEXTURE_COMPARE_FUNC $ fromIntegral $ comparisonFunctionToGLType a

compileTexture :: TextureDescriptor -> IO GLTexture
compileTexture txDescriptor = do
    to <- alloca $! \pto -> glGenTextures 1 pto >> peek pto
    let TextureDescriptor
            { textureType       = txType
            , textureSize       = txSize
            , textureSemantic   = txSemantic
            , textureSampler    = txSampler
            , textureBaseLevel  = txBaseLevel
            , textureMaxLevel   = txMaxLevel
            } = txDescriptor

        txSetup txTarget dTy = do
            let internalFormat  = fromIntegral $ textureDataTypeToGLType txSemantic dTy
                dataFormat      = fromIntegral $ textureDataTypeToGLArityType txSemantic dTy
            glBindTexture txTarget to
            glTexParameteri txTarget gl_TEXTURE_BASE_LEVEL $ fromIntegral txBaseLevel
            glTexParameteri txTarget gl_TEXTURE_MAX_LEVEL $ fromIntegral txMaxLevel
            setTextureSamplerParameters txTarget txSampler
            return (internalFormat,dataFormat)

        mipSize 0 x = [x]
        mipSize n x = x : mipSize (n-1) (x `div` 2)
        mipS = mipSize (txMaxLevel - txBaseLevel)
        levels = [txBaseLevel..txMaxLevel]
    target <- case txType of
        Texture1D dTy layerCnt -> do
            let VWord txW = txSize
                txTarget = if layerCnt > 1 then gl_TEXTURE_1D_ARRAY else gl_TEXTURE_1D
            (internalFormat,dataFormat) <- txSetup txTarget dTy
            forM_ (zip levels (mipS txW)) $ \(l,w) -> case layerCnt > 1 of
                True    -> glTexImage2D txTarget (fromIntegral l) internalFormat (fromIntegral w) (fromIntegral layerCnt) 0 dataFormat gl_UNSIGNED_BYTE nullPtr
                False   -> glTexImage1D txTarget (fromIntegral l) internalFormat (fromIntegral w) 0 dataFormat gl_UNSIGNED_BYTE nullPtr
            return txTarget
        Texture2D dTy layerCnt -> do
            let VV2U (V2 txW txH) = txSize
                txTarget = if layerCnt > 1 then gl_TEXTURE_2D_ARRAY else gl_TEXTURE_2D
            (internalFormat,dataFormat) <- txSetup txTarget dTy
            forM_ (zip3 levels (mipS txW) (mipS txH)) $ \(l,w,h) -> case layerCnt > 1 of
                True    -> glTexImage3D txTarget (fromIntegral l) internalFormat (fromIntegral w) (fromIntegral h) (fromIntegral layerCnt) 0 dataFormat gl_UNSIGNED_BYTE nullPtr
                False   -> glTexImage2D txTarget (fromIntegral l) internalFormat (fromIntegral w) (fromIntegral h) 0 dataFormat gl_UNSIGNED_BYTE nullPtr
            return txTarget
        Texture3D dTy -> do
            let VV3U (V3 txW txH txD) = txSize
                txTarget = gl_TEXTURE_3D
            (internalFormat,dataFormat) <- txSetup txTarget dTy
            forM_ (zip4 levels (mipS txW) (mipS txH) (mipS txD)) $ \(l,w,h,d) ->
                glTexImage3D txTarget (fromIntegral l) internalFormat (fromIntegral w) (fromIntegral h) (fromIntegral d) 0 dataFormat gl_UNSIGNED_BYTE nullPtr
            return txTarget
        TextureCube dTy -> do
            let VV2U (V2 txW txH) = txSize
                txTarget = gl_TEXTURE_CUBE_MAP
                targets =
                    [ gl_TEXTURE_CUBE_MAP_POSITIVE_X 
                    , gl_TEXTURE_CUBE_MAP_NEGATIVE_X
                    , gl_TEXTURE_CUBE_MAP_POSITIVE_Y
                    , gl_TEXTURE_CUBE_MAP_NEGATIVE_Y
                    , gl_TEXTURE_CUBE_MAP_POSITIVE_Z
                    , gl_TEXTURE_CUBE_MAP_NEGATIVE_Z
                    ]
            (internalFormat,dataFormat) <- txSetup txTarget dTy
            forM_ (zip3 levels (mipS txW) (mipS txH)) $ \(l,w,h) -> 
                forM_ targets $ \t -> glTexImage2D t (fromIntegral l) internalFormat (fromIntegral w) (fromIntegral h) 0 dataFormat gl_UNSIGNED_BYTE nullPtr
            return txTarget
        TextureRect dTy -> do
            let VV2U (V2 txW txH) = txSize
                txTarget = gl_TEXTURE_RECTANGLE
            (internalFormat,dataFormat) <- txSetup txTarget dTy
            forM_ (zip3 levels (mipS txW) (mipS txH)) $ \(l,w,h) -> 
                glTexImage2D txTarget (fromIntegral l) internalFormat (fromIntegral w) (fromIntegral h) 0 dataFormat gl_UNSIGNED_BYTE nullPtr
            return txTarget
        Texture2DMS dTy layerCnt sampleCount isFixedLocations -> do
            let VV2U (V2 w h)   = txSize
                txTarget        = if layerCnt > 1 then gl_TEXTURE_2D_MULTISAMPLE_ARRAY else gl_TEXTURE_2D_MULTISAMPLE
                isFixed         = fromIntegral $ if isFixedLocations then gl_TRUE else gl_FALSE
            (internalFormat,dataFormat) <- txSetup txTarget dTy
            case layerCnt > 1 of
                True    -> glTexImage3DMultisample txTarget (fromIntegral sampleCount) internalFormat (fromIntegral w) (fromIntegral h) (fromIntegral layerCnt) isFixed
                False   -> glTexImage2DMultisample txTarget (fromIntegral sampleCount) internalFormat (fromIntegral w) (fromIntegral h) isFixed
            return txTarget
        TextureBuffer dTy -> do
            fail "internal error: buffer texture is not supported yet"
            -- TODO
            let VV2U (V2 w h)   = txSize
                txTarget        = gl_TEXTURE_2D
            (internalFormat,dataFormat) <- txSetup txTarget dTy
            glTexImage2D gl_TEXTURE_2D 0 internalFormat (fromIntegral w) (fromIntegral h) 0 dataFormat gl_UNSIGNED_BYTE nullPtr
            return txTarget
    return $ GLTexture
        { glTextureObject   = to
        , glTextureTarget   = target
        }

primitiveToFetchPrimitive :: Primitive -> FetchPrimitive
primitiveToFetchPrimitive prim = case prim of
    TriangleStrip           -> Triangles
    TriangleList            -> Triangles
    TriangleFan             -> Triangles
    LineStrip               -> Lines
    LineList                -> Lines
    PointList               -> Points
    TriangleStripAdjacency  -> TrianglesAdjacency
    TriangleListAdjacency   -> TrianglesAdjacency
    LineStripAdjacency      -> LinesAdjacency
    LineListAdjacency       -> LinesAdjacency

primitiveToGLType :: Primitive -> GLenum
primitiveToGLType p = case p of
    TriangleStrip           -> gl_TRIANGLE_STRIP
    TriangleList            -> gl_TRIANGLES
    TriangleFan             -> gl_TRIANGLE_FAN
    LineStrip               -> gl_LINE_STRIP
    LineList                -> gl_LINES
    PointList               -> gl_POINTS
    TriangleStripAdjacency  -> gl_TRIANGLE_STRIP_ADJACENCY
    TriangleListAdjacency   -> gl_TRIANGLES_ADJACENCY
    LineStripAdjacency      -> gl_LINE_STRIP_ADJACENCY
    LineListAdjacency       -> gl_LINES_ADJACENCY

inputTypeToTextureTarget :: InputType -> GLenum
inputTypeToTextureTarget ty = case ty of
    STexture1D          -> gl_TEXTURE_1D
    STexture2D          -> gl_TEXTURE_2D
    STextureCube        -> gl_TEXTURE_CUBE_MAP
    STexture1DArray     -> gl_TEXTURE_1D_ARRAY
    STexture2DArray     -> gl_TEXTURE_2D_ARRAY
    STexture2DRect      -> gl_TEXTURE_RECTANGLE

    FTexture1D          -> gl_TEXTURE_1D
    FTexture2D          -> gl_TEXTURE_2D
    FTexture3D          -> gl_TEXTURE_3D
    FTextureCube        -> gl_TEXTURE_CUBE_MAP
    FTexture1DArray     -> gl_TEXTURE_1D_ARRAY
    FTexture2DArray     -> gl_TEXTURE_2D_ARRAY
    FTexture2DMS        -> gl_TEXTURE_2D_MULTISAMPLE
    FTexture2DMSArray   -> gl_TEXTURE_2D_MULTISAMPLE_ARRAY
    FTextureBuffer      -> gl_TEXTURE_BUFFER
    FTexture2DRect      -> gl_TEXTURE_RECTANGLE

    ITexture1D          -> gl_TEXTURE_1D
    ITexture2D          -> gl_TEXTURE_2D
    ITexture3D          -> gl_TEXTURE_3D
    ITextureCube        -> gl_TEXTURE_CUBE_MAP
    ITexture1DArray     -> gl_TEXTURE_1D_ARRAY
    ITexture2DArray     -> gl_TEXTURE_2D_ARRAY
    ITexture2DMS        -> gl_TEXTURE_2D_MULTISAMPLE
    ITexture2DMSArray   -> gl_TEXTURE_2D_MULTISAMPLE_ARRAY
    ITextureBuffer      -> gl_TEXTURE_BUFFER
    ITexture2DRect      -> gl_TEXTURE_RECTANGLE

    UTexture1D          -> gl_TEXTURE_1D
    UTexture2D          -> gl_TEXTURE_2D
    UTexture3D          -> gl_TEXTURE_3D
    UTextureCube        -> gl_TEXTURE_CUBE_MAP
    UTexture1DArray     -> gl_TEXTURE_1D_ARRAY
    UTexture2DArray     -> gl_TEXTURE_2D_ARRAY
    UTexture2DMS        -> gl_TEXTURE_2D_MULTISAMPLE
    UTexture2DMSArray   -> gl_TEXTURE_2D_MULTISAMPLE_ARRAY
    UTextureBuffer      -> gl_TEXTURE_BUFFER
    UTexture2DRect      -> gl_TEXTURE_RECTANGLE

    _ -> error "internal error (inputTypeToTextureTarget)!"
