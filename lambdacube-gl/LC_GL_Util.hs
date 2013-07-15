module LC_GL_Util (
    queryUniforms,
    queryStreams,
    mkUniformSetter,
    mkSSetter,
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
    primitiveToFetchPrimitive
) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Data.IORef
import Data.List as L
import Data.Trie as T
import Foreign
import qualified Data.ByteString.Char8 as SB
import qualified Data.Vector as V
import Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable as MV

import Graphics.Rendering.OpenGL.Raw.Core32
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

    , gl_TEXTURE_CUBE_MAP
    , gl_TEXTURE_CUBE_MAP_POSITIVE_X
    , gl_TEXTURE_CUBE_MAP_NEGATIVE_X
    , gl_TEXTURE_CUBE_MAP_POSITIVE_Y
    , gl_TEXTURE_CUBE_MAP_NEGATIVE_Y
    , gl_TEXTURE_CUBE_MAP_POSITIVE_Z
    , gl_TEXTURE_CUBE_MAP_NEGATIVE_Z
    , gl_TEXTURE_1D
    , gl_TEXTURE_1D_ARRAY
    , gl_TEXTURE_2D
    , gl_TEXTURE_2D_ARRAY
    , gl_TEXTURE_3D
    , gl_TEXTURE_RECTANGLE
    , gl_TEXTURE_2D_MULTISAMPLE
    , gl_TEXTURE_2D_MULTISAMPLE_ARRAY
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
    )

import LC_G_Type
import LC_G_APIType
import LC_U_APIType
import LC_B2_IR
import LC_GL_Type

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

setUni :: IORef a -> (GLint -> a -> IO ()) -> GLint -> IO ()
setUni ref f i = readIORef ref >>= f i

setUniBuf :: Storable a => IORef a -> Ptr () -> IO ()
setUniBuf ref p = readIORef ref >>= poke (castPtr p)

mkUniformSetter :: InputType -> IO (GLint -> IO (), InputSetter, Ptr () -> IO ())
mkUniformSetter Bool    = do {t <- newIORef False;                        return $! (setUni t setUBool,  SBool $!  writeIORef t, setUniBuf t)}
mkUniformSetter V2B     = do {t <- newIORef (V2 False False);             return $! (setUni t setUV2B,   SV2B $!   writeIORef t, setUniBuf t)}
mkUniformSetter V3B     = do {t <- newIORef (V3 False False False);       return $! (setUni t setUV3B,   SV3B $!   writeIORef t, setUniBuf t)}
mkUniformSetter V4B     = do {t <- newIORef (V4 False False False False); return $! (setUni t setUV4B,   SV4B $!   writeIORef t, setUniBuf t)}
mkUniformSetter Word    = do {t <- newIORef 0;                            return $! (setUni t setUWord,  SWord $!  writeIORef t, setUniBuf t)}
mkUniformSetter V2U     = do {t <- newIORef (V2 0 0);                     return $! (setUni t setUV2U,   SV2U $!   writeIORef t, setUniBuf t)}
mkUniformSetter V3U     = do {t <- newIORef (V3 0 0 0);                   return $! (setUni t setUV3U,   SV3U $!   writeIORef t, setUniBuf t)}
mkUniformSetter V4U     = do {t <- newIORef (V4 0 0 0 0);                 return $! (setUni t setUV4U,   SV4U $!   writeIORef t, setUniBuf t)}
mkUniformSetter Int     = do {t <- newIORef 0;                            return $! (setUni t setUInt,   SInt $!   writeIORef t, setUniBuf t)}
mkUniformSetter V2I     = do {t <- newIORef (V2 0 0);                     return $! (setUni t setUV2I,   SV2I $!   writeIORef t, setUniBuf t)}
mkUniformSetter V3I     = do {t <- newIORef (V3 0 0 0);                   return $! (setUni t setUV3I,   SV3I $!   writeIORef t, setUniBuf t)}
mkUniformSetter V4I     = do {t <- newIORef (V4 0 0 0 0);                 return $! (setUni t setUV4I,   SV4I $!   writeIORef t, setUniBuf t)}
mkUniformSetter Float   = do {t <- newIORef 0;                            return $! (setUni t setUFloat, SFloat $! writeIORef t, setUniBuf t)}
mkUniformSetter V2F     = do {t <- newIORef (V2 0 0);                     return $! (setUni t setUV2F,   SV2F $!   writeIORef t, setUniBuf t)}
mkUniformSetter V3F     = do {t <- newIORef (V3 0 0 0);                   return $! (setUni t setUV3F,   SV3F $!   writeIORef t, setUniBuf t)}
mkUniformSetter V4F     = do {t <- newIORef (V4 0 0 0 0);                 return $! (setUni t setUV4F,   SV4F $!   writeIORef t, setUniBuf t)}
mkUniformSetter M22F    = do {t <- newIORef (V2 z2 z2);                   return $! (setUni t setUM22F,  SM22F $!  writeIORef t, setUniBuf t)}
mkUniformSetter M23F    = do {t <- newIORef (V3 z2 z2 z2);                return $! (setUni t setUM23F,  SM23F $!  writeIORef t, setUniBuf t)}
mkUniformSetter M24F    = do {t <- newIORef (V4 z2 z2 z2 z2);             return $! (setUni t setUM24F,  SM24F $!  writeIORef t, setUniBuf t)}
mkUniformSetter M32F    = do {t <- newIORef (V2 z3 z3);                   return $! (setUni t setUM32F,  SM32F $!  writeIORef t, setUniBuf t)}
mkUniformSetter M33F    = do {t <- newIORef (V3 z3 z3 z3);                return $! (setUni t setUM33F,  SM33F $!  writeIORef t, setUniBuf t)}
mkUniformSetter M34F    = do {t <- newIORef (V4 z3 z3 z3 z3);             return $! (setUni t setUM34F,  SM34F $!  writeIORef t, setUniBuf t)}
mkUniformSetter M42F    = do {t <- newIORef (V2 z4 z4);                   return $! (setUni t setUM42F,  SM42F $!  writeIORef t, setUniBuf t)}
mkUniformSetter M43F    = do {t <- newIORef (V3 z4 z4 z4);                return $! (setUni t setUM43F,  SM43F $!  writeIORef t, setUniBuf t)}
mkUniformSetter M44F    = do {t <- newIORef (V4 z4 z4 z4 z4);             return $! (setUni t setUM44F,  SM44F $!  writeIORef t, setUniBuf t)}
{-
mkUniformSetter rendState FTexture2D = do
    let texUnitState = textureUnitState rendState
    t <- newIORef (TextureData 0)
    return $! (\i -> readIORef t >>= setTextureData texUnitState i,  SFTexture2D $!  writeIORef t)
-}
-- FIXME: implement properly
setTextureData :: IOVector Int -> GLint -> TextureData -> IO ()
setTextureData texUnitState texUnitIdx (TextureData texObj) = do
    let texUnitIdx' = fromIntegral texUnitIdx
        texObj'     = fromIntegral texObj
    curTexObj <- MV.read texUnitState texUnitIdx'
    when (curTexObj /= texObj') $ do
        MV.write texUnitState texUnitIdx' texObj'
        glActiveTexture $ gl_TEXTURE0 + fromIntegral texUnitIdx
        glBindTexture gl_TEXTURE_2D texObj
        --putStrLn (" -- uniform setup - Texture bind (TexUnit " ++ show (texUnitIdx,texObj) ++ " TexObj)")

b2w :: Bool -> GLuint
b2w True = 1
b2w False = 0

setUBool :: GLint -> Bool -> IO ()
setUV2B  :: GLint -> V2B -> IO ()
setUV3B  :: GLint -> V3B -> IO ()
setUV4B  :: GLint -> V4B -> IO ()
setUBool i v           = with (b2w v) $! \p -> glUniform1uiv i 1 p
setUV2B i (V2 x y)     = with (V2 (b2w x) (b2w y)) $! \p -> glUniform2uiv i 1 $! castPtr p
setUV3B i (V3 x y z)   = with (V3 (b2w x) (b2w y) (b2w z)) $! \p -> glUniform3uiv i 1 $! castPtr p
setUV4B i (V4 x y z w) = with (V4 (b2w x) (b2w y) (b2w z) (b2w w)) $! \p -> glUniform4uiv i 1 $! castPtr p

setUWord :: GLint -> Word32 -> IO ()
setUV2U  :: GLint -> V2U -> IO ()
setUV3U  :: GLint -> V3U -> IO ()
setUV4U  :: GLint -> V4U -> IO ()
setUWord i v = with v $! \p -> glUniform1uiv i 1 $! castPtr p
setUV2U i v  = with v $! \p -> glUniform2uiv i 1 $! castPtr p
setUV3U i v  = with v $! \p -> glUniform3uiv i 1 $! castPtr p
setUV4U i v  = with v $! \p -> glUniform4uiv i 1 $! castPtr p

setUInt :: GLint -> Int32 -> IO ()
setUV2I :: GLint -> V2I -> IO ()
setUV3I :: GLint -> V3I -> IO ()
setUV4I :: GLint -> V4I -> IO ()
setUInt i v = with v $! \p -> glUniform1iv i 1 $! castPtr p
setUV2I i v = with v $! \p -> glUniform2iv i 1 $! castPtr p
setUV3I i v = with v $! \p -> glUniform3iv i 1 $! castPtr p
setUV4I i v = with v $! \p -> glUniform4iv i 1 $! castPtr p

setUFloat :: GLint -> Float -> IO ()
setUV2F   :: GLint -> V2F -> IO ()
setUV3F   :: GLint -> V3F -> IO ()
setUV4F   :: GLint -> V4F -> IO ()
setUFloat i v = with v $! \p -> glUniform1fv i 1 $! castPtr p
setUV2F i v   = with v $! \p -> glUniform2fv i 1 $! castPtr p
setUV3F i v   = with v $! \p -> glUniform3fv i 1 $! castPtr p
setUV4F i v   = with v $! \p -> glUniform4fv i 1 $! castPtr p

setUM22F :: GLint -> M22F -> IO ()
setUM23F :: GLint -> M23F -> IO ()
setUM24F :: GLint -> M24F -> IO ()
setUM22F i v = with v $! \p -> glUniformMatrix2fv i 1 (fromIntegral gl_FALSE) $! castPtr p
setUM23F i v = with v $! \p -> glUniformMatrix2x3fv i 1 (fromIntegral gl_FALSE) $! castPtr p
setUM24F i v = with v $! \p -> glUniformMatrix2x4fv i 1 (fromIntegral gl_FALSE) $! castPtr p

setUM32F :: GLint -> M32F -> IO ()
setUM33F :: GLint -> M33F -> IO ()
setUM34F :: GLint -> M34F -> IO ()
setUM32F i v = with v $! \p -> glUniformMatrix3x2fv i 1 (fromIntegral gl_FALSE) $! castPtr p
setUM33F i v = with v $! \p -> glUniformMatrix3fv i 1 (fromIntegral gl_FALSE) $! castPtr p
setUM34F i v = with v $! \p -> glUniformMatrix3x4fv i 1 (fromIntegral gl_FALSE) $! castPtr p

setUM42F :: GLint -> M42F -> IO ()
setUM43F :: GLint -> M43F -> IO ()
setUM44F :: GLint -> M44F -> IO ()
setUM42F i v = with v $! \p -> glUniformMatrix4x2fv i 1 (fromIntegral gl_FALSE) $! castPtr p
setUM43F i v = with v $! \p -> glUniformMatrix4x3fv i 1 (fromIntegral gl_FALSE) $! castPtr p
setUM44F i v = with v $! \p -> glUniformMatrix4fv i 1 (fromIntegral gl_FALSE) $! castPtr p

-- attribute functions
queryStreams :: GLuint -> IO (Trie GLuint, Trie InputType)
queryStreams po = do
    al <- getNameTypeSize po glGetActiveAttrib glGetAttribLocation gl_ACTIVE_ATTRIBUTES gl_ACTIVE_ATTRIBUTE_MAX_LENGTH
    let aNames = [n | (n,_,_,_) <- al]
        aTypes = [fromGLType (e,s) | (_,_,e,s) <- al]
        aLocation = [fromIntegral i | (_,i,_,_) <- al]
    return $! (T.fromList $! zip aNames aLocation, T.fromList $! zip aNames aTypes)

-- should handle constant value and buffer value as well
mkSSetter :: InputType -> GLuint -> StreamSetter
mkSSetter Word  i (ConstWord v)             = setAWord i v
mkSSetter V2U   i (ConstV2U v)              = setAV2U i v
mkSSetter V3U   i (ConstV3U v)              = setAV3U i v
mkSSetter V4U   i (ConstV4U v)              = setAV4U i v
mkSSetter Word  i (Stream TWord b a s l)    = setBufInteger 1 i b a s
mkSSetter V2U   i (Stream TV2U b a s l)     = setBufInteger 2 i b a s
mkSSetter V3U   i (Stream TV3U b a s l)     = setBufInteger 3 i b a s
mkSSetter V4U   i (Stream TV4U b a s l)     = setBufInteger 4 i b a s
                
mkSSetter Int   i (ConstInt v)              = setAInt i v
mkSSetter V2I   i (ConstV2I v)              = setAV2I i v
mkSSetter V3I   i (ConstV3I v)              = setAV3I i v
mkSSetter V4I   i (ConstV4I v)              = setAV4I i v
mkSSetter Int   i (Stream TInt b a s l)     = setBufInteger 1 i b a s
mkSSetter V2I   i (Stream TV2I b a s l)     = setBufInteger 2 i b a s
mkSSetter V3I   i (Stream TV3I b a s l)     = setBufInteger 3 i b a s
mkSSetter V4I   i (Stream TV4I b a s l)     = setBufInteger 4 i b a s
                
mkSSetter Float i (ConstFloat v)            = setAFloat i v
mkSSetter V2F   i (ConstV2F v)              = setAV2F i v
mkSSetter V3F   i (ConstV3F v)              = setAV3F i v
mkSSetter V4F   i (ConstV4F v)              = setAV4F i v
mkSSetter Float i (Stream TFloat b a s l)   = setBufFloat 1 i b a s
mkSSetter V2F   i (Stream TV2F b a s l)     = setBufFloat 2 i b a s
mkSSetter V3F   i (Stream TV3F b a s l)     = setBufFloat 3 i b a s
mkSSetter V4F   i (Stream TV4F b a s l)     = setBufFloat 4 i b a s
                
mkSSetter M22F  i (ConstM22F v)             = setAM22F i v
mkSSetter M23F  i (ConstM23F v)             = setAM23F i v
mkSSetter M24F  i (ConstM24F v)             = setAM24F i v
mkSSetter M22F  i (Stream TM22F b a s l)    = setBufFloat 4 i b a s
mkSSetter M23F  i (Stream TM23F b a s l)    = setBufFloat 6 i b a s
mkSSetter M24F  i (Stream TM24F b a s l)    = setBufFloat 8 i b a s
                
mkSSetter M32F  i (ConstM32F v)             = setAM32F i v
mkSSetter M33F  i (ConstM33F v)             = setAM33F i v
mkSSetter M34F  i (ConstM34F v)             = setAM34F i v
mkSSetter M32F  i (Stream TM32F b a s l)    = setBufFloat 6 i b a s
mkSSetter M33F  i (Stream TM33F b a s l)    = setBufFloat 9 i b a s
mkSSetter M34F  i (Stream TM34F b a s l)    = setBufFloat 12 i b a s
                
mkSSetter M42F  i (ConstM42F v)             = setAM42F i v
mkSSetter M43F  i (ConstM43F v)             = setAM43F i v
mkSSetter M44F  i (ConstM44F v)             = setAM44F i v
mkSSetter M42F  i (Stream TM42F b a s l)    = setBufFloat 8 i b a s
mkSSetter M43F  i (Stream TM43F b a s l)    = setBufFloat 12 i b a s
mkSSetter M44F  i (Stream TM44F b a s l)    = setBufFloat 16 i b a s
mkSSetter _ _ _                               = fail "mkSSetter type mismatch!"

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

setBufFloat :: GLint -> GLuint -> Buffer -> Int -> Int -> IO ()
setBufFloat compCnt i (Buffer arrs bo) arrIdx start = do
    let ArrayDesc arrType arrLen arrOffs arrSize = arrs V.! arrIdx
        glType = arrayTypeToGLType arrType
        ptr    = intPtrToPtr $! fromIntegral (arrOffs + start * fromIntegral compCnt * sizeOfArrayType arrType)
    glBindBuffer gl_ARRAY_BUFFER bo
    glEnableVertexAttribArray i
    glVertexAttribPointer i compCnt glType (fromIntegral gl_FALSE) 0 ptr

setBufInteger :: GLint -> GLuint -> Buffer -> Int -> Int -> IO ()
--setBufInteger = setBufFloat -- FIXME: GL 2.1 does not have glVertexAttribIPointer
setBufInteger compCnt i (Buffer arrs bo) arrIdx start = do
    let ArrayDesc arrType arrLen arrOffs arrSize = arrs V.! arrIdx
        glType = arrayTypeToGLType arrType
        ptr    = intPtrToPtr $! fromIntegral (arrOffs + start * fromIntegral compCnt * sizeOfArrayType arrType)
    glBindBuffer gl_ARRAY_BUFFER bo
    glEnableVertexAttribArray i
    -- GL 3.X version
    glVertexAttribIPointer i compCnt glType 0 ptr

setAWord :: GLuint -> Word32 -> IO ()
setAV2U  :: GLuint -> V2U -> IO ()
setAV3U  :: GLuint -> V3U -> IO ()
setAV4U  :: GLuint -> V4U -> IO ()
setAWord i v = glDisableVertexAttribArray i >> (with v $! \p -> glVertexAttribI1uiv i $! castPtr p)
setAV2U i v  = glDisableVertexAttribArray i >> (with v $! \p -> glVertexAttribI2uiv i $! castPtr p)
setAV3U i v  = glDisableVertexAttribArray i >> (with v $! \p -> glVertexAttribI3uiv i $! castPtr p)
setAV4U i v  = glDisableVertexAttribArray i >> (with v $! \p -> glVertexAttribI4uiv i $! castPtr p)

setAInt :: GLuint -> Int32 -> IO ()
setAV2I :: GLuint -> V2I -> IO ()
setAV3I :: GLuint -> V3I -> IO ()
setAV4I :: GLuint -> V4I -> IO ()
setAInt i v = glDisableVertexAttribArray i >> (with v $! \p -> glVertexAttribI1iv i $! castPtr p)
setAV2I i v = glDisableVertexAttribArray i >> (with v $! \p -> glVertexAttribI2iv i $! castPtr p)
setAV3I i v = glDisableVertexAttribArray i >> (with v $! \p -> glVertexAttribI3iv i $! castPtr p)
setAV4I i v = glDisableVertexAttribArray i >> (with v $! \p -> glVertexAttribI4iv i $! castPtr p)

setAFloat :: GLuint -> Float -> IO ()
setAV2F   :: GLuint -> V2F -> IO ()
setAV3F   :: GLuint -> V3F -> IO ()
setAV4F   :: GLuint -> V4F -> IO ()
setAFloat i v = glDisableVertexAttribArray i >> (with v $! \p -> glVertexAttrib1fv i $! castPtr p)
setAV2F i v   = glDisableVertexAttribArray i >> (with v $! \p -> glVertexAttrib2fv i $! castPtr p)
setAV3F i v   = glDisableVertexAttribArray i >> (with v $! \p -> glVertexAttrib3fv i $! castPtr p)
setAV4F i v   = glDisableVertexAttribArray i >> (with v $! \p -> glVertexAttrib4fv i $! castPtr p)

setAM22F :: GLuint -> M22F -> IO ()
setAM23F :: GLuint -> M23F -> IO ()
setAM24F :: GLuint -> M24F -> IO ()
setAM22F i (V2 x y)     = setAV2F i x >> setAV2F (i+1) y
setAM23F i (V3 x y z)   = setAV2F i x >> setAV2F (i+1) y >> setAV2F (i+2) z
setAM24F i (V4 x y z w) = setAV2F i x >> setAV2F (i+1) y >> setAV2F (i+2) z >> setAV2F (i+3) w

setAM32F :: GLuint -> M32F -> IO ()
setAM33F :: GLuint -> M33F -> IO ()
setAM34F :: GLuint -> M34F -> IO ()
setAM32F i (V2 x y)     = setAV3F i x >> setAV3F (i+1) y
setAM33F i (V3 x y z)   = setAV3F i x >> setAV3F (i+1) y >> setAV3F (i+2) z
setAM34F i (V4 x y z w) = setAV3F i x >> setAV3F (i+1) y >> setAV3F (i+2) z >> setAV3F (i+3) w

setAM42F :: GLuint -> M42F -> IO ()
setAM43F :: GLuint -> M43F -> IO ()
setAM44F :: GLuint -> M44F -> IO ()
setAM42F i (V2 x y)     = setAV4F i x >> setAV4F (i+1) y
setAM43F i (V3 x y z)   = setAV4F i x >> setAV4F (i+1) y >> setAV4F (i+2) z
setAM44F i (V4 x y z w) = setAV4F i x >> setAV4F (i+1) y >> setAV4F (i+2) z >> setAV4F (i+3) w

-- result list: [(name string,location,gl type,component count)]
getNameTypeSize :: GLuint -> (GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> Ptr GLchar -> IO ())
                   -> (GLuint -> Ptr GLchar -> IO GLint) -> GLenum -> GLenum -> IO [(ByteString,GLint,GLenum,GLint)]
getNameTypeSize o f g enum enumLen = do
    nameLen <- glGetProgramiv1 enumLen o
    allocaArray (fromIntegral nameLen) $! \namep -> alloca $! \sizep -> alloca $! \typep -> do
        n <- glGetProgramiv1 enum o
        forM [0..n-1] $! \i -> f o (fromIntegral i) (fromIntegral nameLen) nullPtr sizep typep namep >>
            (,,,) <$> SB.packCString (castPtr namep) <*> g o namep <*> peek typep <*> peek sizep
{-
filterSamplers :: [(ByteString,GLint,GLenum,GLint)] -> ([(ByteString,GLint,GLenum,GLint)],[(ByteString,GLint,GLenum,GLint)])
filterSamplers l = partition (\(_,_,e,_) -> elem e samplerTypes) l
  where
    samplerTypes = [gl_SAMPLER_2D]
-}
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
    allocaArray (fromIntegral i) $! \ps -> glGetShaderInfoLog o (fromIntegral i) nullPtr ps >> SB.packCString (castPtr ps) >>= SB.putStr

glGetShaderiv1 :: GLenum -> GLuint -> IO GLint
glGetShaderiv1 pname o = alloca $! \pi -> glGetShaderiv o pname pi >> peek pi

glGetProgramiv1 :: GLenum -> GLuint -> IO GLint
glGetProgramiv1 pname o = alloca $! \pi -> glGetProgramiv o pname pi >> peek pi

printProgramLog :: GLuint -> IO ()
printProgramLog o = do
    i <- glGetProgramiv1 gl_INFO_LOG_LENGTH o
    allocaArray (fromIntegral i) $! \ps -> glGetProgramInfoLog o (fromIntegral i) nullPtr ps >> SB.packCString (castPtr ps) >>= SB.putStr

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
textureDataTypeToGLType :: TextureDataType -> GLenum
textureDataTypeToGLType a = case a of
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

textureDataTypeToGLArityType :: TextureDataType -> GLenum
textureDataTypeToGLArityType a = case a of
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

{-
    = TextureDescriptor
    { textureType       :: TextureType
    , textureSize       :: Value
    , textureSemantic   :: ImageSemantic
    , textureSampler    :: SamplerDescriptor
    , textureBaseLevel  :: Int
    , textureMaxLevel   :: Int
    }
-}
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
            let internalFormat  = fromIntegral $ textureDataTypeToGLType dTy
                dataFormat      = fromIntegral $ textureDataTypeToGLArityType dTy
            glBindTexture txTarget to
            glTexParameteri txTarget gl_TEXTURE_BASE_LEVEL $ fromIntegral txBaseLevel
            glTexParameteri txTarget gl_TEXTURE_MAX_LEVEL $ fromIntegral txMaxLevel
            setTextureSamplerParameters txTarget txSampler
            return (internalFormat,dataFormat)

        mipSize 0 x = [x]
        mipSize n x = x : mipSize (n-1) (x `div` 2)
        mipS = mipSize (txMaxLevel - txBaseLevel)
        levels = [txBaseLevel..txMaxLevel]
    {-
        void glTexImage1D( GLenum target, GLint level, GLint internalformat, GLsizei width, GLint border, GLenum format, GLenum type, void *data );
        void glTexImage2D( GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type, void *data );
        void glTexImage3D( GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLenum format, GLenum type, void *data );
        void glTexImage2DMultisample( GLenum target, GLsizei samples, GLint internalformat, GLsizei width, GLsizei height, GLboolean ﬁxedsamplelocations );
        void glTexImage3DMultisample( GLenum target, GLsizei samples, GLint internalformat, GLsizei width, GLsizei height, GLsizei depth, GLboolean ﬁxedsamplelocations );
    -}
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
        Texture2DMS dTy layerCnt -> do
            -- TODO
            let VV2U (V2 w h)   = txSize
                txTarget        = if layerCnt > 1 then gl_TEXTURE_2D_MULTISAMPLE_ARRAY else gl_TEXTURE_2D_MULTISAMPLE
            (internalFormat,dataFormat) <- txSetup txTarget dTy
--        void glTexImage2DMultisample( GLenum target, GLsizei samples, GLint internalformat, GLsizei width, GLsizei height, GLboolean ﬁxedsamplelocations );
--        void glTexImage3DMultisample( GLenum target, GLsizei samples, GLint internalformat, GLsizei width, GLsizei height, GLsizei depth, GLboolean ﬁxedsamplelocations );
            -- TODO: GLsizei samples, GLboolean ﬁxedsamplelocations
            {-
            case layerCnt > 1 of
                True    -> glTexImage3DMultisample txTarget 0 internalFormat (fromIntegral w) (fromIntegral h) (fromIntegral layerCnt) 0 dataFormat gl_UNSIGNED_BYTE nullPtr
                False   -> glTexImage2DMultisample txTarget 0 internalFormat (fromIntegral w) (fromIntegral h) 0 dataFormat gl_UNSIGNED_BYTE nullPtr
            -}
            return txTarget
        TextureBuffer dTy -> do
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
