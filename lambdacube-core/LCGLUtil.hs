module LCGLUtil (
    queryUniforms,
    queryStreams,
    mkUSetter,
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
    checkGL
) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Data.IORef
import Data.List as L
import Data.Trie as T
import Foreign
import Graphics.Rendering.OpenGL.Raw.Core32
import qualified Data.ByteString.Char8 as SB
import qualified Data.Vector as V

import LCType
import LCAPIType hiding (Buffer)

type StreamSetter = Stream Buffer -> IO ()

data Buffer -- internal type
    = Buffer
    { bufArrays :: V.Vector ArrayDesc
    , bufGLObj  :: GLuint
    }
    deriving (Show,Eq)

data ArrayDesc
    = ArrayDesc
    { arrType   :: ArrayType
    , arrLength :: Int  -- item count
    , arrOffset :: Int  -- byte position in buffer
    , arrSize   :: Int  -- size in bytes
    }
    deriving (Show,Eq)

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

mkUSetter :: InputType -> IO (GLint -> IO (), InputSetter)
mkUSetter ITBool    = do {t <- newIORef False;                        return $! (\i -> readIORef t >>= setUBool i,  SBool $!  writeIORef t)}
mkUSetter ITV2B     = do {t <- newIORef (V2 False False);             return $! (\i -> readIORef t >>= setUV2B i,   SV2B $!   writeIORef t)}
mkUSetter ITV3B     = do {t <- newIORef (V3 False False False);       return $! (\i -> readIORef t >>= setUV3B i,   SV3B $!   writeIORef t)}
mkUSetter ITV4B     = do {t <- newIORef (V4 False False False False); return $! (\i -> readIORef t >>= setUV4B i,   SV4B $!   writeIORef t)}
mkUSetter ITWord    = do {t <- newIORef 0;                            return $! (\i -> readIORef t >>= setUWord i,  SWord $!  writeIORef t)}
mkUSetter ITV2U     = do {t <- newIORef (V2 0 0);                     return $! (\i -> readIORef t >>= setUV2U i,   SV2U $!   writeIORef t)}
mkUSetter ITV3U     = do {t <- newIORef (V3 0 0 0);                   return $! (\i -> readIORef t >>= setUV3U i,   SV3U $!   writeIORef t)}
mkUSetter ITV4U     = do {t <- newIORef (V4 0 0 0 0);                 return $! (\i -> readIORef t >>= setUV4U i,   SV4U $!   writeIORef t)}
mkUSetter ITInt     = do {t <- newIORef 0;                            return $! (\i -> readIORef t >>= setUInt i,   SInt $!   writeIORef t)}
mkUSetter ITV2I     = do {t <- newIORef (V2 0 0);                     return $! (\i -> readIORef t >>= setUV2I i,   SV2I $!   writeIORef t)}
mkUSetter ITV3I     = do {t <- newIORef (V3 0 0 0);                   return $! (\i -> readIORef t >>= setUV3I i,   SV3I $!   writeIORef t)}
mkUSetter ITV4I     = do {t <- newIORef (V4 0 0 0 0);                 return $! (\i -> readIORef t >>= setUV4I i,   SV4I $!   writeIORef t)}
mkUSetter ITFloat   = do {t <- newIORef 0;                            return $! (\i -> readIORef t >>= setUFloat i, SFloat $! writeIORef t)}
mkUSetter ITV2F     = do {t <- newIORef (V2 0 0);                     return $! (\i -> readIORef t >>= setUV2F i,   SV2F $!   writeIORef t)}
mkUSetter ITV3F     = do {t <- newIORef (V3 0 0 0);                   return $! (\i -> readIORef t >>= setUV3F i,   SV3F $!   writeIORef t)}
mkUSetter ITV4F     = do {t <- newIORef (V4 0 0 0 0);                 return $! (\i -> readIORef t >>= setUV4F i,   SV4F $!   writeIORef t)}
mkUSetter ITM22F    = do {t <- newIORef (V2 z2 z2);                   return $! (\i -> readIORef t >>= setUM22F i,  SM22F $!  writeIORef t)}
mkUSetter ITM23F    = do {t <- newIORef (V3 z2 z2 z2);                return $! (\i -> readIORef t >>= setUM23F i,  SM23F $!  writeIORef t)}
mkUSetter ITM24F    = do {t <- newIORef (V4 z2 z2 z2 z2);             return $! (\i -> readIORef t >>= setUM24F i,  SM24F $!  writeIORef t)}
mkUSetter ITM32F    = do {t <- newIORef (V2 z3 z3);                   return $! (\i -> readIORef t >>= setUM32F i,  SM32F $!  writeIORef t)}
mkUSetter ITM33F    = do {t <- newIORef (V3 z3 z3 z3);                return $! (\i -> readIORef t >>= setUM33F i,  SM33F $!  writeIORef t)}
mkUSetter ITM34F    = do {t <- newIORef (V4 z3 z3 z3 z3);             return $! (\i -> readIORef t >>= setUM34F i,  SM34F $!  writeIORef t)}
mkUSetter ITM42F    = do {t <- newIORef (V2 z4 z4);                   return $! (\i -> readIORef t >>= setUM42F i,  SM42F $!  writeIORef t)}
mkUSetter ITM43F    = do {t <- newIORef (V3 z4 z4 z4);                return $! (\i -> readIORef t >>= setUM43F i,  SM43F $!  writeIORef t)}
mkUSetter ITM44F    = do {t <- newIORef (V4 z4 z4 z4 z4);             return $! (\i -> readIORef t >>= setUM44F i,  SM44F $!  writeIORef t)}

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
mkSSetter ITWord  i (ConstWord v)             = setAWord i v
mkSSetter ITV2U   i (ConstV2U v)              = setAV2U i v
mkSSetter ITV3U   i (ConstV3U v)              = setAV3U i v
mkSSetter ITV4U   i (ConstV4U v)              = setAV4U i v
mkSSetter ITWord  i (Stream TWord b a s l)    = setBufInteger 1 i b a s
mkSSetter ITV2U   i (Stream TV2U b a s l)     = setBufInteger 2 i b a s
mkSSetter ITV3U   i (Stream TV3U b a s l)     = setBufInteger 3 i b a s
mkSSetter ITV4U   i (Stream TV4U b a s l)     = setBufInteger 4 i b a s
                
mkSSetter ITInt   i (ConstInt v)              = setAInt i v
mkSSetter ITV2I   i (ConstV2I v)              = setAV2I i v
mkSSetter ITV3I   i (ConstV3I v)              = setAV3I i v
mkSSetter ITV4I   i (ConstV4I v)              = setAV4I i v
mkSSetter ITInt   i (Stream TInt b a s l)     = setBufInteger 1 i b a s
mkSSetter ITV2I   i (Stream TV2I b a s l)     = setBufInteger 2 i b a s
mkSSetter ITV3I   i (Stream TV3I b a s l)     = setBufInteger 3 i b a s
mkSSetter ITV4I   i (Stream TV4I b a s l)     = setBufInteger 4 i b a s
                
mkSSetter ITFloat i (ConstFloat v)            = setAFloat i v
mkSSetter ITV2F   i (ConstV2F v)              = setAV2F i v
mkSSetter ITV3F   i (ConstV3F v)              = setAV3F i v
mkSSetter ITV4F   i (ConstV4F v)              = setAV4F i v
mkSSetter ITFloat i (Stream TFloat b a s l)   = setBufFloat 1 i b a s
mkSSetter ITV2F   i (Stream TV2F b a s l)     = setBufFloat 2 i b a s
mkSSetter ITV3F   i (Stream TV3F b a s l)     = setBufFloat 3 i b a s
mkSSetter ITV4F   i (Stream TV4F b a s l)     = setBufFloat 4 i b a s
                
mkSSetter ITM22F  i (ConstM22F v)             = setAM22F i v
mkSSetter ITM23F  i (ConstM23F v)             = setAM23F i v
mkSSetter ITM24F  i (ConstM24F v)             = setAM24F i v
mkSSetter ITM22F  i (Stream TM22F b a s l)    = setBufFloat 4 i b a s
mkSSetter ITM23F  i (Stream TM23F b a s l)    = setBufFloat 6 i b a s
mkSSetter ITM24F  i (Stream TM24F b a s l)    = setBufFloat 8 i b a s
                
mkSSetter ITM32F  i (ConstM32F v)             = setAM32F i v
mkSSetter ITM33F  i (ConstM33F v)             = setAM33F i v
mkSSetter ITM34F  i (ConstM34F v)             = setAM34F i v
mkSSetter ITM32F  i (Stream TM32F b a s l)    = setBufFloat 6 i b a s
mkSSetter ITM33F  i (Stream TM33F b a s l)    = setBufFloat 9 i b a s
mkSSetter ITM34F  i (Stream TM34F b a s l)    = setBufFloat 12 i b a s
                
mkSSetter ITM42F  i (ConstM42F v)             = setAM42F i v
mkSSetter ITM43F  i (ConstM43F v)             = setAM43F i v
mkSSetter ITM44F  i (ConstM44F v)             = setAM44F i v
mkSSetter ITM42F  i (Stream TM42F b a s l)    = setBufFloat 8 i b a s
mkSSetter ITM43F  i (Stream TM43F b a s l)    = setBufFloat 12 i b a s
mkSSetter ITM44F  i (Stream TM44F b a s l)    = setBufFloat 16 i b a s
mkSSetter _ _ _                               = fail "mkSSetter type mismatch!"

arrayTypeToGLType :: ArrayType -> GLenum
arrayTypeToGLType ArrWord8  = gl_UNSIGNED_BYTE
arrayTypeToGLType ArrWord16 = gl_UNSIGNED_SHORT
arrayTypeToGLType ArrWord32 = gl_UNSIGNED_INT
arrayTypeToGLType ArrInt8   = gl_BYTE
arrayTypeToGLType ArrInt16  = gl_SHORT
arrayTypeToGLType ArrInt32  = gl_INT
arrayTypeToGLType ArrFloat  = gl_FLOAT
arrayTypeToGLType ArrHalf   = gl_HALF_FLOAT

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
    | t == gl_BOOL              = ITBool
    | t == gl_BOOL_VEC2         = ITV2B
    | t == gl_BOOL_VEC3         = ITV3B
    | t == gl_BOOL_VEC4         = ITV4B
    | t == gl_UNSIGNED_INT      = ITWord
    | t == gl_UNSIGNED_INT_VEC2 = ITV2U
    | t == gl_UNSIGNED_INT_VEC3 = ITV3U
    | t == gl_UNSIGNED_INT_VEC4 = ITV4U
    | t == gl_INT               = ITInt
    | t == gl_INT_VEC2          = ITV2I
    | t == gl_INT_VEC3          = ITV3I
    | t == gl_INT_VEC4          = ITV4I
    | t == gl_FLOAT             = ITFloat
    | t == gl_FLOAT_VEC2        = ITV2F
    | t == gl_FLOAT_VEC3        = ITV3F
    | t == gl_FLOAT_VEC4        = ITV4F
    | t == gl_FLOAT_MAT2        = ITM22F
    | t == gl_FLOAT_MAT2x3      = ITM23F
    | t == gl_FLOAT_MAT2x4      = ITM24F
    | t == gl_FLOAT_MAT3x2      = ITM32F
    | t == gl_FLOAT_MAT3        = ITM33F
    | t == gl_FLOAT_MAT3x4      = ITM34F
    | t == gl_FLOAT_MAT4x2      = ITM42F
    | t == gl_FLOAT_MAT4x3      = ITM43F
    | t == gl_FLOAT_MAT4        = ITM44F
    | t == gl_SAMPLER_1D_ARRAY_SHADOW                   = ITSTexture1DArray
    | t == gl_SAMPLER_1D_SHADOW                         = ITSTexture1D
    | t == gl_SAMPLER_2D_ARRAY_SHADOW                   = ITSTexture2DArray
    | t == gl_SAMPLER_2D_RECT_SHADOW                    = ITSTexture2DRect
    | t == gl_SAMPLER_2D_SHADOW                         = ITSTexture2D
    | t == gl_SAMPLER_CUBE_SHADOW                       = ITSTextureCube
    | t == gl_INT_SAMPLER_1D                            = ITITexture1D
    | t == gl_INT_SAMPLER_1D_ARRAY                      = ITITexture1DArray
    | t == gl_INT_SAMPLER_2D                            = ITITexture2D
    | t == gl_INT_SAMPLER_2D_ARRAY                      = ITITexture2DArray
    | t == gl_INT_SAMPLER_2D_MULTISAMPLE                = ITITexture2DMS
    | t == gl_INT_SAMPLER_2D_MULTISAMPLE_ARRAY          = ITITexture2DMSArray
    | t == gl_INT_SAMPLER_2D_RECT                       = ITITexture2DRect
    | t == gl_INT_SAMPLER_3D                            = ITITexture3D
    | t == gl_INT_SAMPLER_BUFFER                        = ITITextureBuffer
    | t == gl_INT_SAMPLER_CUBE                          = ITITextureCube
    | t == gl_SAMPLER_1D                                = ITFTexture1D
    | t == gl_SAMPLER_1D_ARRAY                          = ITFTexture1DArray
    | t == gl_SAMPLER_2D                                = ITFTexture2D
    | t == gl_SAMPLER_2D_ARRAY                          = ITFTexture2DArray
    | t == gl_SAMPLER_2D_MULTISAMPLE                    = ITFTexture2DMS
    | t == gl_SAMPLER_2D_MULTISAMPLE_ARRAY              = ITFTexture2DMSArray
    | t == gl_SAMPLER_2D_RECT                           = ITFTexture2DRect
    | t == gl_SAMPLER_3D                                = ITFTexture3D
    | t == gl_SAMPLER_BUFFER                            = ITFTextureBuffer
    | t == gl_SAMPLER_CUBE                              = ITFTextureCube
    | t == gl_UNSIGNED_INT_SAMPLER_1D                   = ITUTexture1D
    | t == gl_UNSIGNED_INT_SAMPLER_1D_ARRAY             = ITUTexture1DArray
    | t == gl_UNSIGNED_INT_SAMPLER_2D                   = ITUTexture2D
    | t == gl_UNSIGNED_INT_SAMPLER_2D_ARRAY             = ITUTexture2DArray
    | t == gl_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE       = ITUTexture2DMS
    | t == gl_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY = ITUTexture2DMSArray
    | t == gl_UNSIGNED_INT_SAMPLER_2D_RECT              = ITUTexture2DRect
    | t == gl_UNSIGNED_INT_SAMPLER_3D                   = ITUTexture3D
    | t == gl_UNSIGNED_INT_SAMPLER_BUFFER               = ITUTextureBuffer
    | t == gl_UNSIGNED_INT_SAMPLER_CUBE                 = ITUTextureCube
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
            | e == gl_NO_ERROR                      = "NO_ERROR"
            | otherwise                             = "Unknown error"
    e <- glGetError
    return $ f e

streamToInputType :: Stream Buffer -> InputType
streamToInputType (ConstWord  _) = ITWord
streamToInputType (ConstV2U   _) = ITV2U
streamToInputType (ConstV3U   _) = ITV3U
streamToInputType (ConstV4U   _) = ITV4U
streamToInputType (ConstInt   _) = ITInt
streamToInputType (ConstV2I   _) = ITV2I
streamToInputType (ConstV3I   _) = ITV3I
streamToInputType (ConstV4I   _) = ITV4I
streamToInputType (ConstFloat _) = ITFloat
streamToInputType (ConstV2F   _) = ITV2F
streamToInputType (ConstV3F   _) = ITV3F
streamToInputType (ConstV4F   _) = ITV4F
streamToInputType (ConstM22F  _) = ITM22F
streamToInputType (ConstM23F  _) = ITM23F
streamToInputType (ConstM24F  _) = ITM24F
streamToInputType (ConstM32F  _) = ITM32F
streamToInputType (ConstM33F  _) = ITM33F
streamToInputType (ConstM34F  _) = ITM34F
streamToInputType (ConstM42F  _) = ITM42F
streamToInputType (ConstM43F  _) = ITM43F
streamToInputType (ConstM44F  _) = ITM44F
streamToInputType (Stream t (Buffer a _) i _ _)
    | 0 <= i && i < V.length a &&
      if elem t integralTypes then elem at integralArrTypes else True
    = fromStreamType t
    | otherwise = throw $ userError "streamToInputType failed"
  where
    at = arrType $! (a V.! i)
    integralTypes    = [TWord, TV2U, TV3U, TV4U, TInt, TV2I, TV3I, TV4I]
    integralArrTypes = [ArrWord8, ArrWord16, ArrWord32, ArrInt8, ArrInt16, ArrInt32]

comparisonFunctionToGLType :: ComparisonFunction -> GLenum
comparisonFunctionToGLType Always   = gl_ALWAYS
comparisonFunctionToGLType Equal    = gl_EQUAL
comparisonFunctionToGLType Gequal   = gl_GEQUAL
comparisonFunctionToGLType Greater  = gl_GREATER
comparisonFunctionToGLType Lequal   = gl_LEQUAL
comparisonFunctionToGLType Less     = gl_LESS
comparisonFunctionToGLType Never    = gl_NEVER
comparisonFunctionToGLType Notequal = gl_NOTEQUAL

logicOperationToGLType :: LogicOperation -> GLenum
logicOperationToGLType Clear        = gl_CLEAR
logicOperationToGLType And          = gl_AND
logicOperationToGLType AndReverse   = gl_AND_REVERSE
logicOperationToGLType Copy         = gl_COPY
logicOperationToGLType AndInverted  = gl_AND_INVERTED
logicOperationToGLType Noop         = gl_NOOP
logicOperationToGLType Xor          = gl_XOR
logicOperationToGLType Or           = gl_OR
logicOperationToGLType Nor          = gl_NOR
logicOperationToGLType Equiv        = gl_EQUIV
logicOperationToGLType Invert       = gl_INVERT
logicOperationToGLType OrReverse    = gl_OR_REVERSE
logicOperationToGLType CopyInverted = gl_COPY_INVERTED
logicOperationToGLType OrInverted   = gl_OR_INVERTED
logicOperationToGLType Nand         = gl_NAND
logicOperationToGLType Set          = gl_SET

blendEquationToGLType :: BlendEquation -> GLenum
blendEquationToGLType FuncAdd               = gl_FUNC_ADD
blendEquationToGLType FuncSubtract          = gl_FUNC_SUBTRACT
blendEquationToGLType FuncReverseSubtract   = gl_FUNC_REVERSE_SUBTRACT
blendEquationToGLType Min                   = gl_MIN
blendEquationToGLType Max                   = gl_MAX


blendingFactorToGLType :: BlendingFactor -> GLenum
blendingFactorToGLType Zero                  = gl_ZERO
blendingFactorToGLType One                   = gl_ONE
blendingFactorToGLType SrcColor              = gl_SRC_COLOR
blendingFactorToGLType OneMinusSrcColor      = gl_ONE_MINUS_SRC_COLOR
blendingFactorToGLType DstColor              = gl_DST_COLOR
blendingFactorToGLType OneMinusDstColor      = gl_ONE_MINUS_DST_COLOR
blendingFactorToGLType SrcAlpha              = gl_SRC_ALPHA
blendingFactorToGLType OneMinusSrcAlpha      = gl_ONE_MINUS_SRC_ALPHA
blendingFactorToGLType DstAlpha              = gl_DST_ALPHA
blendingFactorToGLType OneMinusDstAlpha      = gl_ONE_MINUS_DST_ALPHA
blendingFactorToGLType ConstantColor         = gl_CONSTANT_COLOR
blendingFactorToGLType OneMinusConstantColor = gl_ONE_MINUS_CONSTANT_COLOR
blendingFactorToGLType ConstantAlpha         = gl_CONSTANT_ALPHA
blendingFactorToGLType OneMinusConstantAlpha = gl_ONE_MINUS_CONSTANT_ALPHA
blendingFactorToGLType SrcAlphaSaturate      = gl_SRC_ALPHA_SATURATE
