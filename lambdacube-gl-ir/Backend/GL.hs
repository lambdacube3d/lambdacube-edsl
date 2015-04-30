module Backend.GL (
    -- IR
    V2(..),V3(..),V4(..),
    -- Array, Buffer, Texture
    Array(..),
    ArrayType(..),
    Buffer,
    BufferSetter,
    IndexStream(..),
    Stream(..),
    StreamSetter,
    StreamType(..),
    Primitive(..),
    SetterFun,
    TextureData,
    InputSetter(..),
    fromStreamType,
    sizeOfArrayType,
    toStreamType,
    compileBuffer,
    updateBuffer,
    bufferSize,
    arraySize,
    arrayType,
    compileTexture2DRGBAF,

    -- GL Pipeline Input, Object
    GLPipeline,
    GLPipelineInput,
    Object,
    PipelineSchema(..),
    SlotSchema(..),
    schema,
    schemaFromPipeline,
    allocPipeline,
    disposePipeline,
    setPipelineInput,
    renderPipeline,
    mkGLPipelineInput,
    uniformSetter,
    addObject,
    removeObject,
    enableObject,
    setObjectOrder,
    objectUniformSetter,
    setScreenSize,
    sortSlotObjects,

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

    uniformFTexture2D
) where

import Backend.GL.Type
import Backend.GL.Backend
import Backend.GL.Data
import Backend.GL.Input
import IR