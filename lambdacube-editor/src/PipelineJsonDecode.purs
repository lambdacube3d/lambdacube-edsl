module PipelineJsonDecode where

import Data.Argonaut ((~>), (:=), (.?), jsonEmptyObject, printJson)
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Argonaut.Decode (DecodeJson, decodeJson)

import Data.Maybe
import Data.StrMap
import IR

instance decodeJsonV2 :: (DecodeJson a) => DecodeJson (V2 a) where
  decodeJson json = do
    obj <- decodeJson json
    V2 <$> obj .? "x" <*> obj .? "y"

instance decodeJsonV3 :: (DecodeJson a) => DecodeJson (V3 a) where
  decodeJson json = do
    obj <- decodeJson json
    V3 <$> obj .? "x" <*> obj .? "y" <*> obj .? "z"

instance decodeJsonV4 :: (DecodeJson a) => DecodeJson (V4 a) where
  decodeJson json = do
    obj <- decodeJson json
    V4 <$> obj .? "x" <*> obj .? "y" <*> obj .? "z" <*> obj .? "w"

instance decodeJsonValue :: DecodeJson Value where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "VBool"   -> VBool  <$> obj .? "value"
      "VV2B"    -> VV2B   <$> obj .? "value"
      "VV3B"    -> VV3B   <$> obj .? "value"
      "VV4B"    -> VV4B   <$> obj .? "value"
      "VWord"   -> VWord  <$> obj .? "value"
      "VV2U"    -> VV2U   <$> obj .? "value"
      "VV3U"    -> VV3U   <$> obj .? "value"
      "VV4U"    -> VV4U   <$> obj .? "value"
      "VInt"    -> VInt   <$> obj .? "value"
      "VV2I"    -> VV2I   <$> obj .? "value"
      "VV3I"    -> VV3I   <$> obj .? "value"
      "VV4I"    -> VV4I   <$> obj .? "value"
      "VFloat"  -> VFloat <$> obj .? "value"
      "VV2F"    -> VV2F   <$> obj .? "value"
      "VV3F"    -> VV3F   <$> obj .? "value"
      "VV4F"    -> VV4F   <$> obj .? "value"
      "VM22F"   -> VM22F  <$> obj .? "value"
      "VM23F"   -> VM23F  <$> obj .? "value"
      "VM24F"   -> VM24F  <$> obj .? "value"
      "VM32F"   -> VM32F  <$> obj .? "value"
      "VM33F"   -> VM33F  <$> obj .? "value"
      "VM34F"   -> VM34F  <$> obj .? "value"
      "VM42F"   -> VM42F  <$> obj .? "value"
      "VM43F"   -> VM43F  <$> obj .? "value"
      "VM44F"   -> VM44F  <$> obj .? "value"

instance decodeJsonInputType :: DecodeJson InputType where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    return $ case tag of
      "Bool"  -> Bool 
      "V2B"   -> V2B  
      "V3B"   -> V3B  
      "V4B"   -> V4B  
      "Word"  -> Word 
      "V2U"   -> V2U  
      "V3U"   -> V3U  
      "V4U"   -> V4U  
      "Int"   -> Int  
      "V2I"   -> V2I  
      "V3I"   -> V3I  
      "V4I"   -> V4I  
      "Float" -> Float
      "V2F"   -> V2F  
      "V3F"   -> V3F  
      "V4F"   -> V4F  
      "M22F"  -> M22F 
      "M23F"  -> M23F 
      "M24F"  -> M24F 
      "M32F"  -> M32F 
      "M33F"  -> M33F 
      "M34F"  -> M34F 
      "M42F"  -> M42F 
      "M43F"  -> M43F 
      "M44F"  -> M44F 
      "STexture1D"        -> STexture1D       
      "STexture2D"        -> STexture2D       
      "STextureCube"      -> STextureCube     
      "STexture1DArray"   -> STexture1DArray  
      "STexture2DArray"   -> STexture2DArray  
      "STexture2DRect"    -> STexture2DRect   
      "FTexture1D"        -> FTexture1D       
      "FTexture2D"        -> FTexture2D       
      "FTexture3D"        -> FTexture3D       
      "FTextureCube"      -> FTextureCube     
      "FTexture1DArray"   -> FTexture1DArray  
      "FTexture2DArray"   -> FTexture2DArray  
      "FTexture2DMS"      -> FTexture2DMS     
      "FTexture2DMSArray" -> FTexture2DMSArray
      "FTextureBuffer"    -> FTextureBuffer   
      "FTexture2DRect"    -> FTexture2DRect   
      "ITexture1D"        -> ITexture1D       
      "ITexture2D"        -> ITexture2D       
      "ITexture3D"        -> ITexture3D       
      "ITextureCube"      -> ITextureCube     
      "ITexture1DArray"   -> ITexture1DArray  
      "ITexture2DArray"   -> ITexture2DArray  
      "ITexture2DMS"      -> ITexture2DMS     
      "ITexture2DMSArray" -> ITexture2DMSArray
      "ITextureBuffer"    -> ITextureBuffer   
      "ITexture2DRect"    -> ITexture2DRect   
      "UTexture1D"        -> UTexture1D       
      "UTexture2D"        -> UTexture2D       
      "UTexture3D"        -> UTexture3D       
      "UTextureCube"      -> UTextureCube     
      "UTexture1DArray"   -> UTexture1DArray  
      "UTexture2DArray"   -> UTexture2DArray  
      "UTexture2DMS"      -> UTexture2DMS     
      "UTexture2DMSArray" -> UTexture2DMSArray
      "UTextureBuffer"    -> UTextureBuffer   
      "UTexture2DRect"    -> UTexture2DRect   

instance decodeJsonPointSpriteCoordOrigin :: DecodeJson PointSpriteCoordOrigin where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    return $ case tag of
      "LowerLeft" -> LowerLeft
      "UpperLeft" -> UpperLeft

instance decodeJsonPointSize :: DecodeJson PointSize where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "PointSize"         -> PointSize <$> obj .? "size"
      "ProgramPointSize"  -> pure ProgramPointSize

instance decodeJsonPolygonOffset :: DecodeJson PolygonOffset where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Offset"    -> Offset <$> obj .? "factor" <*> obj .? "units"
      "NoOffset"  -> pure NoOffset

instance decodeJsonFrontFace :: DecodeJson FrontFace where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    return $ case tag of
      "CCW" -> CCW
      "CW"  -> CW

instance decodeJsonPolygonMode :: DecodeJson PolygonMode where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "PolygonPoint"  -> PolygonPoint <$> obj .? "size"
      "PolygonLine"   -> PolygonLine <$> obj .? "size"
      "PolygonFill"   -> pure PolygonFill

instance decodeJsonProvokingVertex :: DecodeJson ProvokingVertex where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    return $ case tag of
      "FirstVertex" -> FirstVertex
      "LastVertex"  -> LastVertex

instance decodeJsonCullMode :: DecodeJson CullMode where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "CullFront" -> CullFront <$> obj .? "face"
      "CullBack"  -> CullBack <$> obj .? "face"
      "CullNone"  -> pure CullNone

instance decodeJsonComparisonFunction :: DecodeJson ComparisonFunction where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    return $ case tag of
      "Never"    -> Never
      "Less"     -> Less
      "Equal"    -> Equal
      "Lequal"   -> Lequal
      "Greater"  -> Greater
      "Notequal" -> Notequal
      "Gequal"   -> Gequal
      "Always"   -> Always

instance decodeJsonStencilOperation :: DecodeJson StencilOperation where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    return $ case tag of
      "OpZero"     -> OpZero
      "OpKeep"     -> OpKeep
      "OpReplace"  -> OpReplace
      "OpIncr"     -> OpIncr
      "OpIncrWrap" -> OpIncrWrap
      "OpDecr"     -> OpDecr
      "OpDecrWrap" -> OpDecrWrap
      "OpInvert"   -> OpInvert

instance decodeJsonBlendEquation :: DecodeJson BlendEquation where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    return $ case tag of
      "FuncAdd" -> FuncAdd
      "FuncSubtract" -> FuncSubtract
      "FuncReverseSubtract" -> FuncReverseSubtract
      "Min" -> Min
      "Max" -> Max

instance decodeJsonBlendingFactor :: DecodeJson BlendingFactor where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    return $ case tag of
      "Zero" -> Zero
      "One" -> One
      "SrcColor" -> SrcColor
      "OneMinusSrcColor" -> OneMinusSrcColor
      "DstColor" -> DstColor
      "OneMinusDstColor" -> OneMinusDstColor
      "SrcAlpha" -> SrcAlpha
      "OneMinusSrcAlpha" -> OneMinusSrcAlpha
      "DstAlpha" -> DstAlpha
      "OneMinusDstAlpha" -> OneMinusDstAlpha
      "ConstantColor" -> ConstantColor
      "OneMinusConstantColor" -> OneMinusConstantColor
      "ConstantAlpha" -> ConstantAlpha
      "OneMinusConstantAlpha" -> OneMinusConstantAlpha
      "SrcAlphaSaturate" -> SrcAlphaSaturate

instance decodeJsonLogicOperation :: DecodeJson LogicOperation where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    return $ case tag of
      "Clear" -> Clear
      "And" -> And
      "AndReverse" -> AndReverse
      "Copy" -> Copy
      "AndInverted" -> AndInverted
      "Noop" -> Noop
      "Xor" -> Xor
      "Or" -> Or
      "Nor" -> Nor
      "Equiv" -> Equiv
      "Invert" -> Invert
      "OrReverse" -> OrReverse
      "CopyInverted" -> CopyInverted
      "OrInverted" -> OrInverted
      "Nand" -> Nand
      "Set" -> Set

instance decodeJsonStencilOps :: DecodeJson StencilOps where
  decodeJson json = do
    obj <- decodeJson json
    frontStencilOp <- obj .? "frontStencilOp"
    backStencilOp <- obj .? "backStencilOp"
    pure $ StencilOps {frontStencilOp:frontStencilOp, backStencilOp:backStencilOp}

instance decodeJsonStencilTests :: DecodeJson StencilTests where
  decodeJson json = do
    obj <- decodeJson json
    StencilTests <$> obj .? "front" <*> obj .? "back"

instance decodeJsonStencilTest :: DecodeJson StencilTest where
  decodeJson json = do
    obj <- decodeJson json
    stencilComparision <- obj .? "stencilComparision"
    stencilReference <- obj .? "stencilReference"
    stencilMask <- obj .? "stencilMask"
    pure $ StencilTest {stencilComparision:stencilComparision, stencilReference:stencilReference, stencilMask:stencilMask}

instance decodeJsonFetchPrimitive :: DecodeJson FetchPrimitive where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    return $ case tag of
      "Points" -> Points
      "Lines" -> Lines
      "Triangles" -> Triangles
      "LinesAdjacency" -> LinesAdjacency
      "TrianglesAdjacency" -> TrianglesAdjacency

instance decodeJsonOutputPrimitive :: DecodeJson OutputPrimitive where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    return $ case tag of
      "TrianglesOutput" -> TrianglesOutput
      "LinesOutput" -> LinesOutput
      "PointsOutput" -> PointsOutput

instance decodeJsonColorArity :: DecodeJson ColorArity where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    return $ case tag of
      "Red" -> Red
      "RG" -> RG
      "RGB" -> RGB
      "RGBA" -> RGBA

instance decodeJsonBlendingFactorPair :: DecodeJson BlendingFactorPair where
  decodeJson json = do
    obj <- decodeJson json
    src <- obj .? "src"
    dst <- obj .? "dst"
    pure $ BlendingFactorPair {src:src, dst:dst}

instance decodeJsonBlending :: DecodeJson Blending where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "NoBlending"    -> pure NoBlending
      "BlendLogicOp"  -> BlendLogicOp <$> obj .? "op"
      "Blend"         -> do
        colorEq <- obj .? "colorEq"
        alphaEq <- obj .? "alphaEq"
        colorF <- obj .? "colorF"
        alphaF <- obj .? "alphaF"
        Blend {colorEq:colorEq, alphaEq:alphaEq} {colorF:colorF, alphaF:alphaF} <$> obj .? "color"

instance decodeJsonRasterContext :: DecodeJson RasterContext where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "PointCtx" -> PointCtx <$> obj .? "size" <*> obj .? "threshold" <*> obj .? "origin"
      "LineCtx" -> LineCtx <$> obj .? "size" <*> obj .? "provokingVertex"
      "TriangleCtx" -> TriangleCtx <$> obj .? "cullMode" <*> obj .? "polygonMode" <*> obj .? "polygonOffset" <*> obj .? "provokingVertex"

instance decodeJsonFragmentOperation :: DecodeJson FragmentOperation where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "DepthOp" -> DepthOp <$> obj .? "depthFunction" <*> obj .? "depthMask"
      "StencilOp" -> StencilOp <$> obj .? "stencilTests" <*> obj .? "front" <*> obj .? "back"
      "ColorOp" -> ColorOp <$> obj .? "blend" <*> obj .? "mask"

instance decodeJsonAccumulationContext :: DecodeJson AccumulationContext where
  decodeJson json = do
    obj <- decodeJson json
    accViewportName <- obj .? "viewportName"
    accOperations <- obj .? "operations"
    pure $ AccumulationContext {accViewportName:accViewportName, accOperations:accOperations}

instance decodeJsonImage :: DecodeJson Image where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "DepthImage" -> DepthImage <$> obj .? "layers" <*> obj .? "value"
      "StencilImage" -> StencilImage <$> obj .? "layers" <*> obj .? "value"
      "ColorImage" -> ColorImage <$> obj .? "layers" <*> obj .? "value"

instance decodeJsonTextureDataType :: DecodeJson TextureDataType where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "FloatT" -> FloatT <$> obj .? "arity"
      "IntT" -> IntT <$> obj .? "arity"
      "WordT" -> WordT <$> obj .? "arity"
      "ShadowT" ->  pure ShadowT

instance decodeJsonTextureType :: DecodeJson TextureType where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Texture1D" -> Texture1D <$> obj .? "texType" <*> obj .? "layers"
      "Texture2D" -> Texture2D <$> obj .? "texType" <*> obj .? "layers"
      "Texture3D" -> Texture3D <$> obj .? "texType"
      "TextureCube" -> TextureCube <$> obj .? "texType"
      "TextureRect" -> TextureRect <$> obj .? "texType"
      "Texture2DMS" -> Texture2DMS <$> obj .? "texType" <*> obj .? "layers" <*> obj .? "param1" <*> obj .? "param2"
      "TextureBuffer" -> TextureBuffer <$> obj .? "texType"

instance decodeJsonMipMap :: DecodeJson MipMap where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Mip" -> Mip <$> obj .? "base" <*> obj .? "max"
      "NoMip" -> pure NoMip
      "AutoMip" -> AutoMip <$> obj .? "base" <*> obj .? "max"

instance decodeJsonFilter :: DecodeJson Filter where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    return $ case tag of
      "Nearest" -> Nearest
      "Linear" -> Linear
      "NearestMipmapNearest" -> NearestMipmapNearest
      "NearestMipmapLinear" -> NearestMipmapLinear
      "LinearMipmapNearest" -> LinearMipmapNearest
      "LinearMipmapLinear" -> LinearMipmapLinear

instance decodeJsonEdgeMode :: DecodeJson EdgeMode where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    return $ case tag of
      "Repeat" -> Repeat
      "MirroredRepeat" -> MirroredRepeat
      "ClampToEdge" -> ClampToEdge
      "ClampToBorder" -> ClampToBorder

instance decodeJsonImageRef :: DecodeJson ImageRef where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "TextureImage" -> TextureImage <$> obj .? "tex" <*> obj .? "mip" <*> obj .? "layer"
      "Framebuffer" -> Framebuffer <$> obj .? "semantic"

instance decodeJsonImageSemantic :: DecodeJson ImageSemantic where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    return $ case tag of
      "Depth" -> Depth
      "Stencil" -> Stencil
      "Color" -> Color

instance decodeJsonClearImage :: DecodeJson ClearImage where
  decodeJson json = do
    obj <- decodeJson json
    semantic <- obj .? "semantic"
    value <- obj .? "value"
    pure $ ClearImage {semantic:semantic, value:value}

instance decodeJsonCommand :: DecodeJson Command where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "SetRasterContext" -> SetRasterContext <$> obj .? "context"
      "SetAccumulationContext" -> SetAccumulationContext <$> obj .? "context"
      "SetRenderTarget" -> SetRenderTarget <$> obj .? "target"
      "SetProgram" -> SetProgram <$> obj .? "program"
      "SetSamplerUniform" -> SetSamplerUniform <$> obj .? "name" <*> obj .? "texUnit"
      "SetTexture" -> SetTexture <$> obj .? "texUnit" <*> obj .? "tex"
      "SetSampler" -> SetSampler <$> obj .? "texUnit" <*> obj .? "sampler"
      "RenderSlot" -> RenderSlot <$> obj .? "slot"
      "ClearRenderTarget" -> ClearRenderTarget <$> obj .? "values"
      "GenerateMipMap" -> GenerateMipMap <$> obj .? "texUnit"
      "SaveImage" -> SaveImage <$> obj .? "src" <*> obj .? "dst"
      "LoadImage" -> LoadImage <$> obj .? "src" <*> obj .? "dst"

instance decodeJsonTextureDescriptor :: DecodeJson TextureDescriptor where
  decodeJson json = do
    obj <- decodeJson json
    textureType <- obj .? "textureType"
    textureSize <- obj .? "textureSize"
    textureSemantic <- obj .? "textureSemantic"
    textureSampler <- obj .? "textureSampler"
    textureBaseLevel <- obj .? "textureBaseLevel"
    textureMaxLevel <- obj .? "textureMaxLevel"
    pure $ TextureDescriptor {textureType:textureType, textureSize:textureSize, textureSemantic:textureSemantic, textureSampler:textureSampler, textureBaseLevel:textureBaseLevel, textureMaxLevel:textureMaxLevel}

instance decodeJsonSamplerDescriptor :: DecodeJson SamplerDescriptor where
  decodeJson json = do
    obj <- decodeJson json
    samplerWrapS <- obj .? "samplerWrapS"
    samplerWrapT <- obj .? "samplerWrapT"
    samplerWrapR <- obj .? "samplerWrapR"
    samplerMinFilter <- obj .? "samplerMinFilter"
    samplerMagFilter <- obj .? "samplerMagFilter"
    samplerBorderColor <- obj .? "samplerBorderColor"
    samplerMinLod <- obj .? "samplerMinLod"
    samplerMaxLod <- obj .? "samplerMaxLod"
    samplerLodBias <- obj .? "samplerLodBias"
    samplerCompareFunc <- obj .? "samplerCompareFunc"
    pure $ SamplerDescriptor {samplerWrapS:samplerWrapS, samplerWrapT:samplerWrapT, samplerWrapR:samplerWrapR, samplerMinFilter:samplerMinFilter, samplerMagFilter:samplerMagFilter,
          samplerBorderColor:samplerBorderColor, samplerMinLod:samplerMinLod, samplerMaxLod:samplerMaxLod, samplerLodBias:samplerLodBias, samplerCompareFunc:samplerCompareFunc}

instance decodeJsonParameter :: DecodeJson Parameter where
  decodeJson json = do
    obj <- decodeJson json
    name <- obj .? "name"
    ty <- obj .? "ty"
    pure $ Parameter {name:name, ty:ty}

instance decodeJsonProgram :: DecodeJson Program where
  decodeJson json = do
    obj <- decodeJson json
    programUniforms <- obj .? "programUniforms"
    programStreams <- obj .? "programStreams"
    programInTextures <- obj .? "programInTextures"
    programOutput <- obj .? "programOutput"
    vertexShader <- obj .? "vertexShader"
    geometryShader <- obj .? "geometryShader"
    fragmentShader <- obj .? "fragmentShader"
    pure $ Program {programUniforms:programUniforms, programStreams:programStreams, programInTextures:programInTextures, programOutput:programOutput,
          vertexShader:vertexShader, geometryShader:geometryShader, fragmentShader:fragmentShader}

instance decodeJsonSlot :: DecodeJson Slot where
  decodeJson json = do
    obj <- decodeJson json
    slotName <- obj .? "slotName"
    slotUniforms <- obj .? "slotUniforms"
    slotStreams <- obj .? "slotStreams"
    slotPrimitive <- obj .? "slotPrimitive"
    slotPrograms <- obj .? "slotPrograms"
    pure $ Slot {slotName:slotName, slotUniforms:slotUniforms, slotStreams:slotStreams, slotPrimitive:slotPrimitive, slotPrograms:slotPrograms}

instance decodeJsonTargetItem :: DecodeJson TargetItem where
  decodeJson json = do
    obj <- decodeJson json
    semantic <- obj .? "semantic"
    ref <- obj .? "ref"
    pure $ TargetItem {semantic:semantic, ref:ref}

instance decodeJsonRenderTarget :: DecodeJson RenderTarget where
  decodeJson json = do
    obj <- decodeJson json
    renderTargets <- obj .? "renderTargets"
    pure $ RenderTarget {renderTargets:renderTargets}

instance decodeJsonPipeline :: DecodeJson Pipeline where
  decodeJson json = do
    obj <- decodeJson json
    textures <- obj .? "textures"
    samplers <- obj .? "samplers"
    targets <- obj .? "targets"
    programs <- obj .? "programs"
    slots <- obj .? "slots"
    commands <- obj .? "commands"
    pure $ Pipeline {textures:textures, samplers:samplers, targets:targets, programs:programs, slots:slots, commands:commands}

data MyEither
  = MyLeft String
  | MyRight Pipeline

instance decodeJsonMyEither :: DecodeJson MyEither where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Left" -> MyLeft <$> obj .? "value"
      "Right" -> MyRight <$> obj .? "value"
