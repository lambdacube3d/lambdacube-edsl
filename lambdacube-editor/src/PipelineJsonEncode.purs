module PipelineJsonEncode where

import Data.Argonaut ((~>), (:=), (.?), jsonEmptyObject, printJson)
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Argonaut.Decode (DecodeJson, decodeJson)

import Data.Maybe
import Data.StrMap
import IR

instance encodeJsonV2 :: (EncodeJson a) => EncodeJson (V2 a) where
  encodeJson (V2 x y) =  "x" := x ~> "y" := y ~> jsonEmptyObject

instance encodeJsonV3 :: (EncodeJson a) => EncodeJson (V3 a) where
  encodeJson (V3 x y z) = "x" := x ~> "y" := y ~> "z" := z ~> jsonEmptyObject

instance encodeJsonV4 :: (EncodeJson a) => EncodeJson (V4 a) where
  encodeJson (V4 x y z w) = "x" := x ~> "y" := y ~> "z" := z ~> "w" := w ~> jsonEmptyObject

instance encodeJsonValue :: EncodeJson Value where
  encodeJson v = case v of
    VBool  a -> "tag" := "VBool"  ~> "value" := a ~> jsonEmptyObject
    VV2B   a -> "tag" := "VV2B"   ~> "value" := a ~> jsonEmptyObject
    VV3B   a -> "tag" := "VV3B"   ~> "value" := a ~> jsonEmptyObject
    VV4B   a -> "tag" := "VV4B"   ~> "value" := a ~> jsonEmptyObject
    VWord  a -> "tag" := "VWord"  ~> "value" := a ~> jsonEmptyObject
    VV2U   a -> "tag" := "VV2U"   ~> "value" := a ~> jsonEmptyObject
    VV3U   a -> "tag" := "VV3U"   ~> "value" := a ~> jsonEmptyObject
    VV4U   a -> "tag" := "VV4U"   ~> "value" := a ~> jsonEmptyObject
    VInt   a -> "tag" := "VInt"   ~> "value" := a ~> jsonEmptyObject
    VV2I   a -> "tag" := "VV2I"   ~> "value" := a ~> jsonEmptyObject
    VV3I   a -> "tag" := "VV3I"   ~> "value" := a ~> jsonEmptyObject
    VV4I   a -> "tag" := "VV4I"   ~> "value" := a ~> jsonEmptyObject
    VFloat a -> "tag" := "VFloat" ~> "value" := a ~> jsonEmptyObject
    VV2F   a -> "tag" := "VV2F"   ~> "value" := a ~> jsonEmptyObject
    VV3F   a -> "tag" := "VV3F"   ~> "value" := a ~> jsonEmptyObject
    VV4F   a -> "tag" := "VV4F"   ~> "value" := a ~> jsonEmptyObject
    VM22F  a -> "tag" := "VM22F"  ~> "value" := a ~> jsonEmptyObject
    VM23F  a -> "tag" := "VM23F"  ~> "value" := a ~> jsonEmptyObject
    VM24F  a -> "tag" := "VM24F"  ~> "value" := a ~> jsonEmptyObject
    VM32F  a -> "tag" := "VM32F"  ~> "value" := a ~> jsonEmptyObject
    VM33F  a -> "tag" := "VM33F"  ~> "value" := a ~> jsonEmptyObject
    VM34F  a -> "tag" := "VM34F"  ~> "value" := a ~> jsonEmptyObject
    VM42F  a -> "tag" := "VM42F"  ~> "value" := a ~> jsonEmptyObject
    VM43F  a -> "tag" := "VM43F"  ~> "value" := a ~> jsonEmptyObject
    VM44F  a -> "tag" := "VM44F"  ~> "value" := a ~> jsonEmptyObject

instance encodeJsonInputType :: EncodeJson InputType where
  encodeJson v = case v of
    Bool  -> "tag" := "Bool"   ~> jsonEmptyObject
    V2B   -> "tag" := "V2B"    ~> jsonEmptyObject
    V3B   -> "tag" := "V3B"    ~> jsonEmptyObject
    V4B   -> "tag" := "V4B"    ~> jsonEmptyObject
    Word  -> "tag" := "Word"   ~> jsonEmptyObject
    V2U   -> "tag" := "V2U"    ~> jsonEmptyObject
    V3U   -> "tag" := "V3U"    ~> jsonEmptyObject
    V4U   -> "tag" := "V4U"    ~> jsonEmptyObject
    Int   -> "tag" := "Int"    ~> jsonEmptyObject
    V2I   -> "tag" := "V2I"    ~> jsonEmptyObject
    V3I   -> "tag" := "V3I"    ~> jsonEmptyObject
    V4I   -> "tag" := "V4I"    ~> jsonEmptyObject
    Float -> "tag" := "Float"  ~> jsonEmptyObject
    V2F   -> "tag" := "V2F"    ~> jsonEmptyObject
    V3F   -> "tag" := "V3F"    ~> jsonEmptyObject
    V4F   -> "tag" := "V4F"    ~> jsonEmptyObject
    M22F  -> "tag" := "M22F"   ~> jsonEmptyObject
    M23F  -> "tag" := "M23F"   ~> jsonEmptyObject
    M24F  -> "tag" := "M24F"   ~> jsonEmptyObject
    M32F  -> "tag" := "M32F"   ~> jsonEmptyObject
    M33F  -> "tag" := "M33F"   ~> jsonEmptyObject
    M34F  -> "tag" := "M34F"   ~> jsonEmptyObject
    M42F  -> "tag" := "M42F"   ~> jsonEmptyObject
    M43F  -> "tag" := "M43F"   ~> jsonEmptyObject
    M44F  -> "tag" := "M44F"   ~> jsonEmptyObject
    STexture1D        -> "tag" := "STexture1D"         ~> jsonEmptyObject
    STexture2D        -> "tag" := "STexture2D"         ~> jsonEmptyObject
    STextureCube      -> "tag" := "STextureCube"       ~> jsonEmptyObject
    STexture1DArray   -> "tag" := "STexture1DArray"    ~> jsonEmptyObject
    STexture2DArray   -> "tag" := "STexture2DArray"    ~> jsonEmptyObject
    STexture2DRect    -> "tag" := "STexture2DRect"     ~> jsonEmptyObject
    FTexture1D        -> "tag" := "FTexture1D"         ~> jsonEmptyObject
    FTexture2D        -> "tag" := "FTexture2D"         ~> jsonEmptyObject
    FTexture3D        -> "tag" := "FTexture3D"         ~> jsonEmptyObject
    FTextureCube      -> "tag" := "FTextureCube"       ~> jsonEmptyObject
    FTexture1DArray   -> "tag" := "FTexture1DArray"    ~> jsonEmptyObject
    FTexture2DArray   -> "tag" := "FTexture2DArray"    ~> jsonEmptyObject
    FTexture2DMS      -> "tag" := "FTexture2DMS"       ~> jsonEmptyObject
    FTexture2DMSArray -> "tag" := "FTexture2DMSArray"  ~> jsonEmptyObject
    FTextureBuffer    -> "tag" := "FTextureBuffer"     ~> jsonEmptyObject
    FTexture2DRect    -> "tag" := "FTexture2DRect"     ~> jsonEmptyObject
    ITexture1D        -> "tag" := "ITexture1D"         ~> jsonEmptyObject
    ITexture2D        -> "tag" := "ITexture2D"         ~> jsonEmptyObject
    ITexture3D        -> "tag" := "ITexture3D"         ~> jsonEmptyObject
    ITextureCube      -> "tag" := "ITextureCube"       ~> jsonEmptyObject
    ITexture1DArray   -> "tag" := "ITexture1DArray"    ~> jsonEmptyObject
    ITexture2DArray   -> "tag" := "ITexture2DArray"    ~> jsonEmptyObject
    ITexture2DMS      -> "tag" := "ITexture2DMS"       ~> jsonEmptyObject
    ITexture2DMSArray -> "tag" := "ITexture2DMSArray"  ~> jsonEmptyObject
    ITextureBuffer    -> "tag" := "ITextureBuffer"     ~> jsonEmptyObject
    ITexture2DRect    -> "tag" := "ITexture2DRect"     ~> jsonEmptyObject
    UTexture1D        -> "tag" := "UTexture1D"         ~> jsonEmptyObject
    UTexture2D        -> "tag" := "UTexture2D"         ~> jsonEmptyObject
    UTexture3D        -> "tag" := "UTexture3D"         ~> jsonEmptyObject
    UTextureCube      -> "tag" := "UTextureCube"       ~> jsonEmptyObject
    UTexture1DArray   -> "tag" := "UTexture1DArray"    ~> jsonEmptyObject
    UTexture2DArray   -> "tag" := "UTexture2DArray"    ~> jsonEmptyObject
    UTexture2DMS      -> "tag" := "UTexture2DMS"       ~> jsonEmptyObject
    UTexture2DMSArray -> "tag" := "UTexture2DMSArray"  ~> jsonEmptyObject
    UTextureBuffer    -> "tag" := "UTextureBuffer"     ~> jsonEmptyObject
    UTexture2DRect    -> "tag" := "UTexture2DRect"     ~> jsonEmptyObject
