module LC_I_APIType where

import GHC.Exts

import Data.ByteString.Char8
import Data.Int
import Data.Word

import TypeLevel.Number.Nat
import TypeLevel.Number.Nat.Num

import LC_APIType
import LC_T_DSLType
import LC_T_APIType
import LC_U_APIType hiding  ( Blending
                            , ColorArity
                            , FragmentOperation
                            , Image
                            , MipMap
                            , Primitive
                            , RasterContext
                            , Texture
                            , TextureDataType
                            , TextureType)

import qualified LC_U_APIType as U

newtype TupleIdxI a b = TupleIdxI Int
instance TupleIdx TupleIdxI where
    zeroTupIdx                  = TupleIdxI 0
    succTupIdx (TupleIdxI i)    = TupleIdxI (i+1)

newtype InputI t = InputI (ByteString, InputType)
instance Input InputI where
    iBool   n = InputI (n,Bool)
    iV2B    n = InputI (n,V2B)
    iV3B    n = InputI (n,V3B)
    iV4B    n = InputI (n,V4B)
    iWord   n = InputI (n,Word)
    iV2U    n = InputI (n,V2U)
    iV3U    n = InputI (n,V3U)
    iV4U    n = InputI (n,V4U)
    iInt    n = InputI (n,Int)
    iV2I    n = InputI (n,V2I)
    iV3I    n = InputI (n,V3I)
    iV4I    n = InputI (n,V4I)
    iFloat  n = InputI (n,Float)
    iV2F    n = InputI (n,V2F)
    iV3F    n = InputI (n,V3F)
    iV4F    n = InputI (n,V4F)
    iM22F   n = InputI (n,M22F)
    iM23F   n = InputI (n,M23F)
    iM24F   n = InputI (n,M24F)
    iM32F   n = InputI (n,M32F)
    iM33F   n = InputI (n,M33F)
    iM34F   n = InputI (n,M34F)
    iM42F   n = InputI (n,M42F)
    iM43F   n = InputI (n,M43F)
    iM44F   n = InputI (n,M44F)

newtype BlendingI t = BlendingI U.Blending
instance Blending BlendingI where
    noBlending      = BlendingI NoBlending
    blendLogicOp op = BlendingI (BlendLogicOp op)
    blend e f c     = BlendingI (Blend e f c)

newtype FlatTupleI e (c :: * -> Constraint) (a :: * -> *) (stage :: * -> *) t = FlatTupleI [e]
{-
instance FlatTuple FragmentOperationI (FlatTupleI U.FragmentOperation) where
    zt      = FlatTupleI []
    (FragmentOperationI a) .:. (FlatTupleI b) = FlatTupleI (a : b)

instance FlatTuple (ImageI layerCount) (FlatTupleI U.Image) where
    zt      = FlatTupleI []
    (ImageI a) .:. (FlatTupleI b) = FlatTupleI (a : b)
-}
newtype RasterContextI t = RasterContextI U.RasterContext
instance RasterContext RasterContextI where
    pointCtx            = RasterContextI PointCtx
    lineCtx a b         = RasterContextI (LineCtx a b)
    triangleCtx a b c d = RasterContextI (TriangleCtx a b c d)

newtype FragmentOperationI t = FragmentOperationI U.FragmentOperation
instance FragmentOperation FragmentOperationI where
    type FragmentOperation_Blending FragmentOperationI = BlendingI
    depthOp a b             = FragmentOperationI (DepthOp a b)
    stencilOp a b c         = FragmentOperationI (StencilOp a b c)
    colorOp (BlendingI a) b = FragmentOperationI (ColorOp a (toValue b))

newtype ImageI a b = ImageI U.Image
instance Image ImageI where
    depthImage a b      = ImageI (DepthImage (toInt a) b)
    stencilImage a b    = ImageI (StencilImage (toInt a) b)
    colorImage a b      = ImageI (ColorImage (toInt a) (toValue b))

newtype ColorArityI a = ColorArityI U.ColorArity
instance ColorArity ColorArityI where
    red     = ColorArityI Red
    rg      = ColorArityI RG
    rgb     = ColorArityI RGB
    rgba    = ColorArityI RGBA

newtype TextureDataTypeI a b = TextureDataTypeI U.TextureDataType
instance TextureDataType TextureDataTypeI where
    type TextureDataType_ColorArity TextureDataTypeI = ColorArityI
    float (ColorArityI a)   = TextureDataTypeI (FloatT a)
    int (ColorArityI a)     = TextureDataTypeI (IntT a)
    word (ColorArityI a)    = TextureDataTypeI (WordT a)
    shadow                  = TextureDataTypeI ShadowT

newtype TextureTypeI a b c d e f = TextureTypeI U.TextureType
instance TextureType TextureTypeI where
    type TextureType_TextureDataType TextureTypeI = TextureDataTypeI
    texture1D (TextureDataTypeI a) b    = TextureTypeI (Texture1D a (toInt b))
    texture2D (TextureDataTypeI a) b    = TextureTypeI (Texture2D a (toInt b))
    texture3D (TextureDataTypeI a)      = TextureTypeI (Texture3D a)
    textureCube (TextureDataTypeI a)    = TextureTypeI (TextureCube a)
    textureRect (TextureDataTypeI a)    = TextureTypeI (TextureRect a)
    texture2DMS (TextureDataTypeI a) b  = TextureTypeI (Texture2DMS a (toInt b))
    textureBuffer (TextureDataTypeI a)  = TextureTypeI (TextureBuffer a)

newtype MipMapI a = MipMapI U.MipMap
instance MipMap MipMapI where
    mip     = MipMapI Mip
    noMip   = MipMapI NoMip
    autoMip = MipMapI AutoMip

newtype PrimitiveI a = PrimitiveI U.PrimitiveType
instance Primitive PrimitiveI where
    triangle    = PrimitiveI Triangle
    line        = PrimitiveI Line
    point       = PrimitiveI Point
