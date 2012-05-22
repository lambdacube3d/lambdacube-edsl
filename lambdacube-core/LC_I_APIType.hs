module LC_I_APIType where

import GHC.Exts

import Data.ByteString.Char8
import Data.Int
import Data.Word

import TypeLevel.Number.Nat
import TypeLevel.Number.Nat.Num

import LC_APIType
import LC_T_APIType
import LC_U_APIType hiding (Blending,RasterContext,FragmentOperation,Image,TextureDataType)
import qualified LC_U_APIType as U

newtype InputI t = InputI (ByteString, InputType)
instance Input InputI where
    iBool   n = InputI (n,ITBool)
    iV2B    n = InputI (n,ITV2B)
    iV3B    n = InputI (n,ITV3B)
    iV4B    n = InputI (n,ITV4B)
    iWord   n = InputI (n,ITWord)
    iV2U    n = InputI (n,ITV2U)
    iV3U    n = InputI (n,ITV3U)
    iV4U    n = InputI (n,ITV4U)
    iInt    n = InputI (n,ITInt)
    iV2I    n = InputI (n,ITV2I)
    iV3I    n = InputI (n,ITV3I)
    iV4I    n = InputI (n,ITV4I)
    iFloat  n = InputI (n,ITFloat)
    iV2F    n = InputI (n,ITV2F)
    iV3F    n = InputI (n,ITV3F)
    iV4F    n = InputI (n,ITV4F)
    iM22F   n = InputI (n,ITM22F)
    iM23F   n = InputI (n,ITM23F)
    iM24F   n = InputI (n,ITM24F)
    iM32F   n = InputI (n,ITM32F)
    iM33F   n = InputI (n,ITM33F)
    iM34F   n = InputI (n,ITM34F)
    iM42F   n = InputI (n,ITM42F)
    iM43F   n = InputI (n,ITM43F)
    iM44F   n = InputI (n,ITM44F)

newtype BlendingI t = BlendingI U.Blending
instance Blending BlendingI where
    noBlending      = BlendingI NoBlending
    blendLogicOp op = BlendingI (BlendLogicOp op)
    blend e f c     = BlendingI (Blend e f c)

newtype FlatTupleI e (c :: * -> Constraint) (a :: * -> *) t = FlatTupleI [e]
instance FlatTuple FragmentOperationI (FlatTupleI U.FragmentOperation) where
    zt      = FlatTupleI []
    (FragmentOperationI a) .:. (FlatTupleI b) = FlatTupleI (a : b)

instance FlatTuple (ImageI layerCount) (FlatTupleI U.Image) where
    zt      = FlatTupleI []
    (ImageI a) .:. (FlatTupleI b) = FlatTupleI (a : b)

newtype RasterContextI t = RasterContextI U.RasterContext
instance RasterContext RasterContextI where
    pointCtx            = RasterContextI PointCtx
    lineCtx a b         = RasterContextI (LineCtx a b)
    triangleCtx a b c d = RasterContextI (TriangleCtx a b c d)

newtype FragmentOperationI t = FragmentOperationI U.FragmentOperation
instance FragmentOperation FragmentOperationI where
    depthOp a b     = FragmentOperationI (DepthOp a b)
--    stencilOp a b c = FragmentOperationI (StencilOp a b c)
--    colorOp a b     = FragmentOperationI (ColorOp a b)

{-
data FragmentOperation
    = DepthOp       DepthFunction Bool
    | StencilOp     StencilTests StencilOps StencilOps
    | ColorOp       Blending [Bool]
    deriving (Show, Eq, Ord)

class FragmentOperation fragmentOperation where
    depthOp         :: DepthFunction
                    -> Bool     -- depth write
                    -> fragmentOperation (Depth Float)

    stencilOp       :: StencilTests
                    -> StencilOps
                    -> StencilOps
                    -> fragmentOperation (Stencil Int32)

    colorOp         :: (IsVecScalar d mask Bool, IsVecScalar d color c, IsNum c,
                        Blending reprB)
                    => reprB c   -- blending type
                    -> mask         -- write mask
                    -> fragmentOperation (Color color)
-}

newtype ImageI a b = ImageI U.Image
instance Image ImageI where
    depthImage a b      = ImageI (DepthImage (toInt a) b)
    stencilImage a b    = ImageI (StencilImage (toInt a) b)
--    colorImage a b      = ImageI (ColorImage (toInt a) b)

{-
data Image
    = DepthImage    Int Float
    | StencilImage  Int Int32
    | ColorImage    Int V4F
    deriving (Show, Eq, Ord)

class Image image where
    depthImage      :: Nat layerCount
                    => layerCount
                    -> Float    -- initial value
                    -> image layerCount (Depth Float)

    stencilImage    :: Nat layerCount
                    => layerCount
                    -> Int32    -- initial value
                    -> image layerCount (Stencil Int32)

    colorImage      :: (IsNum t, IsVecScalar d color t, Nat layerCount)
                    => layerCount
                    -> color    -- initial value
                    -> image layerCount (Color color)
-}

newtype TextureDataTypeI a b = TextureDataTypeI U.TextureDataType
instance TextureDataType TextureDataTypeI where
--    float a = TextureDataTypeI (Float a)
{-
data TextureDataType
    = Float         ColorArity
    | Int           ColorArity
    | Word          ColorArity
    | Shadow
    deriving (Show, Eq, Ord)

class TextureDataType textureDataType where
    float   :: (Eq a, ColorArity a)
            => a
            -> textureDataType (Regular Float) a

    int     :: (Eq a, ColorArity a)
            => a
            -> textureDataType (Regular Int) a

    word    :: (Eq a, ColorArity a)
            => a
            -> textureDataType (Regular Word) a

    shadow  :: textureDataType (Shadow Float) Red   -- TODO: add params required by shadow textures
-}

