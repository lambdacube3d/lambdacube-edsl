module LC_I_HOAS where

import Data.ByteString.Char8
import Data.Typeable
import Data.Int

import TypeLevel.Number.Nat
import TypeLevel.Number.Nat.Num

import LCType

import LC_APIType
import LC_T_APIType
import LC_I_APIType
import LC_T_DSLType
import LC_T_PrimFun
import LC_I_PrimFun

import LC_T_HOAS
import LC_U_HOAS hiding (Exp
                        ,FragmentFilter
                        ,FragmentOut
                        ,GP
                        ,GeometryOut
                        ,GeometryShader
                        ,VertexOut)

import qualified LC_U_HOAS as U
import qualified LC_U_APIType as U


newtype ExpI stage t = ExpI (U.Exp)
instance Exp ExpI where
    type Exp_PrimFun ExpI               = PrimFunI
    type Exp_Input ExpI                 = InputI
    type Exp_FlatTuple ExpI             = FlatTupleI U.Exp
    type Exp_InterpolatedFlatTuple ExpI = FlatTupleI (U.Interpolated U.Exp)
    type Exp_TupleIdx ExpI              = TupleIdxI

    tag                             = ExpI . Tag
    lam a                           = ExpI (Lam (\x -> let ExpI b = a (ExpI x) in b))
    cnst                            = ExpI . Const . toValue
    primVar (InputI a)              = ExpI (uncurry PrimVar a)
    uni (InputI a)                  = ExpI (uncurry Uni a)
    cond (ExpI c) (ExpI t) (ExpI e) = ExpI (Cond c t e)
    primApp (PrimFunI f) (ExpI a)   = ExpI (PrimApp f a)
    tup (FlatTupleI a)              = ExpI (Tup a)
    prj (TupleIdxI a) (ExpI b)      = ExpI (Prj a b)
--    sampler a b (TextureI c)                    = ExpI (Sampler a b c)
    vertexOut (ExpI a) (ExpI b) (FlatTupleI c)  = ExpI (U.VertexOut a b c)
    geometryOut (ExpI a) (ExpI b) (ExpI c)
               (ExpI d) (ExpI e) (FlatTupleI f) = ExpI (U.GeometryOut a b c d e f)
    fragmentOut (FlatTupleI a)                  = ExpI (U.FragmentOut a)
    fragmentOutDepth (ExpI a) (FlatTupleI b)    = ExpI (U.FragmentOutDepth a b)
    fragmentOutRastDepth (FlatTupleI a)         = ExpI (U.FragmentOutRastDepth a)

{-
newtype TextureI a b c d e = TextureI U.Texture
instance Texture TextureI where
    type Texture_TextureType TextureI   = TextureTypeI
    type Texture_Image TextureI         = ImageI
    type Texture_MipMap TextureI        = MipMapI
    textureSlot a (TextureTypeI b)      = TextureI (TextureSlot a b)
--    texture (TextureTypeI a)
--            (MipMapI b)                 = TextureI (Texture a b)
-}
{-
data Texture gp
    = TextureSlot   ByteString TextureType
    | Texture       TextureType MipMap [gp]
    deriving (Show, Eq, Ord)

class Texture texture where
    type Texture_TextureType texture :: * -> * -> * -> * -> * -> * -> *
    type Texture_Image texture :: * -> * -> *
    type Texture_MipMap texture :: * -> *

    textureSlot     :: (IsValidTextureSlot t
                       ,TextureType textureType, textureType ~ Texture_TextureType texture)
                    => ByteString -- texture slot name
                    -> textureType dim mip arr layerCount t ar
                    -> texture gp dim arr t ar
    -- TODO:
    --  add texture internal format specification
    texture         :: (TextureType textureType, textureType ~ Texture_TextureType texture
                       ,Image image, image ~ Texture_Image texture
                       ,MipMap mipMap, mipMap ~ Texture_MipMap texture)
                    => textureType dim (MipRepr mip) arr layerCount t ar
                    -- -> TexSizeRepr dim
                    -> mipMap mip
                    -> TexRepr dim mip gp image layerCount (TexDataRepr ar t) -- FIXME: for cube it will give wrong type
                    -> texture gp dim arr t ar
-}

newtype GeometryShaderI a b c d e = GeometryShaderI U.GeometryShader
instance GeometryShader GeometryShaderI where
    type GeometryShader_Exp GeometryShaderI         = ExpI
    type GeometryShader_Primitive GeometryShaderI   = PrimitiveI
    noGeometryShader                                = GeometryShaderI NoGeometryShader
    geometryShader a (PrimitiveI b) c
                  (ExpI d) (ExpI e) (ExpI f)        = GeometryShaderI (U.GeometryShader (toInt a) b c d e f)

newtype FragmentFilterI a = FragmentFilterI U.FragmentFilter
instance FragmentFilter FragmentFilterI where
    type FragmentFilter_Exp FragmentFilterI = ExpI
    passAll         = FragmentFilterI PassAll
    filter (ExpI a) = FragmentFilterI (Filter a)

newtype GPI a = GPI U.GP
instance GP GPI where
    type GP_Exp GPI                         = ExpI
    type GP_GeometryShader GPI              = GeometryShaderI
    type GP_RasterContext GPI               = RasterContextI
    type GP_FragmentFilter GPI              = FragmentFilterI
    type GP_Primitive GPI                   = PrimitiveI
    type GP_TupleIdx GPI                    = TupleIdxI
    type GP_FlatTupleImage GPI              = FlatTupleI U.Image
    type GP_FlatTupleFragmentOperation GPI  = FlatTupleI U.FragmentOperation
    gpTag a                                                     = GPI (GPTag a)
    fetch a (PrimitiveI b) c                                    = GPI (Fetch a b (toInputList c))
    transform (ExpI a) (GPI b)                                  = GPI (Transform a b)
    rasterize (RasterContextI a) (GeometryShaderI b) (GPI c)    = GPI (Rasterize a b c)
    frameBuffer a (FlatTupleI b)                                = GPI (FrameBuffer a b)
    accumulate (FlatTupleI a) (FragmentFilterI b)
               (ExpI c) (GPI d) (GPI e)                         = GPI (Accumulate a b c d e)
    prjFrameBuffer a (TupleIdxI b) (GPI c)                      = GPI (PrjFrameBuffer a b c)
    prjImage a b (GPI c)                                        = GPI (PrjImage a (toInt b) c)
