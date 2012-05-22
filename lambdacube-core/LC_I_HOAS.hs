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
                        ,VertexOut
                        ,GeometryShader
                        ,GeometryOut
                        ,FragmentOut)

import qualified LC_U_HOAS as U
import qualified LC_U_APIType as U


newtype ExpI stage t = ExpI (U.Exp)
instance Exp ExpI where
    type Exp_PrimFun ExpI           = PrimFunI
    type Exp_Input ExpI             = InputI
    type Exp_FlatTuple ExpI         = FlatTupleI U.Exp
    type Exp_TupleIdx ExpI          = TupleIdxI
    tag                             = ExpI . Tag
    lam a                           = ExpI (Lam (\x -> let ExpI b = a (ExpI x) in b))
    cnst                            = ExpI . Const . toValue
    primVar (InputI a)              = ExpI (uncurry PrimVar a)
    uni (InputI a)                  = ExpI (uncurry Uni a)
    cond (ExpI c) (ExpI t) (ExpI e) = ExpI (Cond c t e)
    primApp (PrimFunI f) (ExpI a)   = ExpI (PrimApp f a)
    tup (FlatTupleI a)              = ExpI (Tup a)
    prj (TupleIdxI a) (ExpI b)      = ExpI (Prj a b)
--    sampler a b (TextureI c)        = ExpI (Sampler a b c)
{-
data Exp
    = Tag       Int
    | Const     Value
    | PrimVar   ByteString InputType
    | Uni       ByteString InputType
    | Cond      Exp Exp Exp
    | PrimApp   PrimFun Exp
    | Tup       [Exp]
    | Prj       Int Exp
    | Sampler   Filter EdgeMode (Texture GP)

class Exp exp where
    type Exp_GP exp :: * -> *
    type Exp_Input exp :: * -> *
    type Exp_PrimFun exp :: * -> * -> *
    type Exp_Texture exp :: (* -> *) -> * -> * -> * -> * -> *
    type Exp_FlatTuple exp :: (* -> Constraint) -> (* -> *) -> * -> *
    type Exp_TupleIdx exp :: * -> * -> *

    -- Needed for conversion to de Bruijn form
    tag     :: GPU t
            => Int
            -> exp stage t
                 -- environment size at defining occurrence

    -- constant value
    cnst    :: IsScalar t
            => t
            -> exp stage t

    -- builtin variable
    primVar :: (GPU t
               , Input input, input ~ Exp_Input exp)
            => input t
            -> exp stage t

    -- uniform value
    uni     :: (GPU t
               , Input input, input ~ Exp_Input exp)
            => input t
            -> exp stage t

    -- conditional expression
    cond    :: GPU t
            => exp stage Bool
            -> exp stage t
            -> exp stage t
            -> exp stage t

    primApp :: (GPU a, GPU r
               , PrimFun primFun, primFun ~ Exp_PrimFun exp)
            => primFun stage (a -> r)
            -> exp stage a
            -> exp stage r

    -- tuple support
    tup     :: (GPU t, IsTuple t
               , Tuple tuple, tuple ~ Exp_Tuple exp)
            => tuple (exp stage) (TupleRepr t)
            -> exp stage t

    prj     :: (GPU e, GPU t, IsTuple t
               , TupleIdx tupleIdx, tupleIdx ~ Exp_TupleIdx exp)
            => tupleIdx (TupleRepr t) e
            -> exp stage t
            -> exp stage e

    -- sampler support
    sampler :: (GPU (Sampler dim arr t ar)
               , GP gp, gp ~ Exp_GP exp
               , Texture gp texture, texture ~ Exp_Texture exp)
            => Filter
            -> EdgeMode
            -> texture gp dim arr t ar
            -> exp stage (Sampler dim arr t ar)
-}

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

newtype VertexOutI a = VertexOutI (U.VertexOut)
instance VertexOut VertexOutI where
    type VertexOut_Exp VertexOutI               = ExpI
    type VertexOut_FlatTuple VertexOutI         = FlatTupleI (U.Interpolated U.Exp)
    vertexOut (ExpI a) (ExpI b) (FlatTupleI c)  = VertexOutI (U.VertexOut a b c)

newtype GeometryShaderI a b c d e = GeometryShaderI U.GeometryShader
instance GeometryShader GeometryShaderI where
    type GeometryShader_Exp GeometryShaderI         = ExpI
    type GeometryShader_GeometryOut GeometryShaderI = VertexOutI
    type GeometryShader_Primitive GeometryShaderI   = PrimitiveI
    noGeometryShader                                = GeometryShaderI NoGeometryShader
    geometryShader a (PrimitiveI b) c (ExpI d) (ExpI e) f         = GeometryShaderI (U.GeometryShader (toInt a) b c d e undefined)
{-
data GeometryShader
    = NoGeometryShader
    | GeometryShader Int PrimitiveType Int (Exp -> Exp) (Exp -> Exp) (Exp -> GeometryOut)

class GeometryShader geometryShader where
    type GeometryShader_Exp geometryShader :: * -> * -> *
    type GeometryShader_GeometryOut geometryShader :: * -> *

    noGeometryShader    :: geometryShader prim prim N1 a a

    geometryShader      :: (GPU (PrimitiveVertices primIn a), GPU i, GPU j, GPU b, IsPrimitive primIn, IsPrimitive primOut, Nat layerNum
                           , Exp exp, exp ~ GeometryShader_Exp geometryShader
                           , GeometryOut geometryOut, geometryOut ~ GeometryShader_GeometryOut geometryShader
                           )
                        => layerNum                                                 -- geometry shader:
                        -> primOut                                                  -- output primitive
                        -> Int                                                      -- max amount of generated vertices
                        -> (exp G (PrimitiveVertices primIn a) -> exp G (i,Int32))  -- how many primitives?
                        -> (exp G i -> exp G (i,j,Int32))                           -- how many vertices?
                        -> (exp G j -> geometryOut (j,b))                           -- generate vertices
                        -> geometryShader primIn primOut layerNum a b
-}

newtype GeometryOutI a = GeometryOutI U.GeometryOut
instance GeometryOut GeometryOutI where
    type GeometryOut_Exp GeometryOutI       = ExpI
    type GeometryOut_FlatTuple GeometryOutI = FlatTupleI (U.Interpolated U.Exp)
    geometryOut (ExpI a) (ExpI b) (ExpI c) (ExpI d) (ExpI e) (FlatTupleI f) = GeometryOutI (U.GeometryOut a b c d e f)

newtype FragmentOutI a = FragmentOutI U.FragmentOut
instance FragmentOut FragmentOutI where
    type FragmentOut_Exp FragmentOutI           = ExpI
    type FragmentOut_FlatTuple FragmentOutI     = FlatTupleI U.Exp
    fragmentOut (FlatTupleI a)                  = FragmentOutI (U.FragmentOut a)
    fragmentOutDepth (ExpI a) (FlatTupleI b)    = FragmentOutI (FragmentOutDepth a b)
    fragmentOutRastDepth (FlatTupleI a)         = FragmentOutI (FragmentOutRastDepth a)
