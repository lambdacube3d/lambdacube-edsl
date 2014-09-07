module LambdaCube.Language.ReifyType where

import Data.Int
import Data.Word
import Data.Typeable

import LambdaCube.Core.Type
import qualified LambdaCube.Core.Type as U

data TextureShape
    = Tex1D
    | Tex2D
    | Tex3D
    | TexRect

deriving instance Typeable Tex1D
deriving instance Typeable Tex2D
deriving instance Typeable Tex3D
deriving instance Typeable TexRect

data Red    = Red  deriving (Typeable,Eq,Ord)
data RG     = RG   deriving (Typeable,Eq,Ord)
data RGB    = RGB  deriving (Typeable,Eq,Ord)
data RGBA   = RGBA deriving (Typeable,Eq,Ord)

data TextureSemantics a
    = Regular a
    | Shadow a
    | MultiSample a
    | Buffer a

deriving instance Typeable Regular
deriving instance Typeable Shadow
deriving instance Typeable MultiSample
deriving instance Typeable Buffer

data TextureArray
    = SingleTex     -- singleton texture
    | ArrayTex      -- array texture
    | CubeTex       -- cube texture = array with size 6

deriving instance Typeable SingleTex
deriving instance Typeable ArrayTex
deriving instance Typeable CubeTex

--data Sampler dim layerCount t ar
data Sampler :: TextureShape -> TextureArray -> TextureSemantics * -> * -> *

deriving instance Typeable Sampler

-- IsScalar means here that the related type is not a tuple, but a GPU primitive type
class GPU a => IsScalar a where
    toValue     :: a -> Value
    toType      :: a -> InputType
{-
instance Nat sh => IsScalar (Sampler dim sh t ar) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = error "toType Sampler is not implemented yet" -- TODO
-}
-- Float
instance (Typeable a) => IsScalar (Sampler Tex1D SingleTex (Regular Float) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.FTexture1D
instance (Typeable a) => IsScalar (Sampler Tex2D SingleTex (Regular Float) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.FTexture2D
instance (Typeable a) => IsScalar (Sampler Tex3D SingleTex (Regular Float) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.FTexture3D
instance (Typeable a) => IsScalar (Sampler Tex2D CubeTex (Regular Float) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.FTextureCube
instance (Typeable a) => IsScalar (Sampler Tex1D ArrayTex (Regular Float) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.FTexture1DArray
instance (Typeable a) => IsScalar (Sampler Tex2D ArrayTex (Regular Float) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.FTexture2DArray
instance (Typeable a) => IsScalar (Sampler Tex2D SingleTex (MultiSample Float) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.FTexture2DMS
instance (Typeable a) => IsScalar (Sampler Tex2D ArrayTex (MultiSample Float) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.FTexture2DMSArray
instance (Typeable a) => IsScalar (Sampler Tex1D SingleTex (Buffer Float) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.FTextureBuffer
instance (Typeable a) => IsScalar (Sampler TexRect SingleTex (Regular Float) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.FTexture2DRect

-- Int
instance (Typeable a) => IsScalar (Sampler Tex1D SingleTex (Regular Int) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.ITexture1D
instance (Typeable a) => IsScalar (Sampler Tex2D SingleTex (Regular Int) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.ITexture2D
instance (Typeable a) => IsScalar (Sampler Tex3D SingleTex (Regular Int) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.ITexture3D
instance (Typeable a) => IsScalar (Sampler Tex2D CubeTex (Regular Int) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.ITextureCube
instance (Typeable a) => IsScalar (Sampler Tex1D ArrayTex (Regular Int) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.ITexture1DArray
instance (Typeable a) => IsScalar (Sampler Tex2D ArrayTex (Regular Int) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.ITexture2DArray
instance (Typeable a) => IsScalar (Sampler Tex2D SingleTex (MultiSample Int) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.ITexture2DMS
instance (Typeable a) => IsScalar (Sampler Tex2D ArrayTex (MultiSample Int) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.ITexture2DMSArray
instance (Typeable a) => IsScalar (Sampler Tex1D SingleTex (Buffer Int) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.ITextureBuffer
instance (Typeable a) => IsScalar (Sampler TexRect SingleTex (Regular Int) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.ITexture2DRect

-- Word
instance (Typeable a) => IsScalar (Sampler Tex1D SingleTex (Regular Word) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.UTexture1D
instance (Typeable a) => IsScalar (Sampler Tex2D SingleTex (Regular Word) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.UTexture2D
instance (Typeable a) => IsScalar (Sampler Tex3D SingleTex (Regular Word) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.UTexture3D
instance (Typeable a) => IsScalar (Sampler Tex2D CubeTex (Regular Word) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.UTextureCube
instance (Typeable a) => IsScalar (Sampler Tex1D ArrayTex (Regular Word) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.UTexture1DArray
instance (Typeable a) => IsScalar (Sampler Tex2D ArrayTex (Regular Word) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.UTexture2DArray
instance (Typeable a) => IsScalar (Sampler Tex2D SingleTex (MultiSample Word) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.UTexture2DMS
instance (Typeable a) => IsScalar (Sampler Tex2D ArrayTex (MultiSample Word) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.UTexture2DMSArray
instance (Typeable a) => IsScalar (Sampler Tex1D SingleTex (Buffer Word) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.UTextureBuffer
instance (Typeable a) => IsScalar (Sampler TexRect SingleTex (Regular Word) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.UTexture2DRect

-- Shadow
instance (Typeable a) => IsScalar (Sampler Tex1D SingleTex (Shadow Float) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.STexture1D
instance (Typeable a) => IsScalar (Sampler Tex2D SingleTex (Shadow Float) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.STexture2D
instance (Typeable a) => IsScalar (Sampler Tex2D CubeTex (Shadow Float) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.STextureCube
instance (Typeable a) => IsScalar (Sampler Tex1D ArrayTex (Shadow Float) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.STexture1DArray
instance (Typeable a) => IsScalar (Sampler Tex2D ArrayTex (Shadow Float) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.STexture2DArray
instance (Typeable a) => IsScalar (Sampler TexRect SingleTex (Shadow Float) a) where
    toValue v    = error "toValue Sampler is not implemented yet" -- TODO
    toType _     = U.STexture2DRect

instance IsScalar Int32 where
    toValue v    = VInt v
    toType _     = U.Int
instance IsScalar Word32 where
    toValue v    = VWord v
    toType _     = U.Word
instance IsScalar Float where
    toValue v    = VFloat v
    toType _     = U.Float
instance IsScalar Bool where
    toValue v    = VBool v
    toType _     = U.Bool
instance IsScalar M22F where
    toValue v    = VM22F v
    toType _     = U.M22F
instance IsScalar M23F where
    toValue v    = VM23F v
    toType _     = U.M23F
instance IsScalar M24F where
    toValue v    = VM24F v
    toType _     = U.M24F
instance IsScalar M32F where
    toValue v    = VM32F v
    toType _     = U.M32F
instance IsScalar M33F where
    toValue v    = VM33F v
    toType _     = U.M33F
instance IsScalar M34F where
    toValue v    = VM34F v
    toType _     = U.M34F
instance IsScalar M42F where
    toValue v    = VM42F v
    toType _     = U.M42F
instance IsScalar M43F where
    toValue v    = VM43F v
    toType _     = U.M43F
instance IsScalar M44F where
    toValue v    = VM44F v
    toType _     = U.M44F
instance IsScalar V2F where
    toValue v    = VV2F v
    toType _     = U.V2F
instance IsScalar V3F where
    toValue v    = VV3F v
    toType _     = U.V3F
instance IsScalar V4F where
    toValue v    = VV4F v
    toType _     = U.V4F
instance IsScalar V2I where
    toValue v    = VV2I v
    toType _     = U.V2I
instance IsScalar V3I where
    toValue v    = VV3I v
    toType _     = U.V3I
instance IsScalar V4I where
    toValue v    = VV4I v
    toType _     = U.V4I
instance IsScalar V2U where
    toValue v    = VV2U v
    toType _     = U.V2U
instance IsScalar V3U where
    toValue v    = VV3U v
    toType _     = U.V3U
instance IsScalar V4U where
    toValue v    = VV4U v
    toType _     = U.V4U
instance IsScalar V2B where
    toValue v    = VV2B v
    toType _     = U.V2B
instance IsScalar V3B where
    toValue v    = VV3B v
    toType _     = U.V3B
instance IsScalar V4B where
    toValue v    = VV4B v
    toType _     = U.V4B

instance Show (Sampler dim layerCount t ar) where
    show _ = "Sampler dim layerCount t ar"

-- GPU type restriction, the functions are used in shader codegen
class (Show a, Typeable a) => GPU a where
    tupleType   :: a -> Ty

-- Float
instance (Typeable a) => GPU (Sampler Tex1D SingleTex (Regular Float) a) where
    tupleType v = Single $ toType v
instance (Typeable a) => GPU (Sampler Tex2D SingleTex (Regular Float) a) where
    tupleType v = Single $ toType v
instance (Typeable a) => GPU (Sampler Tex3D SingleTex (Regular Float) a) where
    tupleType v = Single $ toType v
instance (Typeable a) => GPU (Sampler Tex2D CubeTex (Regular Float) a) where
    tupleType v = Single $ toType v
instance (Typeable a) => GPU (Sampler Tex1D ArrayTex (Regular Float) a) where
    tupleType v = Single $ toType v
instance (Typeable a) => GPU (Sampler Tex2D ArrayTex (Regular Float) a) where
    tupleType v = Single $ toType v
instance (Typeable a) => GPU (Sampler Tex2D SingleTex (MultiSample Float) a) where
    tupleType v = Single $ toType v
instance (Typeable a) => GPU (Sampler Tex2D ArrayTex (MultiSample Float) a) where
    tupleType v = Single $ toType v
instance (Typeable a) => GPU (Sampler Tex1D SingleTex (Buffer Float) a) where
    tupleType v = Single $ toType v
instance (Typeable a) => GPU (Sampler TexRect SingleTex (Regular Float) a) where
    tupleType v = Single $ toType v

-- Int
instance (Typeable a) => GPU (Sampler Tex1D SingleTex (Regular Int) a) where
    tupleType v = Single $ toType v
instance (Typeable a) => GPU (Sampler Tex2D SingleTex (Regular Int) a) where
    tupleType v = Single $ toType v
instance (Typeable a) => GPU (Sampler Tex3D SingleTex (Regular Int) a) where
    tupleType v = Single $ toType v
instance (Typeable a) => GPU (Sampler Tex2D CubeTex (Regular Int) a) where
    tupleType v = Single $ toType v
instance (Typeable a) => GPU (Sampler Tex1D ArrayTex (Regular Int) a) where
    tupleType v = Single $ toType v
instance (Typeable a) => GPU (Sampler Tex2D ArrayTex (Regular Int) a) where
    tupleType v = Single $ toType v
instance (Typeable a) => GPU (Sampler Tex2D SingleTex (MultiSample Int) a) where
    tupleType v = Single $ toType v
instance (Typeable a) => GPU (Sampler Tex2D ArrayTex (MultiSample Int) a) where
    tupleType v = Single $ toType v
instance (Typeable a) => GPU (Sampler Tex1D SingleTex (Buffer Int) a) where
    tupleType v = Single $ toType v
instance (Typeable a) => GPU (Sampler TexRect SingleTex (Regular Int) a) where
    tupleType v = Single $ toType v

-- Word
instance (Typeable a) => GPU (Sampler Tex1D SingleTex (Regular Word) a) where
    tupleType v = Single $ toType v
instance (Typeable a) => GPU (Sampler Tex2D SingleTex (Regular Word) a) where
    tupleType v = Single $ toType v
instance (Typeable a) => GPU (Sampler Tex3D SingleTex (Regular Word) a) where
    tupleType v = Single $ toType v
instance (Typeable a) => GPU (Sampler Tex2D CubeTex (Regular Word) a) where
    tupleType v = Single $ toType v
instance (Typeable a) => GPU (Sampler Tex1D ArrayTex (Regular Word) a) where
    tupleType v = Single $ toType v
instance (Typeable a) => GPU (Sampler Tex2D ArrayTex (Regular Word) a) where
    tupleType v = Single $ toType v
instance (Typeable a) => GPU (Sampler Tex2D SingleTex (MultiSample Word) a) where
    tupleType v = Single $ toType v
instance (Typeable a) => GPU (Sampler Tex2D ArrayTex (MultiSample Word) a) where
    tupleType v = Single $ toType v
instance (Typeable a) => GPU (Sampler Tex1D SingleTex (Buffer Word) a) where
    tupleType v = Single $ toType v
instance (Typeable a) => GPU (Sampler TexRect SingleTex (Regular Word) a) where
    tupleType v = Single $ toType v

-- Shadow
instance (Typeable a) => GPU (Sampler Tex1D SingleTex (Shadow Float) a) where
    tupleType v = Single $ toType v
instance (Typeable a) => GPU (Sampler Tex2D SingleTex (Shadow Float) a) where
    tupleType v = Single $ toType v
instance (Typeable a) => GPU (Sampler Tex2D CubeTex (Shadow Float) a) where
    tupleType v = Single $ toType v
instance (Typeable a) => GPU (Sampler Tex1D ArrayTex (Shadow Float) a) where
    tupleType v = Single $ toType v
instance (Typeable a) => GPU (Sampler Tex2D ArrayTex (Shadow Float) a) where
    tupleType v = Single $ toType v
instance (Typeable a) => GPU (Sampler TexRect SingleTex (Shadow Float) a) where
    tupleType v = Single $ toType v

instance GPU () where
    tupleType v = Tuple []
instance GPU Bool where
    tupleType v = Single $ toType v
instance GPU Float where
    tupleType v = Single $ toType v
instance GPU Int32 where
    tupleType v = Single $ toType v
instance GPU Word32 where
    tupleType v = Single $ toType v
instance GPU V2B where
    tupleType v = Single $ toType v
instance GPU V2F where
    tupleType v = Single $ toType v
instance GPU V2I where
    tupleType v = Single $ toType v
instance GPU V2U where
    tupleType v = Single $ toType v
instance GPU V3B where
    tupleType v = Single $ toType v
instance GPU V3F where
    tupleType v = Single $ toType v
instance GPU V3I where
    tupleType v = Single $ toType v
instance GPU V3U where
    tupleType v = Single $ toType v
instance GPU V4B where
    tupleType v = Single $ toType v
instance GPU V4F where
    tupleType v = Single $ toType v
instance GPU V4I where
    tupleType v = Single $ toType v
instance GPU V4U where
    tupleType v = Single $ toType v
instance GPU M22F where
    tupleType v = Single $ toType v
instance GPU M23F where
    tupleType v = Single $ toType v
instance GPU M24F where
    tupleType v = Single $ toType v
instance GPU M32F where
    tupleType v = Single $ toType v
instance GPU M33F where
    tupleType v = Single $ toType v
instance GPU M34F where
    tupleType v = Single $ toType v
instance GPU M42F where
    tupleType v = Single $ toType v
instance GPU M43F where
    tupleType v = Single $ toType v
instance GPU M44F where
    tupleType v = Single $ toType v
instance (GPU a, GPU b) => GPU (a, b) where
    tupleType (v :: (a,b)) = Tuple
        [ tupleType (undefined :: a)
        , tupleType (undefined :: b)
        ]
instance (GPU a, GPU b, GPU c) => GPU (a, b, c) where
    tupleType (v :: (a,b,c)) = Tuple
        [ tupleType (undefined :: a)
        , tupleType (undefined :: b)
        , tupleType (undefined :: c)
        ]
instance (GPU a, GPU b, GPU c, GPU d) => GPU (a, b, c, d) where
    tupleType (v :: (a,b,c,d)) = Tuple
        [ tupleType (undefined :: a)
        , tupleType (undefined :: b)
        , tupleType (undefined :: c)
        , tupleType (undefined :: d)
        ]
instance (GPU a, GPU b, GPU c, GPU d, GPU e) => GPU (a, b, c, d, e) where
    tupleType (v :: (a,b,c,d,e)) = Tuple
        [ tupleType (undefined :: a)
        , tupleType (undefined :: b)
        , tupleType (undefined :: c)
        , tupleType (undefined :: d)
        , tupleType (undefined :: e)
        ]
instance (GPU a, GPU b, GPU c, GPU d, GPU e, GPU f) => GPU (a, b, c, d, e, f) where
    tupleType (v :: (a,b,c,d,e,f)) = Tuple
        [ tupleType (undefined :: a)
        , tupleType (undefined :: b)
        , tupleType (undefined :: c)
        , tupleType (undefined :: d)
        , tupleType (undefined :: e)
        , tupleType (undefined :: f)
        ]
instance (GPU a, GPU b, GPU c, GPU d, GPU e, GPU f, GPU g) => GPU (a, b, c, d, e, f, g) where
    tupleType (v :: (a,b,c,d,e,f,g)) = Tuple
        [ tupleType (undefined :: a)
        , tupleType (undefined :: b)
        , tupleType (undefined :: c)
        , tupleType (undefined :: d)
        , tupleType (undefined :: e)
        , tupleType (undefined :: f)
        , tupleType (undefined :: g)
        ]
instance (GPU a, GPU b, GPU c, GPU d, GPU e, GPU f, GPU g, GPU h) => GPU (a, b, c, d, e, f, g, h) where
    tupleType (v :: (a,b,c,d,e,f,g,h)) = Tuple
        [ tupleType (undefined :: a)
        , tupleType (undefined :: b)
        , tupleType (undefined :: c)
        , tupleType (undefined :: d)
        , tupleType (undefined :: e)
        , tupleType (undefined :: f)
        , tupleType (undefined :: g)
        , tupleType (undefined :: h)
        ]
instance (GPU a, GPU b, GPU c, GPU d, GPU e, GPU f, GPU g, GPU h, GPU i) => GPU (a, b, c, d, e, f, g, h, i) where
    tupleType (v :: (a,b,c,d,e,f,g,h,i)) = Tuple
        [ tupleType (undefined :: a)
        , tupleType (undefined :: b)
        , tupleType (undefined :: c)
        , tupleType (undefined :: d)
        , tupleType (undefined :: e)
        , tupleType (undefined :: f)
        , tupleType (undefined :: g)
        , tupleType (undefined :: h)
        , tupleType (undefined :: i)
        ]

-- stream type restriction, these types can be used in vertex shader input
class GPU a => SGPU a
instance SGPU Int32
instance SGPU Word32
instance SGPU Float
instance SGPU M22F
instance SGPU M23F
instance SGPU M24F
instance SGPU M32F
instance SGPU M33F
instance SGPU M34F
instance SGPU M42F
instance SGPU M43F
instance SGPU M44F
instance SGPU V2F
instance SGPU V3F
instance SGPU V4F
instance SGPU V2I
instance SGPU V3I
instance SGPU V4I
instance SGPU V2U
instance SGPU V3U
instance SGPU V4U
instance (SGPU a, SGPU b) => SGPU (a, b)
instance (SGPU a, SGPU b, SGPU c) => SGPU (a, b, c)
instance (SGPU a, SGPU b, SGPU c, SGPU d) => SGPU (a, b, c, d)
instance (SGPU a, SGPU b, SGPU c, SGPU d, SGPU e) => SGPU (a, b, c, d, e)
instance (SGPU a, SGPU b, SGPU c, SGPU d, SGPU e, SGPU f) => SGPU (a, b, c, d, e, f)
instance (SGPU a, SGPU b, SGPU c, SGPU d, SGPU e, SGPU f, SGPU g) => SGPU (a, b, c, d, e, f, g)
instance (SGPU a, SGPU b, SGPU c, SGPU d, SGPU e, SGPU f, SGPU g, SGPU h) => SGPU (a, b, c, d, e, f, g, h)
instance (SGPU a, SGPU b, SGPU c, SGPU d, SGPU e, SGPU f, SGPU g, SGPU h, SGPU i) => SGPU (a, b, c, d, e, f, g, h, i)

-- uniform type restriction
-- hint: EltRepr stands for Elementary Type Representation
type family EltRepr a :: *
type instance EltRepr (Sampler dim sh t ar) = ((), Sampler dim sh t ar)
type instance EltRepr () = ()
type instance EltRepr Int32 = ((), Int32)
type instance EltRepr Word32 = ((), Word32)
type instance EltRepr Float = ((), Float)
type instance EltRepr Bool = ((), Bool)
type instance EltRepr V2F = ((), V2F)
type instance EltRepr V2I = ((), V2I)
type instance EltRepr V2U = ((), V2U)
type instance EltRepr V2B = ((), V2B)
type instance EltRepr M22F = ((), M22F)
type instance EltRepr M23F = ((), M23F)
type instance EltRepr M24F = ((), M24F)
type instance EltRepr V3F = ((), V3F)
type instance EltRepr V3I = ((), V3I)
type instance EltRepr V3U = ((), V3U)
type instance EltRepr V3B = ((), V3B)
type instance EltRepr M32F = ((), M32F)
type instance EltRepr M33F = ((), M33F)
type instance EltRepr M34F = ((), M34F)
type instance EltRepr V4F = ((), V4F)
type instance EltRepr V4I = ((), V4I)
type instance EltRepr V4U = ((), V4U)
type instance EltRepr V4B = ((), V4B)
type instance EltRepr M42F = ((), M42F)
type instance EltRepr M43F = ((), M43F)
type instance EltRepr M44F = ((), M44F)
type instance EltRepr (a, b) = (EltRepr a, EltRepr' b)
type instance EltRepr (a, b, c) = (EltRepr (a, b), EltRepr' c)
type instance EltRepr (a, b, c, d) = (EltRepr (a, b, c), EltRepr' d)
type instance EltRepr (a, b, c, d, e) = (EltRepr (a, b, c, d), EltRepr' e)
type instance EltRepr (a, b, c, d, e, f) = (EltRepr (a, b, c, d, e), EltRepr' f)
type instance EltRepr (a, b, c, d, e, f, g) = (EltRepr (a, b, c, d, e, f), EltRepr' g)
type instance EltRepr (a, b, c, d, e, f, g, h) = (EltRepr (a, b, c, d, e, f, g), EltRepr' h)
type instance EltRepr (a, b, c, d, e, f, g, h, i) = (EltRepr (a, b, c, d, e, f, g, h), EltRepr' i)

type family EltRepr' a :: *
type instance EltRepr' (Sampler dim sh t ar) = Sampler dim sh t ar
type instance EltRepr' () = ()
type instance EltRepr' Int32 = Int32
type instance EltRepr' Word32 = Word32
type instance EltRepr' Float = Float
type instance EltRepr' Bool = Bool
type instance EltRepr' V2F = V2F
type instance EltRepr' V2I = V2I
type instance EltRepr' V2U = V2U
type instance EltRepr' V2B = V2B
type instance EltRepr' M22F = M22F
type instance EltRepr' M23F = M23F
type instance EltRepr' M24F = M24F
type instance EltRepr' V3F = V3F
type instance EltRepr' V3I = V3I
type instance EltRepr' V3U = V3U
type instance EltRepr' V3B = V3B
type instance EltRepr' M32F = M32F
type instance EltRepr' M33F = M33F
type instance EltRepr' M34F = M34F
type instance EltRepr' V4F = V4F
type instance EltRepr' V4I = V4I
type instance EltRepr' V4U = V4U
type instance EltRepr' V4B = V4B
type instance EltRepr' M42F = M42F
type instance EltRepr' M43F = M43F
type instance EltRepr' M44F = M44F
type instance EltRepr' (a, b) = (EltRepr a, EltRepr' b)
type instance EltRepr' (a, b, c) = (EltRepr (a, b), EltRepr' c)
type instance EltRepr' (a, b, c, d) = (EltRepr (a, b, c), EltRepr' d)
type instance EltRepr' (a, b, c, d, e) = (EltRepr (a, b, c, d), EltRepr' e)
type instance EltRepr' (a, b, c, d, e, f) = (EltRepr (a, b, c, d, e), EltRepr' f)
type instance EltRepr' (a, b, c, d, e, f, g) = (EltRepr (a, b, c, d, e, f), EltRepr' g)
type instance EltRepr' (a, b, c, d, e, f, g, h) = (EltRepr (a, b, c, d, e, f, g), EltRepr' h)
type instance EltRepr' (a, b, c, d, e, f, g, h, i) = (EltRepr (a, b, c, d, e, f, g, h), EltRepr' i)

-- |Conversion between surface n-tuples and our tuple representation.
--
-- our language uses nested tuple representation
class IsTuple tup where
    type TupleRepr tup

instance IsTuple () where
    type TupleRepr () = ()
            
instance IsTuple (a, b) where
    type TupleRepr (a, b) = (((), a), b)
            
instance IsTuple (a, b, c) where
    type TupleRepr (a, b, c)  = (TupleRepr (a, b), c)

instance IsTuple (a, b, c, d) where
    type TupleRepr (a, b, c, d)    = (TupleRepr (a, b, c), d)

instance IsTuple (a, b, c, d, e) where
    type TupleRepr (a, b, c, d, e)      = (TupleRepr (a, b, c, d), e)

instance IsTuple (a, b, c, d, e, f) where
    type TupleRepr (a, b, c, d, e, f)        = (TupleRepr (a, b, c, d, e), f)

instance IsTuple (a, b, c, d, e, f, g) where
    type TupleRepr (a, b, c, d, e, f, g)          = (TupleRepr (a, b, c, d, e, f), g)

instance IsTuple (a, b, c, d, e, f, g, h) where
    type TupleRepr (a, b, c, d, e, f, g, h)            = (TupleRepr (a, b, c, d, e, f, g), h)

instance IsTuple (a, b, c, d, e, f, g, h, i) where
    type TupleRepr (a, b, c, d, e, f, g, h, i)              = (TupleRepr (a, b, c, d, e, f, g, h), i)

-- Tuple representation
-- --------------------

-- |We represent tuples as heterogenous lists, typed by a type list.
--
data Tuple c t where
    NilTup  ::                     Tuple c ()
    SnocTup :: GPU t => Tuple c s -> c t -> Tuple c (s, t)

-- |Type-safe projection indicies for tuples.
--
-- NB: We index tuples by starting to count from the *right*!
--
data TupleIdx t e where
    ZeroTupIdx :: GPU s =>        TupleIdx (t, s) s
    SuccTupIdx :: TupleIdx t e -> TupleIdx (t, s) e

-- Auxiliary tuple index constants
--
tix0 :: GPU s => TupleIdx (t, s) s
tix0 = ZeroTupIdx
tix1 :: GPU s => TupleIdx ((t, s), s1) s
tix1 = SuccTupIdx tix0
tix2 :: GPU s => TupleIdx (((t, s), s1), s2) s
tix2 = SuccTupIdx tix1
tix3 :: GPU s => TupleIdx ((((t, s), s1), s2), s3) s
tix3 = SuccTupIdx tix2
tix4 :: GPU s => TupleIdx (((((t, s), s1), s2), s3), s4) s
tix4 = SuccTupIdx tix3
tix5 :: GPU s => TupleIdx ((((((t, s), s1), s2), s3), s4), s5) s
tix5 = SuccTupIdx tix4
tix6 :: GPU s => TupleIdx (((((((t, s), s1), s2), s3), s4), s5), s6) s
tix6 = SuccTupIdx tix5
tix7 :: GPU s => TupleIdx ((((((((t, s), s1), s2), s3), s4), s5), s6), s7) s
tix7 = SuccTupIdx tix6
tix8 :: GPU s => TupleIdx (((((((((t, s), s1), s2), s3), s4), s5), s6), s7), s8) s
tix8 = SuccTupIdx tix7

deriving instance Typeable (,,,,,,,)
deriving instance Typeable (,,,,,,,,)
