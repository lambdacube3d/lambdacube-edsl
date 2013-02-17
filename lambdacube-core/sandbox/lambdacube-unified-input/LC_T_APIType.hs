module LC_T_APIType where

import Data.ByteString.Char8
import Data.Int
import Data.Typeable
import Data.Word

import TypeLevel.Number.Nat
import TypeLevel.Number.Nat.Num

import LC_G_LinearAlgebraTypes
import LC_G_APIType
import LC_U_APIType (FrequencyType, ExpType)

-- natural number helper
type N10 = O (I (O (I Z)))
type N11 = I (I (O (I Z)))
type N12 = O (O (I (I Z)))
type N13 = I (O (I (I Z)))
type N14 = O (I (I (I Z)))
type N15 = I (I (I (I Z)))

-- internal tuple representation

-- means unit
data ZZ

-- used for tuple type description
infixr 1 :+:
data tail :+: head = !tail :+: !head deriving Show

-- used for tuple value description
infixr 1 :.
data Tuple c a t where
    ZT      :: Tuple c a ZZ

    (:.)    :: c t
            => a t
            -> Tuple c a t'
            -> Tuple c a (t :+: t')

type family PrjTup idx t
type instance PrjTup N1  (e :+: l) = e
type instance PrjTup N2  (e :+: l) = PrjTup N1 l
type instance PrjTup N3  (e :+: l) = PrjTup N2 l
type instance PrjTup N4  (e :+: l) = PrjTup N3 l
type instance PrjTup N5  (e :+: l) = PrjTup N4 l
type instance PrjTup N6  (e :+: l) = PrjTup N5 l
type instance PrjTup N7  (e :+: l) = PrjTup N6 l
type instance PrjTup N8  (e :+: l) = PrjTup N7 l
type instance PrjTup N9  (e :+: l) = PrjTup N8 l
type instance PrjTup N10 (e :+: l) = PrjTup N9 l
type instance PrjTup N11 (e :+: l) = PrjTup N10 l
type instance PrjTup N12 (e :+: l) = PrjTup N11 l
type instance PrjTup N13 (e :+: l) = PrjTup N12 l
type instance PrjTup N14 (e :+: l) = PrjTup N13 l
type instance PrjTup N15 (e :+: l) = PrjTup N14 l

class IsFloatTuple a
instance IsFloatTuple ZZ
instance IsFloatTuple l => IsFloatTuple (Float :+: l)

-- all LC supported types including all types of every computation frequency (Obj,V,G,F)
class Freq freq where
    reifyFreq :: freq -> FrequencyType

class Freq freq => LCType freq a where
    reifyType :: freq -> a -> ExpType

{-
-- types supported on V,G,F
class GPUType a

-- types supported as vertex attribute
class AttributeType a
-}

{-
User Input New Feature:
    - support tuples
    - support arrays
    - support textures/samplers
-}
data Ordered
data Unordered

data Array orderType a

data OrderingType a where
    Ordered     :: OrderingType Ordered
    Unordered   :: OrderingType Unordered

data InputType freq a where
    Bool'       :: InputType freq Bool
    V2B'        :: InputType freq V2B
    V3B'        :: InputType freq V3B
    V4B'        :: InputType freq V4B
    Word'       :: InputType freq Word32
    V2U'        :: InputType freq V2U
    V3U'        :: InputType freq V3U
    V4U'        :: InputType freq V4U
    Int'        :: InputType freq Int32
    V2I'        :: InputType freq V2I
    V3I'        :: InputType freq V3I
    V4I'        :: InputType freq V4I
    Float'      :: InputType freq Float
    V2F'        :: InputType freq V2F
    V3F'        :: InputType freq V3F
    V4F'        :: InputType freq V4F
    M22F'       :: InputType freq M22F
    M23F'       :: InputType freq M23F
    M24F'       :: InputType freq M24F
    M32F'       :: InputType freq M32F
    M33F'       :: InputType freq M33F
    M34F'       :: InputType freq M34F
    M42F'       :: InputType freq M42F
    M43F'       :: InputType freq M43F
    M44F'       :: InputType freq M44F

    Tuple'      :: Tuple (LCType freq) (InputType freq) t
                -> InputType freq t

    Array'      :: OrderingType ordering
                -> InputType freq t
                -> InputType freq (Array ordering t)

    Texture'    :: IsValidTextureSlot t
                => TextureType dim mip arr layerCount t ar
                -> InputType freq (Texture dim arr t ar)

data InputValue freq a where
    Bool    :: Bool     -> InputValue freq Bool
    V2B     :: V2B      -> InputValue freq V2B
    V3B     :: V3B      -> InputValue freq V3B
    V4B     :: V4B      -> InputValue freq V4B
    Word    :: Word     -> InputValue freq Word32
    V2U     :: V2U      -> InputValue freq V2U
    V3U     :: V3U      -> InputValue freq V3U
    V4U     :: V4U      -> InputValue freq V4U
    Int     :: Int      -> InputValue freq Int32
    V2I     :: V2I      -> InputValue freq V2I
    V3I     :: V3I      -> InputValue freq V3I
    V4I     :: V4I      -> InputValue freq V4I
    Float   :: Float    -> InputValue freq Float
    V2F     :: V2F      -> InputValue freq V2F
    V3F     :: V3F      -> InputValue freq V3F
    V4F     :: V4F      -> InputValue freq V4F
    M22F    :: M22F     -> InputValue freq M22F
    M23F    :: M23F     -> InputValue freq M23F
    M24F    :: M24F     -> InputValue freq M24F
    M32F    :: M32F     -> InputValue freq M32F
    M33F    :: M33F     -> InputValue freq M33F
    M34F    :: M34F     -> InputValue freq M34F
    M42F    :: M42F     -> InputValue freq M42F
    M43F    :: M43F     -> InputValue freq M43F
    M44F    :: M44F     -> InputValue freq M44F

    Tuple   :: Tuple (LCType freq) (InputValue freq) t
            -> InputValue freq t

    Array   :: OrderingType ordering
            -> [InputValue freq t]
            -> InputValue freq (Array ordering t)

    TextureSetting  :: TextureType dim mip arr layerCount t ar
                    -> InputValue freq (TexSizeRepr dim)
                    -> MipMap mip
                    -> InputValue freq (TextureSetting dim arr layerCount t ar)

    SamplerSetting  :: Filter
                    -> EdgeMode
                    -> InputValue freq SamplerSetting

-- primitive types
data Adjacency
data NoAdjacency

data Triangle
data Line
data Point

data FetchPrimitive primitive adjacency where
    Points                  :: FetchPrimitive Point     NoAdjacency
    LineStrip               :: FetchPrimitive Line      NoAdjacency
    LineLoop                :: FetchPrimitive Line      NoAdjacency
    Lines                   :: FetchPrimitive Line      NoAdjacency
    TriangleStrip           :: FetchPrimitive Triangle  NoAdjacency
    TriangleFan             :: FetchPrimitive Triangle  NoAdjacency
    Triangles               :: FetchPrimitive Triangle  NoAdjacency
    LinesAdjacency          :: FetchPrimitive Line      Adjacency
    LineStripAdjacency      :: FetchPrimitive Line      Adjacency
    TrianglesAdjacency      :: FetchPrimitive Triangle  Adjacency
    TriangleStripAdjacency  :: FetchPrimitive Triangle  Adjacency

data OutputPrimitive primitive where
    TrianglesOutput :: OutputPrimitive Triangle
    LinesOutput     :: OutputPrimitive Line
    PointsOutput    :: OutputPrimitive Point

-- describe geometry shader input 
type family PrimitiveVertices primitive adajcency a
type instance PrimitiveVertices Point NoAdjacency a     = a
type instance PrimitiveVertices Line NoAdjacency a      = a:+:a:+:ZZ
type instance PrimitiveVertices Line Adjacency a        = a:+:a:+:a:+:a:+:ZZ
type instance PrimitiveVertices Triangle NoAdjacency a  = a:+:a:+:a:+:ZZ
type instance PrimitiveVertices Triangle Adjacency a    = a:+:a:+:a:+:a:+:a:+:a:+:ZZ

data True
data False
-- depth generation
data DepthOutput hasDepth t where
    NoDepth     :: DepthOutput False a
    RastDepth   :: DepthOutput True a
    Depth       :: a
                -> DepthOutput True a

-- abstract types, used in language AST
data VertexStream primitive adjacency t
data PrimitiveStream primitive adjacency clipDistances layerCount freq t
data FragmentStream layerCount t

-- framebuffer data / fragment output semantic
data Color a    deriving Typeable
data Depth a    deriving Typeable
data Stencil a  deriving Typeable

data RasterContext t

-- restriction for framebuffer structure according content semantic
-- supported configurations: optional stencil + optional depth + [zero or more color]
class IsColorOutput a
instance IsColorOutput ZZ
instance (IsColorOutput b) => IsColorOutput (Color c :+: b)

class IsValidOutput a
instance (IsColorOutput a) => IsValidOutput (Color c :+: a)
instance (IsColorOutput a) => IsValidOutput (Depth d :+: a)
instance (IsColorOutput a) => IsValidOutput (Stencil s :+: a)
instance (IsColorOutput a) => IsValidOutput (Stencil s :+: Depth d :+: a)

-- helper type level function, used in language AST
type family FilterColor a
type instance FilterColor ZZ = ZZ
type instance FilterColor (Stencil a :+: b) = FilterColor b
type instance FilterColor (Depth a :+: b)   = FilterColor b
type instance FilterColor (Color a :+: b)   = a :+: (FilterColor b)

-- sampler and texture specification

data Mip
data NoMip

data MipMap t where
    NoMip   :: MipMap NoMip

    Mip     :: Int  -- base level
            -> Int  -- max level
            -> MipMap Mip

    AutoMip :: Int  -- base level
            -> Int  -- max level
            -> MipMap Mip

-- helper type level function, used in language AST
type family TexDataRepr arity t
type instance TexDataRepr Red  (v a) = a
type instance TexDataRepr RG   (v a) = V2 a
type instance TexDataRepr RGB  (v a) = V3 a
type instance TexDataRepr RGBA (v a) = V4 a

-- describes texel (texture component) type
data TextureDataType t arity where
    FloatTexel  :: ColorArity a
                -> TextureDataType (Regular Float) a

    IntTexel    :: ColorArity a
                -> TextureDataType (Regular Int) a

    WordTexel   :: ColorArity a
                -> TextureDataType (Regular Word) a

    ShadowTexel :: TextureDataType (Shadow Float) Red   -- TODO: add params required by shadow textures


-- helper type level function for texture specification
-- tells whether a texture is a single or an array texture
type family TexArrRepr a
type instance TexArrRepr N1 = SingleTex
type instance TexArrRepr (Greater t N1 => t) = ArrayTex

-- supported texture component arities

data ColorArity a where
    Red     :: ColorArity Red
    RG      :: ColorArity RG
    RGB     :: ColorArity RGB
    RGBA    :: ColorArity RGBA

-- component arity specification (Red,RG,RGB,RGBA)
--          hint: there is an interference with Shadow component format
--                  alternatives:
--                      A: move Shadow from TextureDataType to TextureType, this will introduce some new TextureType constructors (1D,2D,Cube,Rect)
--                      B: restrict ColorArity for Shadow
--                      C: add color arity definition to TextureDataType, this will solve the problem (best solution)

-- fully describes a texture type
data TextureType dim mip arr layerCount t ar where -- hint: arr - single or array texture, ar - arity (Red,RG,RGB,..)
    Texture1D       :: Nat layerCount
                    => TextureDataType t ar
                    -> layerCount
                    -> TextureType DIM1 Mip (TexArrRepr layerCount) layerCount t ar

    Texture2D       :: Nat layerCount
                    => TextureDataType t ar
                    -> layerCount
                    -> TextureType DIM2 Mip (TexArrRepr layerCount) layerCount t ar

    Texture3D       :: TextureDataType (Regular t) ar
                    -> TextureType DIM3 Mip SingleTex N1 (Regular t) ar

    TextureCube     :: TextureDataType t ar
                    -> TextureType DIM2 Mip CubeTex N1 t ar

    TextureRect     :: TextureDataType t ar
                    -> TextureType Rect NoMip SingleTex N1 t ar

    Texture2DMS     :: Nat layerCount
                    => TextureDataType (Regular t) ar
                    -> layerCount
                    -> TextureType DIM2 NoMip (TexArrRepr layerCount) layerCount (MultiSample t) ar

    TextureBuffer   :: TextureDataType (Regular t) ar
                    -> TextureType DIM1 NoMip SingleTex N1 (Buffer t) ar
{-
-- MipMap validation
class IsMipValid canMip mip
instance IsMipValid Mip Mip
instance IsMipValid Mip NoMip
instance IsMipValid NoMip NoMip
-}
-- restriction for texture types what can be specified as texture slots, e.g. multisample textures cannot be created in this way
class IsValidTextureSlot a
instance IsValidTextureSlot (Regular a)
instance IsValidTextureSlot (Shadow a)
instance IsValidTextureSlot (Buffer a)

-- type level hepler function, used for texture specification
type family TexSizeRepr a
type instance TexSizeRepr (DIM1) = Word32
type instance TexSizeRepr (DIM2) = V2U
type instance TexSizeRepr (Rect) = V2U
type instance TexSizeRepr (DIM3) = V3U
{-
-- type level hepler function, used for texture specification
type family TexRepr dim mip (gp :: * -> *) layerCount t :: *
type instance TexRepr DIM1 NoMip   gp layerCount t = gp (Image layerCount t)
type instance TexRepr DIM1 AutoMip gp layerCount t = gp (Image layerCount t)
type instance TexRepr DIM1 Mip     gp layerCount t = [gp (Image layerCount t)]

type instance TexRepr DIM2 NoMip   gp layerCount t = gp (Image layerCount t)
type instance TexRepr DIM2 AutoMip gp layerCount t = gp (Image layerCount t)
type instance TexRepr DIM2 Mip     gp layerCount t = [gp (Image layerCount t)]

type instance TexRepr DIM3 NoMip   gp layerCount t = [gp (Image layerCount t)]
type instance TexRepr DIM3 AutoMip gp layerCount t = [gp (Image layerCount t)]
type instance TexRepr DIM3 Mip     gp layerCount t = [[gp (Image layerCount t)]] -- 3D layers contain mipmap
-}

data TextureSetting dim arr layerCount t ar
data SamplerSetting
data Texture dim arr t ar

data Vertex clipDistances t
data GeometryShader inputPrimitive inputAdjacency outputPrimitive inputClipDistances outputClipDistances layerCount a b
data Output
data OcclusionQuery

type family ToOcclusionQuery a
type instance ToOcclusionQuery True     = OcclusionQuery
type instance ToOcclusionQuery False    = ZZ

-- shader freq tags: vertex, geometry, fragment
-- used in language AST, for primfun restriction and in shader codegen
data C
data Obj
data V
data G
data F

-- type sets
data Rect

data Red
data RG
data RGB
data RGBA

data Regular a
data Shadow a
data MultiSample a
data Buffer a

data SingleTex  -- singleton texture
data ArrayTex   -- array texture
data CubeTex    -- cube texture = array with size 6

data Sampler dim layerCount t ar
data Image layerCount t
