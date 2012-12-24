module LC_T_HOAS where

import Data.ByteString.Char8
import Data.Typeable
import Data.Int
import Data.Word

import TypeLevel.Number.Nat
import TypeLevel.Number.Nat.Num

import LC_G_Type
import LC_G_APIType
import LC_T_APIType
import LC_T_DSLType
import LC_T_PrimFun

-- all LC supported types including all types of every computation frequency (Obj,V,G,F)
class LCType a

-- types supported on V,G,F
class GPUType a

{-
User Input New Feature:
    - support tuples
    - support arrays
    - support textures/samplers
-}
data InputType a where
    Bool'       :: InputType Bool
    V2B'        :: InputType V2B
    V3B'        :: InputType V3B
    V4B'        :: InputType V4B
    Word'       :: InputType Word32
    V2U'        :: InputType V2U
    V3U'        :: InputType V3U
    V4U'        :: InputType V4U
    Int'        :: InputType Int32
    V2I'        :: InputType V2I
    V3I'        :: InputType V3I
    V4I'        :: InputType V4I
    Float'      :: InputType Float
    V2F'        :: InputType V2F
    V3F'        :: InputType V3F
    V4F'        :: InputType V4F
    M22F'       :: InputType M22F
    M23F'       :: InputType M23F
    M24F'       :: InputType M24F
    M32F'       :: InputType M32F
    M33F'       :: InputType M33F
    M34F'       :: InputType M34F
    M42F'       :: InputType M42F
    M43F'       :: InputType M43F
    M44F'       :: InputType M44F

    Tuple'      :: Tuple Typeable InputType t
                -> InputType t -- TODO: accept only at least two long flat tuples

    Array'      :: ordering
                -> InputType t
                -> InputType (Array ordering t)

    Texture'    :: TextureType dim mip arr layerCount t ar
                -> InputType (Texture dim arr t ar)

    deriving Typeable

data InputValue a where
    Bool    :: Bool     -> InputValue Bool
    V2B     :: V2B      -> InputValue V2B
    V3B     :: V3B      -> InputValue V3B
    V4B     :: V4B      -> InputValue V4B
    Word    :: Word     -> InputValue Word32
    V2U     :: V2U      -> InputValue V2U
    V3U     :: V3U      -> InputValue V3U
    V4U     :: V4U      -> InputValue V4U
    Int     :: Int      -> InputValue Int32
    V2I     :: V2I      -> InputValue V2I
    V3I     :: V3I      -> InputValue V3I
    V4I     :: V4I      -> InputValue V4I
    Float   :: Float    -> InputValue Float
    V2F     :: V2F      -> InputValue V2F
    V3F     :: V3F      -> InputValue V3F
    V4F     :: V4F      -> InputValue V4F
    M22F    :: M22F     -> InputValue M22F
    M23F    :: M23F     -> InputValue M23F
    M24F    :: M24F     -> InputValue M24F
    M32F    :: M32F     -> InputValue M32F
    M33F    :: M33F     -> InputValue M33F
    M34F    :: M34F     -> InputValue M34F
    M42F    :: M42F     -> InputValue M42F
    M43F    :: M43F     -> InputValue M43F
    M44F    :: M44F     -> InputValue M44F

    Tuple   :: Tuple Typeable InputValue t
            -> InputValue t -- TODO: accept only at least two long flat tuples

    Array   :: ordering
            -> [InputValue t]
            -> InputValue (Array ordering t)

    TextureSetting  :: TextureType dim mip arr layerCount t ar
                    -> InputValue (TexSizeRepr dim)
                    -> MipMap mip
                    -> InputValue (TextureSetting dim arr layerCount t ar)

    SamplerSetting  :: Filter
                    -> EdgeMode
                    -> InputValue SamplerSetting

type N10 = O (I (O (I Z)))
type N11 = I (I (O (I Z)))
type N12 = O (O (I (I Z)))
type N13 = I (O (I (I Z)))
type N14 = O (I (I (I Z)))
type N15 = I (I (I (I Z)))

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

--TODO: check whether we should distinct size limited arrays and arbitrary sized arrays.
--          former is due to GLSL and GPU restrictions, latter are stored in CPU RAM

-- Common Exp, describes shader functions
data Exp freq t where
    -- Needed for conversion to de Bruijn form
    Tag     :: LCType t
            => Int
            -> TypeRep
            -> Exp freq t
                 -- environment size at defining occurrence

    -- constant value
    -- TODO: support constants for all LCTypes
    Const   :: LCType t
            => InputValue t
            -> Exp freq t

    -- User input constant variable
    Var     :: LCType t
            => ByteString
            -> InputType t
            -> Exp Obj t

    -- Lift Obj expressions to higher frequencies
    Use     :: Exp Obj t
            -> Exp freq t

    -- conditional expression
    Cond    :: LCType t
            => Exp freq Bool
            -> Exp freq t
            -> Exp freq t
            -> Exp freq t

    PrimApp :: (LCType a, LCType r)
            => PrimFun freq (a -> r)
            -> Exp freq a
            -> Exp freq r

    Tup     :: Tuple LCType (Exp freq) t
            -> Exp freq t

    Prj     :: (Nat idx)
            => idx
            -> Exp freq t
            -> Exp freq (PrjTup idx t)

    -- loop support
    Loop    :: (LCType s, LCType a)
            => (Exp freq s -> Exp freq s)     -- state transform function
            -> (Exp freq s -> Exp freq Bool)  -- loop condition function
            -> (Exp freq s -> Exp freq a)     -- state to result transform function
            -> Exp freq s                      -- initial state
            -> Exp freq a                      -- result

    -- Array operations
    -- Construction
    ArrayFromList   :: [Exp Obj a]
                    -> Exp Obj (Array order a)

    ArrayReplicate  :: Exp Obj Int32
                    -> Exp Obj a
                    -> Exp Obj (Array order a)

    ArrayGenerate   :: Exp Obj Int32
                    -> (Exp Obj Int32 -> Exp Obj a)
                    -> Exp Obj (Array order a)

    ArrayIterateN   :: Exp Obj Int32
                    -> (Exp Obj a -> Exp Obj a)
                    -> Exp Obj a
                    -> Exp Obj (Array order a)
    -- Elementwise operations
    ArrayIndex      :: Exp Obj Int32
                    -> Exp Obj (Array order a)
                    -> Exp Obj a

    ArrayFilter     :: (Exp freq a -> Exp freq Bool)
                    -> Exp freq (Array order a)
                    -> Exp freq (Array order a)

    ArrayMap        :: (Exp freq a -> Exp freq b)
                    -> Exp freq (Array order a)
                    -> Exp freq (Array order b)

    ArrayZipWith    :: (Exp freq a -> Exp freq b -> Exp freq c)
                    -> Exp freq (Array order a)
                    -> Exp freq (Array order b)
                    -> Exp freq (Array order c)

    ArrayAccumulate :: (Exp freq a -> Exp freq b -> Exp freq a)
                    -> Exp freq a
                    -> Exp freq (Array order b)
                    -> Exp freq a

    -- Graphics pipeline extensibility
    -- dynamic extension support
    AccumulateSet   :: GPU a
                    => ByteString
                    -> Exp Obj (FrameBuffer layerCount a)
                    -> Exp Obj (FrameBuffer layerCount a)

    -- GPU pipeline model
    Fetch           :: SGPU a
                    => Primitive prim
                    -> Exp Obj (Array order a)
                    -> Maybe (Exp Obj (Array order Int32))
                    -> Exp Obj (VertexStream prim a)

    Transform       :: (GPU a, GPU b)
                    => (Exp V a -> VertexOut b)                       -- vertex shader
                    -> Exp Obj (VertexStream prim a)
                    -> Exp Obj (PrimitiveStream prim N1 V b)

    Reassemble      :: GeometryShader primIn primOut layerCount a b
                    -> Exp Obj (PrimitiveStream primIn N1 V a)
                    -> Exp Obj (PrimitiveStream primOut layerCount G b)

    Rasterize       :: RasterContext prim
                    -> Exp Obj (PrimitiveStream prim layerCount freq a)
                    -> Exp Obj (FragmentStream layerCount a)

    FrameBuffer     :: FrameBuffer layerCount t
                    -> Exp Obj (FrameBuffer layerCount (t))--TODO ftrepr'

    Accumulate      :: (GPU a, GPU b, IsValidOutput b)    -- restriction: depth and stencil optional, arbitrary color component
                    => AccumulationContext b
                    -> FragmentFilter a
                    -> (Exp F a -> FragmentOut (NoStencilRepr b))     -- fragment shader
                    -> Exp Obj (FragmentStream layerCount a)
                    -> Exp Obj (FrameBuffer layerCount (b))--TODO ftrepr'
                    -> Exp Obj (FrameBuffer layerCount (b))--TODO ftrepr'

    -- Transform feedback support
    ArrayFromStream :: Exp Obj (PrimitiveStream prim layerCount freq a)
                    -> Exp Obj (Array order a)

    -- FrameBuffer and Image helpers
    PrjFrameBuffer  :: Nat idx
                    => idx
                    -> Exp Obj (FrameBuffer layerCount b)
                    -> Exp Obj (Image layerCount (PrjTup idx t))

    PrjImage        :: (Nat idx, LesserEq idx layerCount)
                    => idx
                    -> Exp Obj (Image layerCount t)
                    -> Exp Obj (Image N1 t)



type InterpolatedExpTuple freq a = Tuple GPU (Interpolated (Exp freq)) a
type ExpTuple freq a = Tuple GPU (Exp freq) a

-- Vertex
{-
    Vertex shader builtin output:
            gl_PerVertex {
                vec4  gl_Position
                float gl_PointSize
                float gl_ClipDistance[]
            }
-}
-- TODO: add support for gl_ClipDistance setup
-- result of a vertex shader function
data VertexOut t where
    VertexOut   :: Exp V V4F      -- position
                -> Exp V Float    -- point size
                -> InterpolatedExpTuple V a
                -> VertexOut a

-- Geometry
-- describes a geometry shader
data GeometryShader primIn primOut layerNum a b where
    GeometryShader      :: (GPU (PrimitiveVertices primIn a), GPU i, GPU j, GPU b, Nat layerNum)
                        => layerNum                                                 -- geometry shader:
                        -> Primitive primOut                                        -- output primitive
                        -> Int                                                      -- max amount of generated vertices
                        -> (Exp G (PrimitiveVertices primIn a) -> Exp G (i,Int32))  -- how many primitives?
                        -> (Exp G i -> Exp G (i,j,Int32))                           -- how many vertices?
                        -> (Exp G j -> GeometryOut (j,b))                           -- generate vertices
                        -> GeometryShader primIn primOut layerNum a b

{-
    Geometry shader builtin output:
            gl_PerVertex {
                vec4  gl_Position
                float gl_PointSize
                float gl_ClipDistance[]
            }
            int gl_PrimitiveID
            int gl_Layer
-}
-- result of a geometry shader function
data GeometryOut t where
    GeometryOut :: Exp G V4F      -- position
                -> Exp G Float    -- point size
                -> Exp G Int32    -- primitive ID
                -> Exp G Int32    -- layer
                -> Exp G j
                -> InterpolatedExpTuple G a
                -> GeometryOut (j,a)

-- Fragment
{-
    Fragment shader builtin output:
            float gl_FragDepth  -- Optional
-}
-- result of a fragment shader function
data FragmentOut t where
    FragmentOut             :: ExpTuple F a
                            -> FragmentOut (ColorRepr a)

    FragmentOutDepth        :: Exp F Float
                            -> ExpTuple F a
                            -> FragmentOut (Depth Float :+: ColorRepr a)

    FragmentOutRastDepth    :: ExpTuple F a
                            -> FragmentOut (Depth Float :+: ColorRepr a)

-- fragment filter function, we express discard using a filter function
data FragmentFilter a where
    PassAll :: FragmentFilter a

    Filter  :: (Exp F a -> Exp F Bool)
            -> FragmentFilter a


data GPOutput where
    ImageOut    :: ByteString
                -> Exp Obj (Image layerCount t)
                -> GPOutput

    ScreenOut   :: Exp Obj (Image N1 t)
                -> GPOutput
