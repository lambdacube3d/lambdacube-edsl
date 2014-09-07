module LCDSL where

import Data.Word

import TypeLevel.Number.Nat
import TypeLevel.Number.Nat.Num

import LCDSLLinAlg
import LCDSLTexture
import LCDSLContext

-- recursive tuple definition
infixl 1 :.
data head :. tail = head :. tail

infixl 9 :@
data t :@ freq

type family Freq t :: *
type instance Freq (a :@ f) = f
type instance Freq (a :@ f :. b :@ f) = f
type instance Freq (a :@ f :. b :@ f :. c) = Freq (b :@ f :. c)

-- vertex attribute interpolation
data Interpolated a

flat        :: a
            -> Interpolated a

smooth      :: IsFloating a
            => a
            -> Interpolated a

linear      :: IsFloating a
            => a
            -> Interpolated a

-- check that all type components are interpolated
type family CheckInterpolated t :: *
type instance CheckInterpolated ()                      = OK
type instance CheckInterpolated (Interpolated a)        = OK
type instance CheckInterpolated (Interpolated a :. b)   = CheckInterpolated b

-- removes interpolation tag
type family Untag t :: *
type instance Untag ()                      = ()
type instance Untag (Interpolated a)        = a
type instance Untag (Interpolated a :. b)   = a :. (Untag b)

-- shader stage tags: vertex, geometry, fragment
-- used in language AST, for primfun restriction and in shader codegen

--data C
data V
data G
--data F

data VertexOut t
data GeometryOut t
data FragmentOut t

-- abstract types, used in language AST
data VertexStream prim t
data PrimitiveStream prim layerCount stage t
data FragmentStream layerCount t

data GeometryShader primIn primOut layerNum a b where
    GeometryShader      :: (IsPrimitive primIn, IsPrimitive primOut, Nat layerNum)
                        => layerNum                                                 -- geometry shader:
                        -> primOut                                                  -- output primitive
                        -> Int                                                      -- max amount of generated vertices
                        -> ((PrimitiveVertices primIn a) -> (i,Int))    -- how many primitives?
                        -> (i -> (i,j,Int))                             -- how many vertices?
                        -> (j -> GeometryOut (j,b))                     -- generate vertices
                        -> GeometryShader primIn primOut layerNum a b

vertexOut               :: ( CheckInterpolated a ~ OK
                           , Untag a ~ t
                           )
                        => V4F      -- position
                        -> Float    -- point size
                        -> a        -- output vertex
                        -> VertexOut t

geometryOut             :: ( CheckInterpolated a ~ OK
                           , Untag a ~ t
                           )
                        => V4F      -- position
                        -> Float    -- point size
                        -> Int      -- primitive ID
                        -> Int      -- layer
                        -> j        -- new loop state
                        -> a        -- output vertex
                        -> GeometryOut (j,t)

data NoDepth

fragmentOut             :: a
                        -> FragmentOut (NoDepth,a)

fragmentOutDepth        :: Float
                        -> a
                        -> FragmentOut (Depth Float, a)

fragmentOutRastDepth    :: a
                        -> FragmentOut (Depth Float, a)

data FragmentFilter a where
    PassAll :: FragmentFilter a

    Filter  :: (a -> Bool)
            -> FragmentFilter a

sampler             :: Filter
                    -> EdgeMode
                    -> Texture dim arr t ar
                    -> Sampler dim arr t ar

input               :: t

transform           :: (a -> VertexOut b)                       -- vertex shader
                    -> VertexStream prim a
                    -> PrimitiveStream prim N1 V b

reassemble          :: GeometryShader primIn primOut layerCount a b
                    -> PrimitiveStream primIn N1 V a
                    -> PrimitiveStream primOut layerCount G b

rasterize           :: RasterContext prim
                    -> PrimitiveStream prim layerCount stage a
                    -> FragmentStream layerCount a
{-
    CONSTRAINT
        - framebuffer is a tuple from images with same size and layer count
        - frament output type must match the framebuffer content type
        - accumulation content must be valid on framebuffer content
        - only one stencil and depth content can be specified
-}

accumulate          :: ( CheckImages layerCount images ~ OK
                       , CheckOperationMatch operations images ~ OK
                       , CheckOutputMatch operations b ~ OK
                       , CheckSemantic (ToSemantic operations) ~ OK
                       )
                    => AccumulationContext operations
                    -> FragmentFilter a
                    -> (a -> FragmentOut b)     -- fragment shader
                    -> FragmentStream layerCount a
                    -> images
                    -> images

-- framebuffer is a tuple from images with same size and layer count
type family CheckImages layerCount a :: *
type instance CheckImages layerCount (Image layerCount a)       = OK
type instance CheckImages layerCount (Image layerCount a :. b)  = CheckImages layerCount b

-- accumulation content must be valid on framebuffer content
type family CheckOperationMatch operations images :: *
type instance CheckOperationMatch (FragmentOperation (s a)) (Image l a)                 = OK
type instance CheckOperationMatch (FragmentOperation (s a) :. op) (Image l a :. img)    = CheckOperationMatch op img

-- frament output type must match the framebuffer content type
type family CheckOutputMatch operations output :: *
type instance CheckOutputMatch (FragmentOperation (Color a)) (NoDepth,a)                                        = OK
type instance CheckOutputMatch (FragmentOperation (Depth a)) (Depth a,())                                       = OK
type instance CheckOutputMatch (FragmentOperation (Stencil a)) (NoDepth,())                                     = OK
type instance CheckOutputMatch (FragmentOperation (Color a) :. FragmentOperation (Stencil b)) (NoDepth,a)       = OK
type instance CheckOutputMatch (FragmentOperation (Color a) :. FragmentOperation (Color b)) (NoDepth,a :. b)    = OK
type instance CheckOutputMatch (FragmentOperation (Color a) :. FragmentOperation (Depth b)) (Depth b,a)         = OK
type instance CheckOutputMatch (FragmentOperation (Depth a) :. FragmentOperation (Stencil b)) (Depth a,())      = OK
type instance CheckOutputMatch (FragmentOperation (Depth a) :. FragmentOperation (Color b)) (Depth a,b)         = OK
type instance CheckOutputMatch (FragmentOperation (Color a) :. op1 :. op2 ) (d,a :. out)                        = CheckOutputMatch (op1 :. op2) (d, out)
type instance CheckOutputMatch (FragmentOperation (Depth a) :. op1 :. op2 ) (Depth a, out)                      = CheckOutputMatch (op1 :. op2) (NoDepth, out)
type instance CheckOutputMatch (FragmentOperation (Stencil a) :. op) b                                          = CheckOutputMatch op b

-- only one stencil and depth content can be specified
type family ToSemantic operations :: *
type instance ToSemantic (FragmentOperation (sem a))        = sem ()
type instance ToSemantic (FragmentOperation (sem a) :. op)  = sem () :. ToSemantic op

type family CheckSemantic sem :: *
type instance CheckSemantic (Color ())          = OK
type instance CheckSemantic (Depth ())          = OK
type instance CheckSemantic ((Color ()) :. a)   = CheckSemantic a
type instance CheckSemantic ((Depth ()) :. a)   = CheckCS a
type instance CheckSemantic ((Stencil ()) :. a) = CheckCD a

type family CheckCD sem :: *
type instance CheckCD (Color ())        = OK
type instance CheckCD (Depth ())        = OK
type instance CheckCD ((Color ()) :. a) = CheckCD a
type instance CheckCD ((Depth ()) :. a) = CheckC a

type family CheckCS sem :: *
type instance CheckCS (Color ())            = OK
type instance CheckCS (Stencil ())          = OK
type instance CheckCS ((Color ()) :. a)     = CheckCS a
type instance CheckCS ((Stencil ()) :. a)   = CheckC a

type family CheckC sem :: *
type instance CheckC (Color ())         = OK
type instance CheckC ((Color ()) :. a)  = CheckC a


accumulateSet       :: images
                    -> images

data Output

imageOut            :: Image layerCount t
                    -> Output

screenOut           :: Image N1 t
                    -> Output

data Array a

arrayMap            :: (a -> b)
                    -> Array a
                    -> Array b

arrayZipWith        :: (a -> b -> c)
                    -> Array a
                    -> Array b
                    -> Array c

arrayAccumulate     :: (a -> b -> a)
                    -> a
                    -> Array b
                    -> a

data OK

{-

TODO:
    restrict lambda abstraction only for GPU types (e.g. context must be constant)

-}
primop :: a
primop = undefined

flat                    = primop
smooth                  = primop
linear                  = primop

vertexOut               = primop
geometryOut             = primop
fragmentOut             = primop
fragmentOutDepth        = primop
fragmentOutRastDepth    = primop

sampler                 = primop
input                   = primop
transform               = primop
reassemble              = primop
rasterize               = primop
accumulate              = primop
accumulateSet           = primop

imageOut                = primop
screenOut               = primop

arrayAccumulate         = undefined
arrayMap                = undefined
arrayZipWith            = undefined
