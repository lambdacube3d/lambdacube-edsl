module LC_U_DeBruijn where

import Data.ByteString.Char8
import Data.Set (Set)
import Data.Typeable
import qualified Data.Set as Set
import Data.Int

import LCType

import LC_APIType
import LC_U_APIType
import LC_U_PrimFun

data OpenFun
    = Body  OpenExp
    | Lam   OpenFun
    deriving (Show, Eq, Ord)

data OpenExp
    = Var       Int
    | Const     Value
    | PrimVar   ByteString InputType
    | Uni       ByteString InputType
    | Tup       [OpenExp]
    | Prj       Int OpenExp
    | Cond      OpenExp OpenExp OpenExp
    | PrimApp   PrimFun OpenExp
    | Sampler   Filter EdgeMode (Texture OpenGP)
    deriving (Show, Eq, Ord)

data OpenFragmentOut
    = FragmentOut           [OpenExp]
    | FragmentOutDepth      OpenExp [OpenExp]
    | FragmentOutRastDepth  [OpenExp]
    deriving (Show, Eq, Ord)

data FragmentFilter
    = PassAll
    | Filter    OpenFun
    deriving (Show, Eq, Ord)

data OpenVertexOut
    = VertexOut OpenExp OpenExp [Interpolated OpenExp]
    deriving (Show, Eq, Ord)

data OpenGeometryOut
    = GeometryOut   OpenExp OpenExp OpenExp OpenExp OpenExp [Interpolated OpenExp]
    deriving (Show, Eq, Ord)

data GeometryShader
    = NoGeometryShader 
    | GeometryShader    Int PrimitiveType Int OpenFun OpenFun OpenFun
    deriving (Show, Eq, Ord)

data OpenGPfun
    = GPBody    OpenGP
    | GPLam     OpenGPfun
    deriving (Show, Eq, Ord)

data OpenGP
    = GPLet             OpenGP OpenGP
    | GPVar             Int
    | Apply             OpenGPfun OpenGP
    | Fetch             ByteString PrimitiveType [(ByteString,InputType)]
    | Transform         OpenFun OpenGP
    | Rasterize         RasterContext GeometryShader OpenGP
    | FrameBuffer       V2U [Image]
    | Accumulate        [FragmentOperation] FragmentFilter OpenFun OpenGP OpenGP
    | PrjFrameBuffer    ByteString Int OpenGP
    | PrjImage          ByteString Int OpenGP
    deriving (Show, Eq, Ord)

--type GP t = OpenGP openGP => openGP () t
--deriving instance Typeable2 OpenGP

-- utility functions
{-
data StreamInput -- slot name, primitive type, stream input name and type
    = StreamInput ByteString PrimitiveType [(ByteString,InputType)] deriving Show

streamInput :: GP t -> [StreamInput]
streamInput (Fetch n p a)               = [StreamInput n (toPrimitive p) (toInputList a)]
streamInput (Transform _ vs)            = streamInput vs
streamInput (Rasterize _ _ ps)          = streamInput ps
streamInput (Accumulate _ _ _ fs fb)    = streamInput fs ++ streamInput fb
streamInput _                           = []
-}
{-
uniformInputExp :: OpenExp stage env genv t -> Set (ByteString,InputType)
uniformInputExp (Uni a)         = Set.fromList $! toInputList a
uniformInputExp (Tup a)         = collect a
  where
    collect :: Tuple (OpenExp stage env genv) t' -> Set (ByteString,InputType)
    collect NilTup          = Set.empty
    collect (SnocTup tup e) = uniformInputExp e `Set.union` collect tup
uniformInputExp (Prj _ a)       = uniformInputExp a
uniformInputExp (Cond a b c)    = uniformInputExp a `Set.union` uniformInputExp b `Set.union` uniformInputExp c
uniformInputExp (PrimApp _ a)   = uniformInputExp a
uniformInputExp _               = Set.empty

uniformInputInterpolatedFlatExp :: OpenInterpolatedFlatExp stage env genv t -> Set (ByteString,InputType)
uniformInputInterpolatedFlatExp (Flat e :. xs)          = uniformInputExp e `Set.union` uniformInputInterpolatedFlatExp xs
uniformInputInterpolatedFlatExp (Smooth e :. xs)        = uniformInputExp e `Set.union` uniformInputInterpolatedFlatExp xs
uniformInputInterpolatedFlatExp (NoPerspective e :. xs) = uniformInputExp e `Set.union` uniformInputInterpolatedFlatExp xs
uniformInputInterpolatedFlatExp _ = Set.empty

uniformInputFlatExp :: OpenFlatExp stage env genv t -> Set (ByteString,InputType)
uniformInputFlatExp (e :. xs)   = uniformInputExp e `Set.union` uniformInputFlatExp xs
uniformInputFlatExp _           = Set.empty
-}
--uniformInputGS = undefined
--uniformInputFun = undefined
{-
uniformInputFun :: OpenFun OpenExp env genv a -> Set (ByteString,InputType)
uniformInputFun (Lam f)   = uniformInputFun f
uniformInputFun (Body e)  = uniformInputExp e
-}
