module LC_I_DeBruijn where

import TypeLevel.Number.Nat
import TypeLevel.Number.Nat.Num

import LC_I_APIType
import LC_I_PrimFun
import LC_T_APIType (toInputList)
import LC_T_DSLType
import LC_U_APIType as U

import LC_T_DeBruijn
import LC_U_DeBruijn hiding (FragmentFilter,GeometryShader)
import qualified LC_U_DeBruijn as U

newtype IdxI a b = IdxI Int
instance Idx IdxI where
    zeroIdx             = IdxI 0
    succIdx (IdxI a)    = IdxI (1+a)

newtype OpenFunI a b c = OpenFunI (Fun Exp)
instance OpenFun OpenFunI where
    type OpenFun_OpenExp OpenFunI   = OpenExpI
    body (OpenExpI a)   = OpenFunI (Body a)
    lam (OpenFunI a)    = OpenFunI (Lam a)

newtype OpenExpI a b c = OpenExpI Exp
instance OpenExp OpenExpI where
    type OpenExp_FlatTuple OpenExpI             = FlatTupleI Exp
    type OpenExp_Idx OpenExpI                   = IdxI
    type OpenExp_Input OpenExpI                 = InputI
    type OpenExp_OpenFun OpenExpI               = OpenFunI
    type OpenExp_PrimFun OpenExpI               = PrimFunI
    type OpenExp_InterpolatedFlatTuple OpenExpI = FlatTupleI (Interpolated Exp)
    type OpenExp_TupleIdx OpenExpI              = TupleIdxI
    type OpenExp_Texture OpenExpI               = TextureI

    let_ (OpenExpI a) (OpenExpI b)              = OpenExpI (Let a b)
    var (IdxI a)                                = OpenExpI (Var a)
    apply (OpenFunI a) (OpenExpI b)             = OpenExpI (Apply a b)
    cnst a                                      = OpenExpI (Const (toValue a))
    primVar (InputI a)                          = OpenExpI (uncurry PrimVar a)
    uni (InputI a)                              = OpenExpI (uncurry Uni a)
    tup (FlatTupleI a)                          = OpenExpI (Tup a)
    prj (TupleIdxI a) (OpenExpI b)              = OpenExpI (Prj a b)
    cond (OpenExpI c) (OpenExpI t) (OpenExpI e) = OpenExpI (Cond c t e)
    primApp (PrimFunI a) (OpenExpI b)           = OpenExpI (PrimApp a b)
    vertexOut (OpenExpI a) (OpenExpI b)
              (FlatTupleI c)                    = OpenExpI (VertexOut a b c)
    geometryOut (OpenExpI a) (OpenExpI b)
                (OpenExpI c) (OpenExpI d)
                (OpenExpI e) (FlatTupleI f)     = OpenExpI (GeometryOut a b c d e f)
    fragmentOut (FlatTupleI a)                  = OpenExpI (FragmentOut a)
    fragmentOutDepth (OpenExpI a)(FlatTupleI b) = OpenExpI (FragmentOutDepth a b)
    fragmentOutRastDepth (FlatTupleI a)         = OpenExpI (FragmentOutRastDepth a)
    sampler a b (TextureI c)                    = OpenExpI (Sampler a b c)

newtype TextureI a b c d = TextureI (Texture GP)
-- TODO

newtype GeometryShaderI a b c d e f = GeometryShaderI U.GeometryShader
instance GeometryShader GeometryShaderI where
    type GeometryShader_OpenFun GeometryShaderI     = OpenFunI
    type GeometryShader_Primitive GeometryShaderI   = PrimitiveI
    noGeometryShader                                = GeometryShaderI NoGeometryShader
    geometryShader a (PrimitiveI b) c
            (OpenFunI d) (OpenFunI e) (OpenFunI f)  = GeometryShaderI (U.GeometryShader (toInt a) b c d e f)

newtype FragmentFilterI a b = FragmentFilterI U.FragmentFilter
instance FragmentFilter FragmentFilterI where
    type FragmentFilter_OpenFun FragmentFilterI = OpenFunI
    passAll             = FragmentFilterI PassAll
    filter (OpenFunI a) = FragmentFilterI (Filter a)

newtype OpenGPFunI a b = OpenGPFunI (Fun GP)
instance OpenGPFun OpenGPFunI where
    type OpenGPFun_OpenGP OpenGPFunI   = OpenGPI
    gpBody (OpenGPI a)      = OpenGPFunI (Body a)
    gpLam (OpenGPFunI a)    = OpenGPFunI (Lam a)

newtype OpenGPI a b = OpenGPI GP
instance OpenGP OpenGPI where
    type OpenGP_Idx OpenGPI                         = IdxI
    type OpenGP_OpenGPFun OpenGPI                   = OpenGPFunI
    type OpenGP_OpenFun OpenGPI                     = OpenFunI
    type OpenGP_FlatTupleFragmentOperation OpenGPI  = FlatTupleI U.FragmentOperation
    type OpenGP_FlatTupleImage OpenGPI              = FlatTupleI U.Image
    type OpenGP_FragmentFilter OpenGPI              = FragmentFilterI
    type OpenGP_FragmentOperation OpenGPI           = FragmentOperationI
    type OpenGP_GeometryShader OpenGPI              = GeometryShaderI
    type OpenGP_Image OpenGPI                       = ImageI
    type OpenGP_Primitive OpenGPI                   = PrimitiveI
    type OpenGP_RasterContext OpenGPI               = RasterContextI
    type OpenGP_TupleIdx OpenGPI                    = TupleIdxI

    gpLet (OpenGPI a) (OpenGPI b)                   = OpenGPI (GPLet a b)
    gpVar (IdxI a)                                  = OpenGPI (GPVar a)
    gpApply (OpenGPFunI a) (OpenGPI b)              = OpenGPI (GPApply a b)

    fetch a (PrimitiveI b) c                        = OpenGPI (Fetch a b (toInputList c))
    transform (OpenFunI a) (OpenGPI b)              = OpenGPI (Transform a b)
    rasterize (RasterContextI a)
            (GeometryShaderI b) (OpenGPI c)         = OpenGPI (Rasterize a b c)
    frameBuffer a (FlatTupleI b)                    = OpenGPI (FrameBuffer a b)
    accumulate (FlatTupleI a) (FragmentFilterI b)
               (OpenFunI c) (OpenGPI d) (OpenGPI e) = OpenGPI (Accumulate a b c d e)
    prjFrameBuffer a (TupleIdxI b) (OpenGPI c)      = OpenGPI (PrjFrameBuffer a b c)
    prjImage a b (OpenGPI c)                        = OpenGPI (PrjImage a (toInt b) c)
