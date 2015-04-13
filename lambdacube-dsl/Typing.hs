{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Typing where

import Data.List
import Data.Maybe
import Data.Monoid
import Text.PrettyPrint.ANSI.Leijen (pretty)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)
import qualified Data.Traversable as T
import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.Writer
import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Trifecta.Delta
import Text.Trifecta hiding (err)

import Type

matches TVar{} _ = True
matches x ts = x `elem` ts

unifyMaps :: Ord a => [Map a b] -> [[b]]
unifyMaps = Map.elems . Map.unionsWith (++) . map ((:[]) <$>)

isRec TRecord{} = True
isRec t = isVar t

isVar TVar{} = True
isVar _ = False

iff x y b = if b then x else y

{- TODO
  type family NoStencilRepr a :: *
    type instance NoStencilRepr ZZ = ZZ
    type instance NoStencilRepr (Stencil a :+: b) = NoStencilRepr b
    type instance NoStencilRepr (Color a :+: b) = Color a :+: NoStencilRepr b
    type instance NoStencilRepr (Depth a :+: b) = Depth a :+: NoStencilRepr b
-}

{- currently not used
  [injective] type family PrimitiveVertices (primitive :: PrimitiveType) a
    type instance PrimitiveVertices Point a             = a
    type instance PrimitiveVertices Line a              = (a,a)
    type instance PrimitiveVertices LineAdjacency a     = (a,a,a,a)
    type instance PrimitiveVertices Triangle a          = (a,a,a)
    type instance PrimitiveVertices TriangleAdjacency a = (a,a,a,a,a,a)
-}
{- currently not used
  - texturing -
  [semiinjective] type family TexDataRepr arity (t :: TextureSemantics *)
    type instance TexDataRepr Red  (v a) = a
    type instance TexDataRepr RG   (v a) = V2 a
    type instance TexDataRepr RGB  (v a) = V3 a
    type instance TexDataRepr RGBA (v a) = V4 a

  [injective if (= SigleTex)] type family TexArrRepr (a :: Nat) :: TextureArray
    --type instance TexArrRepr 1 = SingleTex
    --type instance TexArrRepr ((2 <= t) => t) = ArrayTex
    -- FIXME: implement properly
    type instance TexArrRepr 1 = SingleTex
    type instance TexArrRepr 2 = ArrayTex
    type instance TexArrRepr 3 = ArrayTex
    type instance TexArrRepr 4 = ArrayTex
    type instance TexArrRepr 5 = ArrayTex
    type instance TexArrRepr 6 = ArrayTex
    type instance TexArrRepr 7 = ArrayTex
    type instance TexArrRepr 8 = ArrayTex
    type instance TexArrRepr 9 = ArrayTex

  [semiinjective] type family TexSizeRepr (a :: TextureShape)
    type instance TexSizeRepr (Tex1D)   = Word32
    type instance TexSizeRepr (Tex2D)   = V2U
    type instance TexSizeRepr (TexRect) = V2U
    type instance TexSizeRepr (Tex3D)   = V3U

  [injective in 4th param, semiinjective in 3rd param] type family TexelRepr sampler
    type instance TexelRepr (Sampler dim arr (v t) Red)     = t
    type instance TexelRepr (Sampler dim arr (v t) RG)      = V2 t
    type instance TexelRepr (Sampler dim arr (v t) RGB)     = V3 t
    type instance TexelRepr (Sampler dim arr (v t) RGBA)    = V4 t
-}

type TCMC = WriterT [[Ty]] TCM

reduceConstraint :: Constraint Ty -> TCMC [Constraint Ty]
reduceConstraint x = case x of

    Split (TRecord a) (TRecord b) (TRecord c) ->
      case (Map.keys $ Map.intersection b c, Map.keys $ a Map.\\ (b <> c), Map.keys $ (b <> c) Map.\\ a) of
        ([], [], []) -> discard $ unifyMaps [a, b, c]
        ks -> failure $ "extra keys: " ++ show ks
    Split (TRecord a) (TRecord b) c@TVar{} -> diff a b c
    Split (TRecord a) c@TVar{} (TRecord b) -> diff a b c
    Split c@TVar{} (TRecord a) (TRecord b) -> case Map.keys $ Map.intersection a b of
        [] -> discard [[c, TRecord $ a <> b]]
        ks -> failure $ "extra keys: " ++ show ks
    Split a b c
        | isRec a && isRec b && isRec c -> nothing
        | otherwise -> failure $ "bad split: " ++ show x

    CClass _ TVar{} -> nothing
    CClass c t -> case c of

        IsTypeLevelNatural -> case t of
            TNat{} -> discard []
            _ -> noInstance

        IsValidOutput -> discard [] -- TODO

        IsValidFrameBuffer -> case t of
            TTuple C ts
                | any isVar ts -> nothing
                | sum [1 | Depth{} <- ts] <= 1 && sum [1 | Stencil{} <- ts] <= 1 -> discard []
                | otherwise -> noInstance
            _ -> discard []

        IsInputTuple -> case t of
            TTuple C ts
                | any isVar ts -> nothing
                | length [() | TInput{} <- ts] == length ts -> discard []
                | otherwise -> noInstance
            _ -> discard []

        _ -> maybe noInstance (iff (discard []) noInstance . Set.member t) $ Map.lookup c instances
      where
        noInstance = failure $ "no " ++ show c ++ " instance for " ++ show t

    CUnify a b -> discard [[a, b]]

    CEq res f -> case f of

        TFMat (TVec n t1) (TVec m t2) | t1 `elem` [TFloat C] && t1 == t2 -> reduced $ TMat n m t1
        TFMat a b -> check (a `matches` floatVectors && b `matches` floatVectors) $ observe res $ \case
            TMat n m t -> keep [[a, TVec n t], [b, TVec m t]]
            _ -> fail "no instance"

        TFVec (TNat n) ty | n `elem` [2,3,4] && ty `elem` floatIntWordBool -> reduced $ TVec n ty
        TFVec a b -> check (a `matches` nat234 && b `matches` floatIntWordBool) $ observe res $ \case
            TVec n t -> keep [[a, TNat n], [b, t]]
            _ -> fail "no instance"

        TFVecScalar a b -> case a of
            TNat 1 -> case b of
                TVar{} | res `matches` floatIntWordBool -> keep [[b, res]]
                b -> check (b `elem` floatIntWordBool) $ reduced b
            TVar{} -> check (b `matches` floatIntWordBool) $ observe res $ \case
                t | t `elem` floatIntWordBool -> keep [[a, TNat 1], [b, t]]
                _ -> like $ TFVec a b
            _ -> like $ TFVec a b

        TFMatVecElem t -> observe t $ \case
            TVec n t -> reduced t
            TMat _ _ t -> reduced t
            _ -> fail "no instance"

        TFMatVecScalarElem t -> observe t $ \case
            t | t `elem` floatIntWordBool -> reduced t
            t -> like $ TFMatVecElem t

        TFColorRepr ty -> observe ty $ \case
            TTuple C ts -> reduced . TTuple C $ map Color ts
            ty -> reduced $ Color ty

        TFFTRepr' ty -> caseTuple "expected Input/Interpolated/Depth/Color" ty (reduced . tTuple C) $ \case
            TInterpolated C a -> reduce' a
            TInput C a        -> reduce' a
            Depth a           -> reduce' a
            Color a           -> reduce' a
            _ -> fail'

        TFFragOps ty -> caseTuple "expected FragmentOperation" ty (reduced . tTuple C) $ \case
            TFragmentOperation C a -> reduce' a
            _ -> fail'

        TFFrameBuffer ty -> caseTuple "expected (Image Nat)" ty end $ \case
            TImage C a b -> observe' a $ \case
                TNat n -> reduce' (n, b)
                _ -> fail'
            _ -> fail'
          where
            end (unzip -> (n: ns, tys))
                | all (==n) ns = reduced $ TFrameBuffer C (TNat n) $ tTuple C tys
                | otherwise = fail "frambuffer number of layers differ"

        TFJoinTupleType TVar{} _ -> nothing  -- TODO: observe res?
        TFJoinTupleType _ TVar{} -> nothing  -- TODO: observe res?
        TFJoinTupleType (TTuple f l) (TTuple _ r) -> reduced $ TTuple f (l ++ r)
        TFJoinTupleType l (TTuple f r) -> reduced $ TTuple f (l : r)
        TFJoinTupleType (TTuple f l) r -> reduced $ TTuple f (l ++ [r])
        TFJoinTupleType l r -> reduced $ TTuple C [l,r]

        _ -> nothing
      where
        like f = reduceConstraint (CEq res f)
        reduced t = discard [[res, t]]
        check b m = if b then m else fail "no instance"
        fail = failure . (("error during reduction of " ++ show res ++ " ~ " ++ show f ++ "  ") ++)

        reduce' = Just . Just
        nothing' = Just Nothing
        fail' = Nothing
        observe' TVar{} _ = nothing'
        observe' x f = f x

        caseTuple :: String -> Ty -> ([a] -> TCMC [Constraint Ty]) -> (Ty -> Maybe (Maybe a)) -> TCMC [Constraint Ty]
        caseTuple msg ty end f = observe ty $ \case
            TTuple C ts -> maybe (fail $ msg ++ " inside tuple") (maybe nothing end . sequence) $ mapM f' ts
            _ -> maybe (fail msg) (maybe nothing (end . (:[]))) $ f' ty
          where f' x = observe' x f

        tTuple f [x] = x
        tTuple f xs = TTuple f xs
  where
    diff a b c = case Map.keys $ b Map.\\ a of
        [] -> discard $ [c, TRecord $ a Map.\\ b]: unifyMaps [a, b]
        ks -> failure $ "extra keys: " ++ show ks

    discard xs = tell xs >> return []
    keep xs = tell xs >> return [x]
    failure = lift . throwErrorTCM

    nothing = keep []
    observe TVar{} _ = nothing
    observe x f = f x

instances :: Map Class (Set Ty)
instances = Map.fromList
    [ item CNum         [TInt C, TFloat C]
    , item IsIntegral   [TInt C, TWord C]
    , item IsNumComponent $ floatIntWord ++ floatVectors
    , item IsSigned     [TFloat C, TInt C]
    , item IsNum        floatIntWord
    , item IsFloating   $ TFloat C: floatVectors ++ matrices
    , item IsComponent  $ floatIntWordBool ++ floatVectors
    ]
  where
    item a b = (a, Set.fromList b)

nat234 = [TNat i | i <-[2..4]]
floatIntWord = [TFloat C, TInt C, TWord C]
floatIntWordBool = [TFloat C, TInt C, TWord C, TBool C]
matrices = [TMat i j (TFloat C) | i <- [2..4], j <- [2..4]]
vectors t = [TVec i t | i <- [2..4]]
floatVectors = vectors $ TFloat C

data InjType
    = ITMat | ITVec | ITVecScalar
    deriving (Eq, Ord, Show)

injType :: TypeFun Ty -> Maybe (InjType, [Ty])
injType = \case
    TFMat a b -> Just (ITMat, [a, b])
    TFVec a b -> Just (ITVec, [a, b])
    TFVecScalar a b -> Just (ITVecScalar, [a, b])
    _ -> Nothing

vecS = TFVecScalar
floatVecS d = vecS d (TFloat C)

infix 0 --->, -->
ss ---> m = tell [(s, newV m) | s <- ss]
s --> m = [s] ---> m

isPrimFun n = Map.member n primFunMap
{-
    -- Vec/Mat (de)construction
    PrimTupToV2             :: IsComponent a                            => PrimFun stage ((a,a)     -> V2 a)
    PrimTupToV3             :: IsComponent a                            => PrimFun stage ((a,a,a)   -> V3 a)
    PrimTupToV4             :: IsComponent a                            => PrimFun stage ((a,a,a,a) -> V4 a)
    PrimV2ToTup             :: IsComponent a                            => PrimFun stage (V2 a     -> (a,a))
    PrimV3ToTup             :: IsComponent a                            => PrimFun stage (V3 a   -> (a,a,a))
    PrimV4ToTup             :: IsComponent a                            => PrimFun stage (V4 a -> (a,a,a,a))
-}

primFunMapM :: TCM (Map EName Typing)
primFunMapM = T.sequence primFunMap

primFunMap :: Map EName (TCM Typing)
primFunMap = Map.fromList $ execWriter $ do
  ["Tup", "Const"]  ---> \a -> a ~> a       -- temporary const constructor
  ["True", "False"] ---> TBool C

  forM_ [2..4] $ \i -> do
    forM_ (zip ["F","I","U","B"] floatIntWordBool) $ \(tn, t) ->
        "V" ++ show i ++ tn --> replicate i t ~~> TVec i t      -- like  "V2B" --> TBool C ~> TBool C ~> TV2B C
    forM_ [2..4] $ \j ->
        "M" ++ show i ++ show j ++ "F" --> replicate j (TVec i $ TFloat C) ~~> TMat i j (TFloat C)
                                                                -- like  "M22F" --> TV2F C ~> TV2F C ~> TM22F C

  -- Input declaration
  "Uni"     --> \t -> TInput C t ~> t
  forM_ (  zip ["IFloat", "IInt", "IWord", "IBool"] floatIntWordBool
        ++ [("IM" ++ show i ++ show j ++ "F", TMat i j (TFloat C)) | i <- [2..4], j <- [2..4]]
        ++ [ ("IV" ++ show i ++ tn, TVec i t)
           | i <- [2..4]
           , (tn, t) <- zip ["F","I","U","B"] floatIntWordBool
           ]
        ) $ \(name, t) ->
    name --> TString C ~> TInput C t                            -- like  "IBool" --> TString C ~> TInput C (TBool  C)

  ["Zero", "One", "SrcColor", "OneMinusSrcColor", "DstColor", "OneMinusDstColor", "SrcAlpha", "OneMinusSrcAlpha", "DstAlpha", "OneMinusDstAlpha", "ConstantColor", "OneMinusConstantColor", "ConstantAlpha", "OneMinusConstantAlpha", "SrcAlphaSaturate"]
                        ---> TBlendingFactor C
  ["FuncAdd", "FuncSubtract", "FuncReverseSubtract", "Min", "Max"]
                        ---> TBlendEquation C
  ["Clear", "And", "AndReverse", "Copy", "AndInverted", "Noop", "Xor", "Or", "Nor", "Equiv", "Invert", "OrReverse", "CopyInverted", "OrInverted", "Nand", "Set"]
                        ---> TLogicOperation C
  ["OpZero", "OpKeep", "OpReplace", "OpIncr", "OpIncrWrap", "OpDecr", "OpDecrWrap", "OpInvert"]
                        ---> TStencilOperation C
  ["Never", "Less", "Equal", "Lequal", "Greater", "Notequal", "Gequal", "Always"]
                        ---> TComparisonFunction C
  ["LastVertex", "FirstVertex"]
                        ---> TProvokingVertex C

  ["CW", "CCW"]         ---> TFrontFace C
  ["CullFront", "CullBack"]
                        ---> TFrontFace C ~> TCullMode C
  "CullNone"            -->                  TCullMode C

  "PolygonFill"         -->                 TPolygonMode C
  "PolygonPoint"        --> TPointSize C ~> TPolygonMode C
  "PolygonLine"         --> TFloat C ~>     TPolygonMode C

  "NoOffset"            -->                         TPolygonOffset C
  "Offset"              --> TFloat C ~> TFloat C ~> TPolygonOffset C

  "PointSize"           --> TFloat C ~> TPointSize C
  "ProgramPointSize"    -->             TPointSize C

  "FragmentOut"         --> \a t -> [a ~~ TFColorRepr t] ==> t  ~> TFragmentOut C a
  "FragmentOutDepth"    --> \a b t -> [a ~~ TFColorRepr t, b ~~ TFJoinTupleType (Depth $ TFloat C) a] ==> TFloat C ~> t
                                                                ~> TFragmentOut C b
  "FragmentOutRastDepth"--> \a b t -> [a ~~ TFColorRepr t, b ~~ TFJoinTupleType (Depth $ TFloat C) a] ==> t
                                                                ~> TFragmentOut C b

  "VertexOut"           --> \a t -> [t ~~ TFFTRepr' a] ==> TVec 4 (TFloat C) ~> TFloat C ~> TTuple C [] ~> a ~> TVertexOut C t

  ["LowerLeft", "UpperLeft"]
                        ---> TPointSpriteCoordOrigin C

  "TriangleCtx"         --> TCullMode C ~> TPolygonMode C ~> TPolygonOffset C ~> TProvokingVertex C ~> TRasterContext C TTriangle
  "PointCtx"            --> TPointSize C ~> TFloat C ~> TPointSpriteCoordOrigin C                   ~> TRasterContext C TPoint
  "LineCtx"             --> TFloat C ~> TProvokingVertex C                                          ~> TRasterContext C TLine

  "Points"              --> TFetchPrimitive C TPoint
  "Lines"               --> TFetchPrimitive C TLine
  "Triangles"           --> TFetchPrimitive C TTriangle
  "LinesAdjacency"      --> TFetchPrimitive C TLineAdjacency
  "TrianglesAdjacency"  --> TFetchPrimitive C TTriangleAdjacency

  "AccumulationContext" --> \t' t -> [t' ~~ TFFragOps t] ==> t ~> TAccumulationContext C t'

  "ColorImage"          --> \a d color t -> [IsTypeLevelNatural @@ a, IsNum @@ t, color ~~ vecS d t] ==> a ~> color
                                                                               ~> TImage C a (Color color)
  "DepthImage"          --> \a -> [IsTypeLevelNatural @@ a] ==> a ~> TFloat C  ~> TImage C a (Depth $ TFloat C)
  "StencilImage"        --> \a -> [IsTypeLevelNatural @@ a] ==> a ~> TInt C    ~> TImage C a (Stencil $ TInt C)

  ["Smooth", "NoPerspective"]
                        ---> \t -> [IsFloating @@ t] ==> t ~> TInterpolated C t
  ["Flat"]              ---> \t ->                       t ~> TInterpolated C t

  "ColorOp"             --> \d mask c color -> [mask ~~ vecS d (TBool C), color ~~ vecS d c, IsNum @@ c] ==> TBlending C c ~> mask
                                                                ~> TFragmentOperation C (Color color)
  "DepthOp"             --> TComparisonFunction C ~> TBool C    ~> TFragmentOperation C (Depth $ TFloat C)
    -- "StencilOp       :: StencilTests -> StencilOps -> StencilOps -> FragmentOperation (Stencil Int32)

  -- Blending
  "NoBlending"          --> \t ->                                            TBlending C t
  "BlendLogicOp"        --> \t -> [IsIntegral @@ t] ==> TLogicOperation C ~> TBlending C t
  "Blend"               --> TTuple C [TBlendEquation C,TBlendEquation C]
                         ~> TTuple C [TTuple C [TBlendingFactor C,TBlendingFactor C],TTuple C [TBlendingFactor C,TBlendingFactor C]]
                         ~> TVec 4 (TFloat C) ~>                             TBlending C (TFloat C)
  -- Fragment Filter
  "PassAll"             --> \t ->                   TFragmentFilter C t
  "Filter"              --> \t -> (t ~> TBool C) ~> TFragmentFilter C t
  -- Render Operations
  "Fetch"               --> \a t b -> [IsInputTuple @@ t, b ~~ TFFTRepr' t] ==> TString C ~> TFetchPrimitive C a ~> t ~> TVertexStream C a b
  "Transform"           --> \a b p -> (a ~> TVertexOut C b) ~> TVertexStream C p a ~> TPrimitiveStream C p (TNat 1) C b
  "Rasterize"           --> \a b c -> TRasterContext C a ~> TPrimitiveStream C a b C c ~> TFragmentStream C b c

  "Accumulate"          --> \a b n t -> [IsValidOutput @@ b, t ~~ TFFTRepr' b] ==>
                           TAccumulationContext C b
                        ~> TFragmentFilter C a
                        ~> (a ~> TFragmentOut C b)
                        ~> TFragmentStream C n a
                        ~> TFrameBuffer C n t
                        ~> TFrameBuffer C n t
  "FrameBuffer"         --> \a t t' n
                        -> [t' ~~ TFFTRepr' t, IsValidFrameBuffer @@ t, TFrameBuffer C n t ~~ TFFrameBuffer a]
                        ==> a ~> TFrameBuffer C n t'
  "ScreenOut"           --> \a b -> TFrameBuffer C a b ~> TOutput C
  -- * Primitive Functions *
  -- Arithmetic Functions (componentwise)
  ["PrimAdd", "PrimSub", "PrimMul"]     ---> \a t   -> [t ~~ TFMatVecElem a, IsNum @@ t] ==> a ~> a ~> a
  ["PrimAddS", "PrimSubS", "PrimMulS"]  ---> \a t   -> [t ~~ TFMatVecScalarElem a, IsNum @@ t] ==> a ~> t ~> a
  ["PrimDiv", "PrimMod"]                ---> \d a t -> [IsNum @@ t, a ~~ vecS d t] ==> a ~> a ~> a
  ["PrimDivS", "PrimModS"]              ---> \d a t -> [IsNum @@ t, a ~~ vecS d t] ==> a ~> t ~> a
  ["PrimNeg"]                           ---> \a t   -> [t ~~ TFMatVecScalarElem a, IsSigned @@ t] ==> a ~> a
  -- Bit-wise Functions
  ["PrimBAnd", "PrimBOr", "PrimBXor"]   ---> \d a t -> [IsIntegral @@ t, a ~~ vecS d t] ==> a ~> a ~> a
  ["PrimBAndS", "PrimBOrS", "PrimBXorS"]---> \d a t -> [IsIntegral @@ t, a ~~ vecS d t] ==> a ~> t ~> a
  ["PrimBNot"]                          ---> \d a t -> [IsIntegral @@ t, a ~~ vecS d t] ==> a ~> a
  ["PrimBShiftL", "PrimBShiftR"]        ---> \d a b t -> [IsIntegral @@ t, a ~~ vecS d t, b ~~ vecS d (TWord C)] ==> a ~> b ~> a
  ["PrimBShiftLS", "PrimBShiftRS"]      ---> \d a t -> [IsIntegral @@ t, a ~~ vecS d t] ==> a ~> TWord C ~> a
  -- Logic Functions
  ["PrimAnd", "PrimOr", "PrimXor"]      ---> TBool C ~> TBool C ~> TBool C
  ["PrimNot"]                           ---> \d a   -> [a ~~ vecS d (TBool C)] ==> a ~> a
  ["PrimAny", "PrimAll"]                ---> \d a   -> [a ~~ vecS d (TBool C)] ==> a ~> TBool C
  -- Angle, Trigonometry and Exponential Functions
  ["PrimACos", "PrimACosH", "PrimASin", "PrimASinH", "PrimATan", "PrimATanH", "PrimCos", "PrimCosH", "PrimDegrees", "PrimRadians", "PrimSin", "PrimSinH", "PrimTan", "PrimTanH", "PrimExp", "PrimLog", "PrimExp2", "PrimLog2", "PrimSqrt", "PrimInvSqrt"]
                                        ---> \d a   -> [a ~~ floatVecS d] ==> a ~> a
  ["PrimPow", "PrimATan2"]              ---> \d a   -> [a ~~ floatVecS d] ==> a ~> a ~> a
  -- Common Functions
  ["PrimFloor", "PrimTrunc", "PrimRound", "PrimRoundEven", "PrimCeil", "PrimFract"]
                                        ---> \d a   -> [a ~~ floatVecS d] ==> a ~> a
  ["PrimMin", "PrimMax"]                ---> \d a t -> [IsNum @@ t, a ~~ vecS d t] ==> a ~> a ~> a
  ["PrimMinS", "PrimMaxS"]              ---> \d a t -> [IsNum @@ t, a ~~ vecS d t] ==> a ~> t ~> a
  ["PrimIsNan", "PrimIsInf"]            ---> \d a b -> [a ~~ floatVecS d, b ~~ vecS d (TBool C)] ==> a ~> b
  ["PrimAbs", "PrimSign"]               ---> \d a t -> [IsSigned @@ t, a ~~ vecS d t] ==> a ~> a
  "PrimModF"            --> \d a   -> [a ~~ floatVecS d] ==> a ~> TTuple C [a, a]
  "PrimClamp"           --> \d a t -> [IsNum @@ t, a ~~ vecS d t] ==> a ~> a ~> a ~> a
  "PrimClampS"          --> \d a t -> [IsNum @@ t, a ~~ vecS d t] ==> a ~> t ~> t ~> a
  "PrimMix"             --> \d a   -> [a ~~ floatVecS d] ==> a ~> a ~> a ~> a
  "PrimMixS"            --> \d a   -> [a ~~ floatVecS d] ==> a ~> a ~> TFloat C ~> a
  "PrimMixB"            --> \d a b -> [a ~~ floatVecS d, b ~~ vecS d (TBool C)] ==> a ~> a ~> b ~> a
  "PrimStep"            --> \d a   -> [a ~~ TFVec d (TFloat C)] ==> a ~> a ~> a
  "PrimStepS"           --> \d a   -> [a ~~ floatVecS d] ==> TFloat C ~> a ~> a
  "PrimSmoothStep"      --> \d a   -> [a ~~ TFVec d (TFloat C)] ==> a ~> a ~> a ~> a
  "PrimSmoothStepS"     --> \d a   -> [a ~~ floatVecS d] ==> TFloat C ~> TFloat C ~> a ~> a
  -- Integer/Float Conversion Functions
  "PrimFloatBitsToInt"  --> \d fv iv -> [fv ~~ floatVecS d, iv ~~ vecS d (TInt C)]  ==> fv ~> iv
  "PrimFloatBitsToUInt" --> \d fv uv -> [fv ~~ floatVecS d, uv ~~ vecS d (TWord C)] ==> fv ~> uv
  "PrimIntBitsToFloat"  --> \d fv iv -> [fv ~~ floatVecS d, iv ~~ vecS d (TInt C)]  ==> iv ~> fv
  "PrimUIntBitsToFloat" --> \d fv uv -> [fv ~~ floatVecS d, uv ~~ vecS d (TWord C)] ==> uv ~> fv
  -- Geometric Functions
  "PrimLength"          --> \d a -> [a ~~ floatVecS d] ==> a ~> TFloat C
  ["PrimDistance", "PrimDot"]
                        ---> \d a -> [a ~~ floatVecS d] ==> a ~> a ~> TFloat C
  "PrimCross"           --> \a    -> [a ~~ floatVecS (TNat 3)] ==> a ~> a ~> a
  "PrimNormalize"       --> \d a  -> [a ~~ floatVecS d] ==> a ~> a
  ["PrimFaceForward", "PrimRefract"]
                        ---> \d a -> [a ~~ floatVecS d] ==> a ~> a ~> a ~> a
  "PrimReflect"         --> \d a  -> [a ~~ floatVecS d] ==> a ~> a ~> a
  -- Matrix Functions
  "PrimTranspose"       --> \a b h w -> [a ~~ TFMat h w, b ~~ TFMat w h] ==> a ~> b
  "PrimDeterminant"     --> \m s   -> [m ~~ TFMat s s] ==> m ~> TFloat C
  "PrimInverse"         --> \m s   -> [m ~~ TFMat s s] ==> m ~> m
  "PrimOuterProduct"    --> \m h w -> [m ~~ TFMat h w] ==> w ~> h ~> m
  "PrimMulMatVec"       --> \m h w -> [m ~~ TFMat h w] ==> m ~> w ~> h
  "PrimMulVecMat"       --> \m h w -> [m ~~ TFMat h w] ==> h ~> m ~> w
  "PrimMulMatMat"       --> \a b c i j k -> [a ~~ TFMat i j, b ~~ TFMat j k, c ~~ TFMat i k] ==> a ~> b ~> c
  -- Vector and Scalar Relational Functions
  ["PrimLessThan", "PrimLessThanEqual", "PrimGreaterThan", "PrimGreaterThanEqual", "PrimEqualV", "PrimNotEqualV"]
                        ---> \d a b t -> [IsNum @@ t, a ~~ vecS d t, b ~~ vecS d (TBool C)] ==> a ~> a ~> b
  ["PrimEqual", "PrimNotEqual"]
                        ---> \a t  -> [t ~~ TFMatVecScalarElem a] ==> a ~> a ~> TBool C
  -- Fragment Processing Functions
  ["PrimDFdx", "PrimDFdy", "PrimFWidth"]
                        ---> \d a  -> [a ~~ floatVecS d] ==> a ~> a
  -- Noise Functions
  "PrimNoise1"          --> \d a   -> [a ~~ floatVecS d] ==> a ~> TFloat C
  forM_ [2..4] $ \i ->
      "PrimNoise" ++ show i  --> \d a b -> [a ~~ floatVecS d, b ~~ floatVecS (TNat i)] ==> a ~> b

fieldProjType :: FName -> TCM Typing
fieldProjType fn = newV $ \a r r' -> return $ [Split r r' (TRecord $ Map.singleton fn a)] ==> r ~> a :: TCM Typing

inferLit :: Lit -> TCM Typing
inferLit a = case a of
  LInt _ -> do
    --t <- newVar C
    --return (mempty,[(CNum,t)],t) -- ????
    ty $ TInt C
  LChar   _ -> ty $ TChar C
  LFloat  _ -> ty $ TFloat C
  LString _ -> ty $ TString C
  LNat i    -> ty $ TNat i
 where
  ty t = return $ [] ==> t

checkUnambError = do
    cs <- gets fst
    case cs of
        (Just _: _) -> throwError $ head $ catMaybes $ reverse cs
        _ -> return ()

throwErrorTCM :: String -> TCM a
throwErrorTCM s = checkUnambError >> errorTCM >>= throwError . fmap (++ s)

errorTCM :: TCM ErrorMsg
errorTCM = do
  rl <- asks snd
  return $ \src -> let
      sl = map mkSpan rl
      fullCode = True
      mkSpan (s,e) = unlines [show $ pretty (s,e), if fullCode then BS.unpack str else show $ pretty r]
        where
          r = render spn
          str = x -- <> BS.takeWhile (\a -> notElem a ['\n','\r']) y
          spn = Span s e str
          (x,y) = BS.splitAt (se - sb) $ BS.drop sb src
          b = rewind s
          sb = fromIntegral $ bytes b
          se = fromIntegral $ bytes e
    in concat sl

class NewVar a where
    type NewVarRes a :: *
    type NewVarRes a = a
    newV :: a -> TCM (NewVarRes a)

instance NewVar (TCM a) where
    type NewVarRes (TCM a) = a
    newV = id

instance NewVar Typing where newV = return
instance NewVar (a, b) where newV = return

instance NewVar Ty where
    type NewVarRes Ty = Typing
    newV t = return $ [] ==> t

instance NewVar a => NewVar (Ty -> a) where
    type NewVarRes (Ty -> a) = NewVarRes a
    newV f = newVar C >>= newV . f

newVar :: Frequency -> TCM Ty
newVar f = do
  (d, n) <- get
  put (d, n+1)
  return $ TVar f $ 't':show n
