{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-} -- for ghc-7.10.1
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
            TTuple ts
                | any isVar ts -> nothing
                | sum [1 | Depth{} <- ts] <= 1 && sum [1 | Stencil{} <- ts] <= 1 -> discard []
                | otherwise -> noInstance
            _ -> discard []

        IsInputTuple -> case t of
            TTuple ts
                | any isVar ts -> nothing
                | length [() | TInput{} <- ts] == length ts -> discard []
                | otherwise -> noInstance
            _ -> discard []

        _ -> maybe noInstance (iff (discard []) noInstance . Set.member t) $ Map.lookup c builtinInstances
      where
        noInstance = failure $ "no " ++ show c ++ " instance for " ++ show t

    CUnify a b -> discard [[a, b]]

    CEq res f -> case f of

        TFMat (TVec n t1) (TVec m t2) | t1 `elem` [TFloat] && t1 == t2 -> reduced $ TMat n m t1
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
            TTuple ts -> reduced . TTuple $ map Color ts
            ty -> reduced $ Color ty

        TFFTRepr' ty -> caseTuple "expected Input/Interpolated/Depth/Color" ty (reduced . tTuple) $ \case
            TInterpolated a -> reduce' a
            TInput a        -> reduce' a
            Depth a         -> reduce' a
            Color a         -> reduce' a
            _ -> fail'

        TFFragOps ty -> caseTuple "expected FragmentOperation" ty (reduced . tTuple) $ \case
            TFragmentOperation a -> reduce' a
            _ -> fail'

        TFFrameBuffer ty -> caseTuple "expected (Image Nat)" ty end $ \case
            TImage a b -> observe' a $ \case
                TNat n -> reduce' (n, b)
                _ -> fail'
            _ -> fail'
          where
            end (unzip -> (n: ns, tys))
                | all (==n) ns = reduced $ TFrameBuffer (TNat n) $ tTuple tys
                | otherwise = fail "frambuffer number of layers differ"

        TFJoinTupleType TVar{} _ -> nothing  -- TODO: observe res?
        TFJoinTupleType _ TVar{} -> nothing  -- TODO: observe res?
        TFJoinTupleType (TTuple l) (TTuple r) -> reduced $ TTuple (l ++ r)
        TFJoinTupleType l (TTuple r) -> reduced $ TTuple (l : r)
        TFJoinTupleType (TTuple l) r -> reduced $ TTuple (l ++ [r])
        TFJoinTupleType l r -> reduced $ TTuple [l,r]

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
            TTuple ts -> maybe (fail $ msg ++ " inside tuple") (maybe nothing end . sequence) $ mapM f' ts
            _ -> maybe (fail msg) (maybe nothing (end . (:[]))) $ f' ty
          where f' x = observe' x f

        tTuple [x] = x
        tTuple xs = TTuple xs
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

builtinInstances :: Map Class (Set Ty)
builtinInstances = Map.fromList
    [ item CNum         [TInt, TFloat]
    , item IsIntegral   [TInt, TWord]
    , item IsComponent  $ floatIntWordBool ++ floatVectors
    , item IsNumComponent $ floatIntWord ++ floatVectors
    , item IsSigned     [TFloat, TInt]
    , item IsNum        floatIntWord
    , item IsFloating   $ TFloat: floatVectors ++ matrices
    ]
  where
    item a b = (a, Set.fromList b)

nat234 = [TNat i | i <-[2..4]]
floatIntWord = [TFloat, TInt, TWord]
floatIntWordBool = [TFloat, TInt, TWord, TBool]
matrices = [TMat i j TFloat | i <- [2..4], j <- [2..4]]
vectors t = [TVec i t | i <- [2..4]]
floatVectors = vectors TFloat

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
floatVecS d = vecS d TFloat

infix 0 --->, -->, ---->
ss ---> m = tell [(s, newV m) | s <- ss]
s --> m = [s] ---> m
ss ----> m = map ('\'':) ss ---> m

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

primFunMap :: Map EName (TCM (Subst, Typing))
primFunMap = Map.fromList $ execWriter $ do

  -- kind of type constructors
  ["()", "Char", "String", "Bool", "Word", "Int", "Float"] ----> Star
  ["[]", "Detph", "Stencil", "Color"] ----> Star ~> Star
  ["Triangle", "Line", "Point", "TriangleAdjacency", "LineAdjacency"] ----> Star
  ["CullMode", "PolygonMode", "PolygonOffset", "ProvokingVertex", "FrontFace", "PointSize", "BlendingFactor", "BlendEquation", "LogicOperation", "StencilOperation", "ComparisonFunction", "PointSpriteCoordOrigin"] ----> Star
  -- TODO: more precise kinds
  ["Output"] ----> Star
  ["AccumulationContext", "Blending", "FetchPrimitive", "FragmentFilter", "FragmentOperation", "FragmentOut", "Input", "Interpolated", "RasterContext", "VertexOut"] ----> Star ~> Star
  ["FragmentStream", "FrameBuffer", "Image", "VertexStream"] ----> Star ~> Star ~> Star
  ["Vec"] ----> NatKind ~> Star ~> Star

  "V2" --> \a -> [IsComponent @@ a] ==> a ~> a ~> TVec 2 a
  "V3" --> \a -> [IsComponent @@ a] ==> a ~> a ~> a ~> TVec 3 a
  "V4" --> \a -> [IsComponent @@ a] ==> a ~> a ~> a ~> a ~> TVec 4 a
  "[]" --> \a -> TList a
  "$" --> \a b -> (a ~> b) ~> a ~> b
  "negate" --> \a -> [IsNum @@ a] ==> a ~> a
  "fromInt" --> \a -> [IsNum @@ a] ==> TInt ~> a
  ["zero'", "one'"] ---> \a -> [IsComponent @@ a] ==> (a :: Ty)
  "texture'" --> TUnit ~> TVec 2 TFloat ~> TVec 4 TFloat

  -- temporary?
  ["Tup", "Const", "pack'", "unpack'", "singT", "tup2", "untup2", "tup3", "untup3", "tup4", "untup4", "tup5", "untup5", "tup6", "untup6"]  ---> \a -> a ~> a

  ["True", "False"] ---> TBool

  forM_ [2..4] $ \i -> do
    forM_ (zip ["F","I","U","B"] floatIntWordBool) $ \(tn, t) ->
        "V" ++ show i ++ tn --> replicate i t ~~> TVec i t      -- like  "V2B" --> TBool ~> TBool ~> TV2B
    forM_ [2..4] $ \j ->
        "M" ++ show i ++ show j ++ "F" --> replicate j (TVec i TFloat) ~~> TMat i j TFloat
                                                                -- like  "M22F" --> TV2F ~> TV2F ~> TM22F

  -- Input declaration
  "Uni"     --> \t -> TInput t ~> t
  forM_ (  zip ["IFloat", "IInt", "IWord", "IBool"] floatIntWordBool
        ++ [("IM" ++ show i ++ show j ++ "F", TMat i j TFloat) | i <- [2..4], j <- [2..4]]
        ++ [ ("IV" ++ show i ++ tn, TVec i t)
           | i <- [2..4]
           , (tn, t) <- zip ["F","I","U","B"] floatIntWordBool
           ]
        ) $ \(name, t) ->
    name --> TString ~> TInput t                            -- like  "IBool" --> TString ~> TInput (TBool )

  ["Zero", "One", "SrcColor", "OneMinusSrcColor", "DstColor", "OneMinusDstColor", "SrcAlpha", "OneMinusSrcAlpha", "DstAlpha", "OneMinusDstAlpha", "ConstantColor", "OneMinusConstantColor", "ConstantAlpha", "OneMinusConstantAlpha", "SrcAlphaSaturate"]
                        ---> TBlendingFactor
  ["FuncAdd", "FuncSubtract", "FuncReverseSubtract", "Min", "Max"]
                        ---> TBlendEquation
  ["Clear", "And", "AndReverse", "Copy", "AndInverted", "Noop", "Xor", "Or", "Nor", "Equiv", "Invert", "OrReverse", "CopyInverted", "OrInverted", "Nand", "Set"]
                        ---> TLogicOperation
  ["OpZero", "OpKeep", "OpReplace", "OpIncr", "OpIncrWrap", "OpDecr", "OpDecrWrap", "OpInvert"]
                        ---> TStencilOperation
  ["Never", "Less", "Equal", "Lequal", "Greater", "Notequal", "Gequal", "Always"]
                        ---> TComparisonFunction
  ["LastVertex", "FirstVertex"]
                        ---> TProvokingVertex

  ["CW", "CCW"]         ---> TFrontFace
  ["CullFront", "CullBack"]
                        ---> TFrontFace ~> TCullMode
  "CullNone"            -->                TCullMode

  "PolygonFill"         -->               TPolygonMode
  "PolygonPoint"        --> TPointSize ~> TPolygonMode
  "PolygonLine"         --> TFloat ~>     TPolygonMode

  "NoOffset"            -->                     TPolygonOffset
  "Offset"              --> TFloat ~> TFloat ~> TPolygonOffset

  "PointSize"           --> TFloat ~> TPointSize
  "ProgramPointSize"    -->           TPointSize

  "FragmentOut"         --> \a t -> [a ~~ TFColorRepr t] ==> t  ~> TFragmentOut a
  "FragmentOutDepth"    --> \a b t -> [a ~~ TFColorRepr t, b ~~ TFJoinTupleType (Depth TFloat) a] ==> TFloat ~> t
                                                                ~> TFragmentOut b
  "FragmentOutRastDepth"--> \a b t -> [a ~~ TFColorRepr t, b ~~ TFJoinTupleType (Depth TFloat) a] ==> t
                                                                ~> TFragmentOut b

  "VertexOut"           --> \a t -> [t ~~ TFFTRepr' a] ==> TVec 4 TFloat ~> TFloat ~> TTuple [] ~> a ~> TVertexOut t

  ["LowerLeft", "UpperLeft"]
                        ---> TPointSpriteCoordOrigin

  "TriangleCtx"         --> TCullMode ~> TPolygonMode ~> TPolygonOffset ~> TProvokingVertex ~> TRasterContext TTriangle
  "PointCtx"            --> TPointSize ~> TFloat ~> TPointSpriteCoordOrigin                 ~> TRasterContext TPoint
  "LineCtx"             --> TFloat ~> TProvokingVertex                                      ~> TRasterContext TLine

  "Points"              --> TFetchPrimitive TPoint
  "Lines"               --> TFetchPrimitive TLine
  "Triangles"           --> TFetchPrimitive TTriangle
  "LinesAdjacency"      --> TFetchPrimitive TLineAdjacency
  "TrianglesAdjacency"  --> TFetchPrimitive TTriangleAdjacency

  "AccumulationContext" --> \t' t -> [t' ~~ TFFragOps t] ==> t ~> TAccumulationContext t'

  "ColorImage"          --> \a d color t -> [IsTypeLevelNatural @@ a, IsNum @@ t, color ~~ vecS d t] ==> a ~> color
                                                                             ~> TImage a (Color color)
  "DepthImage"          --> \a -> [IsTypeLevelNatural @@ a] ==> a ~> TFloat  ~> TImage a (Depth TFloat)
  "StencilImage"        --> \a -> [IsTypeLevelNatural @@ a] ==> a ~> TInt    ~> TImage a (Stencil TInt)

  ["Smooth", "NoPerspective"]
                        ---> \t -> [IsFloating @@ t] ==> t ~> TInterpolated t
  ["Flat"]              ---> \t ->                       t ~> TInterpolated t

  "ColorOp"             --> \d mask c color -> [mask ~~ vecS d TBool, color ~~ vecS d c, IsNum @@ c] ==> TBlending c ~> mask
                                                            ~> TFragmentOperation (Color color)
  "DepthOp"             --> TComparisonFunction ~> TBool    ~> TFragmentOperation (Depth TFloat)
    -- "StencilOp       :: StencilTests -> StencilOps -> StencilOps -> FragmentOperation (Stencil Int32)

  -- Blending
  "NoBlending"          --> \t ->                                          TBlending t
  "BlendLogicOp"        --> \t -> [IsIntegral @@ t] ==> TLogicOperation ~> TBlending t
  "Blend"               --> TTuple [TBlendEquation,TBlendEquation]
                         ~> TTuple [TTuple [TBlendingFactor,TBlendingFactor],TTuple [TBlendingFactor,TBlendingFactor]]
                         ~> TVec 4 TFloat ~>                             TBlending TFloat
  -- Fragment Filter
  "PassAll"             --> \t ->                 TFragmentFilter t
  "Filter"              --> \t -> (t ~> TBool) ~> TFragmentFilter t
  -- Render Operations
  "Fetch"               --> \a t b -> [IsInputTuple @@ t, b ~~ TFFTRepr' t] ==> TString ~> TFetchPrimitive a ~> t ~> TVertexStream a b
  "Transform"           --> \a b p -> (a ~> TVertexOut b) ~> TVertexStream p a ~> TPrimitiveStream p (TNat 1) b
  "Rasterize"           --> \a b c -> TRasterContext a ~> TPrimitiveStream a b c ~> TFragmentStream b c

  "Accumulate"          --> \a b n t -> [IsValidOutput @@ b, t ~~ TFFTRepr' b] ==>
                           TAccumulationContext b
                        ~> TFragmentFilter a
                        ~> (a ~> TFragmentOut b)
                        ~> TFragmentStream n a
                        ~> TFrameBuffer n t
                        ~> TFrameBuffer n t
  "FrameBuffer"         --> \a t t' n
                        -> [t' ~~ TFFTRepr' t, IsValidFrameBuffer @@ t, TFrameBuffer n t ~~ TFFrameBuffer a]
                        ==> a ~> TFrameBuffer n t'
  "ScreenOut"           --> \a b -> TFrameBuffer a b ~> TOutput
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
  ["PrimBShiftL", "PrimBShiftR"]        ---> \d a b t -> [IsIntegral @@ t, a ~~ vecS d t, b ~~ vecS d TWord] ==> a ~> b ~> a
  ["PrimBShiftLS", "PrimBShiftRS"]      ---> \d a t -> [IsIntegral @@ t, a ~~ vecS d t] ==> a ~> TWord ~> a
  -- Logic Functions
  ["PrimAnd", "PrimOr", "PrimXor"]      ---> TBool ~> TBool ~> TBool
  ["PrimNot"]                           ---> \d a   -> [a ~~ vecS d TBool] ==> a ~> a
  ["PrimAny", "PrimAll"]                ---> \d a   -> [a ~~ vecS d TBool] ==> a ~> TBool
  -- Angle, Trigonometry and Exponential Functions
  ["PrimACos", "PrimACosH", "PrimASin", "PrimASinH", "PrimATan", "PrimATanH", "PrimCos", "PrimCosH", "PrimDegrees", "PrimRadians", "PrimSin", "PrimSinH", "PrimTan", "PrimTanH", "PrimExp", "PrimLog", "PrimExp2", "PrimLog2", "PrimSqrt", "PrimInvSqrt"]
                                        ---> \d a   -> [a ~~ floatVecS d] ==> a ~> a
  ["PrimPow", "PrimATan2"]              ---> \d a   -> [a ~~ floatVecS d] ==> a ~> a ~> a
  --ommon Functions
  ["PrimFloor", "PrimTrunc", "PrimRound", "PrimRoundEven", "PrimCeil", "PrimFract"]
                                        ---> \d a   -> [a ~~ floatVecS d] ==> a ~> a
  ["PrimMin", "PrimMax"]                ---> \d a t -> [IsNum @@ t, a ~~ vecS d t] ==> a ~> a ~> a
  ["PrimMinS", "PrimMaxS"]              ---> \d a t -> [IsNum @@ t, a ~~ vecS d t] ==> a ~> t ~> a
  ["PrimIsNan", "PrimIsInf"]            ---> \d a b -> [a ~~ floatVecS d, b ~~ vecS d TBool] ==> a ~> b
  ["PrimAbs", "PrimSign"]               ---> \d a t -> [IsSigned @@ t, a ~~ vecS d t] ==> a ~> a
  "PrimModF"            --> \d a   -> [a ~~ floatVecS d] ==> a ~> TTuple [a, a]
  "PrimClamp"           --> \d a t -> [IsNum @@ t, a ~~ vecS d t] ==> a ~> a ~> a ~> a
  "PrimClampS"          --> \d a t -> [IsNum @@ t, a ~~ vecS d t] ==> a ~> t ~> t ~> a
  "PrimMix"             --> \d a   -> [a ~~ floatVecS d] ==> a ~> a ~> a ~> a
  "PrimMixS"            --> \d a   -> [a ~~ floatVecS d] ==> a ~> a ~> TFloat ~> a
  "PrimMixB"            --> \d a b -> [a ~~ floatVecS d, b ~~ vecS d TBool] ==> a ~> a ~> b ~> a
  "PrimStep"            --> \d a   -> [a ~~ TFVec d TFloat] ==> a ~> a ~> a
  "PrimStepS"           --> \d a   -> [a ~~ floatVecS d] ==> TFloat ~> a ~> a
  "PrimSmoothStep"      --> \d a   -> [a ~~ TFVec d TFloat] ==> a ~> a ~> a ~> a
  "PrimSmoothStepS"     --> \d a   -> [a ~~ floatVecS d] ==> TFloat ~> TFloat ~> a ~> a
  -- Integer/Floatonversion Functions
  "PrimFloatBitsToInt"  --> \d fv iv -> [fv ~~ floatVecS d, iv ~~ vecS d TInt]  ==> fv ~> iv
  "PrimFloatBitsToUInt" --> \d fv uv -> [fv ~~ floatVecS d, uv ~~ vecS d TWord] ==> fv ~> uv
  "PrimIntBitsToFloat"  --> \d fv iv -> [fv ~~ floatVecS d, iv ~~ vecS d TInt]  ==> iv ~> fv
  "PrimUIntBitsToFloat" --> \d fv uv -> [fv ~~ floatVecS d, uv ~~ vecS d TWord] ==> uv ~> fv
  -- Geometric Functions
  "PrimLength"          --> \d a -> [a ~~ floatVecS d] ==> a ~> TFloat
  ["PrimDistance", "PrimDot"]
                        ---> \d a -> [a ~~ floatVecS d] ==> a ~> a ~> TFloat
  "PrimCross"           --> \a    -> [a ~~ floatVecS (TNat 3)] ==> a ~> a ~> a
  "PrimNormalize"       --> \d a  -> [a ~~ floatVecS d] ==> a ~> a
  ["PrimFaceForward", "PrimRefract"]
                        ---> \d a -> [a ~~ floatVecS d] ==> a ~> a ~> a ~> a
  "PrimReflect"         --> \d a  -> [a ~~ floatVecS d] ==> a ~> a ~> a
  -- Matrix Functions
  "PrimTranspose"       --> \a b h w -> [a ~~ TFMat h w, b ~~ TFMat w h] ==> a ~> b
  "PrimDeterminant"     --> \m s   -> [m ~~ TFMat s s] ==> m ~> TFloat
  "PrimInverse"         --> \m s   -> [m ~~ TFMat s s] ==> m ~> m
  "PrimOuterProduct"    --> \m h w -> [m ~~ TFMat h w] ==> w ~> h ~> m
  "PrimMulMatVec"       --> \m h w -> [m ~~ TFMat h w] ==> m ~> w ~> h
  "PrimMulVecMat"       --> \m h w -> [m ~~ TFMat h w] ==> h ~> m ~> w
  "PrimMulMatMat"       --> \a b c i j k -> [a ~~ TFMat i j, b ~~ TFMat j k, c ~~ TFMat i k] ==> a ~> b ~> c
  -- Vector and Scalar Relational Functions
  ["PrimLessThan", "PrimLessThanEqual", "PrimGreaterThan", "PrimGreaterThanEqual", "PrimEqualV", "PrimNotEqualV"]
                        ---> \d a b t -> [IsNum @@ t, a ~~ vecS d t, b ~~ vecS d TBool] ==> a ~> a ~> b
  ["PrimEqual", "PrimNotEqual"]
                        ---> \a t  -> [t ~~ TFMatVecScalarElem a] ==> a ~> a ~> TBool
  -- Fragment Processing Functions
  ["PrimDFdx", "PrimDFdy", "PrimFWidth"]
                        ---> \d a  -> [a ~~ floatVecS d] ==> a ~> a
  -- Noise Functions
  "PrimNoise1"          --> \d a   -> [a ~~ floatVecS d] ==> a ~> TFloat
  forM_ [2..4] $ \i ->
      "PrimNoise" ++ show i  --> \d a b -> [a ~~ floatVecS d, b ~~ floatVecS (TNat i)] ==> a ~> b

fieldProjType :: FName -> TCM (Subst, Typing)
fieldProjType fn = newV $ \a r r' -> return $ [Split r r' (TRecord $ Map.singleton fn a)] ==> r ~> a :: TCM Typing

inferLit :: Lit -> TCM Typing
inferLit a = case a of
  LInt _ -> do
    --t <- newVar
    --return (mempty,[(CNum,t)],t) -- ????
    ty TInt
  LChar   _ -> ty TChar
  LFloat  _ -> ty TFloat
  LString _ -> ty TString
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
    newV_ :: Subst -> a -> TCM (Subst, Typing)

newV = newV_ mempty

instance NewVar (TCM Typing)    where newV_ s = fmap $ (,) s
instance NewVar Typing          where newV_ s t = return (s, t)
instance NewVar Ty              where newV_ s t = return (s, [] ==> t)
instance NewVar a => NewVar (Ty -> a) where
    newV_ s f = do
        v@(TVar n) <- newVar C
        newV_ (Map.insert n v s) $ f v

-- don't use this, use newV instead
newVar :: Frequency -> TCM Ty
newVar f = do
  (d, n: ns) <- get
  put (d, ns)
  return $ TVar n
