{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
module CoreToGLSL where

import Debug.Trace
import Text.Show.Pretty (ppShow)

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Arrow
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Foldable (Foldable)
import qualified Data.Foldable as F

import Type hiding (ELet, EApp, ELam, EVar, ELit, ETuple, Exp, Exp_ (..), Pat, PVar, PLit, PTuple)
import Core

toGLSLType msg t = case t of
  TBool  -> "bool"
  TWord  -> "uint"
  TInt   -> "int"
  TFloat -> "float"
  TVec 2 (TBool) -> "bvec2"
  TVec 3 (TBool) -> "bvec3"
  TVec 4 (TBool) -> "bvec4"
  TVec 2 (TWord) -> "uvec2"
  TVec 3 (TWord) -> "uvec3"
  TVec 4 (TWord) -> "uvec4"
  TVec 2 (TInt) -> "ivec2"
  TVec 3 (TInt) -> "ivec3"
  TVec 4 (TInt) -> "ivec4"
  TVec 2 (TFloat) -> "vec2"
  TVec 3 (TFloat) -> "vec3"
  TVec 4 (TFloat) -> "vec4"
  TMat 2 2 (TFloat) -> "mat2"
  TMat 2 3 (TFloat) -> "mat2x3"
  TMat 2 4 (TFloat) -> "mat2x4"
  TMat 3 2 (TFloat) -> "mat3x2"
  TMat 3 3 (TFloat) -> "mat3"
  TMat 3 4 (TFloat) -> "mat3x4"
  TMat 4 2 (TFloat) -> "mat4x2"
  TMat 4 3 (TFloat) -> "mat4x3"
  TMat 4 4 (TFloat) -> "mat4"
  TTuple []         -> "void"
  t -> error $ "toGLSLType: " ++ msg ++ " " ++ ppShow t

pattern ELString s = ELit (LString s)

genUniforms e = case e of
  A1 "Uni" (A1 _ (ELString s)) -> Set.singleton [unwords ["uniform",toGLSLType "1" $ tyOf e,s,";"]]
  Exp e -> F.foldMap genUniforms e

genStreamInput i = do
  let input (PVar (x@(VarE n t))) = tell [unwords ["in",toGLSLType (ppShow x ++ "\n") t,n,";"]] >> return [n]
      input a = error $ "genStreamInput " ++ ppShow a
  case i of
    PTuple l -> foldM (\a b -> (a ++) <$> input b) [] l
    x -> input x

genStreamOutput a = do
  let f "Smooth" = "smooth"
      f "Flat" = "flat"
      f "NoPerspective" = "noperspective"
      go n (A1 i (toGLSLType "3" . tyOf -> t)) = do
        let var = "v" <> show n
        tell [unwords [f i,"out",t,var,";"]] >> return [(f i,t,var)]
  case a of
    ETuple l -> concat <$> sequence (map (uncurry go) $ zip [0..] l)
    x -> go 0 x

genFragmentInput s = tell [unwords [i,"in",t,n,";"] | (i,t,n) <- s]
genFragmentOutput a@(toGLSLType "4" . tyOf -> t) = case tyOf a of
  TUnit -> return False
  _ -> tell [unwords ["out",t,"f0",";"]] >> return True

genVertexGLSL :: Exp -> (([String],[(String,String,String)]),String)
genVertexGLSL e@(ELam i (A4 "VertexOut" p s c o)) = id *** unlines $ runWriter $ do
  tell ["#version 330 core"]
  F.mapM_ tell $ genUniforms e
  input <- genStreamInput i
  out <- genStreamOutput o
  tell ["void main() {"]
  unless (null out) $ do
    let go ((_,_,var),x) = tell $ [var <> " = " <> unwords (genGLSL x) <> ";"]
    case o of
      ETuple l -> mapM_ go $ zip out l
      x -> let [out1] = out in go (out1,x)
  tell $ ["gl_Position = " <> unwords (genGLSL p) <> ";"]
  tell $ ["gl_PointSize = " <> unwords (genGLSL s) <> ";"]
  tell ["}"]
  return (input,out)
genVertexGLSL e = error $ "genVertexGLSL: " ++ ppShow e

genFragmentGLSL :: [(String,String,String)] -> Exp -> String
genFragmentGLSL s e@(ELam i fragOut) = unlines $ execWriter $ do
  let o = case fragOut of
        A1 "FragmentOutRastDepth" o -> o
        A1 "FragmentOut" o -> o
      makeSubst (PVar (VarE x _)) [(_,_,n)] = Map.singleton x n
      makeSubst (PTuple l) x = Map.fromList $ go l x where
        go [] [] = []
        go (PVar (VarE x _):al) ((_,_,n):bl) = (x,n) : go al bl
        go _ _ = error $ "genFragmentGLSL illegal input " ++ ppShow (i,s)
  tell ["#version 330 core"]
  F.mapM_ tell $ genUniforms e
  genFragmentInput s
  hasOutput <- genFragmentOutput o
  tell ["void main() {"]
  when hasOutput $ do
    tell $ ["f0 = " <> unwords (genGLSLSubst (makeSubst i s) o) <> ";"]
  tell ["}"]
genFragmentGLSL _ e = error $ "genFragmentGLSL: " ++ ppShow e


genGLSL :: Exp -> [String]
genGLSL = genGLSLSubst mempty

parens a = ["("] <> a <> [")"]
binOp s o a b = parens (genGLSLSubst s a) <> [o] <> parens (genGLSLSubst s b)
functionCall s f a = [f,"(",intercalate "," (map (unwords . genGLSLSubst s) a),")"]

-- todo: (on hold) name mangling to prevent name collisions
-- todo: reader monad
genGLSLSubst :: Map String String -> Exp -> [String]
genGLSLSubst s e = case e of
  ELit (LInt a) -> [show a]
  ELit (LFloat a) -> [show a]
  ELit (LChar a) -> [show a]
  ELit (LString a) -> [show a]
  EVar (VarE a _) -> [Map.findWithDefault a a s]
  A1 "Uni" (A1 _ (ELString s)) -> [s]
  A1 "Smooth" a -> genGLSLSubst s a
  A1 "Flat" a -> genGLSLSubst s a
  A1 "NoPerspecitve" a -> genGLSLSubst s a
  A1 "Const" a -> genGLSLSubst s a
  A4 "V4F" a b c d -> functionCall s "vec4" [a,b,c,d]
  --ETuple a -> ["*TUPLE*"]
  -- Primitive Functions
  A2 "PrimMulMatVec" a b -> binOp s "*" a b

  -- Arithmetic Functions
  A2 "PrimAdd" a b -> binOp s "+" a b
  A2 "PrimAddS" a b -> binOp s "+" a b
  A2 "PrimSub" a b -> binOp s "-" a b
  A2 "PrimSubS" a b -> binOp s "-" a b
  A2 "PrimMul" a b
    | all (isMatrix . tyOf) [a,b] -> functionCall s "matrixCompMult" [a,b]
    | otherwise -> binOp s "*" a b
  A2 "PrimMulS" a b -> binOp s "*" a b
  A2 "PrimDiv" a b -> binOp s "/" a b
  A2 "PrimDivS" a b -> binOp s "/" a b
  A1 "PrimNeg" a -> ["-"] <> parens (genGLSLSubst s a)
  A2 "PrimMod" a b
    | all (isIntegral . tyOf) [a,b] -> binOp s "%" a b
    | otherwise -> functionCall s "mod" [a,b]
  A2 "PrimModS" a b
    | all (isIntegral . tyOf) [a,b] -> binOp s "%" a b
    | otherwise -> functionCall s "mod" [a,b]

  -- Bit-wise Functions
  A2 "PrimBAnd" a b -> binOp s "&" a b
  A2 "PrimBAndS" a b -> binOp s "&" a b
  A2 "PrimBOr" a b -> binOp s "|" a b
  A2 "PrimBOrS" a b -> binOp s "|" a b
  A2 "PrimBXor" a b -> binOp s "^" a b
  A2 "PrimBXorS" a b -> binOp s "^" a b
  A1 "PrimBNot" a -> ["~"] <> parens (genGLSLSubst s a)
  A2 "PrimBShiftL" a b -> binOp s "<<" a b
  A2 "PrimBShiftLS" a b -> binOp s "<<" a b
  A2 "PrimBShiftR" a b -> binOp s ">>" a b
  A2 "PrimBShiftRS" a b -> binOp s ">>" a b

  -- Logic Functions
  A2 "PrimAnd" a b -> binOp s "&&" a b
  A2 "PrimOr" a b -> binOp s "||" a b
  A2 "PrimXor" a b -> binOp s "^" a b
  A1 "PrimNot" a
    | all (isScalar . tyOf) [a] -> ["!"] <> parens (genGLSLSubst s a)
    | otherwise -> functionCall s "not" [a]
  A1 "PrimAny" a -> functionCall s "any" [a]
  A1 "PrimAll" a -> functionCall s "all" [a]

  -- Angle and Trigonometry Functions
  A1 "PrimACos" a -> functionCall s "acos" [a]
  A1 "PrimACos" a -> functionCall s "acos" [a]
  A1 "PrimACosH" a -> functionCall s "acosh" [a]
  A1 "PrimASin" a -> functionCall s "asin" [a]
  A1 "PrimASinH" a -> functionCall s "asinh" [a]
  A1 "PrimATan" a -> functionCall s "atan" [a]
  A2 "PrimATan2" a b -> functionCall s "atan" [a,b]
  A1 "PrimATanH" a -> functionCall s "atanh" [a]
  A1 "PrimCos" a -> functionCall s "cos" [a]
  A1 "PrimCosH" a -> functionCall s "cosh" [a]
  A1 "PrimDegrees" a -> functionCall s "degrees" [a]
  A1 "PrimRadians" a -> functionCall s "radians" [a]
  A1 "PrimSin" a -> functionCall s "sin" [a]
  A1 "PrimSinH" a -> functionCall s "sinh" [a]
  A1 "PrimTan" a -> functionCall s "tan" [a]
  A1 "PrimTanH" a -> functionCall s "tanh" [a]

  -- Exponential Functions
  A2 "PrimPow" a b -> functionCall s "pow" [a,b]
  A1 "PrimExp" a -> functionCall s "exp" [a]
  A1 "PrimLog" a -> functionCall s "log" [a]
  A1 "PrimExp2" a -> functionCall s "exp2" [a]
  A1 "PrimLog2" a -> functionCall s "log2" [a]
  A1 "PrimSqrt" a -> functionCall s "sqrt" [a]
  A1 "PrimInvSqrt" a -> functionCall s "inversesqrt" [a]

  -- Common Functions
  A1 "PrimIsNan" a -> functionCall s "isnan" [a]
  A1 "PrimIsInf" a -> functionCall s "isinf" [a]
  A1 "PrimAbs" a -> functionCall s "abs" [a]
  A1 "PrimSign" a -> functionCall s "sign" [a]
  A1 "PrimFloor" a -> functionCall s "floor" [a]
  A1 "PrimTrunc" a -> functionCall s "trunc" [a]
  A1 "PrimRound" a -> functionCall s "round" [a]
  A1 "PrimRoundEven" a -> functionCall s "roundEven" [a]
  A1 "PrimCeil" a -> functionCall s "ceil" [a]
  A1 "PrimFract" a -> functionCall s "fract" [a]
  A1 "PrimModF" a -> error "PrimModF is not implemented yet!" -- TODO
  A2 "PrimMin" a b -> functionCall s "min" [a,b]
  A2 "PrimMinS" a b -> functionCall s "min" [a,b]
  A2 "PrimMax" a b -> functionCall s "max" [a,b]
  A2 "PrimMaxS" a b -> functionCall s "max" [a,b]
  A3 "PrimClamp" a b c -> functionCall s "clamp" [a,b,c]
  A3 "PrimClampS" a b c -> functionCall s "clamp" [a,b,c]
  A3 "PrimMix" a b c -> functionCall s "mix" [a,b,c]
  A3 "PrimMixS" a b c -> functionCall s "mix" [a,b,c]
  A3 "PrimMixB" a b c -> functionCall s "mix" [a,b,c]
  A2 "PrimStep" a b -> functionCall s "step" [a,b]
  A2 "PrimStepS" a b -> functionCall s "step" [a,b]
  A3 "PrimSmoothStep" a b c -> functionCall s "smoothstep" [a,b,c]
  A3 "PrimSmoothStepS" a b c -> functionCall s "smoothstep" [a,b,c]

  -- Integer/Float Conversion Functions
  A1 "PrimFloatBitsToInt" a -> functionCall s "floatBitsToInt" [a]
  A1 "PrimFloatBitsToUInt" a -> functionCall s "floatBitsToUint" [a]
  A1 "PrimIntBitsToFloat" a -> functionCall s "intBitsToFloat" [a]
  A1 "PrimUIntBitsToFloat" a -> functionCall s "uintBitsToFloat" [a]

  -- Geometric Functions
  A1 "PrimLength" a -> functionCall s "length" [a]
  A2 "PrimDistance" a b -> functionCall s "distance" [a,b]
  A2 "PrimDot" a b -> functionCall s "dot" [a,b]
  A2 "PrimCross" a b -> functionCall s "cross" [a,b]
  A1 "PrimNormalize" a -> functionCall s "normalize" [a]
  A3 "PrimFaceForward" a b c -> functionCall s "faceforward" [a,b,c]
  A2 "PrimReflect" a b -> functionCall s "reflect" [a,b]
  A3 "PrimRefract" a b c -> functionCall s "refract" [a,b,c]

  -- Matrix Functions
  A1 "PrimTranspose" a -> functionCall s "transpose" [a]
  A1 "PrimDeterminant" a -> functionCall s "determinant" [a]
  A1 "PrimInverse" a -> functionCall s "inverse" [a]
  A2 "PrimOuterProduct" a b -> functionCall s "outerProduct" [a,b]
  A2 "PrimMulMatVec" a b -> binOp s "*" a b
  A2 "PrimMulVecMat" a b -> binOp s "*" a b
  A2 "PrimMulMatMat" a b -> binOp s "*" a b

  -- Vector and Scalar Relational Functions
  A2 "PrimLessThan" a b
    | all (isScalarNum . tyOf) [a,b] -> binOp s "<" a b
    | otherwise -> functionCall s "lessThan" [a,b]
  A2 "PrimLessThanEqual" a b
    | all (isScalarNum . tyOf) [a,b] -> binOp s "<=" a b
    | otherwise -> functionCall s "lessThanEqual" [a,b]
  A2 "PrimGreaterThan" a b
    | all (isScalarNum . tyOf) [a,b] -> binOp s ">" a b
    | otherwise -> functionCall s "greaterThan" [a,b]
  A2 "PrimGreaterThanEqual" a b
    | all (isScalarNum . tyOf) [a,b] -> binOp s ">=" a b
    | otherwise -> functionCall s "greaterThanEqual"   [a,b]
  A2 "PrimEqualV" a b
    | all (isScalar . tyOf) [a,b] -> binOp s "==" a b
    | otherwise -> functionCall s "equal" [a,b]
  A2 "PrimEqual" a b -> binOp s "==" a b
  A2 "PrimNotEqualV" a b
    | all (isScalar . tyOf) [a,b] -> binOp s "!=" a b
    | otherwise -> functionCall s "notEqual" [a,b]
  A2 "PrimNotEqual" a b -> binOp s "!=" a b

  -- Fragment Processing Functions
  A1 "PrimDFdx" a -> functionCall s "dFdx" [a]
  A1 "PrimDFdy" a -> functionCall s "dFdy" [a]
  A1 "PrimFWidth" a -> functionCall s "fwidth" [a]

  -- Noise Functions
  A1 "PrimNoise1" a -> functionCall s "noise1" [a]
  A1 "PrimNoise2" a -> functionCall s "noise2" [a]
  A1 "PrimNoise3" a -> functionCall s "noise3" [a]
  A1 "PrimNoise4" a -> functionCall s "noise4" [a]

  -- TODO: Texture Lookup Functions
  A1 "PrimV3FToV4F" a -> ["vec4("] <> genGLSLSubst s a <> [",1.0)"]
  x -> error $ "genGLSLSubst - unknown primitive " ++ ppShow x

isMatrix :: Ty -> Bool
isMatrix (TMat{}) = True
isMatrix _ = False

isIntegral :: Ty -> Bool
isIntegral TWord = True
isIntegral TInt = True
isIntegral (TVec _ TWord) = True
isIntegral (TVec _ TInt) = True
isIntegral _ = False

isScalarNum :: Ty -> Bool
isScalarNum ty = elem ty [TInt, TWord, TFloat]

isScalar :: Ty -> Bool
isScalar ty = elem ty [TBool, TInt, TWord, TFloat]
