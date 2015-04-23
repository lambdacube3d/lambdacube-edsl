{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module CoreToGLSL where

import Debug.Trace
import Text.Show.Pretty (ppShow)
import Text.PrettyPrint.HughesPJClass (pPrint)

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
import GLSLUtil
import qualified Language.GLSL.Syntax as GLSL
import qualified Language.GLSL.Pretty as GLSL

toGLSLType t = show $ pPrint $ case t of
  TBool             -> GLSL.Bool
  TWord             -> GLSL.UInt
  TInt              -> GLSL.Int
  TFloat            -> GLSL.Float
  TVec 2 (TBool)    -> GLSL.BVec2
  TVec 3 (TBool)    -> GLSL.BVec3
  TVec 4 (TBool)    -> GLSL.BVec4
  TVec 2 (TWord)    -> GLSL.UVec2
  TVec 3 (TWord)    -> GLSL.UVec3
  TVec 4 (TWord)    -> GLSL.UVec4
  TVec 2 (TInt)     -> GLSL.IVec2
  TVec 3 (TInt)     -> GLSL.IVec3
  TVec 4 (TInt)     -> GLSL.IVec4
  TVec 2 (TFloat)   -> GLSL.Vec2
  TVec 3 (TFloat)   -> GLSL.Vec3
  TVec 4 (TFloat)   -> GLSL.Vec4
  TMat 2 2 (TFloat) -> GLSL.Mat2
  TMat 2 3 (TFloat) -> GLSL.Mat2x3
  TMat 2 4 (TFloat) -> GLSL.Mat2x4
  TMat 3 2 (TFloat) -> GLSL.Mat3x2
  TMat 3 3 (TFloat) -> GLSL.Mat3
  TMat 3 4 (TFloat) -> GLSL.Mat3x4
  TMat 4 2 (TFloat) -> GLSL.Mat4x2
  TMat 4 3 (TFloat) -> GLSL.Mat4x3
  TMat 4 4 (TFloat) -> GLSL.Mat4

pattern ELString s = ELit (LString s)

genUniforms e = case e of
  A1 "Uni" (A1 _ (ELString s)) -> Set.singleton [unwords ["uniform",toGLSLType $ tyOf e,s,";"]]
  Exp e -> F.foldMap genUniforms e

genStreamInput (VarE n t) = tell [unwords ["in",toGLSLType t,n,";"]]

genStreamOutput (A1 i a@(toGLSLType . tyOf -> t)) = do
  let f "Smooth" = "smooth"
      f "Flat" = "flat"
      f "NoPerspective" = "noperspective"
  tell [unwords [f i,"out",t,"v0",";"]]
  return [(f i,t,"v0")]

genFragmentInput s = tell [unwords [i,"in",t,n,";"] | (i,t,n) <- s]
genFragmentOutput (toGLSLType . tyOf -> t) = tell [unwords ["out",t,"f0",";"]]

genVertexGLSL :: Exp -> ([(String,String,String)],String)
genVertexGLSL e@(ELam i (A4 "VertexOut" p s c o)) = id *** unlines $ runWriter $ do
  tell ["#version 330 core"]
  F.mapM_ tell $ genUniforms e
  genStreamInput i
  out <- genStreamOutput o
  tell ["void main() {"]
  tell $ ["v0 = " <> unwords (genGLSL o) <> ";"]
  tell $ ["gl_Position = " <> unwords (genGLSL p) <> ";"]
  tell $ ["gl_PointSize = " <> unwords (genGLSL s) <> ";"]
  tell ["}"]
  return out

genFragmentGLSL :: [(String,String,String)] -> Exp -> String
genFragmentGLSL s e@(ELam (VarE i _) (A1 "FragmentOutRastDepth" o)) = unlines $ execWriter $ do
  tell ["#version 330 core"]
  F.mapM_ tell $ genUniforms e
  genFragmentInput s
  genFragmentOutput o
  tell ["void main() {"]
  tell $ ["f0 = " <> unwords (genGLSLSubst (let [(_,_,n)] = s in Map.singleton i n) o) <> ";"]
  tell ["}"]


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

  Exp e -> F.foldMap (genGLSLSubst s) e

isMatrix :: Ty -> Bool
isMatrix (TMat{}) = True
isMatrix _ = False

isIntegral :: Ty -> Bool
isIntegral TWord = True
isIntegral TInt = True
isIntegral (TVec _ TWord) = True
isIntegral (TVec _ TInt) = True
isIntegral _ = False
