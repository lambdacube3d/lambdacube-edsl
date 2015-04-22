{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module CoreToGLSL where

import Debug.Trace
import Text.Show.Pretty (ppShow)

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Arrow
import Control.Monad.Writer
import Data.Foldable (Foldable)
import qualified Data.Foldable as F

import Type hiding (ELet, EApp, ELam, EVar, ELit, ETuple, Exp, Exp_ (..), Pat, PVar, PLit, PTuple)
import Core

genUniforms e = case e of
  A1 "Uni" a -> Set.singleton $ (:[]) $ case a of
    A1 "IV4F" (ELit (LString s)) -> unwords ["uniform vec4",s,";"]
    A1 "IM44F" (ELit (LString s)) -> unwords ["uniform mat4",s,";"]
  Exp e -> F.foldMap genUniforms e

toGLSLType t = case t of
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
  TMat 2 2 (TFloat) -> "mat2x2"
  TMat 2 3 (TFloat) -> "mat2x3"
  TMat 2 4 (TFloat) -> "mat2x4"
  TMat 3 2 (TFloat) -> "mat3x2"
  TMat 3 3 (TFloat) -> "mat3x3"
  TMat 3 4 (TFloat) -> "mat3x4"
  TMat 4 2 (TFloat) -> "mat4x2"
  TMat 4 3 (TFloat) -> "mat4x3"
  TMat 4 4 (TFloat) -> "mat4x4"

tyOf :: Exp -> Ty
tyOf (EVar (VarE _ t)) = t
tyOf (EApp (tyOf -> TArr _ t) _) = t
tyOf e = error $ "tyOf " ++ ppShow e

genStreamInput (VarE n t) = tell [unwords ["in",toGLSLType t,n,";"]]

genStreamOutput (A1 "Smooth" a@(toGLSLType . tyOf -> t)) = do
  tell [unwords ["smooth","out",t,"v0",";"]]
  return [("smooth",t,"v0")]

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

parens a = ["("] <> a <> [")"]

genGLSL :: Exp -> [String]
genGLSL = genGLSLSubst mempty

binOp s o a b = parens (genGLSLSubst s a) <> [o] <> parens (genGLSLSubst s b)

genGLSLSubst :: Map String String -> Exp -> [String]
genGLSLSubst s e = case e of
  ELit (LInt a) -> [show a]
  ELit (LFloat a) -> [show a]
  ELit (LChar a) -> [show a]
  ELit (LString a) -> [show a]
  EVar (VarE a _) -> [Map.findWithDefault a a s]
  A1 "Uni" a -> case a of
    A1 "IV4F" (ELit (LString s)) -> [s]
    A1 "IM44F" (ELit (LString s)) -> [s]
  A2 "PrimMulMatVec" a b -> binOp s "*" a b
  A2 "PrimMul" a b -> binOp s "*" a b
  A1 "Smooth" a -> genGLSLSubst s a
  A1 "Const" a -> genGLSLSubst s a
  A4 "V4F" (genGLSLSubst s -> a) (genGLSLSubst s -> b) (genGLSLSubst s -> c) (genGLSLSubst s -> d) -> ["vec4(",intercalate "," (map unwords [a,b,c,d]),")"]
  --ETuple a -> ["*TUPLE*"]
  Exp e -> F.foldMap (genGLSLSubst s) e

