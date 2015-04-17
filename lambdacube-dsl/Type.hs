{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Type where

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Foldable hiding (foldr)
import Data.Traversable
import Data.ByteString (ByteString)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.RWS
import Control.Arrow
import Text.Trifecta.Delta (Delta)

type TName = String
type TCName = String    -- type constructor name; if this turns out to be slow use Int or ADT instead of String
type EName = String
type FName = String

type Subst = Map TName Ty
type MonoEnv a = Map EName a
newtype PolyEnv = PolyEnv { getPolyEnv :: Map EName (TCM (Subst, Typing)) }
    deriving Monoid
type Typing = Typing_ Ty

data Typing_ a
  = Typing
    { monoEnv     :: MonoEnv a
    , constraints :: [Constraint a]
    , typingType  :: a
    , polyVars    :: Set TName      -- subset of free vars; tells which free vars should be instantiated
    }
  deriving (Show,Eq,Ord,Functor,Foldable,Traversable)
{-
instance Show Typing where
    show (Typing me cs t) = sh me' " |- " $ sh cs' " => " t'
      where
        sh "" _ x = x
        sh a b x = a ++ b ++ x

        t' = show t
        cs' | null cs = ""
            | otherwise = "(" ++ intercalate ", " (map show cs) ++ ")"
        me' = case Map.toList me of
            [] -> ""
            xs -> "[" ++ intercalate ", " (map f xs) ++ "]"
        f (n, t) = n ++ " :: " ++ show t
-}
type Range = (Delta, Delta)

type ErrorMsg = ByteString -> String    -- complete source -> message

-- type checking monad
type TCM =
    RWST (PolyEnv, [Range]) () ([Maybe ErrorMsg], [TName]) -- (poly env, stack of ranges) (unamb check results, typevar names)
    (Except ErrorMsg)

data Lit
  = LInt    Integer
  | LChar   Char
  | LString String
  | LFloat  Double
  | LNat    Int
  deriving (Show,Eq,Ord)

data Pat a = Pat a (Pat_ (Pat a))
  deriving (Show,Eq,Ord)

data Pat_ b
  = PLit_ Lit
  | PVar_ EName
  | PCon_ EName [b]
  | PTuple_ [b]
  | PRecord_ [(FName, b)]
  | Wildcard_
  deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

pattern PVar a b = Pat a (PVar_ b)
pattern PLit a b = Pat a (PLit_ b)
pattern PCon a b c = Pat a (PCon_ b c)
pattern PTuple a b = Pat a (PTuple_ b)
pattern PRecord a b = Pat a (PRecord_ b)
pattern Wildcard a = Pat a Wildcard_

getTagP :: Pat a -> a
getTagP (Pat a _) = a

data Exp a = Exp a (Exp_ a (Exp a))
  deriving (Show,Eq,Ord)

data Exp_ a b
  = ELit_      Lit
  | EVar_      EName
  | EApp_      b b
  | ELam_      (Pat a) b
  | ELet_      (Pat a) b b
  | ECase_     b [(Pat a, b)]
  | ETuple_    [b]
  | ERecord_   [(FName, b)]
  | EFieldProj_ FName
  | ETyping_   b Typing
--  | EFix EName Exp
  deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

pattern ELit a b = Exp a (ELit_ b)
pattern EVar a b = Exp a (EVar_ b)
pattern EApp a b c = Exp a (EApp_ b c)
pattern ELam a b c = Exp a (ELam_ b c)
pattern ELet a b c d = Exp a (ELet_ b c d)
pattern ECase a b c = Exp a (ECase_ b c)
pattern ETuple a b = Exp a (ETuple_ b)
pattern ERecord a b = Exp a (ERecord_ b)
pattern EFieldProj a c = Exp a (EFieldProj_ c)
pattern ETyping a b c = Exp a (ETyping_ b c)

getTag :: Exp a -> a
getTag (Exp a _) = a

setTag :: (Pat x -> Pat a) -> Exp_ x b -> Exp_ a b
setTag f = \case
    ELit_      x       -> ELit_ x
    EVar_      x       -> EVar_ x
    EApp_      x y     -> EApp_ x y
    ELam_      x y     -> ELam_ (f x) y
    ELet_      x y z   -> ELet_ (f x) y z
    ECase_     x y     -> ECase_ x (map (f *** id) y)
    ETuple_    x       -> ETuple_ x
    ERecord_   x       -> ERecord_ x
    EFieldProj_ x      -> EFieldProj_ x
    ETyping_   x y     -> ETyping_ x y

data Frequency -- frequency kind
  -- frequency values
  = C
  | O
  | V
  | F
  -- type family representation
  | FVar TName
  | FMax [Frequency]
  deriving (Show,Eq,Ord)

type Semantic = Ty
type PrimitiveType = Ty
type Nat = Ty

data Ty
    = StarToStar !Int
    | Ty_ Ty (Ty_ Ty)
  deriving (Show,Eq,Ord)

data Ty' a = Ty' a (Ty_ (Ty' a))
  deriving (Show,Eq,Ord)

getTagT (Ty' k _) = k

data Ty_ a
  -- kinds
  = Star_       -- type of types
  | NatKind_    -- type of TNat
  -- constraint kinds
  | ClassCK_
  | EqCK_ a a
  | TFunCK_ a
  --
  | TVar_    Frequency TName
  | TApp_    a a
  | TCon_    Frequency TCName
  | TArr_    a a
  | Forall_ TName a
  | TConstraintArg_ (Constraint a) a
  -- composit
  | TTuple_  Frequency [a]
  | TRecord_ (Map FName a)
  -- type families are placed in constraints
  -- | TFun    (TypeFun a)

  -- lambdacube types
  | TNat_    Int
  | TVec_    Int a          -- invariant property: Int = 2,3,4;  a = Bool, Int, Word, Float
  | TMat_    Int Int a      -- invariant property: Int = 2,3,4;  a = Float

  -- GADT/special (two frequencies)
  | TPrimitiveStream_      Frequency a{-PrimitiveType-} a{-Nat-} Frequency a -- ???
{-
  | TFetchPrimitive_       Frequency a{-PrimitiveType-}
  | TFragmentOperation_    Frequency a{-Semantic-}
  | TFragmentOut_          Frequency a{-Semantic-}
  | TFragmentStream_       Frequency a{-Nat-} a
  | TFrameBuffer_          Frequency a{-Nat-} a
  | TImage_                Frequency a{-Nat-} a{-Semantic-}
  | TRasterContext_        Frequency a{-PrimitiveType-}
  | TVertexOut_            Frequency a -- ???
  | TVertexStream_         Frequency a{-PrimitiveType-} a
-}
  deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

pattern StarT = StarToStar 0
pattern StarStar = StarToStar 1
pattern Ty a = Ty_ StarT a

pattern Star = Ty Star_
pattern NatKind = Ty NatKind_
pattern TVar a b = Ty (TVar_ a b)
pattern TApp k a b = Ty_ k (TApp_ a b)
pattern TCon k f a = Ty_ k (TCon_ f a)
pattern TCon0 f a = Ty (TCon_ f a)
pattern TCon1 f a b = TApp StarT (TCon StarStar f a) b
pattern TCon2 f a b c = TApp StarT (TApp StarStar (TCon (StarToStar 2) f a) b) c
pattern TArr a b = Ty (TArr_ a b)
pattern Forall a b = Ty (Forall_ a b)
pattern TConstraintArg a b = Ty (TConstraintArg_ a b)
pattern TTuple a b = Ty (TTuple_ a b)
pattern TRecord b = Ty (TRecord_ b)

pattern TNat a = Ty_ NatKind (TNat_ a)
pattern TVec a b = Ty (TVec_ a b)
pattern TMat a b c = Ty (TMat_ a b c)

-- basic types
pattern TChar a = TCon0 a "Char"
pattern TString a = TCon0 a "String"
pattern TBool a = TCon0 a "Bool"
pattern TWord a = TCon0 a "Word"
pattern TInt a = TCon0 a "Int"
pattern TFloat a = TCon0 a "Float"
pattern TArray a b = TCon1 a "Array" b

-- Semantic
pattern Depth a = TCon1 C "Depth" a
pattern Stencil a = TCon1 C "Stencil" a
pattern Color a = TCon1 C "Color" a

-- PrimitiveType
pattern TTriangle = TCon0 C "Triangle"
pattern TLine = TCon0 C "Line"
pattern TPoint = TCon0 C "Point"
pattern TTriangleAdjacency = TCon0 C "TriangleAdjacency"
pattern TLineAdjacency = TCon0 C "LineAdjacency"

-- ADT
pattern TCullMode a = TCon0 a "CullMode"
pattern TPolygonMode a = TCon0 a "PolygonMode"
pattern TPolygonOffset a = TCon0 a "PolygonOffset"
pattern TProvokingVertex a = TCon0 a "ProvokingVertex"
pattern TFrontFace a = TCon0 a "FrontFace"
pattern TPointSize a = TCon0 a "PointSize"
pattern TBlendingFactor a = TCon0 a "BlendingFactor"
pattern TBlendEquation a = TCon0 a "BlendEquation"
pattern TLogicOperation a = TCon0 a "LogicOperation"
pattern TStencilOperation a = TCon0 a "StencilOperation"
pattern TComparisonFunction a = TCon0 a "ComparisonFunction"
pattern TPointSpriteCoordOrigin a = TCon0 a "PointSpriteCoordOrigin"

-- GADT
pattern TAccumulationContext a b = TCon1 a "AccumulationContext" b
pattern TBlending a b = TCon1 a "Blending" b
pattern TFetchPrimitive a b = TCon1 a "FetchPrimitive" b
pattern TFragmentFilter a b = TCon1 a "FragmentFilter" b
pattern TFragmentOperation a b = TCon1 a "FragmentOperation" b
pattern TFragmentOut a b = TCon1 a "FragmentOut" b
pattern TFragmentStream a b c = TCon2 a "FragmentStream" b c
pattern TFrameBuffer a b c = TCon2 a "FrameBuffer" b c
pattern TImage a b c = TCon2 a "Image" b c
pattern TInput a b = TCon1 a "Input" b
pattern TInterpolated a b = TCon1 a "Interpolated" b
pattern TOutput a = TCon0 a "Output"
pattern TRasterContext a b = TCon1 a "RasterContext" b
pattern TVertexOut a b = TCon1 a "VertexOut" b
pattern TVertexStream a b c = TCon2 a "VertexStream" b c

pattern TPrimitiveStream a b c d e = Ty (TPrimitiveStream_ a b c d e)

infixr 7 ~>, ~~>
a ~> b = TArr a b

(~~>) :: [Ty] -> Ty -> Ty
args ~~> res = foldr (~>) res args

data Constraint a
  = CEq a (TypeFun a)   -- unification between a type and a fully applied type function; CEq t f:  t ~ f
  | CUnify a a          -- unification between (non-type-function) types; CUnify t s:  t ~ s
  | CClass Class a      -- class constraint
  | Split a a a         -- Split x y z:  x, y, z are records; fields of x = disjoint union of the fields of y and z
  deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

infix 6 ==>
cs ==> t = Typing mempty cs t (freeVars cs <> freeVars t)

infix 4 ~~
(~~) = CEq

infix 4 ~~~
(~~~) = CUnify

infix 9 @@
(@@) = CClass

data Class
  = CNum
  | CTextual
  -- lc constraints
  | IsComponent
  | IsFloating
  | IsIntegral
  | IsNum
  | IsNumComponent
  | IsSigned
  | IsValidOutput
  | IsTypeLevelNatural
  | IsValidFrameBuffer
  | IsInputTuple
  deriving (Show,Eq,Ord)

data TypeFun a
  = TFMat a a               -- may be data family
  | TFMatVecElem a
  | TFMatVecScalarElem a
  | TFVec a a               -- may be data family
  | TFVecScalar a a
  | TFFTRepr' a
  | TFColorRepr a
  | TFFrameBuffer a
  | TFFragOps a
  | TFJoinTupleType a a
  deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

class FreeVars a where freeVars :: a -> Set TName

instance FreeVars Ty where
    freeVars = \case
        TVar _ a -> Set.singleton a
        Ty_ k x -> freeVars k `mappend` foldMap freeVars x
        StarToStar _ -> mempty

instance FreeVars a => FreeVars [a]                 where freeVars = foldMap freeVars
instance FreeVars a => FreeVars (Typing_ a)         where freeVars = foldMap freeVars
instance FreeVars a => FreeVars (TypeFun a)         where freeVars = foldMap freeVars
instance FreeVars a => FreeVars (MonoEnv a)         where freeVars = foldMap freeVars
instance FreeVars a => FreeVars (Constraint a)      where freeVars = foldMap freeVars

