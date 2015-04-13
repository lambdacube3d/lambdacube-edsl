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

data Typing_ a = Typing
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

newtype Pat a = Pat (Pat_ a (Pat a))
  deriving (Show,Eq,Ord)

data Pat_ a b
  = PLit_ a Lit
  | PVar_ a EName
  | PCon_ a EName [b]
  | PTuple_ a [b]
  | PRecord_ a [(FName, b)]
  | Wildcard_ a
  deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

pattern PLit a b = Pat (PLit_ a b)
pattern PVar a b = Pat (PVar_ a b)
pattern PCon a b c = Pat (PCon_ a b c)
pattern PTuple a b = Pat (PTuple_ a b)
pattern PRecord a b = Pat (PRecord_ a b)
pattern Wildcard a = Pat (Wildcard_ a)

getTagP :: Pat a -> a
getTagP = \case
    PLit      a x       -> a
    PVar      a x       -> a
    PCon      a n x     -> a
    PTuple    a x       -> a
    PRecord   a x       -> a
    Wildcard  a         -> a

setTagP :: a -> Pat_ x b -> Pat_ a b
setTagP a = \case
    PLit_      _ x       -> PLit_ a x
    PVar_      _ x       -> PVar_ a x
    PCon_      _ n x     -> PCon_ a n x
    PTuple_    _ x       -> PTuple_ a x
    PRecord_   _ x       -> PRecord_ a x
    Wildcard_  _         -> Wildcard_ a

newtype Exp a = Exp (Exp_ a (Exp a))
  deriving (Show,Eq,Ord)

data Exp_ a b
  = ELit_      a Lit
  | EVar_      a EName
  | EApp_      a b b
  | ELam_      a (Pat a) b
  | ELet_      a (Pat a) b b
  | ECase_     a b [(Pat a, b)]
  | ETuple_    a [b]
  | ERecord_   a [(FName, b)]
  | EFieldProj_ a FName
  | ETyping_   a b Typing
--  | EFix EName Exp
  deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

pattern ELit a b = Exp (ELit_ a b)
pattern EVar a b = Exp (EVar_ a b)
pattern EApp a b c = Exp (EApp_ a b c)
pattern ELam a b c = Exp (ELam_ a b c)
pattern ELet a b c d = Exp (ELet_ a b c d)
pattern ECase a b c = Exp (ECase_ a b c)
pattern ETuple a b = Exp (ETuple_ a b)
pattern ERecord a b = Exp (ERecord_ a b)
pattern EFieldProj a c = Exp (EFieldProj_ a c)
pattern ETyping a b c = Exp (ETyping_ a b c)

getTag :: Exp a -> a
getTag = \case
    ELit      r _ -> r
    EVar      r _ -> r
    EApp      r _ _ -> r
    ELam      r _ _ -> r
    ELet      r _ _ _ -> r
    ECase     r _ _ -> r
    ETuple    r _ -> r
    ERecord   r _ -> r
    EFieldProj r _ -> r
    ETyping   r _ _ -> r

setTag :: (Pat x -> Pat a) -> a -> Exp_ x b -> Exp_ a b
setTag f a = \case
    ELit_      _ x       -> ELit_ a x
    EVar_      _ x       -> EVar_ a x
    EApp_      _ x y     -> EApp_ a x y
    ELam_      _ x y     -> ELam_ a (f x) y
    ELet_      _ x y z   -> ELet_ a (f x) y z
    ECase_     _ x y     -> ECase_ a x (map (f *** id) y)
    ETuple_    _ x       -> ETuple_ a x
    ERecord_   _ x       -> ERecord_ a x
    EFieldProj_ _ x      -> EFieldProj_ a x
    ETyping_   _ x y     -> ETyping_ a x y

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

newtype Ty = Ty (Ty_ Ty)
  deriving (Show,Eq,Ord)

data Ty_ a
  -- star kind
  = TVar_    Frequency TName
  | TCon_    Frequency TCName [a]     -- saturated type constructor application
  | TArr_    a a
  | Forall_ TName a
  | TConstraintArg_ (Constraint a) a
  -- composit
  | TTuple_  Frequency [a]
  | TRecord_ (Map FName a)
  | TArray_  Frequency a
  -- type families are placed in constraints
  -- | TFun    (TypeFun a)
  -- primitive types
  | TChar_   Frequency
  | TString_ Frequency
  | TBool_   Frequency
  | TWord_   Frequency
  | TInt_    Frequency
  | TFloat_  Frequency
  | TVec_    Int a          -- invariant property: Int = 2,3,4;  a = Bool, Int, Word, Float
  | TMat_    Int Int a      -- invariant property: Int = 2,3,4;  a = Float

  -- lambdacube types
  | TNat_    Int

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

pattern TVar a b = Ty (TVar_ a b)
pattern TCon f a b = Ty (TCon_ f a b)
pattern TArr a b = Ty (TArr_ a b)
pattern Forall a b = Ty (Forall_ a b)
pattern TConstraintArg a b = Ty (TConstraintArg_ a b)
pattern TTuple a b = Ty (TTuple_ a b)
pattern TRecord b = Ty (TRecord_ b)
pattern TArray a b = Ty (TArray_ a b)
pattern TChar a = Ty (TChar_ a)
pattern TString a = Ty (TString_ a)
pattern TBool a = Ty (TBool_ a)
pattern TWord a = Ty (TWord_ a)
pattern TInt a = Ty (TInt_ a)
pattern TFloat a = Ty (TFloat_ a)
pattern TNat a = Ty (TNat_ a)
pattern TVec a b = Ty (TVec_ a b)
pattern TMat a b c = Ty (TMat_ a b c)

-- Semantic
pattern Depth a = TCon C "Depth" [a]
pattern Stencil a = TCon C "Stencil" [a]
pattern Color a = TCon C "Color" [a]

-- PrimitiveType
pattern TTriangle = TCon C "Triangle" []
pattern TLine = TCon C "Line" []
pattern TPoint = TCon C "Point" []
pattern TTriangleAdjacency = TCon C "TriangleAdjacency" []
pattern TLineAdjacency = TCon C "LineAdjacency" []

-- ADT
pattern TCullMode a = TCon a "CullMode" []
pattern TPolygonMode a = TCon a "PolygonMode" []
pattern TPolygonOffset a = TCon a "PolygonOffset" []
pattern TProvokingVertex a = TCon a "ProvokingVertex" []
pattern TFrontFace a = TCon a "FrontFace" []
pattern TPointSize a = TCon a "PointSize" []
pattern TBlendingFactor a = TCon a "BlendingFactor" []
pattern TBlendEquation a = TCon a "BlendEquation" []
pattern TLogicOperation a = TCon a "LogicOperation" []
pattern TStencilOperation a = TCon a "StencilOperation" []
pattern TComparisonFunction a = TCon a "ComparisonFunction" []
pattern TPointSpriteCoordOrigin a = TCon a "PointSpriteCoordOrigin" []

-- GADT
pattern TAccumulationContext a b = TCon a "AccumulationContext" [b]
pattern TBlending a b = TCon a "Blending" [b]
pattern TFetchPrimitive a b = TCon a "FetchPrimitive" [b]
pattern TFragmentFilter a b = TCon a "FragmentFilter" [b]
pattern TFragmentOperation a b = TCon a "FragmentOperation" [b]
pattern TFragmentOut a b = TCon a "FragmentOut" [b]
pattern TFragmentStream a b c = TCon a "FragmentStream" [b, c]
pattern TFrameBuffer a b c = TCon a "FrameBuffer" [b, c]
pattern TImage a b c = TCon a "Image" [b, c]
pattern TInput a b = TCon a "Input" [b]
pattern TInterpolated a b = TCon a "Interpolated" [b]
pattern TOutput a = TCon a "Output" []
pattern TRasterContext a b = TCon a "RasterContext" [b]
pattern TVertexOut a b = TCon a "VertexOut" [b]
pattern TVertexStream a b c = TCon a "VertexStream" [b, c]

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
    freeVars (TVar _ a) = Set.singleton a
    freeVars (Ty x) = foldMap freeVars x

instance FreeVars a => FreeVars [a]                 where freeVars = foldMap freeVars
instance FreeVars a => FreeVars (Typing_ a)         where freeVars = foldMap freeVars
instance FreeVars a => FreeVars (TypeFun a)         where freeVars = foldMap freeVars
instance FreeVars a => FreeVars (MonoEnv a)         where freeVars = foldMap freeVars
instance FreeVars a => FreeVars (Constraint a)      where freeVars = foldMap freeVars

