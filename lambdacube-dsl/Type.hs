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
import Control.Applicative
import Control.Arrow
import Text.Trifecta.Delta (Delta)

type TName = String
type TCName = String    -- type constructor name; if this turns out to be slow use Int or ADT instead of String
type EName = String
type FName = String
type MName = String     -- module name

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

data Pat a = Pat a (Pat_ EName EName (Pat a))
  deriving (Show,Eq,Ord)

data Pat_ c v b
  = PLit_ Lit
  | PVar_ v
  | PCon_ c [b]
  | PTuple_ [b]
  | PRecord_ [(FName, b)]
  | PAt_ v b
  | Wildcard_
  deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

pattern PVar a b = Pat a (PVar_ b)
pattern PLit a b = Pat a (PLit_ b)
pattern PCon a b c = Pat a (PCon_ b c)
pattern PTuple a b = Pat a (PTuple_ b)
pattern PRecord a b = Pat a (PRecord_ b)
pattern Wildcard a = Pat a Wildcard_

data Exp a = Exp a (Exp_ EName (Typing_ (Ty' a)) (Pat a) (Exp a))
  deriving (Show,Eq,Ord)

data Exp_ v t p b
  = ELit_      Lit
  | EVar_      v
  | EApp_      b b
  | ELam_      p b
  | ELet_      p b b
  | ECase_     b [(p, b)]       -- Not used
  | ETuple_    [b]
  | ERecord_   (Maybe FName) [(FName, b)]
  | EFieldProj_ FName
  | ETyping_   b t
--  | EFix EName Exp
  | EAlts_     [b]  -- function alternatives
  | ENext_     -- go to next alternative
  | EType_     t
  | EConstraint_ (Constraint t)  -- TODO: wittnesses here if needed
  deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

pattern ELit a b = Exp a (ELit_ b)
pattern EVar a b = Exp a (EVar_ b)
pattern EApp a b c = Exp a (EApp_ b c)
pattern ELam a b c = Exp a (ELam_ b c)
pattern ELet a b c d = Exp a (ELet_ b c d)
pattern ECase a b c = Exp a (ECase_ b c)
pattern ETuple a b = Exp a (ETuple_ b)
pattern ERecord a b = Exp a (ERecord_ Nothing b)
pattern ENamedRecord a n b = Exp a (ERecord_ (Just n) b)
pattern EFieldProj a c = Exp a (EFieldProj_ c)
pattern ETyping a b c = Exp a (ETyping_ b c)
pattern EAlts a b = Exp a (EAlts_ b)
pattern ENext a = Exp a ENext_

setTag :: (t -> t') -> (p -> p') -> Exp_ v t p b -> Exp_ v t' p' b
setTag tf f = \case
    ELit_      x       -> ELit_ x
    EVar_      x       -> EVar_ x
    EApp_      x y     -> EApp_ x y
    ELam_      x y     -> ELam_ (f x) y
    ELet_      x y z   -> ELet_ (f x) y z
    ECase_     x y     -> ECase_ x (map (f *** id) y)
    ETuple_    x       -> ETuple_ x
    ERecord_   m x     -> ERecord_ m x
    EFieldProj_ x      -> EFieldProj_ x
    ETyping_   x y     -> ETyping_ x $ tf y
    EAlts_     x       -> EAlts_ x
    ENext_             -> ENext_
--    EType

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

ty_ :: Ty -> Ty_ Ty -> Ty
ty_ = Ty_ --Star (StarToStar

convTy :: Ty' (Subst, Typing) -> Ty
convTy (Ty' (_, k) t) = ty_ (typingType{-TODO-} k) $ convTy <$> t

data Ty_ a
  -- kinds
  = Star_ Frequency      -- type of types
  | NatKind_    -- type of TNat
  -- constraint kinds
  | ClassCK_
  | EqCK_ a a
  | TFunCK_ a
  --
  | TVar_    TName
  | TApp_    a a
  | TCon_    TCName
  | TArr_    a a
  | Forall_ TName a
  | TConstraintArg_ (Constraint a) a
  -- composit
  | TTuple_  [a]
  | TRecord_ (Map FName a)
  -- type families are placed in constraints
  -- | TFun    (TypeFun a)

  -- lambdacube types
  | TNat_    Int
--  | TVec_    Int a          -- invariant property: Int = 2,3,4;  a = Bool, Int, Word, Float
--  | TMat_    Int Int a      -- invariant property: Int = 2,3,4;  a = Float

  -- GADT/special (two frequencies)
--  | TPrimitiveStream_      Frequency a{-PrimitiveType-} a{-Nat-} Frequency a -- ???
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

pattern Star = StarToStar 0
pattern StarStar = StarToStar 1
pattern Ty a = Ty_ Star a

pattern NatKind = Ty NatKind_
pattern TVar b = Ty (TVar_ b)
pattern TApp k a b = Ty_ k (TApp_ a b)
pattern TCon k a = Ty_ k (TCon_ a)
pattern TCon0 a = TCon Star a
pattern TCon1 a b = TApp Star (TCon StarStar a) b
pattern TCon2 a b c = TApp Star (TApp StarStar (TCon (StarToStar 2) a) b) c
pattern TCon2' a b c = TApp Star (TApp StarStar (TCon (NatKind `TArr` StarStar) a) b) c
pattern TCon3 a b c d = TApp Star (TApp StarStar (TApp (StarToStar 2) (TCon (StarToStar 3) a) b) c) d
pattern TArr a b <- Ty (TArr_ a b) where
    TArr Star Star = StarStar
    TArr a b = Ty (TArr_ a b)
pattern Forall a b = Ty (Forall_ a b)
pattern TConstraintArg a b = Ty (TConstraintArg_ a b)
pattern TTuple b = Ty (TTuple_ b)
pattern TUnit = TTuple []
pattern TRecord b = Ty (TRecord_ b)

pattern TNat a = Ty_ NatKind (TNat_ a)
pattern VecKind = TArr NatKind StarStar
pattern TVec a b = TApp Star (TApp StarStar (TCon VecKind "Vec") (TNat a)) b
pattern MatKind = TArr NatKind (TArr NatKind StarStar)
pattern TMat a b c = TApp Star (TApp StarStar (TApp VecKind (TCon MatKind "Mat") (TNat a)) (TNat b)) c

-- basic types
pattern TChar = TCon0 "Char"
pattern TString = TCon0 "String"
pattern TBool = TCon0 "Bool"
pattern TWord = TCon0 "Word"
pattern TInt = TCon0 "Int"
pattern TFloat = TCon0 "Float"
pattern TArray b = TCon1 "Array" b
pattern TList a = TCon1 "[]" a
pattern TMaybe a = TCon1 "Maybe" a

-- Semantic
pattern Depth a = TCon1 "Depth" a
pattern Stencil a = TCon1 "Stencil" a
pattern Color a = TCon1 "Color" a

-- PrimitiveType
pattern TTriangle = TCon0 "Triangle"
pattern TLine = TCon0 "Line"
pattern TPoint = TCon0 "Point"
pattern TTriangleAdjacency = TCon0 "TriangleAdjacency"
pattern TLineAdjacency = TCon0 "LineAdjacency"

-- ADT
pattern TCullMode = TCon0 "CullMode"
pattern TPolygonMode = TCon0 "PolygonMode"
pattern TPolygonOffset = TCon0 "PolygonOffset"
pattern TProvokingVertex = TCon0 "ProvokingVertex"
pattern TFrontFace = TCon0 "FrontFace"
pattern TPointSize = TCon0 "PointSize"
pattern TBlendingFactor = TCon0 "BlendingFactor"
pattern TBlendEquation = TCon0 "BlendEquation"
pattern TLogicOperation = TCon0 "LogicOperation"
pattern TStencilOperation = TCon0 "StencilOperation"
pattern TComparisonFunction = TCon0 "ComparisonFunction"
pattern TPointSpriteCoordOrigin = TCon0 "PointSpriteCoordOrigin"

-- GADT
pattern TAccumulationContext b = TCon1 "AccumulationContext" b
pattern TBlending b = TCon1 "Blending" b
pattern TFetchPrimitive b = TCon1 "FetchPrimitive" b
pattern TFragmentFilter b = TCon1 "FragmentFilter" b
pattern TFragmentOperation b = TCon1 "FragmentOperation" b
pattern TFragmentOut b = TCon1 "FragmentOut" b
pattern TFragmentStream b c = TCon2 "FragmentStream" b c
pattern TFrameBuffer b c = TCon2' "FrameBuffer" b c
pattern TImage b c = TCon2 "Image" b c
pattern TInput b = TCon1 "Input" b
pattern TInterpolated b = TCon1 "Interpolated" b
pattern TOutput = TCon0 "Output"
pattern TRasterContext b = TCon1 "RasterContext" b
pattern TVertexOut b = TCon1 "VertexOut" b
pattern TVertexStream b c = TCon2 "VertexStream" b c
pattern TPrimitiveStream a b c = TCon3 "PrimitiveStream" a b c

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

instance FreeVars a => FreeVars (Ty_ a) where
    freeVars = \case
        TVar_ a -> Set.singleton a
        x -> foldMap freeVars x
instance FreeVars a => FreeVars (Ty' a) where
    freeVars = \case
        Ty' k x -> freeVars k `mappend` freeVars x
instance FreeVars Ty where
    freeVars = \case
        Ty_ k x -> freeVars k `mappend` freeVars x
        StarToStar _ -> mempty
instance FreeVars Range where freeVars = mempty -- TODO: eliminate

instance FreeVars a => FreeVars [a]                 where freeVars = foldMap freeVars
instance FreeVars a => FreeVars (Typing_ a)         where freeVars = foldMap freeVars
instance FreeVars a => FreeVars (TypeFun a)         where freeVars = foldMap freeVars
instance FreeVars a => FreeVars (MonoEnv a)         where freeVars = foldMap freeVars
instance FreeVars a => FreeVars (Constraint a)      where freeVars = foldMap freeVars


-- qualified things
data Q a = Q {qualifier :: [String], qData :: a} -- | UQ a
    deriving (Show)

type Prec = Map EName (FixityDir, Int)

-- AST
data Module a
  = Module
  { moduleImports :: [Q MName]
  , moduleExports :: ()
  , typeAliases   :: ()
  , definitions   :: [ValueDef a]
  , dataDefs      :: [DataDef a]
  , typeClasses   :: ()
  , instances     :: ()
  , precedences   :: Prec
  }
    deriving (Show)

type ValueDef a = (Pat a, Exp a)
data DataDef a = DataDef String [String] [ConDef a]
    deriving (Show)
data ConDef a = ConDef EName [FieldTy (Ty' a)]
    deriving (Show)
data FieldTy a
    = FieldTy {fieldName :: Maybe EName, fieldType :: a}
    deriving (Show)

data TypeClassDefinition
  = TypeClassDefinition -- name, [base class], [type signature (declaration)]

data TypeClassInstance -- name, type, [definition]
  = TypeClassInstance

data FixityDir = FNoDir | FDLeft | FDRight
    deriving (Show)


class GetTag c where getTag :: c a -> a

instance GetTag Exp where getTag (Exp a _) = a
instance GetTag Ty' where getTag (Ty' k _) = k
instance GetTag Pat where getTag (Pat a _) = a

