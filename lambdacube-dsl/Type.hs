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

import Data.Char
import Data.Maybe
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
import Text.Show.Pretty

type TName = String
type TCName = String    -- type constructor name; if this turns out to be slow use Int or ADT instead of String
type EName = String
type FName = String
type MName = String     -- module name
type ClassName = String

primed = ('\'':)

-- TODO
isConstr n@(c:_) = isUpper c || c == ':' -- TODO

type Subst = Map TName Ty
type MonoEnv a = Map EName a
data PolyEnv = PolyEnv
    { instanceDefs :: Map Class (Set Ty)
    , getPolyEnv :: Map EName (TCM STyping)
    }

{- typing

poly vars      env                                        type
|              |                                          |
poly a b ... . {a :: k, x :: ty, C ty, ty ~ F ty, ...} -> ty

Administration of poly vars are needed for instantiation,
which replaces poly vars with fresh variables.

Questions:
 - Only type variables may be polymorphic?

Examples

I)
    x + y  :: {a :: *, x :: a, y :: a, Num a} -> a                 a is fresh

II)
    (\x -> 2 * x) :: poly a . {a :: *, Num a} -> a -> a

III)
    flipImage :: poly i . {i :: Nat} -> Image i -> Image i
-}
data Typing_ a
  = Typing
    { monoEnv     :: MonoEnv a
    , constraints :: [Constraint a]
    , typingType  :: a
    , polyVars    :: Set TName      -- subset of free vars; tells which free vars should be instantiated
    }
  deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

type Typing = Typing_ Ty
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

mapPat :: (c -> c') -> (v -> v') -> Pat_ c v b -> Pat_ c' v' b
mapPat f g = \case
    PLit_ l -> PLit_ l
    PVar_ v -> PVar_ $ g v
    PCon_ c p -> PCon_ (f c) p
    PTuple_ p -> PTuple_ p
    PRecord_ p -> PRecord_ p
    PAt_ v p -> PAt_ (g v) p
    Wildcard_ -> Wildcard_

pattern PAt t a b = Pat t (PAt_ a b)
pattern PVar a b = Pat a (PVar_ b)
pattern PLit a b = Pat a (PLit_ b)
pattern PCon a b c = Pat a (PCon_ b c)
pattern PTuple a b = Pat a (PTuple_ b)
pattern PRecord a b = Pat a (PRecord_ b)
pattern Wildcard a = Pat a Wildcard_

data Exp a = Exp a (Exp_ EName (Ty' a) (Pat a) (Exp a))
  deriving (Show,Eq,Ord)

data Exp_ v t p b
  = ELit_      Lit
  | EVar_      v
  | EApp_      b b
  | ETyApp_    b t
  | ELam_      p b
  | ELet_      p b b
  | ECase_     b [(p, b)]       -- Not used
  | ETuple_    [b]
  | ERecord_   (Maybe FName) [(FName, b)]
  | EFieldProj_ FName
  | ETyping_   b t
--  | EFix EName Exp
  | EAlts_     Int [b]  -- function alternatives
  | ENext_     -- go to next alternative
  | EType_     t
  | EConstraint_ (Constraint t)  -- TODO: wittnesses here if needed
  deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

pattern ELit a b = Exp a (ELit_ b)
pattern EVar a b = Exp a (EVar_ b)
pattern EApp a b c = Exp a (EApp_ b c)
pattern ETyApp a b c = Exp a (ETyApp_ b c)
pattern ELam a b c = Exp a (ELam_ b c)
pattern ELet a b c d = Exp a (ELet_ b c d)
pattern ECase a b c = Exp a (ECase_ b c)
pattern ETuple a b = Exp a (ETuple_ b)
pattern ERecord a b = Exp a (ERecord_ Nothing b)
pattern ENamedRecord a n b = Exp a (ERecord_ (Just n) b)
pattern EFieldProj a c = Exp a (EFieldProj_ c)
pattern ETyping a b c = Exp a (ETyping_ b c)
pattern EAlts a i b = Exp a (EAlts_ i b)
pattern ENext a = Exp a ENext_

setTag = setTag_ id

setTag_ :: (Show v, Show t, Show p, Show b) => (v -> v') -> (t -> t') -> (p -> p') -> Exp_ v t p b -> Exp_ v' t' p' b
setTag_ vf tf f = \case
    ELit_      x       -> ELit_ x
    EVar_      x       -> EVar_ $ vf x
    EApp_      x y     -> EApp_ x y
    ELam_      x y     -> ELam_ (f x) y
    ELet_      x y z   -> ELet_ (f x) y z
    ECase_     x y     -> ECase_ x (map (f *** id) y)
    ETuple_    x       -> ETuple_ x
    ERecord_   m x     -> ERecord_ m x
    EFieldProj_ x      -> EFieldProj_ x
    ETyping_   x y     -> ETyping_ x $ tf y
    EAlts_     x y     -> EAlts_ x y
    ENext_             -> ENext_
    EType_ t           -> EType_ $ tf t
    EConstraint_ c     -> EConstraint_ $ tf <$> c
--    e   -> error $ "setTag_: " ++ ppShow e

type STyping = (Subst, Typing)

buildLet :: [(Pat STyping, Exp STyping)] -> Exp STyping -> Exp STyping
buildLet es e = foldr (\(p, e) x -> ELet (getTag x) p e x) e es

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
    | Ty Ty (Ty_ Ty)
  deriving (Show,Eq,Ord)

pattern Ty_ a b <- Ty a b where
    Ty_ Star (Star_ C) = Star
    Ty_ a b = Ty a b

typingToTy :: Typing -> Ty
typingToTy ty = foldr forall_ (foldr TConstraintArg (typingType ty) $ constraints ty) $ Set.toList $ polyVars ty
  where
    forall_ n t = Forall n (fromMaybe (error $ "typingToTy: " ++ n) $ Map.lookup (primed n) $ monoEnv ty) t

data Ty' a = Ty' a (Ty_ (Ty' a))
  deriving (Show,Eq,Ord)

data Ty_ a
  -- kinds
  = Star_ Frequency      -- type of types
  -- constraint kinds
  | ClassCK_
  | EqCK_ a a
  | TFunCK_ a
  --
  | TVar_    TName
  | TCon_    TCName
  | TApp_    a a
  | TArr_    a a
  | Forall_ TName a a
  | TConstraintArg_ (Constraint a) a
  -- composit
  | TTuple_  [a]
  | TRecord_ (Map FName a)
  -- type families are placed in constraints
  -- | TFun    (TypeFun a)

  -- lambdacube types
  | TNat_    Int
  deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

pattern Star = StarToStar 0
pattern StarStar = StarToStar 1
pattern TyStar a = Ty_ Star a

pattern TApp k a b = Ty_ k (TApp_ a b)
pattern TCon k a = Ty_ k (TCon_ a)
pattern TVar k b = Ty_ k (TVar_ b)

pattern NatKind = TCon0 "NatKind"
pattern VecKind = TArr NatKind StarStar
pattern MatKind = TArr NatKind (TArr NatKind StarStar)

pattern Forall a b c = TyStar (Forall_ a b c)
pattern TConstraintArg a b = TyStar (TConstraintArg_ a b)
pattern TArr a b <- TyStar (TArr_ a b) where
    TArr Star (StarToStar i) = StarToStar (i + 1)
    TArr a b = TyStar (TArr_ a b)
pattern TCon0 a = TCon Star a
pattern TCon1 a b = TApp Star (TCon StarStar a) b
pattern TCon2 a b c = TApp Star (TApp StarStar (TCon (StarToStar 2) a) b) c
pattern TCon2' a b c = TApp Star (TApp StarStar (TCon VecKind a) b) c
pattern TCon3' a b c d = TApp Star (TApp StarStar (TApp VecKind (TCon (TArr Star VecKind) a) b) c) d

pattern TRecord b = TyStar (TRecord_ b)
pattern TTuple b = TyStar (TTuple_ b)
pattern TUnit = TTuple []
pattern TNat a = Ty_ NatKind (TNat_ a)
pattern TVec a b = TCon2' "Vec" (TNat a) b
pattern TMat a b c = TApp Star (TApp StarStar (TApp VecKind (TCon MatKind "Mat") (TNat a)) (TNat b)) c

-- basic types
pattern TChar = TCon0 "Char"
pattern TString = TCon0 "String"
pattern TBool = TCon0 "Bool"
pattern TWord = TCon0 "Word"
pattern TInt = TCon0 "Int"
pattern TFloat = TCon0 "Float"
pattern TArray b = TCon1 "Array" b
pattern TList a = TCon1 "List" a

-- Semantic
pattern Depth a = TCon1 "Depth" a
pattern Stencil a = TCon1 "Stencil" a
pattern Color a = TCon1 "Color" a

-- GADT
pattern TInput b = TCon1 "Input" b
pattern TFragmentOperation b = TCon1 "FragmentOperation" b
pattern TImage b c = TCon2' "Image" b c
pattern TInterpolated b = TCon1 "Interpolated" b
pattern TFrameBuffer b c = TCon2' "FrameBuffer" b c

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

type Class = ClassName
pattern CNum = "Num"
pattern CTextual = "Textual"
pattern IsComponent = "Component"
pattern IsFloating = "Floating"
pattern IsIntegral = "Integral"
pattern IsNum = "Num"
pattern IsNumComponent = "NumComponent"
pattern IsSigned = "Signed"
pattern IsValidOutput = "ValidOutput"
pattern IsTypeLevelNatural = "TNat"
pattern IsValidFrameBuffer = "ValidFrameBuffer"
pattern IsInputTuple = "InputTuple"

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
    deriving (Eq, Ord, Show)

type Prec = Map EName (FixityDir, Int)

data DataDef a = DataDef String [String] [ConDef a]
    deriving (Show)
data ConDef a = ConDef EName [FieldTy a]
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

type PatR = Pat Range
type ExpR' = Exp Range
type ExpR = Prec -> ExpR'
type TyR = Ty' Range
type DataDefR = DataDef TyR
type ModuleR = Module TyR Range
type DefinitionR = Definition TyR ExpR Range
type WhereRHSR = WhereRHS TyR ExpR Range
type GuardedRHSR = GuardedRHS ExpR

type ModuleT = Module Typing STyping

data Module t a
  = Module
  { moduleImports :: [Q MName]
  , moduleExports :: ()
  , typeAliases   :: ()
  , definitions   :: [Definition t (Exp a) a]
--  , axioms        :: [(String, Ty' a)]
  , typeClasses   :: ()
--  , instances     :: Map Class (Set t)
  , precedences   :: Prec
  , moduleFile    :: FilePath
  }
    deriving (Show)

data WhereRHS t e r= WhereRHS (GuardedRHS e) (Maybe [Definition t e r])
    deriving Show
data GuardedRHS e
    = Guards [(e, e)]
    | NoGuards e
    deriving Show

data Definition t e r
  = PreValueDef (Range, EName) [PatR] (WhereRHS t e r)   -- before group
  | ValueDef (Pat r, e)
  | TypeSig (String, (Ty' r){-FIXME:t-})
  | DDataDef (DataDef t)
  | InstanceDef Class t
  | DFixity EName (FixityDir, Int)
    deriving Show

-----------------------------------------

class GetTag c where getTag :: c a -> a

instance GetTag Exp where getTag (Exp a _) = a
instance GetTag Ty' where getTag (Ty' k _) = k
instance GetTag Pat where getTag (Pat a _) = a

