{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Type where

import Data.Map (Map)
import Data.Monoid
import Data.Foldable
import Data.Traversable
import Data.ByteString (ByteString)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.RWS
import Control.Arrow
import Text.Trifecta.Delta (Delta)

type TName = String
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
    }
  deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

type Range = (Delta, Delta)

type ErrorMsg = ByteString -> String    -- complete source -> message

-- type checking monad
type TCM =
    RWST (PolyEnv, [Range]) () ([Maybe ErrorMsg], Int) -- (poly env, stack of ranges) (unamb check results, typevar counter)
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
  | ESubst_    a Subst b
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
pattern ESubst a b c = Exp (ESubst_ a b c)

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
    ESubst    r _ _ -> r

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
    ESubst_    _ x y     -> ESubst_ a x y

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
  | TArr_    a a
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

  -- lambdacube types
  | TNat_    Int

  -- Semantic
  | Depth_   a
  | Stencil_ a
  | Color_   a

  -- PrimitiveType
  | TTriangle_
  | TLine_
  | TPoint_
  | TTriangleAdjacency_
  | TLineAdjacency_

  -- Vector/Matrix
  | TV2B_    Frequency
  | TV3B_    Frequency
  | TV4B_    Frequency
  | TV2U_    Frequency
  | TV3U_    Frequency
  | TV4U_    Frequency
  | TV2I_    Frequency
  | TV3I_    Frequency
  | TV4I_    Frequency
  | TV2F_    Frequency
  | TV3F_    Frequency
  | TV4F_    Frequency
  | TM22F_   Frequency
  | TM23F_   Frequency
  | TM24F_   Frequency
  | TM32F_   Frequency
  | TM33F_   Frequency
  | TM34F_   Frequency
  | TM42F_   Frequency
  | TM43F_   Frequency
  | TM44F_   Frequency

  -- ADT
  | TCullMode_               Frequency
  | TPolygonMode_            Frequency
  | TPolygonOffset_          Frequency
  | TProvokingVertex_        Frequency
  | TFrontFace_              Frequency
  | TPointSize_              Frequency
  | TBlendingFactor_         Frequency
  | TBlendEquation_          Frequency
  | TLogicOperation_         Frequency
  | TStencilOperation_       Frequency
  | TComparisonFunction_     Frequency
  | TPointSpriteCoordOrigin_ Frequency

  -- GADT
  | TAccumulationContext_  Frequency a
  | TBlending_             Frequency a
  | TFetchPrimitive_       Frequency a{-PrimitiveType-}
  | TFragmentFilter_       Frequency a
  | TFragmentOperation_    Frequency a{-Semantic-}
  | TFragmentOut_          Frequency a{-Semantic-}
  | TFragmentStream_       Frequency a{-Nat-} a
  | TFrameBuffer_          Frequency a{-Nat-} a
  | TImage_                Frequency a{-Nat-} a{-Semantic-}
  | TInput_                Frequency a
  | TInterpolated_         Frequency a
  | TOutput_               Frequency
  | TPrimitiveStream_      Frequency a{-PrimitiveType-} a{-Nat-} Frequency a -- ???
  | TRasterContext_        Frequency a{-PrimitiveType-}
  | TVertexOut_            Frequency a -- ???
  | TVertexStream_         Frequency a{-PrimitiveType-} a
  deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

pattern TVar a b = Ty (TVar_ a b)
pattern TArr a b = Ty (TArr_ a b)
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
pattern Depth a = Ty (Depth_ a)
pattern Stencil a = Ty (Stencil_ a)
pattern Color a = Ty (Color_ a)
pattern TTriangle = Ty TTriangle_
pattern TLine = Ty TLine_
pattern TPoint = Ty TPoint_
pattern TTriangleAdjacency = Ty TTriangleAdjacency_
pattern TLineAdjacency = Ty TLineAdjacency_
pattern TV2B a = Ty (TV2B_ a)
pattern TV3B a = Ty (TV3B_ a)
pattern TV4B a = Ty (TV4B_ a)
pattern TV2U a = Ty (TV2U_ a)
pattern TV3U a = Ty (TV3U_ a)
pattern TV4U a = Ty (TV4U_ a)
pattern TV2I a = Ty (TV2I_ a)
pattern TV3I a = Ty (TV3I_ a)
pattern TV4I a = Ty (TV4I_ a)
pattern TV2F a = Ty (TV2F_ a)
pattern TV3F a = Ty (TV3F_ a)
pattern TV4F a = Ty (TV4F_ a)
pattern TM22F a = Ty (TM22F_ a)
pattern TM23F a = Ty (TM23F_ a)
pattern TM24F a = Ty (TM24F_ a)
pattern TM32F a = Ty (TM32F_ a)
pattern TM33F a = Ty (TM33F_ a)
pattern TM34F a = Ty (TM34F_ a)
pattern TM42F a = Ty (TM42F_ a)
pattern TM43F a = Ty (TM43F_ a)
pattern TM44F a = Ty (TM44F_ a)
pattern TCullMode a = Ty (TCullMode_ a)
pattern TPolygonMode a = Ty (TPolygonMode_ a)
pattern TPolygonOffset a = Ty (TPolygonOffset_ a)
pattern TProvokingVertex a = Ty (TProvokingVertex_ a)
pattern TFrontFace a = Ty (TFrontFace_ a)
pattern TPointSize a = Ty (TPointSize_ a)
pattern TBlendingFactor a = Ty (TBlendingFactor_ a)
pattern TBlendEquation a = Ty (TBlendEquation_ a)
pattern TLogicOperation a = Ty (TLogicOperation_ a)
pattern TStencilOperation a = Ty (TStencilOperation_ a)
pattern TComparisonFunction a = Ty (TComparisonFunction_ a)
pattern TPointSpriteCoordOrigin a = Ty (TPointSpriteCoordOrigin_ a)
pattern TAccumulationContext a b = Ty (TAccumulationContext_ a b)
pattern TBlending a b = Ty (TBlending_ a b)
pattern TFetchPrimitive a b = Ty (TFetchPrimitive_ a b)
pattern TFragmentFilter a b = Ty (TFragmentFilter_ a b)
pattern TFragmentOperation a b = Ty (TFragmentOperation_ a b)
pattern TFragmentOut a b = Ty (TFragmentOut_ a b)
pattern TFragmentStream a b c = Ty (TFragmentStream_ a b c)
pattern TFrameBuffer a b c = Ty (TFrameBuffer_ a b c)
pattern TImage a b c = Ty (TImage_ a b c)
pattern TInput a b = Ty (TInput_ a b)
pattern TInterpolated a b = Ty (TInterpolated_ a b)
pattern TOutput a = Ty (TOutput_ a)
pattern TPrimitiveStream a b c d e = Ty (TPrimitiveStream_ a b c d e)
pattern TRasterContext a b = Ty (TRasterContext_ a b)
pattern TVertexOut a b = Ty (TVertexOut_ a b)
pattern TVertexStream a b c = Ty (TVertexStream_ a b c)

data Constraint a
  = CEq a (TypeFun a)  -- CEq t f   ~~~  t ~ TFun f
  | CClass Class a
  | Split a a a
  deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

infixr 7 ~>
a ~> b = TArr a b

infix 6 ==>
cs ==> t = Typing mempty cs t

infix 4 ~~
(~~) = CEq

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

isValidOutput      = CClass IsValidOutput
isNum              = CClass CNum
isSigned           = CClass IsSigned
isIntegral         = CClass IsIntegral
isTypeLevelNatural = CClass IsTypeLevelNatural
isFloating         = CClass IsFloating

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

