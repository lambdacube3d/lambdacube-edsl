{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Type where

import Data.Map (Map)
import Data.Foldable
import Data.Traversable

type TName = String
type EName = String
type FName = String
type MonoEnv = Map EName Ty
type PolyEnv = Map EName Typing
type ClassInstEnv = [ClassConstraint Ty]
type EqInstEnv = [EqConstraint Ty]
type InstEnv = (ClassInstEnv, EqInstEnv)
type Typing = (MonoEnv,InstEnv,Ty)
type Env = (PolyEnv,MonoEnv,InstEnv)

data Lit
  = LInt    Integer
  | LChar   Char
  | LString String
  | LFloat  Double
  | LNat    Int
  deriving (Show,Eq,Ord)

data Exp a
  = ELit      a Lit
  | EVar      a EName
  | EApp      a (Exp a) (Exp a)
  | ELam      a EName (Exp a)
  | ELet      a EName (Exp a) (Exp a)
  | ETuple    a [Exp a]
  | ERecord   a [(FName,Exp a)]
  | EFieldProj a (Exp a) FName
  | ESubst    a Subst (Exp a)
--  | EFix EName Exp
  deriving (Show,Eq,Ord)

type Subst = Map TName Ty

getTag :: Show a => Exp a -> a
getTag (ELit      r _) = r
getTag (EVar      r _) = r
getTag (EApp      r _ _) = r
getTag (ELam      r _ _) = r
getTag (ELet      r _ _ _) = r
getTag (ETuple    r _) = r
getTag (ERecord   r _) = r
getTag (EFieldProj r _ _) = r
getTag (ESubst    r _ _) = r

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

infixr 7 ~>
a ~> b = TArr a b

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

data ClassConstraint a
  = CClass Class a
  deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

data EqConstraint a
  = CEq a (TypeFun a)  -- CEq t f   ~~~  t ~ TFun f
  | Split a a a
  deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

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
  deriving (Show,Eq,Ord)

data TypeFun a
  = TFMat a a               -- TODO: data family
  | TFMatVecElem a
  | TFMatVecScalarElem a
  | TFVec a a               -- TODO: data family
  | TFVecScalar a a         -- injective in both params
  | TFFTRepr' a
  | TFColorRepr a
  | TFFrameBuffer a
  | TFFragOps a
  deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

