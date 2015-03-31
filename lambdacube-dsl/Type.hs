{-# LANGUAGE PatternSynonyms #-}
module Type where

import Data.Map (Map)

type TName = String
type EName = String
type MonoEnv = Map EName Ty
type PolyEnv = Map EName Typing
type InstEnv = [(Constraint,Ty)]
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
  | EVar      a Subst EName
  | EApp      a Subst (Exp a) (Exp a)
  | ELam      a EName (Exp a)
  | ELet      a EName (Exp a) (Exp a)
  | ETuple    a [Exp a]
--  | EFix EName Exp
  deriving (Show,Eq,Ord)

type Subst = Map TName Ty

getTag :: Show a => Exp a -> a
getTag (ELit      r _) = r
getTag (EVar      r _ _) = r
getTag (EApp      r _ _ _) = r
getTag (ELam      r _ _) = r
getTag (ELet      r _ _ _) = r
getTag (ETuple    r _) = r

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

data Ty -- star kind
  = TVar    Frequency TName
  | TArr    Ty Ty
  -- composit
  | TTuple  Frequency [Ty]
  | TArray  Frequency Ty
  -- type family
  | TFun    TName [Ty]
  -- primitive types
  | TChar   Frequency
  | TString Frequency
  | TBool   Frequency
  | TWord   Frequency
  | TInt    Frequency
  | TFloat  Frequency

  -- lambdacube types
  | TNat    Int

  -- Semantic
  | Depth   Ty
  | Stencil Ty
  | Color   Ty

  -- PrimitiveType
  | TTriangle
  | TLine
  | TPoint
  | TTriangleAdjacency
  | TLineAdjacency

  -- Vector/Matrix
  | TV2B    Frequency
  | TV3B    Frequency
  | TV4B    Frequency
  | TV2U    Frequency
  | TV3U    Frequency
  | TV4U    Frequency
  | TV2I    Frequency
  | TV3I    Frequency
  | TV4I    Frequency
  | TV2F    Frequency
  | TV3F    Frequency
  | TV4F    Frequency
  | TM22F   Frequency
  | TM23F   Frequency
  | TM24F   Frequency
  | TM32F   Frequency
  | TM33F   Frequency
  | TM34F   Frequency
  | TM42F   Frequency
  | TM43F   Frequency
  | TM44F   Frequency

  -- ADT
  | TCullMode               Frequency
  | TPolygonMode            Frequency
  | TPolygonOffset          Frequency
  | TProvokingVertex        Frequency
  | TFrontFace              Frequency
  | TPointSize              Frequency
  | TBlendingFactor         Frequency
  | TBlendEquation          Frequency
  | TLogicOperation         Frequency
  | TStencilOperation       Frequency
  | TComparisonFunction     Frequency
  | TPointSpriteCoordOrigin Frequency

  -- GADT
  | TAccumulationContext  Frequency Ty
  | TBlending             Frequency Ty
  | TFetchPrimitive       Frequency PrimitiveType
  | TFragmentFilter       Frequency Ty
  | TFragmentOperation    Frequency Semantic
  | TFragmentOut          Frequency Semantic
  | TFragmentStream       Frequency Nat Ty
  | TFrameBuffer          Frequency -- ???
  | TImage                Frequency Nat -- Semantic -- TODO: ignore semantic temporarly
  | TInput                Frequency Ty
  | TInterpolated         Frequency Ty -- ???
  | TOutput               Frequency
  | TPrimitiveStream      Frequency PrimitiveType Nat Frequency Ty -- ???
  | TRasterContext        Frequency PrimitiveType
  | TVertexOut            Frequency Ty -- ???
  | TVertexStream         Frequency PrimitiveType Ty
  deriving (Show,Eq,Ord)

data Constraint
  = CNum
  | CTextual
  -- lc constraints
  | IsComponent
  | IsFloating
  | IsIntegral
  | IsNum
  | IsNumComponent
  | IsSigned
  deriving (Show,Eq,Ord)

