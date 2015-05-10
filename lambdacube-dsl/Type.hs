{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Type where

import Data.Char
import Data.String
import Data.Maybe
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Foldable hiding (foldr)
import Data.Traversable
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Applicative
import Control.Arrow hiding ((<+>))
import Text.Parsec.Pos
import GHC.Exts (Constraint)

import ParserUtil (ParseError)
import Pretty

-------------------------------------------------------------------------------- literals

data Lit
    = LInt    Integer
    | LNat    Int
    | LChar   Char
    | LString String
    | LFloat  Double
    deriving (Eq, Ord)

-------------------------------------------------------------------------------- types

data Ty_ c n a
    = Star_
    | TLit_    Lit
    | TVar_    n
    | TCon_    c
    | TApp_    a a
    -- | TFun_    f [a]    -- TODO
    | Forall_  (Maybe n) a a
    | TTuple_  [a]
    | TRecord_ (Map n a)
    | ConstraintKind_ (Constraint' n a)        -- flatten?
    | Witness  Witness      -- TODO: here or in Exp?
    deriving (Eq,Ord,Functor,Foldable,Traversable)      -- TODO: elim Eq instance

data Constraint' n a
    = CEq a (TypeFun n a) -- unification between a type and a fully applied type function; CEq t f:  t ~ f
    | CUnify a a          -- unification between (non-type-function) types; CUnify t s:  t ~ s
    | CClass n a          -- class constraint
    | Split a a a         -- Split x y z:  x, y, z are records; fields of x = disjoint union of the fields of y and z
    deriving (Eq,Ord,Functor,Foldable,Traversable)

data TypeFun n a = TypeFun n [a]
    deriving (Eq,Ord,Functor,Foldable,Traversable)

data Witness
    = Refl
    -- TODO
    deriving (Eq, Ord)

--------------------------------------------

newtype Ty' c n m = Ty'' (m (Ty_ c n (Ty' c n m)))

pattern Ty' a b = Ty'' (a, b)

-------------------------------------------- kinded types

type Ty = Ty' IdN IdN TyC

data TyC a
    = StarToStarC !Int
    | TyC Ty a

instance Eq Ty where
    StarToStar i == StarToStar j = i == j
    Ty a b == Ty a' b' = (a, b) == (a', b')
    _ == _ = False
instance Ord Ty where
    StarToStar i `compare` StarToStar j = i `compare` j
    Ty a b `compare` Ty a' b' = (a, b) `compare` (a', b')
    StarToStar _ `compare` _ = LT
    _ `compare` _ = GT

pattern StarToStar i = Ty'' (StarToStarC i)
pattern Ty k t = Ty'' (TyC k t)

pattern Ty_ a b <- Ty a b where
    Ty_ Star StarC = Star
    Ty_ a b = Ty a b

unfoldTy (StarToStar i) = Ty Star $ case i of
    0 -> Star_
    _ -> Forall_ Nothing Star $ StarToStar $ i - 1
unfoldTy x = x

pattern Ty__ a b <- (unfoldTy -> Ty a b) where
    Ty__ Star StarC = Star
    Ty__ a b = Ty a b

pattern StarC = Star_
pattern TApp k a b = Ty_ k (TApp_ a b)
pattern TCon k a <- Ty_ k (TCon_ (TypeN a)) where
    TCon k a = Ty_ k (TCon_ (TypeN' a "typecon"))
pattern TVar k b = Ty_ k (TVar_ b)
pattern TLit k b = Ty_ k (TLit_ b)

pattern Star = StarToStar 0
pattern StarStar = StarToStar 1

pattern TyStar a = Ty_ Star a
pattern TRecord b = TyStar (TRecord_ b)
pattern TTuple b = TyStar (TTuple_ b)
pattern TUnit = TTuple []
pattern ConstraintKind c = TyStar (ConstraintKind_ c)
pattern Forall a b c = TyStar (Forall_ (Just a) b c)
pattern TArr a b <- TyStar (Forall_ Nothing a b) where
    TArr Star (StarToStar i) = StarToStar (i + 1)
    TArr a b = TyStar (Forall_ Nothing a b)

infixr 7 ~>, ~~>
a ~> b = TArr a b

(~~>) :: [Ty] -> Ty -> Ty
args ~~> res = foldr (~>) res args

infix 4 ~~, ~~~
(~~) = CEq
(~~~) = CUnify

kindOf :: Ty -> Ty
kindOf = \case
    Ty_ k _ -> k
    StarToStar _ -> Star


-------------------------------------------------------------------------------- patterns

data Pat_ c v b
    = PLit_ Lit
    | PVar_ v
    | PCon_ c [b]
    | PTuple_ [b]
    | PRecord_ [(FName, b)]
    | PAt_ v b
    | Wildcard_
    deriving (Functor,Foldable,Traversable)

mapPat :: (c -> c') -> (v -> v') -> Pat_ c v b -> Pat_ c' v' b
mapPat f g = \case
    PLit_ l -> PLit_ l
    PVar_ v -> PVar_ $ g v
    PCon_ c p -> PCon_ (f c) p
    PTuple_ p -> PTuple_ p
    PRecord_ p -> PRecord_ p
    PAt_ v p -> PAt_ (g v) p
    Wildcard_ -> Wildcard_

--------------------------------------------

newtype Pat' c n m = Pat' (m (Pat_ c n (Pat' c n m)))

pattern Pat a b = Pat' (a, b)
pattern PVar' a b = Pat a (PVar_ b)
pattern PCon' a b c = Pat a (PCon_ b c)

--------------------------------------------

type Pat = Pat' (EName, Ty) Var Identity

pattern Pat'' a = Pat' (Identity a)

pattern PAt v l = Pat'' (PAt_ v l)
pattern PLit l = Pat'' (PLit_ l)
pattern PVar l = Pat'' (PVar_ l)
pattern PCon c l = Pat'' (PCon_ c l)
pattern PTuple l = Pat'' (PTuple_ l)
pattern Wildcard = Pat'' Wildcard_

-------------------------------------------------------------------------------- expressions

data Exp_ v t p b
    = ELit_      Lit      -- could be replaced by EType + ELit
    | EVar_      v
    | EApp_      b b
    | ETuple_    [b]
    | ELam_      p b
    | ETypeSig_  b t
    | EType_     t

    | ELet_      p b b
    | ENamedRecord_ v [(v, b)]
    | ERecord_   [(Name, b)]
    | EFieldProj_ Name
    | EAlts_     Int [b]  -- function alternatives; Int: arity
    | ENext_              -- go to next alternative
    deriving (Functor,Foldable,Traversable)

mapExp :: (v -> v') -> (t -> t') -> (p -> p') -> Exp_ v t p b -> Exp_ v' t' p' b
mapExp vf tf f = \case
    ELit_      x       -> ELit_ x
    EVar_      x       -> EVar_ $ vf x
    EApp_      x y     -> EApp_ x y
    ELam_      x y     -> ELam_ (f x) y
    ELet_      x y z   -> ELet_ (f x) y z
    ETuple_    x       -> ETuple_ x
    ERecord_   x       -> ERecord_ x
    ENamedRecord_ n x  -> ENamedRecord_ (vf n) $ map (vf *** id) x
    EFieldProj_ x      -> EFieldProj_ x
    ETypeSig_  x y     -> ETypeSig_ x $ tf y
    EAlts_     x y     -> EAlts_ x y
    ENext_             -> ENext_
    EType_ t           -> EType_ $ tf t

--------------------------------------------

data Exp' n t p m = Exp' (m (Exp_ n t p (Exp' n t p m)))

pattern Exp a b = Exp' (a, b)
pattern ELit' a b = Exp a (ELit_ b)
pattern EVar' a b = Exp a (EVar_ b)
pattern EApp' a b c = Exp a (EApp_ b c)
pattern ELam' a b c = Exp a (ELam_ b c)
pattern ELet' a b c d = Exp a (ELet_ b c d)
pattern ETuple' a b = Exp a (ETuple_ b)
pattern ERecord' a b = Exp a (ERecord_ b)
pattern ENamedRecord' a n b = Exp a (ENamedRecord_ n b)
pattern EFieldProj' a c = Exp a (EFieldProj_ c)
pattern ETypeSig' a b c = Exp a (ETypeSig_ b c)
pattern EAlts' a i b = Exp a (EAlts_ i b)
pattern ENext' a = Exp a ENext_
pattern EType' a b = Exp a (EType_ b)

--------------------------------------------

type Exp = Exp' Var Ty Pat Identity

pattern Exp'' a = Exp' (Identity a)
pattern ELit a = Exp'' (ELit_ a)
pattern EVar a = Exp'' (EVar_ a)
pattern EApp a b = Exp'' (EApp_ a b)
pattern ETyApp a b = Exp'' (EApp_ a (EType b))
pattern ELam a b = Exp'' (ELam_ a b)
pattern ELet a b c = Exp'' (ELet_ a b c)
pattern ETuple a = Exp'' (ETuple_ a)
pattern ERecord b = Exp'' (ERecord_ b)
pattern EFieldProj a = Exp'' (EFieldProj_ a)
pattern EType a = Exp'' (EType_ a)
pattern EAlts i b = Exp'' (EAlts_ i b)
pattern ENext = Exp'' ENext_

pattern Va x <- VarE (ExpN x) _
pattern A0 x <- EVar (Va x)
pattern A0t x t <- EVar (VarE (ExpN x) t)
pattern At0 x t <- ETyApp (A0 x) t
pattern A1 f x <- EApp (A0 f) x
pattern A1t f t x <- EApp (A0t f t) x
pattern At1 f t x <- EApp (At0 f t) x
pattern A2 f x y <- EApp (A1 f x) y
pattern A3 f x y z <- EApp (A2 f x y) z
pattern A4 f x y z v <- EApp (A3 f x y z) v
pattern A5 f x y z v w <-  EApp (A4 f x y z v) w

--------------------------------------------

type Thunk = Exp' Var Ty Pat ((,) TEnv)
type Thunk' = Exp_ Var Ty Pat Thunk

-------------------------------------------------------------------------------- tag handling

class GetTag c where
    type Tag c
    getTag :: c -> Tag c

instance GetTag (Exp' n t p ((,) a)) where
    type Tag (Exp' n t p ((,) a)) = a
    getTag (Exp a _) = a
instance GetTag (Ty' c n ((,) a)) where
    type Tag (Ty' c n ((,) a)) = a
    getTag (Ty' k _) = k
instance GetTag (Pat' c n ((,) a)) where
    type Tag (Pat' c n ((,) a)) = a
    getTag (Pat a _) = a


-------------------------------------------------------------------------------- names

data NameInfo = NameInfo (Maybe Fixity) Doc

-- TODO: add definition range info
data N = N
    { nameSpace :: NameSpace
    , qualifier :: [String]
    , qData :: String
    , fixityInfo :: NameInfo
    }

instance Eq N where N a b c d == N a' b' c' d' = (a, b, c) == (a', b', c')
instance Ord N where N a b c d `compare` N a' b' c' d' = (a, b, c) `compare` (a', b', c')

data NameSpace = TypeNS | ExpNS
    deriving (Eq, Ord)

type Fixity = (Maybe FixityDir, Int)
data FixityDir = FDLeft | FDRight

-- TODO: eliminate
showN :: N -> String
showN = ppShow

pattern ExpN n <- N ExpNS [] n _ where
    ExpN n = N ExpNS [] n (NameInfo Nothing "exp")
--pattern ExpN' n i = N ExpNS [] n i
pattern TypeN n <- N TypeNS [] n _
pattern TypeN' n i = N TypeNS [] n (NameInfo Nothing i)

-- TODO: rename/eliminate
type Name = N
type TName = N
type TCName = N    -- type constructor name; if this turns out to be slow use Int or ADT instead of String
type EName = N
type FName = N
type MName = N     -- module name
type ClassName = N

toExpN (N _ a b i) = N ExpNS a b i
toTypeN (N _ a b i) = N TypeNS a b i
isTypeVar (N ns _ _ _) = ns == TypeNS
isConstr (N _ _ (c:_) _) = isUpper c || c == ':'

data Var
  = VarE EName Ty   -- TODO: is Ty needed?
  | VarT EName Ty   -- TODO: eliminate

-------------------------------------------------------------------------------- error handling

-- TODO: add more structure to support desugaring
data Range
    = Range SourcePos SourcePos
    | NoRange

instance Monoid Range where
    mempty = NoRange
    Range a1 a2 `mappend` Range b1 b2 = Range (min a1 a2) (max b1 b2)
    NoRange `mappend` a = a
    a `mappend` b = a

type WithRange = (,) Range

---------------------------------------

type WithExplanation = (,) Doc

pattern WithExplanation d x = (d, x)

-- TODO: add more structure
data ErrorMsg
    = AddRange Range ErrorMsg
    | InFile String ErrorMsg
    | ErrorCtx Doc ErrorMsg
    | ErrorMsg Doc
    | EParseError ParseError
    | UnificationError Ty Ty [WithExplanation [Ty]]

instance Show ErrorMsg where
    show = show . f Nothing Nothing where
        f file rng = \case
            InFile s e -> f (Just s) Nothing e
            AddRange r e -> showRange file (Just r) <$$> f file (Just r) e
            ErrorCtx d e -> "during" <+> d <$$> f file rng e
            EParseError pe -> text $ show pe
            ErrorMsg d -> d
            UnificationError a b tys -> "cannot unify" <+> pShow a </> "with" <+> pShow b
                <$$> "----------- equations"
                <$$> vcat (map (\(s, l) -> s <$$> vcat (map pShow l)) tys)

type ErrorT = ExceptT ErrorMsg

throwParseError = throwError . EParseError

mapError f m = catchError m $ throwError . f

addCtx d = mapError (ErrorCtx d)

addRange :: MonadError ErrorMsg m => Range -> m a -> m a
addRange NoRange = id
addRange r = mapError $ AddRange r

{-
checkUnambError = do
    cs <- get
    case cs of
        (Just _: _) -> throwError $ head $ catMaybes $ reverse cs
        _ -> return ()
-}
--throwErrorTCM :: Doc -> TCM a
throwErrorTCM = throwError . ErrorMsg

showRange :: Maybe String -> Maybe Range -> Doc
showRange Nothing Nothing = "no file position"
showRange Nothing (Just _) = "no file"
showRange (Just _) Nothing = "no position"
showRange (Just src) (Just (Range s e)) = str
    where
      startLine = sourceLine s - 1
      endline = sourceLine e - if sourceColumn e == 1 then 1 else 0
      len = endline - startLine
      str = vcat $ ("position:" <+> text (show s) <+> "-" <+> text (show e)):
                   map text (take len $ drop startLine $ lines src)
                ++ [text $ replicate (sourceColumn s - 1) ' ' ++ replicate (sourceColumn e - sourceColumn s) '^' | len == 1]

-------------------------------------------------------------------------------- parser output

data ValueDef p e = ValueDef p e
data TypeSig n t = TypeSig n t

data ModuleR
  = Module
  { moduleImports :: [Name]    -- TODO
  , moduleExports :: ()     -- TODO
  , definitions   :: [DefinitionR]
  }

type DefinitionR = WithRange Definition
data Definition
    = DValueDef (ValueDef PatR ExpR)
    | DAxiom (TypeSig Name TyR)
    | DDataDef Name [(Name, TyR)] [WithRange ConDef]      -- TODO: remove, use GADT
    | GADT Name [(Name, TyR)] [(Name, TyR)]
    | ClassDef ClassName [(Name, TyR)] [TypeSig Name TyR]
    | InstanceDef ClassName TyR [ValueDef PatR ExpR]
-- used only during parsing
    | PreValueDef (Range, EName) [PatR] WhereRHS
    | DTypeSig (TypeSig EName TyR)
    | PreInstanceDef ClassName TyR [DefinitionR]

-- used only during parsing
data WhereRHS = WhereRHS GuardedRHS (Maybe [DefinitionR])

-- used only during parsing
data GuardedRHS
    = Guards Range [(ExpR, ExpR)]
    | NoGuards ExpR

data ConDef = ConDef Name [FieldTy]
data FieldTy = FieldTy {fieldName :: Maybe Name, fieldType :: TyR}

type TyR = Ty' Name Name WithRange
type PatR = Pat' Name Name WithRange
type ExpR = Exp' Name TyR PatR WithRange
type ConstraintR = Constraint' Name TyR
type TypeFunR = TypeFun Name TyR
type ValueDefR = ValueDef PatR ExpR

-------------------------------------------------------------------------------- names with unique ids

{- TODO
data IdN = IdN !Int N

instance Eq IdN where IdN i _ == IdN j _ = i == j
instance Ord IdN where IdN i _ `compare` IdN j _ = i `compare` j
-}
type IdN = N

type FreshVars = [String]     -- fresh typevar names

type VarMT = StateT FreshVars

newName :: MonadState FreshVars m => Doc -> m Name
newName i = do
  (n: ns) <- get
  put ns
  return $ TypeN' n i

-------------------------------------------------------------------------------- environments

type Env a = Map IdN a

type SubstEnv = Env (Either Ty Ty)  -- either substitution or type signature   TODO: dedicated type instead of Either

type Subst = Env Ty  -- substitutions

data TEnv = TEnv Subst EnvMap       -- TODO: merge into this:   Env (Either Ty (Maybe Thunk))

type EnvMap = Env (Maybe Thunk)   -- Nothing: statically unknown but defined

data PolyEnv = PolyEnv
    { instanceDefs :: InstanceDefs
    , getPolyEnv :: Env InstType
    , precedences :: PrecMap
    , thunkEnv :: EnvMap
    }

type PrecMap = Map Name Fixity

type InstanceDefs = Env (Set Ty)

-------------------------------------------------------------------------------- monads

type TypingT = WriterT' SubstEnv

type InstType = TypingT (VarMT Identity) Ty

pureInstType = lift . pure

-- type checking monad transformer
type TCMT m = ReaderT PolyEnv (ErrorT (VarMT m))

type TCM = TCMT Identity

type TCMS = TypingT TCM

toTCMS :: InstType -> TCMS Ty
toTCMS = mapWriterT' $ lift . lift --mapStateT lift

liftIdentity :: Monad m => Identity a -> m a
liftIdentity = return . runIdentity

-------------------------------------------------------------------------------- typecheck output

type ExpT = (Exp, Ty)
type PatT = Pat
type ConstraintT = Constraint' IdN Ty
type TypeFunT = TypeFun IdN Ty
type ValueDefT = ValueDef PatT ExpT

-------------------------------------------------------------------------------- LambdaCube specific definitions
-- TODO: eliminate most of these

pattern TCon0 a = TCon Star a
pattern TCon1 a b = TApp Star (TCon StarStar a) b
pattern TCon2 a b c = TApp Star (TApp StarStar (TCon (StarToStar 2) a) b) c
pattern TCon2' a b c = TApp Star (TApp StarStar (TCon VecKind a) b) c
pattern TCon3' a b c d = TApp Star (TApp StarStar (TApp VecKind (TCon (TArr Star VecKind) a) b) c) d

pattern TENat a = Ty_ TNat (TLit_ (LNat a))
pattern TVec a b = TCon2' "Vec" (TENat a) b
pattern TMat a b c = TApp Star (TApp StarStar (TApp VecKind (TCon MatKind "Mat") (TENat a)) (TENat b)) c

-- basic types
pattern TChar = TCon0 "Char"
pattern TString = TCon0 "String"
pattern TBool = TCon0 "Bool"
pattern TWord = TCon0 "Word"
pattern TInt = TCon0 "Int"
pattern TNat = TCon0 "Nat"
pattern TFloat = TCon0 "Float"
pattern VecKind = TArr TNat StarStar
pattern MatKind = TArr TNat (TArr TNat StarStar)

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

pattern ClassN n <- TypeN n where
    ClassN n = TypeN' n "class"
pattern IsValidOutput = ClassN "ValidOutput"
pattern IsTypeLevelNatural = ClassN "TNat"
pattern IsValidFrameBuffer = ClassN "ValidFrameBuffer"
pattern IsInputTuple = ClassN "InputTuple"

pattern TypeFunS a b <- TypeFun (TypeN a) b where
    TypeFunS a b = TypeFun (TypeN' a "typefun") b
pattern TFMat a b               = TypeFunS "Mat" [a, b]      -- may be data family
pattern TFVec a b               = TypeFunS "Vec" [a, b]      -- may be data family
pattern TFMatVecElem a          = TypeFunS "MatVecElem" [a]
pattern TFMatVecScalarElem a    = TypeFunS "MatVecScalarElem" [a]
pattern TFVecScalar a b         = TypeFunS "VecScalar" [a, b]
pattern TFFTRepr' a             = TypeFunS "FTRepr'" [a]
pattern TFColorRepr a           = TypeFunS "ColorRepr" [a]
pattern TFFrameBuffer a         = TypeFunS "FrameBuffer" [a]
pattern TFFragOps a             = TypeFunS "FragOps" [a]
pattern TFJoinTupleType a b     = TypeFunS "JoinTupleType" [a, b]

-------------------------------------------------------------------------------- free variables

class FreeVars a where freeVars :: a -> Set TName

instance FreeVars Ty where
    freeVars = \case
        Ty_ k x -> freeVars k `mappend` freeVars x
        StarToStar _ -> mempty
instance FreeVars (Ty_ IdN IdN Ty) where    -- TODO: eliminate
    freeVars = \case
        TVar_ a -> Set.singleton a
        Forall_ (Just v) k t -> freeVars k <> Set.delete v (freeVars t)
        x -> foldMap freeVars x

instance FreeVars a => FreeVars [a]                 where freeVars = foldMap freeVars
--instance FreeVars Typing where freeVars (TypingConstr m t) = freeVars m <> freeVars t
instance FreeVars a => FreeVars (TypeFun n a)       where freeVars = foldMap freeVars
instance FreeVars a => FreeVars (Env a)         where freeVars = foldMap freeVars
instance FreeVars a => FreeVars (Constraint' n a)    where freeVars = foldMap freeVars

-------------------------------------------------------------------------------- Pretty show instances

instance PShow N where
    pShowPrec p = \case
        N _ qs s _ -> hcat $ punctuate (char '.') $ map text $ qs ++ [s]

instance PShow Lit where
    pShowPrec p = \case
        LInt    i -> integer i
        LChar   i -> text $ show i
        LString i -> text $ show i
        LFloat  i -> double i
        LNat    i -> int i

instance (PShow n, PShow c, PShow a) => PShow (Ty_ c n a) where
    pShowPrec p = \case
        Star_ -> "*"
        TLit_ l -> pShowPrec p l
        TVar_ n -> pShow n
        TCon_ n -> pShow n
        TApp_ a b -> pApp p a b
        Forall_ Nothing a b -> pInfixr (-1) "->" p a b
        Forall_ (Just n) a b -> "forall" <+> pShow n <+> "::" <+> pShow a <> "." <+> pShow b
        TTuple_ a -> tupled $ map pShow a
        TRecord_ m -> "Record" <+> showRecord (Map.toList m)
        ConstraintKind_ c -> pShowPrec p c
        Witness w -> pShowPrec p w

instance PShow Witness where
    pShowPrec p = \case
        Refl -> "Refl"

instance PShow Ty where
    pShowPrec p = \case
        Star -> "*"
        StarToStar i -> hcat $ intersperse "->" $ replicate (i+1) "*"
        TyStar i -> pShowPrec p i
        Ty k i -> pInfix (-2) "::" p i k

instance (PShow v, PShow t, PShow p, PShow b) => PShow (Exp_ v t p b) where
    pShowPrec p = \case
        ELit_ l -> pShowPrec p l
        EVar_ v -> pShowPrec p v
        EApp_ a b -> pApp p a b
        ETuple_ a -> tupled $ map pShow a
        ELam_ p b -> "\\" <> pShow p <+> "->" <+> pShow b
        ETypeSig_ b t -> pShow b <+> "::" <+> pShow t
        EType_ t -> pShowPrec p t
        ELet_ a b c -> "let" <+> pShow a <+> "=" <+> pShow b <+> "in" </> pShow c
        ENamedRecord_ n xs -> pShow n <+> showRecord xs
        ERecord_ xs -> showRecord xs
        EFieldProj_ n -> "." <> pShow n
        EAlts_ i b -> int i <> braces (vcat $ punctuate (char ';') $ map pShow b)
        ENext_ -> "SKIP"


showRecord = braces . hsep . punctuate (char ',') . map (\(a, b) -> pShow a <> ":" <+> pShow b)

instance (PShow n, PShow t, PShow p, PShow a) => PShow (Exp' n t p ((,) a)) where
    pShowPrec p e = case getLams e of
        ([], Exp _ e) -> pShowPrec p e
        (ps, Exp _ e) -> "\\" <> hsep (map pShow ps) <+> "->" <+> pShow e

getLams (ELam' _ p e) = (p:) *** id $ getLams e
getLams e = ([], e)

instance (PShow n, PShow t, PShow p) => PShow (Exp' n t p Identity) where
    pShowPrec p e = case getLams' e of
        ([], Exp'' e) -> pShowPrec p e
        (ps, Exp'' e) -> "\\" <> hsep (map pShow ps) <+> "->" <+> pShow e

getLams' (ELam p e) = (p:) *** id $ getLams' e
getLams' e = ([], e)

instance (PShow c, PShow v, PShow b) => PShow (Pat_ c v b) where
    pShowPrec p = \case
        PLit_ l -> pShow l
        PVar_ v -> pShow v
        PCon_ s xs -> pApps p s xs
        PTuple_ a -> tupled $ map pShow a
        PRecord_ xs -> "Record" <+> showRecord xs
        PAt_ v p -> pShow v <> "@" <> pShow p
        Wildcard_ -> "_"

instance (PShow n, PShow c) => PShow (Pat' c n ((,) a)) where
    pShowPrec p (Pat' (_, e)) = pShowPrec p e

instance (PShow n, PShow c) => PShow (Pat' c n Identity) where
    pShowPrec p (Pat'' e) = pShowPrec p e

instance (PShow n, PShow a) => PShow (TypeFun n a) where
    pShowPrec p (TypeFun s xs) = pApps p s xs

instance (PShow n, PShow a) => PShow (Constraint' n a) where
    pShowPrec p = \case
        CEq a b -> pShow a <+> "~" <+> pShow b
        CUnify a b -> pShow a <+> "~" <+> pShow b
        CClass a b -> pShow a <+> pShow b
--        | Split a a a         -- Split x y z:  x, y, z are records; fields of x = disjoint union of the fields of y and z

instance (PShow c, PShow n) => PShow (Ty' c n ((,) a)) where
    pShowPrec p (Ty' a b) = pShowPrec p b

instance PShow Var where
    pShowPrec p = \case
        VarE n t -> pShow n
        VarT n t -> pShow n

instance PShow TEnv where

instance PShow Range where
    pShowPrec p = \case
        Range a b -> text (show a) <+> "--" <+> text (show b)
        NoRange -> ""

-------------------------------------------------------------------------------- replacement

type Repl = Map TName TName

-- TODO: express with Substitute?
class Replace a where repl :: Repl -> a -> a

instance Replace Ty where
    repl st = \case
    --    ty | Map.null st -> ty -- optimization
        StarToStar n -> StarToStar n
        Ty_ k s -> Ty_ (repl st k) $ case s of
            Forall_ (Just n) a b -> Forall_ (Just n) (repl st a) (repl (Map.delete n st) b)
            TVar_ a | Just t <- Map.lookup a st -> TVar_ t
            t -> repl st <$> t

instance Replace a => Replace (Env a) where
    repl st e = Map.fromList $ map (r *** repl st) $ Map.toList e
      where
        r x = fromMaybe x $ Map.lookup x st

instance (Replace a, Replace b) => Replace (Either a b) where
    repl st = either (Left . repl st) (Right . repl st)

-------------------------------------------------------------------------------- pre-substitution

-- TODO: review usage (use only after unification)
class Substitute a where subst_ :: Applicative m => Env (m Ty) -> a -> m a

instance Substitute Ty where
    subst_ st = \case
 --       ty | Map.null st -> pure ty -- optimization
        StarToStar n -> pure $ StarToStar n
        Ty_ k s -> case s of
            Forall_ (Just n) a b -> Ty_ <$> subst_ st k <*> (Forall_ (Just n) <$> subst_ st a <*> subst_ (Map.delete n st) b)
            TVar_ a | Just t <- Map.lookup a st -> t
            _ -> Ty_ <$> subst_ st k <*> traverse (subst_ st) s

instance Substitute (Env Ty) where
    subst_ st = fmap (Map.fromList . concat) . sequenceA . map f . Map.toList where
        f (x, y)
            | Map.member x st = pure []
            | otherwise = (:[]) . (,) x <$> subst_ st y

instance Substitute SubstEnv where
    subst_ st = fmap Map.fromList{-TODO: optimize-} . sequenceA . map f . Map.toList where
        f (x, y)
            | Just t <- Map.lookup x st = (,) x . Left <$> t
            | otherwise = (,) x <$> me (subst_ st) y

        me f = either (fmap Left . f) (fmap Right . f)

instance Substitute a => Substitute (Constraint' n a) where subst_ = traverse . subst_
instance Substitute a => Substitute [a]              where subst_ = traverse . subst_

instance Substitute Var where
{-
    subst s = \case
        VarE n t -> VarE n $ subst s t
        VarC c -> VarC $ subst s c
        VarT n -> VarT n
-}

instance Substitute Pat where
{-
    subst s = \case
        PVar v -> PVar $ subst s v
        PCon (n, ty) l -> PCon (n, subst s ty) $ subst s l
        Pat p -> Pat $ fmap (subst s) p
-}

-------------------------------------------------------------------------------- substitution

-- TODO: review usage (use only after unification)
subst :: Substitute a => Subst -> a -> a
subst s = runIdentity . subst_ (pure <$> s)

-- instance Substitute Ty where --subst st = join . subst_ st
--instance Substitute Typing where subst st = join . subst_ st
-- instance Substitute a => Substitute [a] where subst = fmap . subst

--------------------------------------------------------------------------------

-- Note: domain of substitutions is disjunct
-- semantics:  subst (s2 `composeSubst` s1) = subst s2 . subst s1
-- example:  subst ({y -> z} `composeSubst` {x -> y}) = subst {y -> z} . subst {x -> y} = subst {y -> z, x -> z}
-- example2: subst ({x -> z} `composeSubst` {x -> y}) = subst {x -> z} . subst {x -> y} = subst {x -> y}
composeSubst :: Subst -> Subst -> Subst
s2 `composeSubst` s1 = (subst s2 <$> s1) <> s2


-------------------------------------------------------------------------------- utility

tyOf :: Exp -> Ty
tyOf = \case
    ETuple es -> TTuple $ map tyOf es
    EVar (VarE _ t) -> t
    EApp (tyOf -> TArr _ t) _ -> t
    ELam (tyOfPat -> a) (tyOf -> b) -> TArr a b
    e -> error $ "tyOf " ++ ppShow e

tyOfPat :: Pat -> Ty
tyOfPat = \case
    PCon (_, t) ps -> stripArgs (length ps) t
    e -> error $ "tyOfPat " ++ ppShow e
  where
    stripArgs 0 t = t
    stripArgs n (TArr _ t) = stripArgs (n-1) t

patternEVars (Pat'' p) = case p of
    PVar_ (VarE v _) -> [v]
    p -> foldMap patternEVars p

-------------------------------------------------------------------------------- thunks

instance Monoid TEnv where
    mempty = TEnv mempty mempty
    -- semantics: apply (m1 <> m2) = apply m1 . apply m2;  see 'composeSubst'
    m1@(TEnv x1 y1) `mappend` TEnv x2 y2 = TEnv (x1 `composeSubst` x2) $ ((applyEnv m1 <$>) <$> y2) <> y1

envMap :: Thunk -> EnvMap
envMap (Exp (TEnv _ m) _) = m

subst' :: Substitute a => TEnv -> a -> a
subst' (TEnv s _) = subst s

applyEnv :: TEnv -> Thunk -> Thunk
applyEnv m1 (Exp m exp) = Exp (m1 <> m) exp

applyEnvBefore :: TEnv -> Thunk -> Thunk
applyEnvBefore m1 (Exp m exp) = Exp (m <> m1) exp

--   applySubst s  ===  applyEnv (TEnv s mempty)
-- but the following is more efficient
applySubst :: Subst -> Thunk -> Thunk
applySubst s' (Exp (TEnv s m) exp) = Exp (TEnv (s' `composeSubst` s) m) exp

-- build recursive environment  -- TODO: generalize
recEnv :: Pat -> Thunk -> Thunk
recEnv (PVar (VarE v _)) th_ = th where th = applyEnvBefore (TEnv mempty (Map.singleton v (Just th))) th_
recEnv _ th = th

mkThunk :: Exp -> Thunk
mkThunk (Exp'' e) = thunk $ mkThunk <$> e

thunk :: Thunk' -> Thunk
thunk = Exp mempty

peelThunk :: Thunk -> Thunk'
peelThunk (Exp env e) = mapExp vf (subst' env) (subst' env) $ applyEnv env <$> e
  where
    vf = \case
        VarE v t -> VarE v $ subst' env t

--------------------------------------------------------------------------------

buildLet :: [ValueDefT] -> ExpT -> ExpT
buildLet es e = foldr (\(ValueDef p (e, t')) (x, t'') -> (ELet p e x, t'')) e es


-------------------------------------------------------------------------------- WriterT'

class Monoid' e where
    type MonoidConstraint e (m :: * -> *) :: Constraint
    mempty' :: e
    mappend' :: MonoidConstraint e m => e -> e -> m e

newtype WriterT' e m a
  = WriterT' {runWriterT' :: m (e, a)}
    deriving (Functor,Foldable,Traversable)

instance (Monoid' e) => MonadTrans (WriterT' e) where
    lift m = WriterT' $ (,) mempty' <$> m

instance (Monoid' e, Monad m, MonoidConstraint e m) => Applicative (WriterT' e m) where
    pure a = WriterT' $ pure (mempty', a)
    a <*> b = join $ (<$> b) <$> a

instance (Monoid' e, Monad m, MonoidConstraint e m) => Monad (WriterT' e m) where
    WriterT' m >>= f = WriterT' $ do
            (e1, a) <- m
            (e2, b) <- runWriterT' $ f a
            e <- mappend' e1 e2
            return (e, b)

instance (Monoid' e, MonoidConstraint e m, MonadReader r m) => MonadReader r (WriterT' e m) where
    ask = lift ask
    local f (WriterT' m) = WriterT' $ local f m

instance (Monoid' e, MonoidConstraint e m, MonadState s m) => MonadState s (WriterT' e m) where
    state f = lift $ state f

instance (Monoid' e, MonoidConstraint e m, MonadError err m) => MonadError err (WriterT' e m) where
    catchError (WriterT' m) f = WriterT' $ catchError m $ runWriterT' <$> f
    throwError e = lift $ throwError e

mapWriterT' f (WriterT' m) = WriterT' $ f m
