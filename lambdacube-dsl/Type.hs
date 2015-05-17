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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Type where

import Data.Char
import Data.Either
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
import Debug.Trace

import ParserUtil (ParseError)
import Pretty

trace' x = trace (ppShow x) x

-------------------------------------------------------------------------------- literals

data Lit
    = LInt    Integer
    | LNat    Int
    | LChar   Char
    | LString String
    | LFloat  Double
    deriving (Eq, Ord)

pattern EInt a = ELit (LInt a)
pattern ENat a = ELit (LNat a)
pattern EChar a = ELit (LChar a)
pattern EString a = ELit (LString a)
pattern EFloat a = ELit (LFloat a)

-------------------------------------------------------------------------------- patterns

data Pat_ t c v b
    = PLit_ Lit
    | PVar_ t v
    | PCon_ t c [b]
    | PTuple_ [b]
    | PRecord_ [(Name, b)]
    | PAt_ v b
    | Wildcard_ t
    deriving (Functor,Foldable,Traversable)

-- TODO: remove
instance Eq Pat where (==) = error "Eq Pat"
instance Ord Pat where compare = error "Ord Pat"

mapPat :: (t -> t') -> (c -> c') -> (v -> v') -> Pat_ t c v b -> Pat_ t' c' v' b
mapPat tf f g = \case
    PLit_ l -> PLit_ l
    PVar_ t v -> PVar_ (tf t) $ g v
    PCon_ t c p -> PCon_ (tf t) (f c) p
    PTuple_ p -> PTuple_ p
    PRecord_ p -> PRecord_ p -- $ map (g *** id) p
    PAt_ v p -> PAt_ (g v) p
    Wildcard_ t -> Wildcard_ (tf t)

--------------------------------------------

data PatR = PatR Range (Pat_ () Name Name PatR)

pattern PVar' a b = PatR a (PVar_ () b)
pattern PCon' a b c = PatR a (PCon_ () b c)

--------------------------------------------

newtype Pat = Pat (Pat_ Exp Name Name Pat)

pattern PAt v l = Pat (PAt_ v l)
pattern PLit l = Pat (PLit_ l)
pattern PVar t l = Pat (PVar_ t l)
pattern PCon t c l = Pat (PCon_ t c l)
pattern PTuple l = Pat (PTuple_ l)
pattern Wildcard t = Pat (Wildcard_ t)

patternVars :: Pat -> [(Name, Exp)]
patternVars (Pat p) = case p of
    PVar_ t v -> [(v, t)]
    PAt_ v p -> [(v, tyOfPat p)]
    p -> foldMap patternVars p

-------------------------------------------------------------------------------- types

data Constraint' n a
    = CEq a (TypeFun n a) -- unification between a type and a fully applied type function; CEq t f:  t ~ f
    | CUnify a a          -- unification between (non-type-function) types; CUnify t s:  t ~ s
    | CClass n a          -- class constraint
    | Split a a a         -- Split x y z:  x, y, z are records; fields of x = disjoint union of the fields of y and z
    deriving (Eq,Ord,Functor,Foldable,Traversable)

mapConstraint :: (n -> n') -> (a -> a') -> Constraint' n a -> Constraint' n' a'
mapConstraint nf af = \case
    CEq a (TypeFun n as) -> CEq (af a) (TypeFun (nf n) (af <$> as))
    CUnify a1 a2 -> CUnify (af a1) (af a2)
    CClass n a -> CClass (nf n) (af a)
    Split a1 a2 a3 -> Split (af a1) (af a2) (af a3)

data TypeFun n a = TypeFun n [a]
    deriving (Eq,Ord,Functor,Foldable,Traversable)

data Witness
    = Refl
    | WInstance (Env (Env Exp -> Exp))
--    deriving (Eq,Ord,Functor,Foldable,Traversable)

instance Eq Witness where a == b = error "Eq Witness"
instance Ord Witness where a `compare` b = error "Ord Witness"


-------------------------------------------------------------------------------- expressions

data Exp_ k v t p b
    = ELit_      Lit
    | EVar_      k v
    | EApp_      k b b
    | ETuple_    [b]
    | ELam_      p b
    | ETypeSig_  b t
    | ETyApp_ k b t

    | ELet_      p b b
    | ENamedRecord_ Name [(Name, b)]
    | ERecord_   [(Name, b)]
    | EFieldProj_ k Name
    | EAlts_     Int [b]  -- function alternatives; Int: arity
    | ENext_     k        -- go to next alternative
    | ExtractInstance (Env b) [Name] Name Name
    | PrimFun Name [b] Int

    -- was types
    | Star_
    | TCon_    k v
    -- | TFun_    f [a]    -- TODO
    | Forall_  (Maybe v) b b
    | TTuple_  [b]
    | TRecord_ (Map v b)
    | ConstraintKind_ (Constraint' v b)        -- flatten?
    | Witness_ k (Witness)      -- TODO: make this polymorphic?

    -- aux
    | EPrec_ b [(b, b)]     -- before precedence calculation
    deriving (Eq,Ord,Functor,Foldable,Traversable) -- TODO: elim Eq instance



mapExp_ :: Ord v' => (k -> k') -> (v -> v') -> (t -> t') -> (p -> p') -> Exp_ k v t p b -> Exp_ k' v' t' p' b
mapExp_ kf vf tf f = \case
    ELit_      x       -> ELit_ x
    EVar_      k x     -> EVar_ (kf k) $ vf x
    EApp_      k x y   -> EApp_ (kf k) x y
    ELam_      x y     -> ELam_ (f x) y
    ELet_      x y z   -> ELet_ (f x) y z
    ETuple_    x       -> ETuple_ x
    ERecord_   x       -> ERecord_ $ x --map (vf *** id) x
    ENamedRecord_ n x  -> ENamedRecord_ n x --(vf n) $ map (vf *** id) x
    EFieldProj_ k x    -> EFieldProj_ (kf k) x -- $ vf x
    ETypeSig_  x y     -> ETypeSig_ x $ tf y
    EAlts_     x y     -> EAlts_ x y
    ENext_ k           -> ENext_ (kf k)
    ETyApp_ k b t      -> ETyApp_ (kf k) b $ tf t
    ExtractInstance i j n m -> ExtractInstance i j n m
    PrimFun a b c      -> PrimFun a b c
    Star_              -> Star_
    TCon_    k v       -> TCon_ (kf k) (vf v)
    -- | TFun_    f [a]    -- TODO
    Forall_  mv b1 b2  -> Forall_ (vf <$> mv) b1 b2
    TTuple_  bs        -> TTuple_ bs
    TRecord_ m         -> TRecord_ $ Map.fromList $ map (vf *** id) $ Map.toList m -- (Map v b)
    ConstraintKind_ c  -> ConstraintKind_ $ mapConstraint vf id c
    Witness_ k w       -> Witness_ (kf k) w


-------------------------------------------------------------------------------- cached type inference 

inferLit :: Lit -> Exp
inferLit a = thunk $ TCon_ (thunk Star_) $ flip TypeIdN' "typecon" $ case a of
    LInt _    -> "Int"
    LChar _   -> "Char"
    LFloat _  -> "Float"
    LString _ -> "String"
    LNat _    -> "Nat"

tyOf :: Exp -> Exp
tyOf = \case
    Exp t -> case t of
        ELit_ l -> inferLit l
        EVar_ k _ -> k
        EApp_ k _ _ -> k
        ETyApp_ k _ _ -> k
        ETuple_ es -> TTuple $ map tyOf es 
        ELam_ (tyOfPat -> a) (tyOf -> b) -> TArr a b
--        ETypeSig_ b t -> t  -- TODO?
        ELet_ _ _ e -> tyOf e
--        | ENamedRecord_ Name [(Name, b)]
        ERecord_ (unzip -> (fs, es)) -> TRecord $ Map.fromList $ zip fs $ map tyOf es
        EFieldProj_ k _ -> k
        EAlts_ _ bs -> tyOf $ head bs
        ENext_ k -> k
{-
        | ExtractInstance [b] Int Name
        | PrimFun Name [b] Int
-}
        -- was types
        Star_ -> Star
        TCon_ k _ -> k
        Forall_ _ _ _ -> Star
        TTuple_ _ -> Star
        TRecord_ _ -> Star
        ConstraintKind_ _ -> Star
        Witness_ k _ -> k
        e -> error $ "tyOf " ++ ppShow e

tyOfPat :: Pat -> Exp
tyOfPat = \case
    PCon t _ _ -> t
    PVar t _ -> t
    Wildcard t -> t
    PLit l -> inferLit l
    PTuple xs -> thunk $ TTuple_ $ map tyOfPat xs
--    PRecord xs ->  [(Name, b)]
    PAt _ p -> tyOfPat p
    e -> error $ "tyOfPat " ++ ppShow e

isStar = \case
    Star -> True
    _ -> False

--------------------------------------------------------------------------------

data ExpR = ExpR Range (Exp_ () Name TyR PatR ExpR)

type TyR = ExpR

-- TODO: elim these
pattern ELitR' a b = ExpR a (ELit_ b)
pattern EVarR' a b = ExpR a (EVar_ () b)
pattern EAppR' a b c = ExpR a (EApp_ () b c)
pattern ELamR' a b c = ExpR a (ELam_ b c)
pattern ELetR' a b c d = ExpR a (ELet_ b c d)
pattern ETupleR' a b = ExpR a (ETuple_ b)
pattern ERecordR' a b = ExpR a (ERecord_ b)
pattern ENamedRecordR' a n b = ExpR a (ENamedRecord_ n b)
pattern EFieldProjR' a c = ExpR a (EFieldProj_ () c)
pattern ETypeSigR' a b c = ExpR a (ETypeSig_ b c)
pattern EAltsR' a i b = ExpR a (EAlts_ i b)
pattern ENextR' a = ExpR a (ENext_ ())
pattern ETyAppR a b c = ExpR a (ETyApp_ () b c)

--------------------------------------------------------------------------------

data Exp = ExpTh Subst Exp'
type Exp' = Exp_ Exp Name Exp Pat Exp

type Ty = Exp

pattern Exp a <- (peelThunk -> a) where
    Exp a = thunk a

thunk = ExpTh mempty

-- TODO: eliminate
instance Eq Exp where Exp a == Exp b = a == b
instance Ord Exp where Exp a `compare` Exp b = a `compare` b

pattern TCon k a <- Exp (TCon_ k (TypeIdN a)) where
    TCon k a = Exp (TCon_ k (TypeIdN' a "typecon"))

pattern Star = Exp Star_

pattern TRecord b = Exp (TRecord_ b)
pattern TTuple b = Exp (TTuple_ b)
pattern TUnit = TTuple []
pattern ConstraintKind c = Exp (ConstraintKind_ c)
pattern Forall a b c = Exp (Forall_ (Just a) b c)
pattern TArr a b = Exp (Forall_ Nothing a b)

pattern ELit a = Exp (ELit_ a)
pattern EVar a <- Exp (EVar_ _ a)
pattern TVar k b = Exp (EVar_ k b)
pattern EApp a b <- Exp (EApp_ _ a b)
pattern TApp k a b = Exp (EApp_ k a b)
pattern ETyApp k a b = Exp (ETyApp_ k a b)
pattern ELam a b = Exp (ELam_ a b)
pattern ELet a b c = Exp (ELet_ a b c)
pattern ETuple a = Exp (ETuple_ a)
pattern ERecord b = Exp (ERecord_ b)
pattern EFieldProj k a = Exp (EFieldProj_ k a)
pattern EAlts i b = Exp (EAlts_ i b)
pattern ENext k = Exp (ENext_ k)
pattern Witness k w = Exp (Witness_ k w)

pattern A0 x <- EVar (ExpIdN x)
pattern A1 f x <- EApp (A0 f) x
pattern A2 f x y <- EApp (A1 f x) y
pattern A3 f x y z <- EApp (A2 f x y) z
pattern A4 f x y z v <- EApp (A3 f x y z) v
pattern A5 f x y z v w <- EApp (A4 f x y z v) w
pattern A6 f x y z v w q <- EApp (A5 f x y z v w) q
pattern A7 f x y z v w q r <- EApp (A6 f x y z v w q) r
pattern A8 f x y z v w q r s <- EApp (A7 f x y z v w q r) s
pattern A9 f x y z v w q r s t <- EApp (A8 f x y z v w q r s) t
pattern A10 f x y z v w q r s t a <- EApp (A9 f x y z v w q r s t) a
pattern A11 f x y z v w q r s t a b <- EApp (A10 f x y z v w q r s t a) b

infixr 7 ~>, ~~>
a ~> b = TArr a b

(~~>) :: [Exp] -> Exp -> Exp
args ~~> res = foldr (~>) res args

infix 4 ~~, ~~~
(~~) = CEq
(~~~) = CUnify

buildApp :: (Exp -> Exp) -> Exp -> [Exp] -> Exp
buildApp n restype args = f restype $ reverse args
  where
    f ty [] = n ty
    f ty (a:as) = TApp ty (f (tyOf a ~> ty) as) a

-------------------------------------------------------------------------------- tag handling

class GetTag c where
    type Tag c
    getTag :: c -> Tag c

instance GetTag ExpR where
    type Tag ExpR = Range
    getTag (ExpR a _) = a
instance GetTag PatR where
    type Tag PatR = Range
    getTag (PatR a _) = a

-------------------------------------------------------------------------------- names

data NameSpace = TypeNS | ExpNS
    deriving (Eq, Ord)

-- TODO: more structure instead of Doc
data NameInfo = NameInfo (Maybe Fixity) Doc

data N = N
    { nameSpace :: NameSpace
    , qualifier :: [String]
    , nName :: String
    , nameInfo :: NameInfo
    }

instance Eq N where N a b c d == N a' b' c' d' = (a, b, c) == (a', b', c')
instance Ord N where N a b c d `compare` N a' b' c' d' = (a, b, c) `compare` (a', b', c')

type Fixity = (Maybe FixityDir, Int)
data FixityDir = FDLeft | FDRight

pattern ExpN n <- N ExpNS [] n _ where
    ExpN n = N ExpNS [] n (NameInfo Nothing "exp")
pattern ExpN' n i = N ExpNS [] n (NameInfo Nothing i)
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

--------------------------------------------------------------------------------

type WithExplanation = (,) Doc

pattern WithExplanation d x = (d, x)

-- TODO: add more structure
data ErrorMsg
    = AddRange Range ErrorMsg
    | InFile String ErrorMsg
    | ErrorCtx Doc ErrorMsg
    | ErrorMsg Doc
    | EParseError ParseError
    | UnificationError Exp Exp [WithExplanation [Exp]]

instance Monoid ErrorMsg where
    mempty = ErrorMsg "<<>>"
    mappend a b = a

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
    | TypeFamilyDef Name [(Name, TyR)] TyR
    | PrecDef Name Fixity
-- used only during parsing
    | PreValueDef (Range, EName) [PatR] WhereRHS
    | DTypeSig (TypeSig EName TyR)
    | PreInstanceDef ClassName TyR [DefinitionR]
    | ForeignDef Name TyR

-- used only during parsing
data WhereRHS = WhereRHS GuardedRHS (Maybe [DefinitionR])

-- used only during parsing
data GuardedRHS
    = Guards Range [(ExpR, ExpR)]
    | NoGuards ExpR

data ConDef = ConDef Name [FieldTy]
data FieldTy = FieldTy {fieldName :: Maybe Name, fieldType :: TyR}

type ConstraintR = Constraint' Name TyR
type TypeFunR = TypeFun Name TyR
type ValueDefR = ValueDef PatR ExpR

-------------------------------------------------------------------------------- names with unique ids

type IdN = N
pattern IdN a = a
--newtype IdN = IdN N deriving (Eq, Ord)
{- TODO
data IdN = IdN !Int N

instance Eq IdN where IdN i _ == IdN j _ = i == j
instance Ord IdN where IdN i _ `compare` IdN j _ = i `compare` j
-}

pattern TypeIdN n <- IdN (TypeN n)
pattern TypeIdN' n i = IdN (TypeN' n i)
pattern ExpIdN n <- IdN (ExpN n)
pattern ExpIdN' n i = IdN (ExpN' n i)

type FreshVars = Int --[String]     -- fresh typevar names

type VarMT = StateT FreshVars

newName :: MonadState FreshVars m => Doc -> m IdN
newName info = do
    i <- get
    modify (+1)
    return $ TypeN' ("t" ++ show i) info

newEName = do
    i <- get
    modify (+1)
    return $ ExpN $ "r" ++ show i


-------------------------------------------------------------------------------- environments

type Env' a = Map Name a
type Env a = Map IdN a

data Item = ISubst Exp | ISig Exp

eitherItem f g (ISubst x) = f x
eitherItem f g (ISig x) = g x

newtype Subst = Subst {getSubst :: Env Exp}

instance Monoid Subst where
    mempty = Subst mempty
    -- semantics: subst (m1 <> m2) = subst m1 . subst m2
    -- example:  subst ({y -> z} <> {x -> y}) = subst {y -> z} . subst {x -> y} = subst {y -> z, x -> z}
    -- example2: subst ({x -> z} <> {x -> y}) = subst {x -> z} . subst {x -> y} = subst {x -> y}
    m1@(Subst y1) `mappend` Subst y2 = Subst $ (subst_ m1 <$> y2) <> y1

subst_ = subst
singSubst' a b = Subst $ Map.singleton a b

nullSubst (Subst s) = Map.null s
toTEnv (Subst s) = TEnv $ ISubst <$> s
toSubst (TEnv s) = Subst $ Map.map (\(ISubst e) -> e) $ Map.filter (eitherItem (const True) (const False)) s

newtype TEnv = TEnv {getTEnv :: Env Item}  -- either substitution or bound name

instance Monoid TEnv where
    mempty = TEnv mempty
    -- semantics: apply (m1 <> m2) = apply m1 . apply m2
    -- example:  subst ({y -> z} <> {x -> y}) = subst {y -> z} . subst {x -> y} = subst {y -> z, x -> z}
    -- example2: subst ({x -> z} <> {x -> y}) = subst {x -> z} . subst {x -> y} = subst {x -> y}
    m1@(TEnv y1) `mappend` TEnv y2 = TEnv $ Map.unionWith gg (subst (toSubst m1) <$> y2) y1
      where
        gg (ISubst s) _ = ISubst s
        gg _ b = b

singSubst a b = TEnv $ Map.singleton a $ ISubst b
singSubstTy a b = TEnv $ Map.singleton a $ ISig b
    
-- build recursive environment  -- TODO: generalize
recEnv :: Pat -> Exp -> Exp
recEnv (PVar _ v) th_ = th where th = subst (singSubst' v th) th_
recEnv _ th = th

mapExp' f nf pf e = mapExp_ f nf f pf $ f <$> e

peelThunk :: Exp -> Exp'
peelThunk (ExpTh env@(Subst m) e)
--  | Map.null m = e
  | otherwise = case e of
    Forall_ (Just n) a b -> Forall_ (Just n) (f a) $ subst_ (delEnv n (f a) env) b
    ELam_ x y -> ELam_ (mapPat' x) $ subst_ (delEnvs (patternVars x) env) y
    ELet_ x y z -> ELet_ (mapPat' x) (g y) (g z) where
        g = subst_ (delEnvs (patternVars x) env)
    EVar_ k v -> case Map.lookup v m of
        Just e -> peelThunk e
        _ -> EVar_ (f k) v
    _ -> mapExp' f id (error "peelT") e
  where
    f = subst_ env

    mapPat' :: Pat -> Pat
    mapPat' (Pat p) = Pat $ mapPat f id id $ mapPat' <$> p

    delEnvs xs (Subst env) = Subst $ foldr Map.delete env $ map fst xs
    delEnv n x = delEnvs [(n, x)]

subst1 :: Subst -> Exp -> Exp
subst1 s@(Subst m) = \case
    TVar k v -> case Map.lookup v m of
        Just e -> subst1 s e
        _ -> TVar k v
    e -> e

-------------------------------------------------------------------------------- free variables

class FreeVars a where freeVars :: a -> Set IdN

instance FreeVars Exp where
    freeVars = \case
        Exp x -> case x of
            ELam_ x y -> error "freev elam" --freeVars y Set.\\ freeVars x
            ELet_ x y z -> error "freeVars ELet"
            EVar_ k a -> Set.singleton a <> freeVars k
            TCon_ k a -> freeVars k
            EApp_ k a b -> freeVars k <> freeVars a <> freeVars b
            Forall_ (Just v) k t -> freeVars k <> Set.delete v (freeVars t)
            Witness_ k w -> freeVars k -- TODO: w?
            x -> foldMap freeVars x

instance FreeVars a => FreeVars [a]                 where freeVars = foldMap freeVars
instance FreeVars a => FreeVars (TypeFun n a)       where freeVars = foldMap freeVars
instance FreeVars a => FreeVars (Env a)         where freeVars = foldMap freeVars
instance FreeVars a => FreeVars (Constraint' n a)    where freeVars = foldMap freeVars

-------------------------------------------------------------------------------- replacement

type Repl = Map IdN IdN

-- TODO: express with Substitute?
class Replace a where repl :: Repl -> a -> a

-- TODO: make more efficient
instance Replace Exp where
    repl st = \case
        ty | Map.null st -> ty -- optimization
        Exp s -> Exp $ case s of
            ELam_ _ _ -> error "repl lam"
            ELet_ _ _ _ -> error "repl let"
            Forall_ (Just n) a b -> Forall_ (Just n) (f a) (repl (Map.delete n st) b)
            t -> mapExp' f rn (error "repl") t
      where
        f = repl st
        rn a
            | Just t <- Map.lookup a st = t
            | otherwise = a

instance Replace a => Replace (Env a) where
    repl st e = Map.fromList $ map (r *** repl st) $ Map.toList e
      where
        r x = fromMaybe x $ Map.lookup x st

instance (Replace a, Replace b) => Replace (Either a b) where
    repl st = either (Left . repl st) (Right . repl st)
instance Replace Item where
    repl st = eitherItem (ISubst . repl st) (ISig . repl st)

-------------------------------------------------------------------------------- substitution

-- TODO: review usage (use only after unification)
class Substitute x a where subst :: x -> a -> a

--instance Substitute a => Substitute (Constraint' n a)      where subst = fmap . subst
instance Substitute x a => Substitute x [a]                    where subst = fmap . subst
instance (Substitute x a, Substitute x b) => Substitute x (a, b) where subst s (a, b) = (subst s a, subst s b)
instance (Substitute x a, Substitute x b) => Substitute x (Either a b) where subst s = subst s +++ subst s
instance Substitute x Exp => Substitute x Item where subst s = eitherItem (ISubst . subst s) (ISig . subst s)
{-
instance Substitute Pat where
    subst s = \case
        PVar t v -> PVar $ subst s v
        PCon t n l -> PCon (VarE n $ subst s ty) $ subst s l
        Pat p -> Pat $ subst s <$> p
-}
--instance Substitute TEnv Exp where subst = subst . toSubst --m1 (ExpTh m exp) = ExpTh (toSubst m1 <> m) exp
instance Substitute Subst Exp where subst m1 (ExpTh m exp) = ExpTh (m1 <> m) exp
--instance Substitute TEnv TEnv where subst s (TEnv m) = TEnv $ subst s <$> m
instance Substitute Subst TEnv where subst s (TEnv m) = TEnv $ subst s <$> m

--------------------------------------------------------------------------------

data PolyEnv = PolyEnv
    { instanceDefs :: InstanceDefs
    , classDefs  :: Env' ClassD
    , getPolyEnv :: InstEnv
    , precedences :: PrecMap
    , thunkEnv :: TEnv          -- TODO: merge with getPolyEnv
    , typeFamilies :: InstEnv
    }

data ClassD = ClassD InstEnv

type InstEnv = Env' InstType'

type PrecMap = Env' Fixity

type InstanceDefs = Env' (Map Exp Witness)

emptyPolyEnv :: PolyEnv
emptyPolyEnv = PolyEnv mempty mempty mempty mempty mempty mempty

joinPolyEnvs :: forall m. MonadError ErrorMsg m => [PolyEnv] -> m PolyEnv
joinPolyEnvs ps = PolyEnv
    <$> mkJoin' instanceDefs
    <*> mkJoin classDefs
    <*> mkJoin getPolyEnv
    <*> mkJoin precedences
    <*> (TEnv <$> mkJoin (getTEnv . thunkEnv))
    <*> mkJoin typeFamilies
  where
    mkJoin :: (PolyEnv -> Env a) -> m (Env a)
    mkJoin f = case filter (not . Map.null) . map f $ ps of
        [m] -> return m
        ms -> case filter (not . null . drop 1 . snd) $ Map.toList ms' of
            [] -> return $ fmap head $ Map.filter (not . null) ms'
            xs -> throwErrorTCM $ "Definition clash:" <+> pShow (map fst xs)
          where
            ms' = Map.unionsWith (++) $ map ((:[]) <$>) ms

    mkJoin' f = case [(n, x) | (n, s) <- Map.toList ms', (x, is) <- Map.toList s, not $ null $ drop 1 is] of
        [] -> return $ fmap head . Map.filter (not . null) <$> ms'
        xs -> throwErrorTCM $ "Definition clash:" <+> pShow xs
       where
        ms' = Map.unionsWith (Map.unionWith (++)) $ map ((((:[]) <$>) <$>) . f) ps

addPolyEnv pe m = do
    env <- ask
    env <- joinPolyEnvs [pe, env]
    local (const env) m

--withTyping :: Env InstType -> TCM a -> TCM a
withTyping ts = addPolyEnv $ emptyPolyEnv {getPolyEnv = ts}

-------------------------------------------------------------------------------- monads

type TypingT = WriterT' TEnv

type InstType = TypingT (VarMT Identity) ([Exp], Exp)
type InstType' = Doc -> InstType

pureInstType = lift . pure

-- type checking monad transformer
type TCMT m = ReaderT PolyEnv (ErrorT (VarMT m))

type TCM = TCMT Identity

type TCMS = TypingT TCM

toTCMS :: InstType -> TCMS ([Exp], Exp)
toTCMS = mapWriterT' $ lift . lift

-------------------------------------------------------------------------------- typecheck output

type ConstraintT = Constraint' IdN Exp
type TypeFunT = TypeFun IdN Exp

-------------------------------------------------------------------------------- LambdaCube specific definitions
-- TODO: eliminate most of these

pattern StarStar = TArr Star Star

pattern TCon0 a = TCon Star a
pattern TCon1 a b = TApp Star (TCon StarStar a) b
pattern TCon2 a b c = TApp Star (TApp StarStar (TCon (TArr Star StarStar) a) b) c
pattern TCon2' a b c = TApp Star (TApp StarStar (TCon VecKind a) b) c
pattern TCon3' a b c d = TApp Star (TApp StarStar (TApp VecKind (TCon (TArr Star VecKind) a) b) c) d

pattern TVec a b = TCon2' "Vec" (ENat a) b
pattern TMat a b c = TApp Star (TApp StarStar (TApp VecKind (TCon MatKind "Mat") (ENat a)) (ENat b)) c

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

-------------------------------------------------------------------------------- reduce to Head Normal Form

type ReduceM = ExceptT String (State Int)

reduceFail = throwErrorTCM

reduceHNF :: forall m . (MonadPlus m, MonadError ErrorMsg m, MonadState Int m) => Exp -> m Exp       -- Left: pattern match failure
reduceHNF (Exp exp) = case exp of

    PrimFun (ExpN f) acc 0 -> evalPrimFun f <$> mapM reduceEither (reverse acc)

    ExtractInstance acc [] n m -> reduceHNF (fromMaybe (error "impossible") $ Map.lookup n acc) >>= \case
        Witness t (WInstance im) -> reduceHNF $ (fromMaybe (error $ show $ "member" <+> pShow m <+> "is not defined for" <+> pShow t) $ Map.lookup m im) acc
        x -> error $ "expected instance witness instead of " ++ ppShow x

    ENext_ _ -> reduceFail "? err"
    EAlts_ 0 (map reduceHNF -> es) -> msum $ es -- ++ error ("pattern match failure " ++ show [err | Left err <- es])
    ELet_ p x e -> matchPattern (recEnv p x) p >>= \case
        Just m' -> reduceHNF $ subst m' e
        _ -> keep

    EApp_ _ f x -> reduceHNF f >>= \(Exp f) -> case f of

        PrimFun f acc i
            | i > 0 -> reduceHNF $ Exp $ PrimFun f (x: acc) (i-1)
--            | otherwise -> error $ "too much argument for primfun " ++ ppShow f ++ ": " ++ ppShow exp

        ExtractInstance acc (j:js) n m -> reduceHNF $ Exp $ ExtractInstance (Map.insert j x acc) js n m

        EAlts_ i es | i > 0 -> reduceHNF $ Exp $ EAlts_ (i-1) $ Exp . (\f -> EApp_ (error "eae2") f x) <$> es
        EFieldProj_ _ fi -> reduceHNF x >>= \case
            ERecord fs -> case [e | (fi', e) <- fs, fi' == fi] of
                [e] -> reduceHNF e
            _ -> keep

        ELam_ p e -> matchPattern x p >>= \case
            Just m' -> reduceHNF $ subst m' e
            _ -> keep
        _ -> keep
    _ -> keep
  where
    keep = return $ Exp exp

-- TODO: make this more efficient (memoize reduced expressions)
matchPattern :: forall m . (MonadPlus m, MonadError ErrorMsg m, MonadState Int m) => Exp -> Pat -> m (Maybe Subst)       -- Left: pattern match failure; Right Nothing: can't reduce
matchPattern e = \case
    Wildcard _ -> return $ Just mempty
    PLit l -> reduceHNF e >>= \case
        ELit l'
            | l == l' -> return $ Just mempty
            | otherwise -> reduceFail $ "literals doesn't match:" <+> pShow (l, l')
    PVar _ v -> return $ Just $ singSubst' v e
    PTuple ps -> reduceHNF e >>= \e -> case e of
        ETuple xs -> fmap mconcat . sequence <$> sequence (zipWith matchPattern xs ps)
        _ -> return Nothing
    PCon t c ps -> getApp [] e >>= \case
        Just (xx, xs) -> case xx of
          EVar c'
            | c == c' -> fmap mconcat . sequence <$> sequence (zipWith matchPattern xs ps)
            | otherwise -> reduceFail $ "constructors doesn't match:" <+> pShow (c, c')
          q -> error $ "match rj: " ++ ppShow q
        Nothing | ppShow c == "V3" {-hack!-} -> do
            vs <- replicateM 3 newEName
            return $ Just $ Subst $ Map.fromList $ zip vs $ map (\n -> TApp (error "x1") (TVar (error "x2") (ExpN n)) e) ["V3x","V3y","V3z"]
        Nothing | ppShow c == "V4" {-hack!-} -> do
            vs <- replicateM 4 newEName
            return $ Just $ Subst $ Map.fromList $ zip vs $ map (\n -> TApp (error "x1") (TVar (error "x2") (ExpN n)) e) ["V4x","V4y","V4z","V4v"]
        _ -> return Nothing
    p -> error $ "matchPattern: " ++ ppShow p
  where
    getApp acc e = reduceHNF e >>= \e -> case e of
        EApp a b -> getApp (b: acc) a
        EVar n | isConstr n -> return $ Just (e, acc)
        _ -> return Nothing

evalPrimFun :: String -> [Exp] -> Exp
evalPrimFun "primIntToFloat" [EInt i] = EFloat $ fromIntegral i
evalPrimFun "primNegateFloat" [EFloat i] = EFloat $ negate i
evalPrimFun x args = error $ "evalPrimFun: " ++ x ++ " " ++ ppShow args

-------------------------------------------------------------------------------- full reduction

reduce :: Exp -> Exp
reduce = either (error "pattern match failure.") id . flip evalState 0 . runExceptT . reduceEither

reduceEither :: forall m . (MonadPlus m, MonadError ErrorMsg m, MonadState Int m) => Exp -> m Exp
reduceEither e = reduceHNF e >>= \e -> case e of
    EAlts i [e] -> return e
{-
    EAlts i es -> case [e | Right e <- runExcept . reduceEither <$> es] of     -- simplification
        [e] -> e
        es -> EAlts i es
-}
    Exp e -> Exp <$> traverse reduceEither e --TODO! traverseExp

-------------------------------------------------------------------------------- Pretty show instances

-- TODO: eliminate
showN :: N -> String
showN (N _ qs s _) = show $ hcat (punctuate (pShow '.') $ map text $ qs ++ [s])

showVar (N q _ n (NameInfo _ i)) = pShow q <> text n <> "{" <> i <> "}"

instance PShow N where
    pShowPrec p = \case
        N _ qs s (NameInfo _ i) -> hcat (punctuate (pShow '.') $ map text $ qs ++ [s]) -- <> "{" <> i <> "}"

instance PShow NameSpace where
    pShowPrec p = \case
        TypeNS -> "'"
        ExpNS -> ""

--instance PShow IdN where pShowPrec p (IdN n) = pShowPrec p n

instance PShow Lit where
    pShowPrec p = \case
        LInt    i -> pShow i
        LChar   i -> text $ show i
        LString i -> text $ show i
        LFloat  i -> pShow i
        LNat    i -> pShow i

instance PShow Witness where
    pShowPrec p = \case
        Refl -> "Refl"
        WInstance _ -> "WInstance ..."       

--        Exp k i -> pInfix (-2) "::" p i k
instance (PShow k, PShow v, PShow t, PShow p, PShow b) => PShow (Exp_ k v t p b) where
    pShowPrec p = \case
        EPrec_ e es -> pApps p e $ concatMap (\(a, b) -> [a, b]) es
        ELit_ l -> pShowPrec p l
        EVar_ k v -> pShowPrec p v
        EApp_ k a b -> pApp p a b
        ETyApp_ k a b -> pTyApp p a b
        ETuple_ a -> tupled $ map pShow a
        ELam_ p b -> "(\\" <> pShow p <+> "->" <+> pShow b <> ")"
        ETypeSig_ b t -> pShow b <+> "::" <+> pShow t
        ELet_ a b c -> "let" <+> pShow a <+> "=" <+> pShow b <+> "in" </> pShow c
        ENamedRecord_ n xs -> pShow n <+> showRecord xs
        ERecord_ xs -> showRecord xs
        EFieldProj_ k n -> "." <> pShow n
        EAlts_ i b -> pShow i <> braces (vcat $ punctuate (pShow ';') $ map pShow b)
        ENext_ k -> "SKIP"
        ExtractInstance i j n m -> "extract" <+> pShow i <+> pShow j <+> pShow n <+> pShow m
        PrimFun a b c -> "primfun" <+> pShow a <+> pShow b <+> pShow c

        Star_ -> "*"
        TCon_ k n -> pShow n
        Forall_ Nothing a b -> pInfixr (-1) "->" p a b
        Forall_ (Just n) a b -> "forall" <+> pShow n <+> "::" <+> pShow a <> "." <+> pShow b
        TTuple_ a -> tupled $ map pShow a
        TRecord_ m -> "Record" <+> showRecord (Map.toList m)
        ConstraintKind_ c -> pShowPrec p c
        Witness_ k w -> pShowPrec p w

instance PShow Exp where
    pShowPrec p e = case getLams e of
        ([], Exp e) -> pShowPrec p e
        (ps, Exp e) -> pParens (p > 0) $ "\\" <> hsep (map (pShowPrec 10) ps) <+> "->" <+> pShow e
      where
        getLams (ELam p e) = (p:) *** id $ getLams e
        getLams e = ([], e)

instance PShow ExpR where
    pShowPrec p e = case getLamsR e of
        ([], ExpR _ e) -> pShowPrec p e
        (ps, ExpR _ e) -> "\\" <> hsep (map pShow ps) <+> "->" <+> pShow e
      where
        getLamsR (ELamR' _ p e) = (p:) *** id $ getLamsR e
        getLamsR e = ([], e)

instance (PShow c, PShow v, PShow b) => PShow (Pat_ t c v b) where
    pShowPrec p = \case
        PLit_ l -> pShow l
        PVar_ t v -> pShow v
        PCon_ t s xs -> pApps p s xs
        PTuple_ a -> tupled $ map pShow a
        PRecord_ xs -> "Record" <+> showRecord xs
        PAt_ v p -> pShow v <> "@" <> pShow p
        Wildcard_ t -> "_"

instance PShow PatR where
    pShowPrec p (PatR _ e) = pShowPrec p e

instance PShow Pat where
    pShowPrec p (Pat e) = pShowPrec p e

instance (PShow n, PShow a) => PShow (TypeFun n a) where
    pShowPrec p (TypeFun s xs) = pApps p s xs

instance (PShow n, PShow a) => PShow (Constraint' n a) where
    pShowPrec p = \case
        CEq a b -> pShow a <+> "~~" <+> pShow b
        CUnify a b -> pShow a <+> "~" <+> pShow b
        CClass a b -> pShow a <+> pShow b
        Split a b c -> pShow a <+> "<-" <+> "(" <> pShow b <> "," <+> pShow c <> ")"

instance PShow TEnv where
    pShowPrec p (TEnv e) = showRecord $ Map.toList e

instance PShow Item where
    pShowPrec p = eitherItem (("Subst" <+>) . pShow) (("Sig" <+>) . pShow)

instance PShow Range where
    pShowPrec p = \case
        Range a b -> text (show a) <+> "--" <+> text (show b)
        NoRange -> ""

instance PShow Definition where
    pShowPrec p = \case
        DValueDef (ValueDef x _) -> "ValueDef" <+> pShow x
        DAxiom (TypeSig x _) -> "axiom" <+> pShow x
        DDataDef n _ _ -> "data" <+> pShow n
        GADT n _ _ -> "gadt" <+> pShow n
        ClassDef n _ _ -> "class" <+> pShow n
        InstanceDef n _ _ -> "instance" <+> pShow n
        TypeFamilyDef n _ _ -> "type family" <+> pShow n
    -- used only during parsing
        PreValueDef (_, n) _ _ -> "pre valuedef" <+> pShow n
        DTypeSig (TypeSig n _) -> "typesig" <+> pShow n
        PreInstanceDef n _ _ -> "pre instance" <+> pShow n
        ForeignDef n _ -> "foreign" <+> pShow n
        PrecDef n p -> "precdef" <+> pShow n

instance PShow FixityDir where
    pShowPrec p = \case
        FDLeft -> "infixl"
        FDRight -> "infixr"

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

-------------------------------------------------------------------------------- utils

liftIdentity :: Monad m => Identity a -> m a
liftIdentity = return . runIdentity

-------------------------------------------------------------------------------- not used

data Void

instance PShow Void where pShowPrec = error "PShow Void"
instance Eq Void where (==) = error "Eq Void"
instance Ord Void where compare = error "Ord Void"

