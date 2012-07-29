import Data.Set (Set)
import qualified Data.Set as Set
import Data.Set.Unicode
import Control.Monad.Writer

type Id = String

data TyPrim = TyBool
            | TyInt
            | TyUInt
            | TyFloat
            deriving (Eq, Show)

data Phase = V
           | VOut
           deriving (Eq, Show)

data TyPhase = Phase Phase
             | PhaseVar Tv
             deriving Show

data TyVec = Vector
           | Matrix
           deriving (Eq, Show)

{-
data VecTy = Scalar PrimTy
           | Vector PrimTy Int
           | Matrix PrimTy Int Int
           deriving Show

data ValTy = TyVec VecTy
           | TyTuple [ValTy]
           | TyRec Id
           deriving Show

data InterpTy = PointTy ValTy
              | InterpTy ValTy
              deriving Show

data FunTy = TyValue InterpTy Phase
           | TyFun FunTy FunTy
           deriving Show

(~>) = TyFun
infixr ~>

tyVertexOut :: FunTy
tyVertexOut = TyValue (PointTy $ TyVec $ Vector TyFloat 4) V
-}

type Tv = Id

data TyDim = TyDimLit Int
           | TyDimVar Tv
           deriving Show

data Ty = TyVar Tv
        | TyFun
        | TyApp Ty Ty
        | TyDim TyDim
        | TyInterp Ty
        | TyPrim TyPrim
        | TyTuple Int
        | TyRec Id
        | TyVec TyVec
        | TyPhase TyPhase
        deriving Show

data TyPred a = PredContinuous a
              | PredLinear a
              | PredNum a
              | PredRec a [(Id, Ty)]
              deriving Show

type PolyPred = TyPred Tv

data PolyTy = PTy [PolyPred] Ty
            deriving Show

tyFloat :: Ty
tyFloat = TyPrim TyFloat

infixr ~>
(~>) :: Ty -> Ty -> Ty
t ~> u = TyApp (TyApp TyFun t) u

at :: Ty -> Phase -> Ty
t `at` p = TyApp (TyPhase . Phase $ p) t

at' :: Ty -> Tv -> Ty
t `at'` x = TyApp (TyPhase . PhaseVar $ x) t

vec :: Ty -> Int -> Ty
vec t n = TyApp (TyApp (TyVec Vector) (TyDim $ TyDimLit n)) t

-- vertexOut :: Float[4]@V -> Float@V -> Interp a@V -> a@V*
ptyVertexOut :: PolyTy
ptyVertexOut = PTy [] $ (v4f `at` V) ~> (tyFloat `at` V) ~> (TyInterp a `at` V) ~> (a `at` VOut)
  where
    a = TyVar "a"
    v4f = vec tyFloat 4

-- smooth :: (Continuous a) => a@p -> Interp a@p
ptySmooth :: PolyTy
ptySmooth = PTy [PredContinuous "a"] $ (a `at'` p) ~> (TyInterp a `at'` p)
  where
    a = TyVar "a"
    p = "p"

-- 1 :: (Num a) => a@p
ptyIntLit :: PolyTy
ptyIntLit = PTy [PredNum "a"] $ TyVar "a"

-- (*) :: (Linear a) => a@p -> a@p -> a@p
ptyMul :: PolyTy
ptyMul = PTy [PredLinear "a"] $ a ~> a ~> a
  where
    a = TyVar "a" `at'` "p"

data Coord = CoordX
           | CoordY
           | CoordZ
           | CoordW
           deriving Show

type Var = Id

data Expr = EVar Var
          | EApp Expr Expr
          | EInfixApp Expr Var Expr
          | EIntLit Int
          | EVec [Expr]
          | ERecSel Expr Id
          | EVecSel Expr [Coord] -- XXX
          deriving Show

infixl @@
(@@) :: Expr -> Expr -> Expr
(@@) = EApp

data Pat = PVar Var
         deriving Show

data Match = Match [Pat] Expr
           deriving Show

data Decl = VarDef Var (Maybe PolyTy) Expr [Decl]
          | FunDef Var (Maybe PolyTy) [Match] [Decl]
          deriving Show

-- vert :: InputGeometry@V -> Float[3]@V*
-- vert input = vertexOut position 1 (smooth normal)
--   where position, normal
dVert :: Decl
dVert = FunDef "vert" Nothing [Match [PVar "input"] eVert] [dPosition, dNormal]
  where
    x `mul` y = EInfixApp x "*" y

    -- ptyVert =
    tyVert = (TyRec "InputGeometry" `at` V) ~> (v3f `at` VOut)
      where
        v3f = vec tyFloat 3

    eVert :: Expr
    eVert = EVar "vertexOut" @@ EVar "position" @@ EIntLit 1 @@ (EVar "smooth" @@ EVar "normal")

    -- position :: Float[4]@V
    -- position = cameraProjection * cameraView * [input.position, 1]
    dPosition = VarDef "position" Nothing ePosition []
    ePosition :: Expr
    ePosition = EVar "cameraProjection" `mul`
                EVar "cameraView" `mul`
                EVec [ERecSel (EVar "input") "position", EIntLit 1]

    -- normal :: Float[3]@V
    -- normal = (worldPosition * [input.normal, 0]).xyz
    dNormal = VarDef "normal" Nothing eNormal []
    eNormal :: Expr
    eNormal = EVecSel (EVar "worldPosition" `mul`
                       EVec [ERecSel (EVar "input") "normal", EIntLit 0])
              [CoordX, CoordY, CoordZ]

data TyEq = Ty :~: Ty
data Unification = Skip
                 | Substitute Tv Ty
                 | Recurse [TyEq]
                 | Flip
                 | Incongruent

unify1 :: TyEq -> Unification
unify1 tyEq = case tyEq of
    TyVar x     :~: t                     -> Substitute x t
    t           :~: TyVar x               -> Flip
    TyDim n     :~: TyDim m               -> unifyDim n m
    TyFun       :~: TyFun                 -> Skip
    TyApp t u   :~: TyApp t' u'           -> Recurse [t :~: t', u :~: u']
    TyInterp t  :~: TyInterp t'           -> Recurse [t :~: t']
    TyPrim p    :~: TyPrim p'   | p == p' -> Skip
    TyTuple n   :~: TyTuple n'  | n == n' -> Skip
    TyRec r     :~: TyRec r'    | r == r' -> Skip
    TyVec v     :~: TyVec v'    | v == v' -> Skip
    TyPhase p   :~: TyPhase p'            -> unifyPhase p p'
    _                                     -> Incongruent
  where
    unifyDim  (TyDimVar x)  n                        = Substitute x (TyDim n)
    unifyDim  n             (TyDimVar x)             = Flip
    unifyDim  (TyDimLit n)  (TyDimLit n') | n == n'  = Skip
    unifyDim  _             _                        = Incongruent

    unifyPhase  (PhaseVar x)  p                    = Substitute x (TyPhase p)
    unifyPhase  p             (PhaseVar x)         = Flip
    unifyPhase  (Phase p)     (Phase p') | p == p' = Skip
    unifyPhase  _             _                    = Incongruent

data Substitution = Substitution deriving Show
emptySubst = Substitution

substTy :: Substitution -> Ty -> Ty
substTy θ t = undefined

-- subst

occurs :: Tv -> Ty -> Maybe Bool
occurs x t = case t of
    TyVar y | y == x -> Nothing
    TyPhase (PhaseVar y) | y == x -> Nothing
    TyDim (TyDimVar y) | y == x -> Nothing
    _ -> Just $ x ∈ tyVars t

tyVars :: Ty -> Set Tv
tyVars = execWriter .  go
  where
    go t = case t of
        TyVar x -> collect x
        TyApp t u -> go t >> go u
        TyDim (TyDimVar x) -> collect x
        TyInterp t -> go t
        TyPhase (PhaseVar x) -> collect x
        _ -> return ()

    collect = tell . Set.singleton

data TypingError = TEOccurs Tv Ty
                 deriving Show

addSubst :: Tv -> Ty -> Substitution -> Either TypingError Substitution
addSubst x t θ = case occurs x t of
    Nothing -> return θ
    Just True -> Left $ TEOccurs x t
    Just False -> return undefined

data SrcLoc = SrcLoc deriving Show

-- unify :: Bool -> [(SrcLoc, TyEq)] -> Either TypingError Substitution
