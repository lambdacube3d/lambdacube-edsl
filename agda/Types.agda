open import Data.Nat
open import Data.Fin hiding (_+_)
open import Data.Product hiding (_×_)
open import Data.Bool
open import Data.Maybe
open import Data.Unit
open import Data.Empty
open import Data.Vec
open import Relation.Binary
open import Relation.Binary.PropositionalEquality
open import Relation.Nullary
open import Relation.Nullary.Decidable

fromJust : {A : Set} (ma : Maybe A) → T (is-just ma) → A
fromJust nothing ()
fromJust (just a) tt = a

finMax : {n : ℕ} → Fin n → Fin n → Fin n
finMax zero b = b
finMax (suc a) zero = suc a
finMax (suc a) (suc b) = suc (finMax a b)

finMax-comm : {n : ℕ} (f₁ f₂ : Fin n) → finMax f₁ f₂ ≡ finMax f₂ f₁
finMax-comm zero zero = refl
finMax-comm zero (suc b) = refl
finMax-comm (suc a) zero = refl
finMax-comm (suc a) (suc b) = cong suc (finMax-comm a b) 

--------------------------

data Freq : Set where
    C Obj V G F : Freq

freqToFin : Freq → Fin 5
freqToFin C   = # 0
freqToFin Obj = # 1
freqToFin V   = # 2
freqToFin G   = # 3
freqToFin F   = # 4

finToFreq : Fin 5 → Freq
finToFreq n = lookup n (C ∷ Obj ∷ V ∷ G ∷ F ∷ [])

maxFreq : Freq → Freq → Freq
maxFreq a b = finToFreq (finMax (freqToFin a) (freqToFin b))

maxFreq-comm : (f₁ f₂ : Freq) → maxFreq f₁ f₂ ≡ maxFreq f₂ f₁
maxFreq-comm a b = cong finToFreq (finMax-comm (freqToFin a) (freqToFin b))

cObj : Freq → Bool
cObj C   = true
cObj Obj = true
cObj _   = false

data ImageType : Set where
    Stenciled : ImageType
    Depthed   : ImageType
    Colored   : ImageType

icomp : ∀ {n} → ImageType → Vec ImageType n → Bool
icomp b         []            = true
icomp b         (Colored ∷ _) = true
icomp Stenciled (Depthed ∷ _) = true
icomp _         _             = false

data Primitive : Set where
    Point Line Triangle : Primitive

infixr 5 _∷_
infixr 4 _×_
infixr 4 _,_
infix  3 _⋆_

data LCIndex : Set where
    Addable NonAddable : LCIndex

data StreamKind : Set where
    IS OS : StreamKind

maxS : StreamKind → StreamKind → StreamKind
maxS IS x = x
maxS OS x = OS

data Bits : Set where
    B8 B16 B32 B64 : Bits

data Sign : Set where
    Signed UnSigned : Sign

Dim : Set
Dim = Fin 4

D1 D2 D3 D4 : Dim
D1 = # 0
D2 = # 1
D3 = # 2
D4 = # 3

mutual
  data LCType : LCIndex → StreamKind → Set where
    Int           : Bits → Sign → LCType Addable IS
    Float         : LCType Addable IS
    BoolT         : LCType NonAddable IS
    VecT          : ∀ {i} → Dim → (v : LCType i IS) {ok : T (natFloatBool v)} → LCType i IS
    Mat           : ∀ {i} → Dim → Dim → (v : LCType i IS) {ok : T (natFloatBool v)} → LCType i IS
    Array         : ∀ {i} → LCType i IS → LCType i OS
    Sampler       : LCType NonAddable OS
    TextureFilter : LCType NonAddable OS
    Image         : ImageType → ℕ → LCType NonAddable OS
    FrameBuffer   : {n : ℕ} → Vec ImageType n → ℕ → LCType NonAddable OS
    UnitT         : LCType NonAddable IS
    _×_           : ∀ {i₁ i₂ s₁ s₂} → LCType i₁ s₁  → LCType i₂ s₂ → LCType NonAddable (maxS s₁ s₂)
    RasterContext   : Primitive → ℕ → LCType NonAddable OS
    AccumulationContext : ∀ {n} → Vec ImageType n → LCType NonAddable OS
    VertexStream    : ∀ {i} → Primitive → LCType i IS → LCType NonAddable OS
    PrimitiveStream : ∀ {i} → Primitive → ℕ → LCType i IS → LCType NonAddable OS
    FragmentStream  : ∀ {i} → ℕ → LCType i IS → LCType NonAddable OS
    FetchPrimitive  : Primitive → LCType NonAddable OS

  natFloatBool : ∀ {i s} → LCType i s → Bool
  natFloatBool (Int _ _) = true
  natFloatBool Float = true
  natFloatBool BoolT = true
  natFloatBool _     = false

trITs : ∀ {n} → Vec ImageType n → LCType NonAddable IS
trITs [] = UnitT
trITs (i ∷ is) = f i × trITs is
  where
    f : ImageType → LCType Addable IS
    f Stenciled = Int B32 UnSigned
    f Depthed   = Float
    f Colored   = VecT D4 Float

Name : Set
Name = ⊤
{-
data InputType : Set where
    IBool : InputType

inputType : InputType → LCType NonAddable IS
inputType IBool = BoolT
-}
infixr 2 _⋆_➡_

data _⋆_➡_ : ∀ {i₁ i₂} → Freq → LCType i₁ IS → LCType i₂ IS → Set where
    addV      : ∀ {f} {a : LCType Addable IS} → f ⋆ a × a ➡ a
    dot3      : ∀ {f d} → f ⋆ VecT d Float × VecT d Float ➡ Float
    cross     : ∀ {f} → f ⋆ VecT D3 Float × VecT D3 Float ➡ VecT D3 Float
    transpose : ∀ {f d₁ d₂ i} {v : LCType i IS} {ok : T (natFloatBool v)} → f ⋆ Mat d₁ d₂ v {ok} ➡ Mat d₂ d₁ v {ok}
    fWith     : ∀ {d} → F ⋆ VecT d Float ➡ VecT d Float   -- TODO: skalárra is

data _⋆_ : ∀ {i s} → Freq → LCType i s → Set where
    int           : ∀ {b s} → C ⋆ Int b s
    float         : C ⋆ Float
    sampler       : ∀ f {ok : T (cObj f)} → f ⋆ Sampler
    textureFilter : ∀ f {ok : T (cObj f)} → f ⋆ TextureFilter
    _,_           : ∀ {f₁ f₂ i₁ i₂ s₁ s₂} {a : LCType i₁ s₁} {b : LCType i₂ s₂} → f₁ ⋆ a → f₂ ⋆ b → maxFreq f₁ f₂ ⋆ a × b
    image         : ∀ {lc} f {ok : T (cObj f)} (i : ImageType) → f ⋆ Image i lc

    []            : ∀ {lc} → C ⋆ FrameBuffer [] lc
    _∷_           : ∀ {n i₁ lc f₁ f₂} {vt : Vec ImageType n}
      → f₁ ⋆ Image i₁ lc
      → f₂ ⋆ FrameBuffer vt lc
      → {eq' : T (icomp i₁ vt)}
      → maxFreq f₁ f₂ ⋆ FrameBuffer (i₁ ∷ vt) lc

    input : ∀ {i} → Name → {t : LCType i IS} → Obj ⋆ t

    fetch : ∀ {p i} {a : LCType i IS} → Obj ⋆ FetchPrimitive p → Obj ⋆ Array a → Obj ⋆ VertexStream p a

    transform : ∀ {p i} {a b : LCType i IS} → V ⋆ a ➡ b → Obj ⋆ VertexStream p a → Obj ⋆ PrimitiveStream p 1 b

    rasterize : ∀ {p lc i} {a : LCType i IS} → Obj ⋆ RasterContext p lc → Obj ⋆ PrimitiveStream p lc a → Obj ⋆ FragmentStream lc a

    accumulate : ∀ {n i lc} {v : Vec ImageType n} {a : LCType i IS}
               → Obj ⋆ AccumulationContext v
               → F ⋆ a ➡ trITs v
               → Obj ⋆ FragmentStream lc a
               → Obj ⋆ FrameBuffer v lc
               → Obj ⋆ FrameBuffer v lc
{-
add : ∀ {f₁ f₂ s} {a : LCType Addable s} → f₁ ⋆ a → f₂ ⋆ a → maxFreq f₁ f₂ ⋆ a
add int   int   = int
add float float = float

swap⋆ : ∀ {i₁ i₂ s₁ s₂ f} {a : LCType i₁ s₁} {b : LCType i₂ s₂} → f ⋆ a × b → f ⋆ b × a
swap⋆ (_,_ {f₁} {f₂} a₁ a₂) rewrite maxFreq-comm f₁ f₂ = (a₂ , a₁)
-}
example : _ ⋆ FrameBuffer _ 3
example = image Obj Stenciled
        ∷ image Obj Depthed
        ∷ image C   Colored
        ∷ image Obj Colored
        ∷ []

ex : Obj ⋆ BoolT × Int B16 Signed
ex = input tt
