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
    Stenciled Depthed Colored : ImageType

icomp : ImageType → ImageType → Maybe ImageType
icomp b         Colored = just b
icomp Stenciled Depthed = just Stenciled
icomp _         _       = nothing

infixr 5 _∷_
infixr 4 _×_
infixr 4 _,_
infix  3 _⋆_

data LCIndex : Set where
    Addable NonAddable : LCIndex

data LCType : LCIndex → Set where
    Nat           : LCType Addable
    Float         : LCType Addable
    Sampler       : LCType NonAddable
    TextureFilter : LCType NonAddable
    Image         : ImageType → ℕ → LCType NonAddable
    FrameBuffer   : ImageType → ℕ → LCType NonAddable
    _×_           : ∀ {i₁ i₂} → LCType i₁ → LCType i₂ → LCType NonAddable

data _⋆_ : {i : LCIndex} → Freq → LCType i → Set where
    nat           : ∀ {f} → f ⋆ Nat
    float         : ∀ {f} → f ⋆ Float
    sampler       : ∀ f {ok : T (cObj f)} → f ⋆ Sampler
    textureFilter : ∀ f {ok : T (cObj f)} → f ⋆ TextureFilter
    _,_           : ∀ {f₁ f₂ i₁ i₂} {a : LCType i₁} {b : LCType i₂} → f₁ ⋆ a → f₂ ⋆ b → maxFreq f₁ f₂ ⋆ a × b
    image         : ∀ {lc} f {ok : T (cObj f)} (i : ImageType) → f ⋆ Image i lc
    []            : ∀ {lc} → C ⋆ FrameBuffer Colored lc
    _∷_           : ∀ {i₁ i₂ lc f₁ f₂}
      → f₁ ⋆ Image i₁ lc
      → f₂ ⋆ FrameBuffer i₂ lc
      → let v = icomp i₁ i₂ in {eq' : T (is-just v)}
      → maxFreq f₁ f₂ ⋆ FrameBuffer (fromJust v eq') lc


add : ∀ {f₁ f₂} {a : LCType Addable} → f₁ ⋆ a → f₂ ⋆ a → maxFreq f₁ f₂ ⋆ a
add nat   nat   = nat
add float float = float

swap⋆ : ∀ {i₁ i₂ f} {a : LCType i₁} {b : LCType i₂} → f ⋆ a × b → f ⋆ b × a
swap⋆ (_,_ {f₁} {f₂} a₁ a₂) rewrite maxFreq-comm f₁ f₂ = (a₂ , a₁)

example : _ ⋆ FrameBuffer _ 3
example = image Obj Stenciled
        ∷ image Obj Depthed
        ∷ image C   Colored
        ∷ image Obj Colored
        ∷ []

