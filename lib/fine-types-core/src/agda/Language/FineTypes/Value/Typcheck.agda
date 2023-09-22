module Language.FineTypes.Value.Typcheck where

open import Haskell.Prelude
    hiding (_×_; _+_; Pair; case_of_; Bool; Integer )
open import Function.Base
    using (case_of_)

open import Language.FineTypes.Value.Def
open import Language.FineTypes.Typ
    using (Typ)

import Haskell.Prelude as Hs
import Language.FineTypes.Typ as Typ

{-----------------------------------------------------------------------------
    Haskell — type checking
------------------------------------------------------------------------------}
typOf0 : ∀{@0 A : Typ.TypConst} → ZeroF A -> Typ.TypConst
typOf0 a = case a of λ where
    (Bool _) -> Typ.Bool
--    Bytes _ -> Typ.Bytes
    (Integer _) -> Typ.Integer
    (Natural _) -> Typ.Natural
--    Text _ -> Typ.Text
    Unit -> Typ.Unit
--    Rational _ -> Typ.Rational

-- mutually recursive definitions
hasTyp
    : ∀{@0 A : Typ} → Value A → Typ → Hs.Bool
hasTyp1
    : ∀{@0 A : Typ} {@0 op : Typ.OpOne}
    → OneF (Value A) op
    -> Typ.OpOne -> Typ -> Hs.Bool
hasTyp2
    : ∀{@0 A B : Typ} {@0 op : Typ.OpTwo}
    → TwoF (Value A) (Value B) op
    → Typ.OpTwo -> Typ -> Typ -> Hs.Bool

hasTyp1 (Option v) Typ.Option t =
    all (λ v → hasTyp v t) v
hasTyp1 (Option _) _ _ =
    False
hasTyp1 (Sequence vs) Typ.Sequence t =
    all (λ v → hasTyp v t) vs
hasTyp1 (Sequence _) _ _ =
    False

hasTyp2 = λ()

hasTyp (Zero a) (Typ.Zero b) =
    typOf0 a == b
hasTyp (Zero _) _ =
    False
hasTyp (One a) (Typ.One op typ) =
    hasTyp1 a op typ
hasTyp (One _) _ =
    False
hasTyp (Product2 a b) (Typ.Two Typ.Product2 ta tb) =
    (hasTyp a ta) && (hasTyp b tb)
hasTyp (Product2 _ _) _ =
    False
hasTyp (Sum2L a) (Typ.Two Typ.Sum2 ta _) =
    hasTyp a ta
hasTyp (Sum2L _) _ =
    False
hasTyp (Sum2R b) (Typ.Two Typ.Sum2 _ tb) =
    hasTyp b tb
hasTyp (Sum2R _) _ =
    False
hasTyp (Two a) (Typ.Two op typ1 typ2) =
    hasTyp2 a op typ1 typ2
hasTyp (Two _) _ =
    False

{-# COMPILE AGDA2HS typOf0 #-}
{-# COMPILE AGDA2HS hasTyp1 #-}
{-# COMPILE AGDA2HS hasTyp2 #-}
{-# COMPILE AGDA2HS hasTyp #-}

{-----------------------------------------------------------------------------
    Agda
------------------------------------------------------------------------------}
lemma-hasTyp-accepts-own-Typ
  : ∀ (A : Typ) (v : Value A)
  → hasTyp v A ≡ True

lemma-sequence-accepts-own-Typ
  : ∀ (B : Typ) (vs : List (Value B))
  → hasTyp
    (One (Sequence vs))
    (Typ.One Typ.Sequence B) ≡ True

lemma-sequence-accepts-own-Typ B [] = refl
lemma-sequence-accepts-own-Typ B (v ∷ vs)
    rewrite lemma-hasTyp-accepts-own-Typ B v
    rewrite lemma-sequence-accepts-own-Typ B vs
    = refl

lemma-hasTyp-accepts-own-Typ (Typ.Zero Typ.Unit) (Zero Unit)
    = refl
lemma-hasTyp-accepts-own-Typ (Typ.Zero Typ.Bool) (Zero (Bool _))
    = refl
lemma-hasTyp-accepts-own-Typ (Typ.Zero Typ.Integer) (Zero (Integer x))
    = refl
lemma-hasTyp-accepts-own-Typ (Typ.Zero Typ.Natural) (Zero (Natural x))
    = refl
lemma-hasTyp-accepts-own-Typ (Typ.One Typ.Option A) (One (Option Nothing))
    = refl
lemma-hasTyp-accepts-own-Typ (Typ.One Typ.Option A) (One (Option (Just x)))
    = lemma-hasTyp-accepts-own-Typ A x
lemma-hasTyp-accepts-own-Typ (Typ.One Typ.Sequence A) (One (Sequence []))
    = refl
lemma-hasTyp-accepts-own-Typ (Typ.One Typ.Sequence A) (One (Sequence (v ∷ vs)))
    rewrite lemma-hasTyp-accepts-own-Typ A v
    rewrite lemma-sequence-accepts-own-Typ A vs
    = refl
lemma-hasTyp-accepts-own-Typ (Typ.Two Typ.Product2 A1 A2) (Product2 v1 v2)
    rewrite lemma-hasTyp-accepts-own-Typ A1 v1
    rewrite lemma-hasTyp-accepts-own-Typ A2 v2
    = refl
lemma-hasTyp-accepts-own-Typ (Typ.Two Typ.Sum2 A1 A2) (Sum2L v1)
    = lemma-hasTyp-accepts-own-Typ A1 v1
lemma-hasTyp-accepts-own-Typ (Typ.Two Typ.Sum2 A1 A2) (Sum2R v2)
    = lemma-hasTyp-accepts-own-Typ A2 v2

{-----------------------------------------------------------------------------
    Type checking on the value level
------------------------------------------------------------------------------}
{-
lemma-hasTyp-respects-equality
  : ∀ (A B : Typ) (v : Value A) (w : Value B)
  → eqValue v w ≡ True
  → hasTyp v A ≡ hasTyp w A

lemma-eqZeroF-respects-equality
  : ∀ {A B : Typ.TypConst} (v : ZeroF A) (w : ZeroF B)
  → eqZeroF v w ≡ True
  → typOf0 v ≡ typOf0 w

lemma-eqZeroF-respects-equality (Bool _) (Bool _) = λ _ → refl
lemma-eqZeroF-respects-equality (Bool _) (Integer x) = λ()
lemma-eqZeroF-respects-equality (Bool _) (Natural x) = λ()
lemma-eqZeroF-respects-equality (Bool _) Unit = λ()
lemma-eqZeroF-respects-equality (Integer _) (Integer _) = λ _ → refl
lemma-eqZeroF-respects-equality (Integer _) (Bool x) = λ ()
lemma-eqZeroF-respects-equality (Integer _) (Natural x) = λ ()
lemma-eqZeroF-respects-equality (Integer _) Unit = λ ()
lemma-eqZeroF-respects-equality (Natural _) (Natural _) = λ _ → refl
lemma-eqZeroF-respects-equality (Natural _) (Bool x) = λ ()
lemma-eqZeroF-respects-equality (Natural _) (Integer x) = λ ()
lemma-eqZeroF-respects-equality (Natural _) Unit = λ ()
lemma-eqZeroF-respects-equality Unit Unit = λ _ → refl
lemma-eqZeroF-respects-equality Unit (Bool x) = λ ()
lemma-eqZeroF-respects-equality Unit (Integer x) = λ ()
lemma-eqZeroF-respects-equality Unit (Natural x) = λ ()

lemma-hasTyp-respects-equality _ _ (Zero x) (Zero y) p
    rewrite lemma-eqZeroF-respects-equality x y p
    = {!  !}
lemma-hasTyp-respects-equality (Typ.Zero x) (Typ.One x₂ B) (Zero x₁) (One x₃) ()
lemma-hasTyp-respects-equality (Typ.Zero x) (Typ.Two .Typ.Product2 B B₁) (Zero x₁) (Product2 w w₁) ()
lemma-hasTyp-respects-equality (Typ.Zero x) (Typ.Two .Typ.Sum2 B B₁) (Zero x₁) (Sum2L w) ()
lemma-hasTyp-respects-equality (Typ.Zero x) (Typ.Two .Typ.Sum2 B B₁) (Zero x₁) (Sum2R w) ()
lemma-hasTyp-respects-equality (Typ.One x A) B v w = {!   !}
lemma-hasTyp-respects-equality (Typ.Two x A A₁) B v w = {!   !}
-}

{-
data Typchecks {@0 A : Typ} (va : Val A) (B : Typ) : Set where
  Checks
    : (vb : Val B)
    → Eq-runtime va vb
    → Typchecks va B

-- | Helper function that recurses into the typ and returns proof.
checkTyp
  : ∀ {@0 A : Typ} (v : Val A) (B : Typ)
  → Maybe (Typchecks v B)
checkTyp Unit (Zero TUnit) =
  Just (Checks Unit eq-Unit)
checkTyp (Pair x y) (Two Product2 X Y) =
  case (checkTyp x X) of λ
    { Nothing → Nothing
    ; (Just (Checks x' ex)) →
      case (checkTyp y Y) of λ
        { Nothing → Nothing
        ; (Just (Checks y' ey)) →
          Just (Checks (Pair x' y') (eq-Pair ex ey))
      }
    }
checkTyp (SumL x) (Two Sum2 X _) =
  case (checkTyp x X) of λ
    { Nothing → Nothing
    ; (Just (Checks x' ex)) →
      Just (Checks (SumL x') (eq-SumL ex))
    }
checkTyp (SumR y) (Two Sum2 _ Y) =
  case (checkTyp y Y) of λ
    { Nothing → Nothing
    ; (Just (Checks y' ey)) →
      Just (Checks (SumR y') (eq-SumR ey))
    }
checkTyp _ _ = Nothing


lemma-upgrade-hasTyp
  : ∀ {@0 A : Typ} (B : Typ) (va : Val A)
  → hasTyp va B ≡ isJust (checkTyp va B)
lemma-upgrade-hasTyp (Zero TUnit) Unit = refl
lemma-upgrade-hasTyp (Two Product2 A B) (Pair a b)
  rewrite lemma-upgrade-hasTyp A a
  rewrite lemma-upgrade-hasTyp B b
    with checkTyp a A
... | Nothing = refl
... | Just (Checks _ _) with checkTyp b B
...   | Nothing  = refl
...   | Just (Checks _ _) = refl
lemma-upgrade-hasTyp (Two Sum2 A _) (SumL a)
  rewrite lemma-upgrade-hasTyp A a
    with checkTyp a A
... | Nothing = refl
... | (Just (Checks x' ex)) = refl
lemma-upgrade-hasTyp (Two Sum2 _ B) (SumR b)
  rewrite lemma-upgrade-hasTyp B b
    with checkTyp b B
... | Nothing = refl
... | (Just (Checks y' ey)) = refl
lemma-upgrade-hasTyp (Zero TBool) Unit = refl
lemma-upgrade-hasTyp (Zero TNatural) Unit = refl
lemma-upgrade-hasTyp (One x B) Unit = refl
lemma-upgrade-hasTyp (Two x B B₁) Unit = refl
lemma-upgrade-hasTyp (Zero x) (Pair va va₁) = refl
lemma-upgrade-hasTyp (One x B) (Pair va va₁) = refl
lemma-upgrade-hasTyp (Two Sum2 B B₁) (Pair va va₁) = refl
lemma-upgrade-hasTyp (Zero x) (SumL va) = refl
lemma-upgrade-hasTyp (One x B) (SumL va) = refl
lemma-upgrade-hasTyp (Two Product2 B B₁) (SumL va) = refl
lemma-upgrade-hasTyp (Zero x) (SumR va) = refl
lemma-upgrade-hasTyp (One x B) (SumR va) = refl
lemma-upgrade-hasTyp (Two Product2 B B₁) (SumR va) = refl

-}