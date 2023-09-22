module Language.FineTypes.Value.Def where

open import Haskell.Prelude
    hiding (_×_; _+_; Pair; case_of_; Bool; Integer )
open import Function.Base
    using (case_of_)
open import Language.FineTypes.Typ
    using (Typ; OpOne; OpTwo; _×_; _+_)

import Haskell.Prelude as Hs
import Language.FineTypes.Typ as Typ

{-----------------------------------------------------------------------------
    Values
------------------------------------------------------------------------------}
data ZeroF : @0 Typ.TypConst → Set where
  Bool : Hs.Bool → ZeroF Typ.Bool
-- Bytes : ByteString → ZeroF
  Integer : Hs.Integer → ZeroF Typ.Integer
  Natural : Nat → ZeroF Typ.Natural
--  Rational : Hs.Rational → ZeroF
--  Text : Text → ZeroF
  Unit : ZeroF Typ.Unit


data OneF (a : Set) : @0 Typ.OpOne → Set where
  Option : Maybe a → OneF a Typ.Option
  Sequence : List a → OneF a Typ.Sequence
--  PowerSet : Set a → OneF a

data TwoF (a b : Set) : @0 Typ.OpTwo → Set where
--  FiniteMap : Map a b → TwoF a b

Ix = Hs.Int

data Value : @0 Typ → Set where
  Zero    : ∀ {@0 A : Typ.TypConst} → ZeroF A → Value (Typ.Zero A)
  One     : ∀ {@0 A : Typ} {@0 op : OpOne}
          → OneF (Value A) op → Value (Typ.One op A)
  Two     : ∀ {@0 A B : Typ} {@0 op : OpTwo}
          → TwoF (Value A) (Value B) op
          → Value (Typ.Two op A B)
  Product2 : ∀ {@0 A B : Typ} → Value A → Value B → Value (A × B)
  Sum2L    : ∀ {@0 A B : Typ} → Value A → Value (A + B)
  Sum2R    : ∀ {@0 A B : Typ} → Value B → Value (A + B)

--  Product : ∀ {@0 A : Typ} → List Value → Value
--  Sum     : Ix → Value → Value

{-# COMPILE AGDA2HS ZeroF #-}
{-# COMPILE AGDA2HS OneF #-}
{-# COMPILE AGDA2HS TwoF #-}
{-# COMPILE AGDA2HS Ix #-}
{-# COMPILE AGDA2HS Value #-}

{-----------------------------------------------------------------------------
    Equality of Value
------------------------------------------------------------------------------}
eqZeroF : ∀{@0 A B : Typ.TypConst} → ZeroF A → ZeroF B → Hs.Bool
eqZeroF (Bool x) (Bool y) = x == y
eqZeroF (Bool _) _ = False
eqZeroF (Integer x) (Integer y) = True
eqZeroF (Integer _) _ = False
eqZeroF (Natural _) (Natural _) = True
eqZeroF (Natural _) _ = False
eqZeroF Unit Unit = False
eqZeroF Unit _ = False

-- mutually recursive definitions
eqValue
  : ∀{@0 A B : Typ} → Value A → Value B → Hs.Bool
eqMaybeValue
  : ∀{@0 A B : Typ}
  → Maybe (Value A) → Maybe (Value B) → Hs.Bool
eqListValue
  : ∀{@0 A B : Typ}
  → List (Value A) → List (Value B) → Hs.Bool

eqMaybeValue (Just x) (Just y) = eqValue x y
eqMaybeValue (Just _) _ = False
eqMaybeValue Nothing Nothing = True
eqMaybeValue Nothing _ = False

eqListValue (x ∷ xs) (y ∷ ys) = eqValue x y && eqListValue xs ys
eqListValue (_ ∷ _) _ = False
eqListValue [] [] = True
eqListValue [] _ = False

eqValue (Zero x) (Zero y) = eqZeroF x y
eqValue (Zero _) _ = False
eqValue (One (Option x)) (One (Option y)) = eqMaybeValue x y
eqValue (One (Option _)) _ = False
eqValue (One (Sequence x)) (One (Sequence y)) = eqListValue x y
eqValue (One (Sequence _)) _ = False
eqValue (Two _) (Two _) = True
eqValue (Two _) _ = False
eqValue (Product2 x1 x2) (Product2 y1 y2) =
    (eqValue x1 y1 && eqValue x2 y2)
eqValue (Product2 _ _) _ = False
eqValue (Sum2L x) (Sum2L y) = eqValue x y
eqValue (Sum2L _) _ = False
eqValue (Sum2R x) (Sum2R y) = eqValue x y
eqValue (Sum2R _) _ = False

{-# COMPILE AGDA2HS eqZeroF #-}
{-# COMPILE AGDA2HS eqValue #-}
{-# COMPILE AGDA2HS eqMaybeValue #-}
{-# COMPILE AGDA2HS eqListValue #-}

{-
data Eq-runtime : {@0 A B : Typ} → Value A → Val B → Set where
  eq-Unit : Eq-runtime Unit Unit
  eq-Pair
    : ∀ {@0 A1 A2 B1 B2 : Typ}
      {a1 : Val A1} {a2 : Val A2}
      {b1 : Val B1} {b2 : Val B2}
    → Eq-runtime a1 a2
    → Eq-runtime b1 b2
    → Eq-runtime (Pair a1 b1) (Pair a2 b2)
  eq-SumL
    : ∀ {@0 A1 A2 B1 B2 : Typ}
      {a1 : Val A1} {a2 : Val A2}
    → Eq-runtime a1 a2
    → Eq-runtime (SumL {A1} {B1} a1) (SumL {A2} {B2} a2)
  eq-SumR
    : ∀ {@0 A1 A2 B1 B2 : Typ}
      {b1 : Val B1} {b2 : Val B2}
    → Eq-runtime b1 b2
    → Eq-runtime (SumR {A1} {B1} b1) (SumR {A2} {B2} b2)
-}

-- Lemma:
-- When to values are related by a run-time equality,
-- we can translate this into a function that maps one into the other.

