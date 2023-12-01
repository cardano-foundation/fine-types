module Data.Embedding where

{-# FOREIGN AGDA2HS
-- !!! This Haskell module has been autogenerated by agda2hs.
-- !!! Do NOT change; change the original .agda file instead.
#-}

open import Haskell.Prelude

{-----------------------------------------------------------------------------
    Embedding
------------------------------------------------------------------------------}
record Embedding (a b : Set) : Set where
  field
    to   : a → b
    from : b → a
    @0 from∘to : ∀ (x : a) → from (to x) ≡ x

open Embedding public

{-# COMPILE AGDA2HS Embedding #-}
