module Language.FineTypes.Elaborate
    ( elaborates
    ) where

import Prelude

import Language.FineTypes.Typ
    ( Typ (..), OpTwo (..) )

{-----------------------------------------------------------------------------
    Elaboration
------------------------------------------------------------------------------}
-- | Type A elaborates type B if it adds more specific information,
-- such as field or constructor names, while representing the same structure.
--
-- Specifically,
--
-- * Every type elaborates 'Abstract'.
-- * A record type with field names elaborates a product.
-- * A sum type with constructor names elaborates a disjoint sum.
elaborates :: Typ -> Typ -> Bool
elaborates _ Abstract = True
elaborates (Var name1) (Var name2) = name1 == name2
elaborates (Zero c1) (Zero c2) = c1 == c2
elaborates (One op1 a1) (One op2 a2)
    = op1 == op2 && elaborates a1 a2
elaborates (Two op1 a1 b1) (Two op2 a2 b2)
    = op1 == op2 && elaborates a1 a2 && elaborates b1 b2
elaborates (ProductN fields) a
    | Just components <- matchProduct a =
        elaboratesSeq (map snd fields) components
    | otherwise = False
elaborates (SumN fields) a
    | Just summands <- matchSum a =
        elaboratesSeq (map snd fields) summands
    | otherwise = False
elaborates _ _ = False

-- | Check whether a sequence of types elaborates another sequence.
elaboratesSeq:: [Typ] -> [Typ] -> Bool
elaboratesSeq [] [] = True
elaboratesSeq (x:xs) (y:ys) = elaborates x y && elaboratesSeq xs ys
elaboratesSeq _ _ = False

-- | Match a type @X@ against a product @A1 × A2 × … × An@
-- with at least two components.
--
-- Association does not matter, i.e. @(A × B) × C = A × (B × C)@ have
-- the same components.
matchProduct :: Typ -> Maybe [Typ]
matchProduct typ = case typ of
    e@(Two Product2 _ _) -> Just $ match e
    _ -> Nothing
  where
    match (Two Product2 a b) = match a <> match b
    match e = [e]

-- | Match a type @X@ against a sum @A1 ⊎ A2 ⊎ … ⊎ An@
-- with at least two summands.
--
-- Association does not matter, i.e. @(A ⊎ B) ⊎ C = A ⊎ (B ⊎ C)@ have
-- the same summands.
matchSum :: Typ -> Maybe [Typ]
matchSum typ = case typ of
    e@(Two Sum2 _ _) -> Just $ match e
    _ -> Nothing
  where
    match (Two Sum2 a b) = match a <> match b
    match e = [e]
