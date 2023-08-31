-- | 'Typ' represents the types that can be defined with FineTypes.
module Language.FineTypes.Typ
    ( -- * Typ
      TypName
    , ConstructorName
    , FieldName
    , Typ (..)
    , TypConst (..)
    , OpOne (..)
    , OpTwo (..)

      -- * Traversals
    , everywhere
    , everything
    ) where

import Prelude
    ( Eq
    , Ord
    , Show
    , String
    , (.)
    )

import qualified Data.List as L

{-----------------------------------------------------------------------------
    Typ
------------------------------------------------------------------------------}
type TypName = String
type ConstructorName = String
type FieldName = String

-- | A 'Typ' describes a set of values.
--
-- We call it 'Typ', because 'Type' is reserved for Haskell types.
--
-- You can obtain new types from old types with mathematical constructions,
-- such as taking disjoint sums or cartesian products.
data Typ
    = -- | 'Typ' which is unspecified.
      Abstract
    | -- | Variable
      Var TypName
    | -- | Known 'Typ' constant.
      Zero TypConst
    | -- | Apply an unary operation to a 'Typ'.
      One OpOne Typ
    | -- | Apply a binary operation to two 'Typ's.
      Two OpTwo Typ Typ
    | -- | Cartesian product with component names.
      ProductN [(FieldName, Typ)]
    | -- | Disjoint union with constructor names.
      SumN [(ConstructorName, Typ)]
    deriving (Eq, Ord, Show)

-- | Predefined 'Typ'.
data TypConst
    = -- | Booleans, @Bool@.
      Bool
    | -- | Sequences of bytes, @Bytes@.
      Bytes
    | -- | Integers, @ℤ@.
      Integer
    | -- | Natural numbers, @ℕ@.
      Natural
    | -- | A sequence of unicode characters, @Text@.
      Text
    | -- | Type with a single element, @Unit@.
      Unit
    | -- | Rational numbers, @ℚ@.
      Rational
    deriving (Eq, Ord, Show)

-- | Unary operations on 'Typ'.
data OpOne
    = -- | Given a set A, A? is the set of elements of A
      -- and a separate element (often called "null").
      Option
    | -- | Given a set A, A* is the set of sequences
      -- having elements taken from A.
      Sequence
    | -- | Given a set A, ℙ A is the set of all the subsets of A.
      PowerSet
    deriving (Eq, Ord, Show)

-- | Binary operations on 'Typ'.
data OpTwo
    = -- | A ⊎ B  denotes the disjoint union of A and B.
      Sum2
    | -- | A × B  denotes the cartesian product of A and B.
      Product2
    | -- | A ↦ B  denotes a partial function from A to B,
      -- which can be seen as a map (dictionary)
      -- with keys in A and values in B.
      PartialFunction
    | -- | A →∗ B denotes a finitely supported partial function.
      FiniteSupport
    deriving (Eq, Ord, Show)

{-----------------------------------------------------------------------------
    Traversals

    Terminology from "Scrap your boilerplate"
    https://www.microsoft.com/en-us/research/wp-content/uploads/2003/01/hmap.pdf
------------------------------------------------------------------------------}

-- | Apply a transformation everywhere; bottom-up.
everywhere :: (Typ -> Typ) -> Typ -> Typ
everywhere f = every
  where
    every = f . recurse

    recurse Abstract =
        Abstract
    recurse a@(Var _) =
        a
    recurse a@(Zero _) =
        a
    recurse (One op a) =
        One op (every a)
    recurse (Two op a b) =
        Two op (every a) (every b)
    recurse (ProductN nas) =
        ProductN [(n, every a) | (n, a) <- nas]
    recurse (SumN nas) =
        SumN [(n, every a) | (n, a) <- nas]

-- | Summarise all nodes; top-down, left-to-right.
everything :: (r -> r -> r) -> (Typ -> r) -> (Typ -> r)
everything combine f = recurse
  where
    recurse x@Abstract =
        f x
    recurse x@(Var _) =
        f x
    recurse x@(Zero _) =
        f x
    recurse x@(One _ a) =
        f x `combine` recurse a
    recurse x@(Two _ a b) =
        f x `combine` (recurse a `combine` recurse b)
    recurse x@(ProductN nas) =
        L.foldl' combine (f x) [recurse a | (_, a) <- nas]
    recurse x@(SumN nas) =
        L.foldl' combine (f x) [recurse a | (_, a) <- nas]
