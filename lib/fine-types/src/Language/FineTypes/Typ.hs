{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- | 'Typ' represents the types that can be defined with FineTypes.
module Language.FineTypes.Typ
    ( -- * Typ
      TypName
    , ConstructorName
    , FieldName
    , VarName
    , Typ (..)
    , TypConst (..)
    , OpOne (..)
    , OpTwo (..)
    , Constraint1 (..)
    , Constraint

      -- * Traversals
    , everywhere
    , everything
    , depth
    ) where

import Prelude

import Data.TreeDiff (ToExpr)
import GHC.Generics (Generic)

import qualified Data.List as L

{-----------------------------------------------------------------------------
    Typ
------------------------------------------------------------------------------}
type TypName = String
type ConstructorName = String
type FieldName = String
type VarName = String

-- | A 'Typ' describes a set of values.
--
-- We call it 'Typ', because 'Type' is reserved for Haskell types.
--
-- You can obtain new types from old types with mathematical constructions,
-- such as taking disjoint sums or cartesian products.
data Typ
    = -- | Variable
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
    | -- | A type with a value constraint
      Constrained VarName Typ Constraint
    deriving (Eq, Ord, Show, Generic)

instance ToExpr Typ

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
    deriving (Eq, Ord, Show, Generic)

instance ToExpr TypConst

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
    deriving (Eq, Ord, Show, Generic)

instance ToExpr OpOne

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
    deriving (Eq, Ord, Show, Generic)

instance ToExpr OpTwo

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
    recurse (Constrained v typ c) =
        Constrained v (every typ) c

-- | Summarise all nodes; top-down, left-to-right.
everything :: (r -> r -> r) -> (Typ -> r) -> (Typ -> r)
everything combine f = recurse
  where
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
    recurse x@(Constrained _ typ _) =
        f x `combine` recurse typ

depth :: Typ -> Int
depth = \case
    Zero{} -> 0
    One _ a -> 1 + depth a
    Two _ a b -> 1 + max (depth a) (depth b)
    ProductN fields -> 1 + maximum (fmap (depth . snd) fields)
    SumN constructors -> 1 + maximum (fmap (depth . snd) constructors)
    Constrained _ a _ -> depth a
    Var _ -> 0

type Constraint = [Constraint1]

data Constraint1
    = Braces Constraint
    | -- | String contains anything but whitespace and curly braces
      Token String
    deriving (Eq, Show, Generic, Ord)

instance ToExpr Constraint1
