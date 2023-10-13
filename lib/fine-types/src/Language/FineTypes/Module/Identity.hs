{-# LANGUAGE DeriveGeneric #-}

-- | \"Physical\" identities for 'Module'.
--
-- Inspired by
-- Section 3.1 in
--   S. Kilpatrick, D. Dreyer, S. Peyton Jones, S. Marlow,
--   Backpack: Retrofitting Haskell with Interfaces (2014)
--   https://plv.mpi-sws.org/backpack/
module Language.FineTypes.Module.Identity
    ( Identity (..)
    , ModuleName
    , ModuleIdentity
    , Provenance
    ) where

import Prelude

import Data.TreeDiff (ToExpr)
import GHC.Generics (Generic)

{-----------------------------------------------------------------------------
    General
------------------------------------------------------------------------------}

-- | A __\"physical\" identity__ is a name for a thing that only depends
-- on the properties of the thing itself (\"physical\").
-- It is technically true that any thing would be a good name for itself,
-- but the point is that a name should be much shorter than the thing.
--
-- Two things with equal names are equal.
--
-- For example, the __hash__ of the contents of a file is a
-- a good physical identity for that contents, because
-- it is short enough to serve as a name and only depends on the contents.
-- In contrast, a filename is not a good physical identity
-- for the file contents, because it is additional information and
-- not part of the file contents proper.
--
-- In the context of a packaging system, we will need to give identities
-- to modules which import other modules. Depending on which modules are
-- substituted for the imports, the identity of the definitions
-- may be different.
-- The solution is consider the module to be a function from
-- its imports to its module definitions, and track the physical identity
-- of any specific instantiation of imports using 'Apply'.
data Identity c
    = -- | A known identity, e.g. a hash.
      Const c
    | -- | Apply the identity of a function to a list of arguments.
      Apply (Identity c) [Identity c]
    deriving (Eq, Ord, Show, Generic)

instance ToExpr c => ToExpr (Identity c)

{-----------------------------------------------------------------------------
    Specific to Module
------------------------------------------------------------------------------}
type ModuleName = String

-- | A module 'Identity'.
--
-- We choose 'ModuleName' to be the basic identification for modules,
-- because the module name is part of the parsed module.
--
-- However, in rare circumstances, it could happen that we import
-- modules with different contents, but the same module name.
-- For such cases, a hash would be better.
type ModuleIdentity = Identity ModuleName

-- | The 'Provenance' of a 'TypName' is a the module in which this 'TypName'
-- was the defined.
type Provenance = ModuleIdentity
