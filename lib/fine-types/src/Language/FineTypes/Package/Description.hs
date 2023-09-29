{-# LANGUAGE DeriveGeneric #-}

-- | A package description is a small program which evaluates to a package.
module Language.FineTypes.Package.Description where

import Prelude

import Data.TreeDiff (ToExpr)
import GHC.Generics (Generic)
import Language.FineTypes.Module (ModuleName)

{-----------------------------------------------------------------------------
    Package Description
------------------------------------------------------------------------------}
type PackageName = String

-- | A 'PackageDescription' is a sequence of 'Statement' that evaluate
-- to a 'Package'.
data PackageDescription = PackageDescription
    { packageName :: PackageName
    , packageStatements :: [Statement]
    }
    deriving (Eq, Show, Generic)

instance ToExpr PackageDescription

data Statement
    = Include PackageName Source
    | Module ModuleName Source
    | Signature ModuleName Source
    | Assert Assertion
    deriving (Eq, Show, Generic)

instance ToExpr Statement

newtype Source
    = File FilePath
    deriving (Eq, Show, Generic)

instance ToExpr Source

{-----------------------------------------------------------------------------
    Assertions
------------------------------------------------------------------------------}
type Assertion = ()
