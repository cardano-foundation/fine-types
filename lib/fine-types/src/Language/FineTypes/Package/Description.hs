-- | A package description is a small program which evaluates to a package.
module Language.FineTypes.Package.Description where

import Prelude

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
    deriving (Eq, Show)

data Statement
    = Include PackageName Source
    | Module ModuleName Source
    | Assert Assertion
    deriving (Eq, Show)

newtype Source
    = File FilePath
    deriving (Eq, Show)

{-----------------------------------------------------------------------------
    Assertions
------------------------------------------------------------------------------}
type Assertion = ()
