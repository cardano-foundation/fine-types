-- | Convenient packaging of 'Package'
-- and 'PackageDescription' functionality.
module Language.FineTypes.Package
    ( -- * Package
      Package (..)
    , emptyPackage
    , includePackage
    , addModule
    , addSignature
    , checkAssertion

      -- * Package descriptions
    , PackageDescription (..)
    , PackageName
    , Statement (..)
    , Source (..)
    , parsePackageDescription
    , compilePackageDescription

      -- * Error types
    , ErrParsePackage (..)
    , ErrCompilePackage (..)
    , ErrIncludePackage (..)
    , ErrAddModule (..)
    , ErrAssertion (..)
    ) where

import Language.FineTypes.Package.Compile
import Language.FineTypes.Package.Content
import Language.FineTypes.Package.Description
import Language.FineTypes.Package.Parser
