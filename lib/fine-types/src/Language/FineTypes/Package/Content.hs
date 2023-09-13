{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- | A package contains named modules.
module Language.FineTypes.Package.Content
    ( Package (..)
    , emptyPackage
    , includePackage
    , addModule

      -- * Error types
    , ErrIncludePackage (..)
    , ErrAddModule (..)
    ) where

import Prelude

import Data.Foldable (fold)
import Data.Map (Map)
import Data.Set (Set)
import Language.FineTypes.Module
    ( Import (getImportNames)
    , Module (..)
    , ModuleName
    )
import Language.FineTypes.Typ (TypName)

import qualified Data.Map as Map
import qualified Data.Set as Set

{-----------------------------------------------------------------------------
    Package content
------------------------------------------------------------------------------}

-- | A package is a collection of module or signature definitions.
newtype Package = Package
    { packageModules :: Map ModuleName Module
    }
    deriving (Eq, Show)

emptyPackage :: Package
emptyPackage = Package Map.empty

newtype ErrIncludePackage
    = ErrModulesAlreadyInScope (Set ModuleName)
    deriving (Eq, Show)

-- | Include a package in the current package if possible.
includePackage
    :: Package
    -> Package
    -> Either ErrIncludePackage Package
includePackage include package
    | not (Set.null invalidModules) =
        Left $ ErrModulesAlreadyInScope invalidModules
    | otherwise =
        Right
            package{packageModules = modulesInclude <> modulesPackage}
  where
    modulesInclude = packageModules include
    modulesPackage = packageModules package

    invalidModules =
        Map.keysSet modulesInclude
            `Set.intersection` Map.keysSet modulesPackage

data ErrAddModule
    = ErrModuleAlreadyInScope
    | ErrImportNotInScope (Set (ModuleName, TypName))
    deriving (Eq, Ord, Show)

-- | Add a module to the current package if possible.
addModule
    :: Module -> Package -> Either ErrAddModule Package
addModule mo@Module{..} Package{..}
    | moduleName `Map.member` packageModules =
        Left ErrModuleAlreadyInScope
    | not (Set.null invalidImports) =
        Left $ ErrImportNotInScope invalidImports
    | otherwise =
        Right
            Package
                { packageModules = Map.insert moduleName mo packageModules
                }
  where
    -- FIXME: Substitute provenance for all the defined 'Typ'!.

    invalidImports = fold $ Map.mapWithKey invalidImport moduleImports
    invalidImport m =
        Set.map (m,)
            . Set.filter (not . (`isDefinedIn` m))
            . getImportNames

    isDefinedIn :: TypName -> ModuleName -> Bool
    isDefinedIn typName modName =
        case Map.lookup modName packageModules of
            Nothing -> False
            Just Module{moduleDeclarations = ds} ->
                typName `Map.member` ds
