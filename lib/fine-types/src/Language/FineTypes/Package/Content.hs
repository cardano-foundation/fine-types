{-# LANGUAGE DeriveGeneric #-}
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
    , addSignature
    ) where

import Prelude

import Data.Foldable (fold)
import Data.Map (Map)
import Data.Set (Set)
import Data.TreeDiff (ToExpr)
import GHC.Generics (Generic)
import Language.FineTypes.Module
    ( Import (getImportNames)
    , Module (..)
    , ModuleName
    , collectNotInScope
    , duplicatedImports
    )
import Language.FineTypes.Signature (Signature (..))
import Language.FineTypes.Typ (TypName)

import qualified Data.Map as Map
import qualified Data.Set as Set

{-----------------------------------------------------------------------------
    Package content
------------------------------------------------------------------------------}

-- | A package is a collection of module or signature definitions.
newtype Package = Package
    { packageModules :: Map ModuleName (Either Signature Module)
    }
    deriving (Eq, Show, Generic)

instance ToExpr Package

emptyPackage :: Package
emptyPackage = Package Map.empty

newtype ErrIncludePackage
    = ErrModulesAlreadyInScope (Set ModuleName)
    deriving (Eq, Show, Generic)

instance ToExpr ErrIncludePackage

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
    = -- | The module is already in scope.
      ErrModuleAlreadyInScope
    | -- | The module imports a type that is not exported.
      ErrImportNotInScope (Set (ModuleName, TypName))
    | -- | The added module uses a type which is not defined.
      ErrNamesNotInScope (Set TypName)
    | -- | The module has duplicated imports.
      ErrDuplicatedImports (Map TypName (Set ModuleName))
    deriving (Eq, Ord, Show, Generic)

instance ToExpr ErrAddModule

-- | Add a module to the current package if possible.
addModule
    :: Module -> Package -> Either ErrAddModule Package
addModule mo@Module{..} Package{..}
    | moduleName `Map.member` packageModules =
        Left ErrModuleAlreadyInScope
    | not (Set.null invalidImports) =
        Left $ ErrImportNotInScope invalidImports
    | not (Set.null namesNotInScope) =
        Left $ ErrNamesNotInScope namesNotInScope
    | not (Map.null duplicatedImports') =
        Left $ ErrDuplicatedImports duplicatedImports'
    | otherwise =
        Right
            Package
                { packageModules =
                    Map.insert
                        moduleName
                        (Right mo)
                        packageModules
                }
  where
    -- FIXME: Substitute provenance for all the defined 'Typ'!.
    namesNotInScope = collectNotInScope mo

    invalidImports = fold $ Map.mapWithKey invalidImport moduleImports
    invalidImport m =
        Set.map (m,)
            . Set.filter (not . (`isDefinedIn` m))
            . getImportNames

    isDefinedIn :: TypName -> ModuleName -> Bool
    isDefinedIn typName modName =
        case Map.lookup modName packageModules of
            Nothing -> False
            Just (Right Module{moduleDeclarations = ds}) ->
                typName `Map.member` ds
            Just (Left Signature{signatureDeclarations = ds}) ->
                typName `Set.member` ds
    duplicatedImports' = duplicatedImports mo

addSignature
    :: Signature -> Package -> Either ErrAddModule Package
addSignature sig@Signature{..} Package{..}
    | signatureName `Map.member` packageModules =
        Left ErrModuleAlreadyInScope
    | otherwise =
        Right
            Package
                { packageModules =
                    Map.insert
                        signatureName
                        (Left sig)
                        packageModules
                }
