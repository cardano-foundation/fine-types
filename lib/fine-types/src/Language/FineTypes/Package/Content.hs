{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- | A package contains named modules.
module Language.FineTypes.Package.Content
    ( Package (..)
    , emptyPackage
    , includePackage
    , addModule
    , addSignature
    , checkAssertion

      -- * Error types
    , ErrIncludePackage (..)
    , ErrAddModule (..)
    , ErrAssertion (..)
    ) where

import Prelude

import Data.Foldable (fold)
import Data.Map (Map)
import Data.Maybe (isNothing)
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
import Language.FineTypes.Module.Identity
    ( Identity (..)
    , ModuleIdentity
    )
import Language.FineTypes.Module.Instance
    ( ModuleInstance (..)
    , getDeclarations
    , mkModuleInstance
    )
import Language.FineTypes.Package.Description
    ( Assertion (..)
    )
import Language.FineTypes.Signature (Signature (..))
import Language.FineTypes.Typ (TypName)

import qualified Data.Map as Map
import qualified Data.Set as Set

{-----------------------------------------------------------------------------
    Type
------------------------------------------------------------------------------}

-- | A package is a collection of module or signature definitions.
newtype Package = Package
    { packageModules :: Map ModuleName (Either Signature ModuleInstance)
    }
    deriving (Eq, Show, Generic)

instance ToExpr Package

emptyPackage :: Package
emptyPackage = Package Map.empty

{-----------------------------------------------------------------------------
    Operations
    Include a package
------------------------------------------------------------------------------}

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

{-----------------------------------------------------------------------------
    Operations
    Add a module
------------------------------------------------------------------------------}

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

-- | Add a module (under a given name) to the current 'Package' if possible.
addModule
    :: ModuleName -> Module -> Package -> Either ErrAddModule Package
addModule name mo@Module{..} pkg@Package{packageModules}
    | name `Map.member` packageModules =
        Left ErrModuleAlreadyInScope
    | not (Set.null invalidImports) =
        Left $ ErrImportNotInScope invalidImports
    | not (Set.null namesNotInScope) =
        Left $ ErrNamesNotInScope namesNotInScope
    | not (Map.null duplicatedImports') =
        Left $ ErrDuplicatedImports duplicatedImports'
    | Just moduleInstance <- maybeModuleInstance =
        Right
            Package
                { packageModules =
                    Map.insert
                        name
                        (Right moduleInstance)
                        packageModules
                }
    | otherwise =
        error "addModule: mkModuleInstance should have succeeded"
  where
    namesNotInScope = collectNotInScope mo

    invalidImports = fold $ Map.mapWithKey invalidImport moduleImports
    invalidImport m =
        Set.map (m,)
            . Set.filter (not . isDefinedIn pkg m)
            . getImportNames

    maybeModuleInstance =
        mkModuleInstance lookupProvenance lookupIdentity mo

    duplicatedImports' = duplicatedImports mo

    -- Currently, every 'TypName' is import directly from the module
    -- where it is defined — we do not allow re-exports or renamings.
    -- Hence, the 'Provenance' is always the identity of the module
    -- that the 'TypName' is imported from.
    lookupProvenance (modulename, _) =
        getIdentity <$> Map.lookup modulename packageModules

    lookupIdentity modulename =
        getIdentity <$> Map.lookup modulename packageModules

-- | Get the identity of an element of 'packageModule'.
getIdentity :: Either Signature ModuleInstance -> ModuleIdentity
getIdentity e = case e of
    Left Signature{signatureName} -> Const signatureName
    Right ModuleInstance{identity} -> identity

-- | Test whether a 'TypName' is defined in a module of a 'Package'.
isDefinedIn :: Package -> ModuleName -> TypName -> Bool
isDefinedIn Package{packageModules} modName typName =
    case Map.lookup modName packageModules of
        Nothing -> False
        Just (Right moduleInstance) ->
            let ds = getDeclarations moduleInstance
            in  typName `Map.member` ds
        Just (Left Signature{signatureDeclarations = ds}) ->
            typName `Set.member` ds

{-----------------------------------------------------------------------------
    Operations
    Add a signature
------------------------------------------------------------------------------}

-- | Add a signature to the current package if possible.
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

{-----------------------------------------------------------------------------
    Operations
    Check an assertion
------------------------------------------------------------------------------}

data ErrAssertion
    = -- | Name not in scope
      ErrNameNotInScope ModuleName
    | -- | The two modules are not equal.
      ErrUnequal ModuleIdentity ModuleIdentity
    deriving (Eq, Show, Generic)

instance ToExpr ErrAssertion

-- | Check whether an 'Assertion' on the current 'Package' holds.
checkAssertion :: Package -> Assertion -> Either ErrAssertion ()
checkAssertion Package{packageModules} (Equal a b)
    | mida == midb = Right ()
    | isNothing mida =
        Left $ ErrNameNotInScope a
    | isNothing midb =
        Left $ ErrNameNotInScope b
    | Just ida <- mida
    , Just idb <- midb =
        Left $ ErrUnequal ida idb
    | otherwise =
        error "impossible"
  where
    mida = getIdentity <$> Map.lookup a packageModules
    midb = getIdentity <$> Map.lookup b packageModules
