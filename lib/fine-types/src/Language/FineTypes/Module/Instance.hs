{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Language.FineTypes.Module.Instance
    ( ModuleInstance (..)
    , getDeclarations
    , mkModuleInstance
    ) where

import Prelude

import Data.Map (Map)
import Data.Set (Set)
import Data.TreeDiff (ToExpr)
import GHC.Generics (Generic)
import Language.FineTypes.Module
    ( Declarations
    , Module (..)
    )
import Language.FineTypes.Module.Identity
    ( Identity (..)
    , ModuleIdentity
    , ModuleName
    , Provenance
    )
import Language.FineTypes.Typ
    ( TypName
    , TypV (..)
    )

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Language.FineTypes.Module as Module

{-----------------------------------------------------------------------------
    Types
------------------------------------------------------------------------------}

-- | A 'Module' is like a function that maps imports to a set of definitions.
--
-- A 'ModuleInstance' is a set of definitions that has been obtained
-- by applying such a function to specific imports.
-- Specifically, this means that all 'Var' constructors for newly defined
-- 'Typ' contain a 'ModuleIdentity' that records where the 'TypName' was
-- originally defined.
data ModuleInstance = ModuleInstance
    { source :: Module
    , declarations :: Declarations (Provenance, TypName)
    , identity :: ModuleIdentity
    }
    deriving (Eq, Show, Generic)

instance ToExpr ModuleInstance

-- TODO: Add type-level distinction for 'Var' with and without provenance.

{-----------------------------------------------------------------------------
    Observations
------------------------------------------------------------------------------}

-- | Get the 'Declarations' defined in a 'ModuleInstance'.
-- These include provenance information.
getDeclarations :: ModuleInstance -> Declarations (Provenance, TypName)
getDeclarations = declarations

{-----------------------------------------------------------------------------
    Constructors
------------------------------------------------------------------------------}

-- | Create a 'ModuleInstance' by applying a 'Module' to its imports.
mkModuleInstance
    :: ((ModuleName, TypName) -> Maybe Provenance)
    -- ^ Provenances for each imported name
    -> (ModuleName -> Maybe ModuleIdentity)
    -- ^ Identities of the imported modules.
    -> Module
    -- ^ Module to instantiate
    -> Maybe ModuleInstance
mkModuleInstance lookupProvenance lookupIdentity source = do
    identity <- mkModuleIdentity lookupIdentity source
    let declarations = declarationsWithProvenance identity
    pure ModuleInstance{source, identity, declarations}
  where
    declarationsWithProvenance self =
        addProvenancesToTypNames self lookupProvenance source

-- | Add provenance information to every 'TypName'.
-- i.e. add the 'ModuleIdentity' of the module in which the 'TypName'
-- was originally defined.
addProvenancesToTypNames
    :: Provenance
    -- ^ Provenance for any name declared in the given 'Module'.
    -> ((ModuleName, TypName) -> Maybe Provenance)
    -- ^ Provenances for each imported 'TypName'.
    -> Module
    -- ^ 'Module' without provenance information.
    -> Declarations (Provenance, TypName)
    -- ^ Declarations with provenances information.
addProvenancesToTypNames self lookupProvenance m =
    Map.map addProvenance (moduleDeclarations m)
  where
    addProvenance :: TypV TypName -> TypV (Provenance, TypName)
    addProvenance = fmap addProvenanceToVar

    addProvenanceToVar :: TypName -> (Provenance, TypName)
    addProvenanceToVar typname =
        case lookupImportProvenance typname of
            Just provenance -> (provenance, typname)
            Nothing ->
                if isDeclaredHere typname
                    then (self, typname)
                    else error "unknown identififer"

    isDeclaredHere typname =
        typname `Map.member` moduleDeclarations m

    lookupImportProvenance :: TypName -> Maybe Provenance
    lookupImportProvenance = \typname -> do
        modulename <- lookupImport typname
        lookupProvenance (modulename, typname)
      where
        lookupImport = lookupImportedTypName m

-- | Retrieve the 'ModuleName' for a given 'TypName' from the imports.
lookupImportedTypName :: Module -> TypName -> Maybe ModuleName
lookupImportedTypName Module{moduleImports = m} =
    fmap pickOne . flip Map.lookup w
  where
    pickOne = Set.elemAt 0
    -- TODO: Efficiency can be improved if we store this as
    -- part of the 'Module'.
    w :: Map TypName (Set ModuleName)
    w = inverseMap $ Map.map Module.getImportNames m

-- | Compute the 'ModuleIdentity' obtained
-- when applying a 'Module' to the 'ModuleIdentity' of the imports.
--
-- Fail iff the imports cannot be fully resolved.
mkModuleIdentity
    :: (ModuleName -> Maybe ModuleIdentity)
    -> Module
    -> Maybe ModuleIdentity
mkModuleIdentity lookupIdentity m =
    if null (moduleImports m)
        then Just c
        else Apply c <$> mapM lookupIdentity (Map.keys $ moduleImports m)
  where
    c = Const $ moduleName m

{-----------------------------------------------------------------------------
    Helpers
------------------------------------------------------------------------------}
inverseMap :: (Ord a, Ord b) => Map a (Set b) -> Map b (Set a)
inverseMap = Map.fromListWith (<>) . concatMap swap . Map.toList
  where
    swap (a, b) = (,Set.singleton a) <$> Set.toList b
