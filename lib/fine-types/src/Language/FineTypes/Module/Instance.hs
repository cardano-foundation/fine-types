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
import Language.FineTypes.Module (Module (..))
import Language.FineTypes.Module.Identity
    ( Identity (..)
    , ModuleIdentity
    , ModuleName
    , Provenance
    )
import Language.FineTypes.Typ
    ( Typ (..)
    , TypName
    )

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Language.FineTypes.Module as Module
import qualified Language.FineTypes.Typ as Typ

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
    { content :: Module
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
getDeclarations :: ModuleInstance -> Module.Declarations
getDeclarations = Module.moduleDeclarations . content

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
mkModuleInstance lookupProvenance lookupIdentity m = do
    identity <- mkModuleIdentity lookupIdentity m
    let content =
            m{moduleDeclarations = declarationsWithProvenance identity}
    pure ModuleInstance{content, identity}
  where
    declarationsWithProvenance self =
        addProvenancesToTypNames self lookupProvenance m

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
    -> Module.Declarations
    -- ^ Declarations with provenances information.
addProvenancesToTypNames self lookupProvenance m =
    Map.map addProvenance (moduleDeclarations m)
  where
    addProvenance :: Typ -> Typ
    addProvenance = Typ.everywhere addProvenanceToVar

    addProvenanceToVar :: Typ -> Typ
    addProvenanceToVar t@(Var (_, typname)) =
        case lookupImportProvenance typname of
            Just provenance -> Var (Just provenance, typname)
            Nothing ->
                if isDeclaredHere typname
                    then Var (Just self, typname)
                    else t
    addProvenanceToVar t = t

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
    Apply
        (Const $ moduleName m)
        <$> mapM lookupIdentity (Map.keys $ moduleImports m)

{-----------------------------------------------------------------------------
    Helpers
------------------------------------------------------------------------------}
inverseMap :: (Ord a, Ord b) => Map a (Set b) -> Map b (Set a)
inverseMap = Map.fromListWith (<>) . concatMap swap . Map.toList
  where
    swap (a, b) = (,Set.singleton a) <$> Set.toList b
