{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | A 'Module' is a collection of 'Typ' definitions.
module Language.FineTypes.Module
    ( ModuleName
    , Module (..)
    , Import (..)
    , Imports
    , Declarations

      -- * Name resolution
    , resolveImports
    , collectNotInScope
    , resolveVars
    , redundantImports
    , duplicatedImports
    ) where

import Prelude

import Control.Monad
    ( forM
    , guard
    )
import Data.Map
    ( Map
    )
import Data.Set
    ( Set
    )
import Data.TreeDiff (ToExpr)
import GHC.Generics (Generic)
import Language.FineTypes.Documentation (Documentation)
import Language.FineTypes.Typ
    ( Typ (..)
    , TypName
    , everything
    , everywhere
    )

import qualified Data.Map as Map
import qualified Data.Set as Set

{-----------------------------------------------------------------------------
    Module type
------------------------------------------------------------------------------}
type ModuleName = String

-- | A 'Module' is a collection of 'Typ' definitions and documentation.
data Module = Module
    { moduleName :: ModuleName
    , moduleImports :: Imports
    , moduleDeclarations :: Declarations
    , moduleDocumentation :: Documentation
    }
    deriving (Eq, Show, Generic)

instance ToExpr Module

type Declarations = Map TypName Typ

type Imports = Map ModuleName Import

newtype Import = ImportNames {getImportNames :: Set TypName}
    deriving (Eq, Show, Generic)

instance ToExpr Import

{-----------------------------------------------------------------------------
    Name resolution
------------------------------------------------------------------------------}

-- | Resolve all variables in a 'Typ' that can be resolved
-- using the given declarations.
--
-- This function will loop in the case of recursive definitions.
resolveVars :: Declarations -> Typ -> Typ
resolveVars declarations = everywhere resolve
  where
    resolve (Var name) = case Map.lookup name declarations of
        Nothing -> Var name
        Just typ -> everywhere resolve typ
    resolve a = a

-- | For a given 'Module', resolve @import@ by adding the declarations
-- for the imported names.
--
-- Fails if an imported name cannot be found.
resolveImports
    :: Map ModuleName Module
    -> Module
    -> Maybe Declarations
resolveImports modulesInScope m0 =
    (moduleDeclarations m0 <>) . Map.fromList <$> resolve
  where
    resolve = concat <$> forM (Map.toList $ moduleImports m0) resolveImport
    resolveImport (moduleName, ImportNames names) = do
        m1 <- Map.lookup moduleName modulesInScope
        forM (Set.toList names) $ \name -> do
            typ <- Map.lookup name (moduleDeclarations m1)
            pure (name, typ)

-- | Collect all 'Typ' names that have not been defined or imported
-- in the 'Module'.
collectNotInScope :: Module -> Set TypName
collectNotInScope Module{moduleDeclarations, moduleImports} =
    needed Set.\\ defined
  where
    defined :: Set TypName
    defined = declaredNames <> importedNames

    declaredNames, importedNames :: Set TypName
    declaredNames = Map.keysSet moduleDeclarations
    importedNames = mconcat (getImportNames <$> Map.elems moduleImports)

    needed :: Set TypName
    needed = mconcat . map collectVars $ Map.elems moduleDeclarations

-- | Collect all 'Var' in a 'Typ'.
collectVars :: Typ -> Set TypName
collectVars = everything (<>) vars
  where
    vars (Var name) = Set.singleton name
    vars _ = Set.empty

-- | Find all imports that are not needed.
redundantImports :: Module -> Set (ModuleName, TypName)
redundantImports Module{moduleImports, moduleDeclarations} =
    let needed = mconcat . map collectVars $ Map.elems moduleDeclarations
    in  Set.fromList $ do
            (moduleName, ImportNames names) <- Map.toList moduleImports
            name <- Set.toList names
            guard $ name `Set.notMember` needed
            pure (moduleName, name)

-- | Group modules by the names they import,
-- and filter those names which are imported from multiple modules.
duplicatedImports :: Module -> Map TypName (Set ModuleName)
duplicatedImports Module{moduleImports} =
    Map.filter ((> 1) . Set.size)
        $ Map.fromListWith (<>)
        $ do
            (moduleName, ImportNames names) <- Map.toList moduleImports
            name <- Set.toList names
            pure (name, Set.singleton moduleName)