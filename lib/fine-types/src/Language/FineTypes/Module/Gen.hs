{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Language.FineTypes.Module.Gen where

import Prelude

import Language.FineTypes.Module
    ( Declarations
    , Module (..)
    , ModuleName
    , Import (..)
    , Imports
    )
import Language.FineTypes.Typ (Typ, TypName)
import Language.FineTypes.Typ.Gen
    ( Mode (Complete)
    , capitalise
    , genName
    , genTyp
    , shrinkTyp
    )
import Test.QuickCheck
    ( Gen
    , listOf
    )

import qualified Data.Map as Map
import qualified Data.Set as Set

genModule :: Gen Module
genModule = do
    moduleName <- genModuleName
    moduleImports <- genImports
    moduleDeclarations <- genDeclarations
    pure Module{..}

genImports :: Gen Imports
genImports = Map.fromList <$> listOf genImport

genImport :: Gen (ModuleName, Import)
genImport = do
    moduleName <- genModuleName
    imports <- ImportNames . Set.fromList <$> listOf genTypName
    pure (moduleName, imports)

genDeclarations :: Gen Declarations
genDeclarations = Map.fromList <$> listOf genDeclaration

genDeclaration :: Gen (TypName, Typ)
genDeclaration = do
    typName <- genTypName
    typ <- genTyp Complete 6
    pure (typName, typ)

genTypName :: Gen TypName
genTypName = capitalise <$> genName

genModuleName :: Gen String
genModuleName = capitalise <$> genName

shrinkModule :: Module -> [Module]
shrinkModule m = do
    moduleDeclarations <- shrinkDeclarations (moduleDeclarations m)
    pure $ m{moduleDeclarations}

-- TODO: Shrink import list

shrinkDeclarations :: Declarations -> [Declarations]
shrinkDeclarations xs = do
    (k, v) <- Map.toList xs
    Map.singleton k <$> shrinkTyp v
