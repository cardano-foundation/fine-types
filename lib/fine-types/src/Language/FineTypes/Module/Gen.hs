{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Language.FineTypes.Module.Gen where

import Prelude

import Language.FineTypes.Module
    ( Declarations
    , Import (..)
    , Imports
    , Module (..)
    , ModuleName
    )
import Language.FineTypes.Typ
    ( Typ (..)
    , TypName
    )
import Language.FineTypes.Typ.Gen
    ( Mode (Complete)
    , WithConstraints (..)
    , capitalise
    , genName
    , genTyp
    , logScale
    , shrinkTyp
    )
import Test.QuickCheck
    ( Gen
    , listOf
    , shrinkList
    )

import qualified Data.Map as Map
import qualified Data.Set as Set

genModule :: Gen Module
genModule = do
    moduleName <- genModuleName
    moduleImports <- genImports
    moduleDeclarations <- logScale 1.1 genDeclarations
    let moduleDocumentation = mempty
    pure Module{..}

genImports :: Gen Imports
genImports = Map.fromList <$> logScale 1.5 (listOf genImport)

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
    typ <- genTyp WithConstraints Complete 6
    pure (typName, typ)

genTypName :: Gen TypName
genTypName = capitalise <$> genName

genModuleName :: Gen String
genModuleName = capitalise <$> genName

shrinkModule :: Module -> [Module]
shrinkModule m = do
    moduleDeclarations <- shrinkDeclarations (moduleDeclarations m)
    pure $ m{moduleDeclarations}

shrinkDeclarations :: Declarations -> [Declarations]
shrinkDeclarations xs = fmap Map.fromList $ shrinkList f $ Map.toList xs
  where
    f (k, v) = [(k, v') | v' <- shrinkTyp v]
