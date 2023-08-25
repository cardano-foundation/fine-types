{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Language.FineTypes.Module.Gen where

import Prelude

import Language.FineTypes.Module (Declarations, Module (..))
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

genModule :: Gen Module
genModule = do
    moduleName <- genModuleName
    moduleDeclarations <- genDeclarations
    pure Module{..}

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

shrinkDeclarations :: Declarations -> [Declarations]
shrinkDeclarations xs = do
    (k, v) <- Map.toList xs
    Map.singleton k <$> shrinkTyp v
