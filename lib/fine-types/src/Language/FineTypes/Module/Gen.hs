{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Language.FineTypes.Module.Gen where

import Prelude

import Control.Monad (forM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (evalStateT, get, modify)
import Control.Monad.Trans.Writer (runWriter, tell)
import Data.Set (Set)
import Language.FineTypes.Documentation
    ( Documentation (..)
    , Identifier (..)
    )
import Language.FineTypes.Documentation.Gen
    ( GenerateUniqueWithDocs
    , genDocumentation
    )
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
    , everything
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
import QuickCheck.GenT (MonadGen (liftGen), runGenT, suchThat)
import Test.QuickCheck
    ( Gen
    , listOf
    , shrinkList
    , sublistOf
    )

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified QuickCheck.GenT as GenT

genModule :: Gen Module
genModule = do
    moduleName <- genModuleName
    moduleImports <- genImports
    (moduleDeclarations, moduleDocumentation) <- logScale 1.1 genDeclarations
    pure Module{..}

genImports :: Gen Imports
genImports = Map.fromList <$> logScale 1.5 (listOf genImport)

genImport :: Gen (ModuleName, Import)
genImport = do
    moduleName <- genModuleName
    imports <- ImportNames . Set.fromList <$> listOf genTypName
    pure (moduleName, imports)

genDeclarations :: Gen (Declarations, Documentation)
genDeclarations = do
    (ds, d) <-
        fmap (runWriter . flip evalStateT mempty)
            $ runGenT
            $ GenT.listOf genDeclaration
    pure (Map.fromList ds, d)

genDeclaration :: GenerateUniqueWithDocs (TypName, Typ)
genDeclaration = do
    typName <- genFreshTypName
    typ <- liftGen $ genTyp (const False) WithConstraints Complete 6
    ids <- liftGen $ sublistOf $ Set.toList $ identifiersForTyp typName typ
    docs <- liftGen $ mconcat <$> forM ids genDocumentation
    lift $ lift $ tell docs
    pure (typName, typ)

genFreshTypName :: GenerateUniqueWithDocs TypName
genFreshTypName = do
    used <- lift get
    typName <- liftGen genTypName `suchThat` (`Set.notMember` used)
    lift $ modify $ Set.insert typName
    pure typName

genTypName :: Gen TypName
genTypName = capitalise <$> genName

genModuleName :: Gen ModuleName
genModuleName = capitalise <$> genName

-- | Set of possible identifiers associated with a given 'Typ'.
identifiersForTyp :: TypName -> Typ -> Set Identifier
identifiersForTyp typName = \case
    ProductN fields ->
        Set.fromList $ map (Field typName . fst) fields
    SumN constructors ->
        Set.fromList $ map (Constructor typName . fst) constructors
    _ -> Set.singleton $ Typ typName

shrinkModule :: Module -> [Module]
shrinkModule m = do
    moduleDeclarations <-
        shrinkDeclarations
            $ moduleDeclarations m
    moduleDocumentation <-
        pure
            $ restrict moduleDeclarations
            $ moduleDocumentation m
    pure $ m{moduleDeclarations, moduleDocumentation}

restrict :: Declarations -> Documentation -> Documentation
restrict ds Documentation{..} =
    Documentation
        { getDocumentation =
            Map.restrictKeys getDocumentation identifiers
        }
  where
    identifiers =
        foldMap (\(name, typ) -> everything (<>) (identifiersForTyp name) typ)
            $ Map.assocs ds

shrinkDeclarations :: Declarations -> [Declarations]
shrinkDeclarations xs = fmap Map.fromList $ shrinkList f $ Map.toList xs
  where
    f (k, v) = [(k, v') | v' <- shrinkTyp v]
