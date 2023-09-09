{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Language.FineTypes.Module.Gen where

import Prelude

import Control.Monad.State (MonadState (..), StateT, evalStateT, lift, modify)
import Control.Monad.Writer (Writer, runWriter)
import Data.Foldable (fold)
import Data.Functor.Identity (Identity (..))
import Data.Set (Set)
import Language.FineTypes.Module
    ( Declarations
    , Documentation (..)
    , Identifier (..)
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
    , genDocumentation
    , genName
    , genTyp
    , logScale
    , shrinkTyp
    )
import QuickCheck.GenT (GenT, MonadGen (liftGen), runGenT, suchThat)
import Test.QuickCheck
    ( Gen
    , listOf
    , shrinkList
    )

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified QuickCheck.GenT as GT

logScaleGen :: Double -> Gen a -> Gen a
logScaleGen x f = deGenT $ logScale x $ liftGen f

genModule :: Gen Module
genModule = do
    moduleName <- genModuleName
    moduleImports <- genImports
    (moduleDeclarations, moduleDocumentation) <- logScaleGen 1.1 genDeclarations
    pure Module{..}

genImports :: Gen Imports
genImports = Map.fromList <$> logScaleGen 1.5 (listOf genImport)

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
            $ GT.listOf genDeclaration
    pure (Map.fromList ds, d)

noName :: TypName
noName = error "no typ name"

type GenerateUniqueWithDocs =
    GenT
        (StateT (Set TypName) (Writer Documentation))

genDeclaration :: GenerateUniqueWithDocs (TypName, Typ)
genDeclaration = do
    used <- lift get
    typName <- liftGen genTypName `suchThat` (`Set.notMember` used)
    lift $ modify $ Set.insert typName
    typ <-
        let
            ?typname = typName
         in
            genTyp (const False) WithConstraints Complete 6
    genDocumentation (Typ typName)
    pure (typName, typ)

genTypName :: Gen TypName
genTypName = capitalise <$> deGenT genName

genModuleName :: Gen String
genModuleName = capitalise <$> deGenT genName

deGenT :: GenT Identity a -> Gen a
deGenT = fmap runIdentity . runGenT

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
            Map.restrictKeys getDocumentation $ internals <> typs
        }
  where
    typs = Set.map Typ $ Map.keysSet ds
    internals =
        foldMap (\(name, typ) -> everything (<>) (match name) typ)
            $ Map.assocs ds
    match :: TypName -> Typ -> Set Identifier
    match tn = \case
        ProductN fields ->
            fold $ do
                (fn, _typ) <- fields
                pure $ Set.singleton $ Field tn fn
        SumN fields ->
            fold $ do
                (fn, _typ) <- fields
                pure $ Set.singleton $ Constructor tn fn
        _ -> mempty

shrinkDeclarations :: Declarations -> [Declarations]
shrinkDeclarations xs = fmap Map.fromList $ shrinkList f $ Map.toList xs
  where
    f (k, v) = [(k, v') | v' <- shrinkTyp v]
