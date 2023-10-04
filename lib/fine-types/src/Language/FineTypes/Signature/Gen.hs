{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Language.FineTypes.Signature.Gen
    ( genSignature
    , shrinkSignature
    )
where

import Prelude

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (evalStateT, get, modify)
import Control.Monad.Trans.Writer (runWriter, tell)
import Language.FineTypes.Documentation
    ( Documentation (..)
    , Identifier (..)
    )
import Language.FineTypes.Documentation.Gen
    ( GenerateUniqueWithDocs
    , genDocumentation
    )
import Language.FineTypes.Module (ModuleName)
import Language.FineTypes.Signature
    ( Declarations
    , Signature (..)
    )
import Language.FineTypes.Typ
    ( TypName
    )
import Language.FineTypes.Typ.Gen
    ( capitalise
    , genName
    , logScale
    )
import QuickCheck.GenT (MonadGen (liftGen), runGenT, suchThat)
import Test.QuickCheck
    ( Gen
    , shrink
    )

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified QuickCheck.GenT as GenT

genSignature :: Gen Signature
genSignature = do
    signatureName <- genSignatureName
    (signatureDeclarations, signatureDocumentation) <-
        logScale 1.1 genDeclarations
    pure Signature{..}

genDeclarations :: Gen (Declarations, Documentation)
genDeclarations = do
    (ds, d) <-
        fmap (runWriter . flip evalStateT mempty)
            $ runGenT
            $ GenT.listOf genDeclaration
    pure (Set.fromList ds, d)

genDeclaration :: GenerateUniqueWithDocs TypName
genDeclaration = do
    typName <- genFreshTypName
    docs <- liftGen $ genDocumentation $ Typ typName
    lift $ lift $ tell docs
    pure typName

genFreshTypName :: GenerateUniqueWithDocs TypName
genFreshTypName = do
    used <- lift get
    typName <- liftGen genTypName `suchThat` (`Set.notMember` used)
    lift $ modify $ Set.insert typName
    pure typName

genTypName :: Gen TypName
genTypName = capitalise <$> genName

genSignatureName :: Gen ModuleName
genSignatureName = capitalise <$> genName

shrinkSignature :: Signature -> [Signature]
shrinkSignature m = do
    signatureDeclarations <-
        shrink
            $ signatureDeclarations m
    signatureDocumentation <-
        pure
            $ restrict signatureDeclarations
            $ signatureDocumentation m
    pure $ m{signatureDeclarations, signatureDocumentation}

restrict :: Declarations -> Documentation -> Documentation
restrict ds Documentation{..} =
    Documentation
        { getDocumentation =
            Map.restrictKeys getDocumentation $ Set.map Typ ds
        }
