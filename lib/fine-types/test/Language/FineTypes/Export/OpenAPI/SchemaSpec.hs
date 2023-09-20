{-# LANGUAGE LambdaCase #-}

module Language.FineTypes.Export.OpenAPI.SchemaSpec
    ( spec
    ) where

import Prelude

import Data.Aeson
    ( decode
    , encode
    )
import Data.OpenApi (validateJSON)
import Language.FineTypes.Export.OpenAPI.Schema
    ( schemaFromModule
    , schemaFromTyp
    , supportsJSON
    )
import Language.FineTypes.Export.OpenAPI.Value.ToJSON (jsonFromValue)
import Language.FineTypes.Export.OpenAPI.ValueSpec (jsonInfo)
import Language.FineTypes.Module (Module, fmapTyp)
import Language.FineTypes.Parser
    ( parseFineTypes
    )
import Language.FineTypes.Typ
    ( OpTwo (FiniteSupport, PartialFunction)
    , Typ (Two)
    , depth
    )
import Language.FineTypes.Typ.Gen
    ( Mode (Concrete)
    , WithConstraints (..)
    , genTyp
    )
import Language.FineTypes.Typ.Rewrite.Constraints (removeConstraints)
import Language.FineTypes.Typ.Rewrite.Maps (rewriteMapsAsTuples)
import Language.FineTypes.Value (Value)
import Language.FineTypes.Value.Gen (genTypAndValue, genTypValue)
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (classify, counterexample, forAll)
import Test.QuickCheck.Property (Property)

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
unsafeReadModule :: FilePath -> IO Module
unsafeReadModule filePath = do
    file <- readFile filePath
    Just m <- pure $ parseFineTypes file
    pure m

validateJSONSchema :: Typ -> Either a Value -> Property
validateJSONSchema typ evalue =
    counterexample (jsonInfo typ evalue)
        $ classify (depth typ > 3) "3-deep"
        $ case evalue of
            Left _ -> error "should not happen"
            Right value ->
                let schema = schemaFromTyp typ
                    json = jsonFromValue typ value
                in  validateJSON mempty schema json `shouldBe` []
spec :: Spec
spec = do
    describe "OpenApi export on JsonUTxO.fine" $ do
        it "has a 'Typ' that supports JSON" $ do
            m <- unsafeReadModule "test/data/JsonUTxO.fine"
            supportsJSON m `shouldBe` True
        it "works, i.e. does not contain ⊥" $ do
            m <- unsafeReadModule "test/data/JsonUTxO.fine"
            let schema = schemaFromModule m
            (decode . encode) schema `shouldBe` Just schema
    describe "OpenAPI export on spec Babbage.Tx.fine"
        $ it "does contain ⊥"
        $ do
            m <-
                unsafeReadModule
                    "test/data/Cardano/Ledger/Specs/Babbage/Tx.fine"
            supportsJSON m `shouldBe` False
    describe "OpenAPI export on rewritten Babbage.Tx.fine"
        $ it "works, i.e. does not contain ⊥"
        $ do
            m <-
                unsafeReadModule
                    "test/data/Cardano/Ledger/Specs/Babbage/Tx.fine"
            let schema = schemaFromModule $ rewriteMapsAsTuples `fmapTyp` m
            (decode . encode) schema `shouldBe` Just schema
    describe "Schema derived from a module" $ do
        prop "validates json values computed from the same module"
            $ forAll
                ( genTypAndValue
                    (const True)
                    unsupported
                    WithoutConstraints
                    Concrete
                    6
                )
            $ uncurry validateJSONSchema

    describe "Schema derived with constraints from a rewritten module" $ do
        prop "validates json values computed from the same rewritten module"
            $ forAll
                (genTyp (const False) WithConstraints Concrete 6)
            $ \typ ->
                let typ' = removeConstraints $ rewriteMapsAsTuples typ
                in  forAll (genTypValue typ') $ validateJSONSchema typ'

    describe "Schema derived without constraints from a rewritten module" $ do
        prop "validates json values computed from the same rewritten module"
            $ forAll
                (genTyp (const False) WithoutConstraints Concrete 6)
            $ \typ ->
                let typ' = rewriteMapsAsTuples typ
                in  forAll (genTypValue typ') $ validateJSONSchema typ'

unsupported :: Typ -> Bool
unsupported = \case
    Two FiniteSupport _ _ -> True
    Two PartialFunction _ _ -> True
    _ -> False
