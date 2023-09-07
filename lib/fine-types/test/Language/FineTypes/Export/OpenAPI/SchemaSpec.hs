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
    , WithConstraints (WithoutConstraints)
    )
import Language.FineTypes.Value.Gen (genTypAndValue)
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (classify, counterexample, forAll)

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
spec :: Spec
spec = do
    describe "OpenApi export on JsonUTxO.fine" $ do
        let readModule = do
                file <- readFile "test/data/JsonUTxO.fine"
                Just m <- pure $ parseFineTypes file
                pure m
        it "has a 'Typ' that supports JSON" $ do
            m <- readModule
            supportsJSON m `shouldBe` True
        it "works, i.e. does not contain ⊥" $ do
            m <- readModule
            let schema = schemaFromModule m
            (decode . encode) schema `shouldBe` Just schema
    describe "Schema derived from a module" $ do
        prop "validates json values computed from the same module"
            $ forAll
                (genTypAndValue unsupported WithoutConstraints Concrete 6)
            $ \(typ, evalue) ->
                counterexample (jsonInfo typ evalue)
                    $ classify (depth typ > 3) "3-deep"
                    $ case evalue of
                        Left _ -> error "should not happen"
                        Right value ->
                            let schema = schemaFromTyp typ
                                json = jsonFromValue typ value
                            in  validateJSON mempty schema json `shouldBe` []

unsupported :: Typ -> Bool
unsupported = \case
    Two FiniteSupport _ _ -> True
    Two PartialFunction _ _ -> True
    _ -> False
