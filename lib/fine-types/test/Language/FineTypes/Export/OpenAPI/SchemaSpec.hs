module Language.FineTypes.Export.OpenAPI.SchemaSpec
    ( spec
    ) where

import Prelude

import Data.Aeson
    ( decode
    , encode
    )
import Language.FineTypes.Export.OpenAPI.Schema
    ( schemaFromModule
    , supportsJSON
    )
import Language.FineTypes.Parser
    ( parseFineTypes
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )

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
