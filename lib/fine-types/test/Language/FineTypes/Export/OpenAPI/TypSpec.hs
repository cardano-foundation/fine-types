module Language.FineTypes.Export.OpenAPI.TypSpec
    ( spec
    ) where

import Prelude

import Control.DeepSeq
    ( NFData
    , rnf
    )
import Language.FineTypes.Export.OpenAPI.Typ
    ( schemaFromModule
    , supportsJSON
    )
import Language.FineTypes.Parser
    ( parseFineTypes
    )
import Test.Hspec
    ( Expectation
    , Spec
    , describe
    , it
    , shouldBe
    )

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
spec :: Spec
spec = do
    describe "OpenAPI export on JsonUTxO.fine" $ do
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
            hasNormalForm schema

hasNormalForm :: NFData a => a -> Expectation
hasNormalForm x = rnf x `shouldBe` ()
