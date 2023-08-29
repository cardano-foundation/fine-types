module Language.FineTypes.ParserSpec
    ( spec
    ) where

import Prelude

import Data.Maybe
    ( isJust
    )
import Language.FineTypes.Module
    ( collectNotInScope
    )
import Language.FineTypes.Module.PrettyPrinter (prettyPrintModule)
import Language.FineTypes.Parser
    ( parseFineTypes
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldSatisfy
    )

import qualified Data.Set as Set

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
spec :: Spec
spec = do
    describe "parseFineTypes applied to ParseTestBabbage.fine" $ do
        it "parses the file" $ do
            file <- readFile "test/data/ParseTestBabbage.fine"
            parseFineTypes file `shouldSatisfy` isJust

        it "detects constants" $ do
            file <- readFile "test/data/ParseTestBabbage.fine"
            Just m <- pure $ parseFineTypes file
            collectNotInScope m `shouldBe` Set.empty
    describe "pretty print + parse idempotency" $ do
        it "holds for the ParseTestBabbage.fine file" $ do
            file <- readFile "test/data/ParseTestBabbage.fine"
            Just m <- pure $ parseFineTypes file
            let output = prettyPrintModule m
                m' = parseFineTypes output
            m' `shouldBe` Just m
