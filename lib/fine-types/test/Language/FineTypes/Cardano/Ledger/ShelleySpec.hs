module Language.FineTypes.Cardano.Ledger.ShelleySpec where

import Prelude

import Data.Either (isRight)
import Language.FineTypes.Module (collectNotInScope)
import Language.FineTypes.Parser (parseFineTypes, parseFineTypes')
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import qualified Data.Set as Set

ledgerShelleySpec :: FilePath -> Spec
ledgerShelleySpec fp = do
    describe ("parseFineTypes applied to " <> fp) $ do
        it "parses the file" $ do
            file <- readFile fp
            parseFineTypes' file `shouldSatisfy` isRight
        it "detects constants" $ do
            file <- readFile fp
            Just m <- pure $ parseFineTypes file
            collectNotInScope m `shouldBe` Set.empty

spec :: Spec
spec = do
    ledgerShelleySpec "test/data/Cardano/Ledger/Shelley/Crypto.fine"
    ledgerShelleySpec "test/data/Cardano/Ledger/Shelley/PParams.fine"
    ledgerShelleySpec "test/data/Cardano/Ledger/Shelley/Address.fine"
    ledgerShelleySpec "test/data/Cardano/Ledger/Shelley/Block.fine"
    ledgerShelleySpec "test/data/Cardano/Ledger/Shelley/Tx.fine"
    ledgerShelleySpec "test/data/Cardano/Ledger/Shelley/Delegation.fine"
    ledgerShelleySpec "test/data/Cardano/Ledger/Shelley/Script.fine"
