module Language.FineTypes.Cardano.Ledger.AllegraSpec where

import Prelude

import Control.Arrow (left)
import Data.Either (isRight)
import Language.FineTypes.Module (collectNotInScope)
import Language.FineTypes.Parser (parseFineTypes, parseFineTypes')
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (errorBundlePretty)

import qualified Data.Set as Set

ledgerAllegraSpec :: FilePath -> Spec
ledgerAllegraSpec fp = do
    describe ("parseFineTypes applied to " <> fp) $ do
        it "parses the file" $ do
            file <- readFile fp
            left errorBundlePretty (parseFineTypes' file)
                `shouldSatisfy` isRight
        it "detects constants" $ do
            file <- readFile fp
            Just m <- pure $ parseFineTypes file
            collectNotInScope m `shouldBe` Set.empty

spec :: Spec
spec = do
    ledgerAllegraSpec "test/data/Cardano/Ledger/Allegra/Crypto.fine"
    ledgerAllegraSpec "test/data/Cardano/Ledger/Allegra/PParams.fine"
    ledgerAllegraSpec "test/data/Cardano/Ledger/Allegra/Address.fine"
    ledgerAllegraSpec "test/data/Cardano/Ledger/Allegra/Block.fine"
    ledgerAllegraSpec "test/data/Cardano/Ledger/Allegra/Tx.fine"
    ledgerAllegraSpec "test/data/Cardano/Ledger/Allegra/Delegation.fine"
    ledgerAllegraSpec "test/data/Cardano/Ledger/Allegra/Script.fine"
