module Language.FineTypes.Cardano.Ledger.Common
    ( ledgerSpec
    , ledgerMultiFileSpec
    )
where

import Prelude

import Control.Arrow (left)
import Data.Either (isRight)
import Language.FineTypes.Module (collectNotInScope)
import Language.FineTypes.Parser (parseFineTypes, parseFineTypes')
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (errorBundlePretty)

import Data.Foldable (traverse_)
import qualified Data.Set as Set
import System.FilePath (joinPath, (</>))

ledgerSpec :: FilePath -> Spec
ledgerSpec fp = do
    describe ("parseFineTypes applied to " <> fp) $ do
        it "parses the file" $ do
            file <- readFile fp
            left errorBundlePretty (parseFineTypes' file)
                `shouldSatisfy` isRight
        it "detects constants" $ do
            file <- readFile fp
            Just m <- pure $ parseFineTypes file
            collectNotInScope m `shouldBe` Set.empty

basePath :: FilePath
basePath = joinPath ["test", "data", "Cardano", "Ledger"]

ledgerMultiFileSpec :: FilePath -> [FilePath] -> Spec
ledgerMultiFileSpec era =
    traverse_ (ledgerSpec . (\x -> basePath </> era </> x))
