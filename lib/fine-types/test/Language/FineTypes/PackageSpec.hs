module Language.FineTypes.PackageSpec
    ( spec
    ) where

import Prelude

import Data.Either (isRight)
import Language.FineTypes.Package
    ( compilePackageDescription
    , parsePackageDescription
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldSatisfy
    )

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
spec :: Spec
spec = do
    describe "Shelley package" $ do
        let dir = "test/data/Cardano/Ledger"
            fp = dir <> "/Shelley.fine"

        it "parses" $ do
            file <- readFile fp
            parsePackageDescription file `shouldSatisfy` isRight

        it "compiles" $ do
            file <- readFile fp
            Right pkg <- pure $ parsePackageDescription file
            epkg <- compilePackageDescription dir pkg
            epkg `shouldSatisfy` isRight
