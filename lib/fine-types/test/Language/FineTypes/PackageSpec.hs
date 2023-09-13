module Language.FineTypes.PackageSpec
    ( spec
    ) where

import Prelude

import Data.Either (isRight)
import Language.FineTypes.Package
    ( compilePackageDescription
    , parsePackageDescription
    )
import System.FilePath
    ( (</>)
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
    specOnFile "test/data/Cardano/Ledger" "Shelley.fine"
    specOnFile "test/data/" "PackageTest.fine"

specOnFile :: FilePath -> FilePath -> Spec
specOnFile dir filename =
    describe ("on package " <> fp) $ do
        it "parses" $ do
            file <- readFile fp
            parsePackageDescription file `shouldSatisfy` isRight

        it "compiles" $ do
            file <- readFile fp
            Right pkg <- pure $ parsePackageDescription file
            epkg <- compilePackageDescription dir pkg
            epkg `shouldSatisfy` isRight
  where
    fp = dir </> filename
