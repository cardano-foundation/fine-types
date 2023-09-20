module Language.FineTypes.Cardano.Ledger.Common
    ( moduleSpec
    , moduleMultiFileSpec
    , packageSpec
    )
where

import Prelude

import Control.Arrow (left)
import Data.Either (isRight)
import Data.Foldable (traverse_)
import Language.FineTypes.Module (collectNotInScope)
import Language.FineTypes.Package
    ( compilePackageDescription
    , parsePackageDescription
    )
import Language.FineTypes.Parser (parseFineTypes, parseFineTypes')
import System.FilePath ((</>))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (errorBundlePretty)

import qualified Data.Set as Set

moduleSpec :: FilePath -> Spec
moduleSpec fp = do
    describe ("on module " <> fp) $ do
        it "parses" $ do
            file <- readFile fp
            left errorBundlePretty (parseFineTypes' file)
                `shouldSatisfy` isRight
        it "detects constants" $ do
            file <- readFile fp
            Just m <- pure $ parseFineTypes file
            collectNotInScope m `shouldBe` Set.empty

moduleMultiFileSpec :: FilePath -> FilePath -> [FilePath] -> Spec
moduleMultiFileSpec basePath era =
    traverse_ (moduleSpec . (\x -> basePath </> era </> x))

packageSpec :: FilePath -> FilePath -> Spec
packageSpec basePath era = do
    let package = basePath </> era
    describe ("on package " <> era) $ do
        it "parses" $ do
            file <- readFile package
            parsePackageDescription file `shouldSatisfy` isRight
        it "compiles" $ do
            file <- readFile package
            Right pkg <- pure $ parsePackageDescription file
            epkg <- compilePackageDescription basePath pkg
            epkg `shouldSatisfy` isRight
