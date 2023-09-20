module Language.FineTypes.Cardano.Ledger.Specs.Common where

import System.FilePath (joinPath)
import Test.Hspec (Spec)
import Prelude

import qualified Language.FineTypes.Cardano.Ledger.Common as Common

basePath :: FilePath
basePath = joinPath ["test", "data", "Cardano", "Ledger", "Specs"]

moduleMultiFileSpec :: FilePath -> [FilePath] -> Spec
moduleMultiFileSpec = Common.moduleMultiFileSpec basePath

packageSpec :: FilePath -> Spec
packageSpec = Common.packageSpec basePath
