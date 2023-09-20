module Language.FineTypes.Cardano.Ledger.JSON.BabbageSpec
    ( spec
    )
where

import Prelude

import Language.FineTypes.Cardano.Ledger.Common
    ( moduleMultiFileSpec
    , packageSpec
    )
import System.FilePath (joinPath)
import Test.Hspec (Spec)

basePath :: FilePath
basePath = joinPath ["test", "data", "Cardano", "Ledger", "JSON"]

spec :: Spec
spec = do
    moduleMultiFileSpec
        basePath
        "Babbage"
        [ "Crypto.fine"
        , "PParams.fine"
        , "Address.fine"
        , "Block.fine"
        , "Tx.fine"
        , "Delegation.fine"
        , "Script.fine"
        , "CostModel.fine"
        ]
    packageSpec basePath "Babbage.fine"
