module Language.FineTypes.Cardano.Ledger.Specs.BabbageSpec
    ( spec
    )
where

import Language.FineTypes.Cardano.Ledger.Specs.Common
    ( moduleMultiFileSpec
    , packageSpec
    )
import Test.Hspec (Spec)

spec :: Spec
spec = do
    moduleMultiFileSpec
        "Babbage"
        [ "Crypto.fine"
        , "PParams.fine"
        , "Address.fine"
        , "Block.fine"
        , "Tx.fine"
        , "Delegation.fine"
        , "Script.fine"
        , "CostModel.fine"
        , "Value.fine"
        ]
    packageSpec "Babbage.fine"
