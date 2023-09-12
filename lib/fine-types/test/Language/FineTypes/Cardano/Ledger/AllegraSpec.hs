module Language.FineTypes.Cardano.Ledger.AllegraSpec
    ( spec
    )
where

import Language.FineTypes.Cardano.Ledger.Common (ledgerMultiFileSpec)
import Test.Hspec (Spec)

spec :: Spec
spec =
    ledgerMultiFileSpec
        "Allegra"
        [ "Crypto.fine"
        , "PParams.fine"
        , "Address.fine"
        , "Block.fine"
        , "Tx.fine"
        , "Delegation.fine"
        , "Script.fine"
        ]
