module Language.FineTypes.Cardano.Ledger.ShelleySpec
    ( spec
    )
where

import Language.FineTypes.Cardano.Ledger.Common
    ( moduleMultiFileSpec
    , packageSpec
    )
import Test.Hspec (Spec)

spec :: Spec
spec = do
    moduleMultiFileSpec
        "Shelley"
        [ "Crypto.fine"
        , "PParams.fine"
        , "Address.fine"
        , "Block.fine"
        , "Tx.fine"
        , "Delegation.fine"
        , "Script.fine"
        ]
    packageSpec "Shelley.fine"