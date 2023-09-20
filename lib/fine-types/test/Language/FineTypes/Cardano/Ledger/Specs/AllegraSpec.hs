module Language.FineTypes.Cardano.Ledger.Specs.AllegraSpec
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
        "Allegra"
        [ "Crypto.fine"
        , "PParams.fine"
        , "Address.fine"
        , "Block.fine"
        , "Tx.fine"
        , "Delegation.fine"
        , "Script.fine"
        ]
    packageSpec "Allegra.fine"
