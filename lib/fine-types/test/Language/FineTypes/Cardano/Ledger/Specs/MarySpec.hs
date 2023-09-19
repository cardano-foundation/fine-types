module Language.FineTypes.Cardano.Ledger.Specs.MarySpec
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
        "Mary"
        [ "Crypto.fine"
        , "PParams.fine"
        , "Address.fine"
        , "Block.fine"
        , "Tx.fine"
        , "Delegation.fine"
        , "Script.fine"
        , "Value.fine"
        ]
    packageSpec "Mary.fine"
