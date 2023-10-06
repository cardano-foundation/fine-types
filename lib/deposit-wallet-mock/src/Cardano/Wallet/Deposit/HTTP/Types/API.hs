{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Deposit.HTTP.Types.API where

import Prelude

import Cardano.Wallet.Deposit.HTTP.Types.JSON
    ( Address
    , Addresses
    , Index
    )
import Servant.API
    ( Capture
    , StdMethod (..)
    , Verb
    , (:<|>)
    , (:>)
    )
import Servant.FineTypes
    ( FineJSON
    )

{-----------------------------------------------------------------------------
    HTTP API
------------------------------------------------------------------------------}

type DepositWalletAPI =
    "addresses" :> Verb 'GET 200 '[FineJSON] Addresses
        :<|> "addresses"
            :> Capture "index" Index
            :> Verb 'GET 200 '[FineJSON] (Maybe Address)
        :<|> "addresses"
            :> Capture "index" Index
            :> Verb 'POST 200 '[FineJSON] Address
