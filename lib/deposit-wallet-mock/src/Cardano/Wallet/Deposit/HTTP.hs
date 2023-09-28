module Cardano.Wallet.Deposit.HTTP
    ( appDepositWallet
    ) where

import Prelude

import Cardano.Wallet.Deposit.HTTP.Types.API
    ( DepositWalletAPI
    )
import Cardano.Wallet.Deposit.HTTP.Types.JSON
    ( Address
    , Addresses
    , Index
    )
import Servant
    ( Proxy (..)
    , Server
    , serve
    , (:<|>) (..)
    )

import qualified Data.ByteString.Char8 as B
import qualified Network.Wai

appDepositWallet :: Network.Wai.Application
appDepositWallet = serve (Proxy :: Proxy DepositWalletAPI) depositWallet

{-----------------------------------------------------------------------------
    Server for HTTP API
------------------------------------------------------------------------------}

depositWallet :: Server DepositWalletAPI
depositWallet =
    pure getAddresses
        :<|> pure . getAddress
        :<|> pure . createAddress

addr0 :: Address
addr0 = B.pack "dummy"

getAddresses :: Addresses
getAddresses = [(0, addr0)]

getAddress :: Index -> Maybe Address
getAddress ix
    | ix == 0 = Just addr0
    | otherwise = Nothing

createAddress :: Index -> Address
createAddress _ = addr0
