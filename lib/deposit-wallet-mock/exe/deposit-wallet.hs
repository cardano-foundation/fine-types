module Main where

import Prelude

import Cardano.Wallet.Deposit.HTTP
    ( appDepositWallet
    )
import Network.Wai.Handler.Warp
    ( run
    )

main :: IO ()
main = run 3000 appDepositWallet
