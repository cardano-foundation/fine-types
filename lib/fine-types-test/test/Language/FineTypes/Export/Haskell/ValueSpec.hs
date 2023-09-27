{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Language.FineTypes.Export.Haskell.ValueSpec
    ( spec
    ) where

import Prelude

import Data.Proxy (Proxy (..))
import Language.FineTypes.Export.Haskell.Value.Runtime
    ( FromValue (..)
    , ToTyp (..)
    , ToValue (..)
    )
import Language.FineTypes.Test.UTxO
    ( AssetID (..)
    , AssetIDNonAda (..)
    , Value
    )
import Language.FineTypes.Value (hasTyp)
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldSatisfy
    )

import qualified Data.Map
import qualified Language.FineTypes.Value as FineTypes

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
spec :: Spec
spec = do
    describe "exported Haskell module" $ do
        it "compiles" True
        it "maps to Value"
            $ toValue example `shouldSatisfy` isFiniteMap
        it "respects typ vs value law"
            $ toValue example `shouldSatisfy` flip hasTyp (toTyp (Proxy @Value))
        it "respect from-to value law"
            $ fromValue (toValue example) `shouldSatisfy` (== example)
        it "respects to-from value law"
            $ toValue (fromValue (toValue example) :: Value)
                `shouldSatisfy` (== toValue example)

isFiniteMap :: FineTypes.Value -> Bool
isFiniteMap (FineTypes.Two (FineTypes.FiniteMap _)) = True
isFiniteMap _ = False

example :: Value
example =
    Data.Map.fromList
        [ (Ada (), 1000)
        , (Asset asset1, 42)
        , (Asset asset2, 21)
        ]

asset1 :: AssetIDNonAda
asset1 =
    AssetIDNonAda
        { policyId = "1337"
        , assetName = "Leetcoin"
        }

asset2 :: AssetIDNonAda
asset2 =
    AssetIDNonAda
        { policyId = "101"
        , assetName = "Lolcoin"
        }
