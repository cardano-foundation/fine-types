{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.FineTypes.Export.OpenAPI.ValueSpec where

import Prelude

import Control.Exception (assert)
import Data.Text (Text)
import Language.FineTypes.Export.OpenAPI.Value.FromJSON (valueFromJson)
import Language.FineTypes.Export.OpenAPI.Value.ToJSON (jsonFromValue)
import Language.FineTypes.Typ
    ( OpTwo (FiniteSupport, PartialFunction)
    , Typ (..)
    )
import Language.FineTypes.Typ.Gen
    ( Mode (..)
    , WithConstraints (..)
    )
import Language.FineTypes.Value (Value, hasTyp)
import Language.FineTypes.Value.Gen (genTypAndValue, genTypValue)
import Test.Hspec (Spec, describe, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
    ( Gen
    , Property
    , classify
    , counterexample
    , forAll
    )
import Text.Pretty.Simple (pShow)

import qualified Data.Text.Lazy as TL
import qualified Language.FineTypes.Typ as Typ

defaultOption :: Typ
defaultOption = ProductN [("a", Typ.One Typ.Option (Typ.Zero Typ.Text))]

roundTrip :: Typ -> Value -> Either String Value
roundTrip t = valueFromJson t . jsonFromValue t

singleRoundtrip :: Typ -> Either a Value -> Property
singleRoundtrip typ evalue =
    classify (Typ.depth typ > 3) "3-deep" $ do
        case evalue of
            Left _ -> error "should not happen"
            Right value ->
                assert (hasTyp value typ)
                    $ roundTrip typ value `shouldBe` Right value

roundTripProp :: String -> (Typ -> Bool) -> Spec
roundTripProp s f =
    prop ("should roundtrip " <> s)
        $ forAll (genValue f)
        $ uncurry singleRoundtrip

jsonInfo :: Typ -> Either a Value -> String
jsonInfo typ (Right value) =
    TL.unpack
        $ pShow
            ( ("type: " :: Text, typ)
            , ("value: " :: Text, value)
            , ("json: " :: Text, jsonFromValue typ value)
            )
jsonInfo _ (Left _) = "error"

roundTripSpecial :: String -> Typ -> Spec
roundTripSpecial s typ = prop ("should roundtrip " <> s)
    $ forAll (genTypValue typ)
    $ \evalue ->
        counterexample (jsonInfo typ evalue)
            $ singleRoundtrip typ evalue

spec :: Spec
spec = describe "JSON client" $ do
    roundTripProp "Zeros" isZero
    roundTripProp "Ones" isOne
    roundTripProp "Two" isTwo
    roundTripProp "ProductN" isProductN
    roundTripProp "SumN" isSumN
    roundTripProp "all together" concert
    roundTripSpecial "defaultOption" defaultOption

isZero :: Typ -> Bool
isZero Zero{} = True
isZero _ = False

isOne :: Typ -> Bool
isOne One{} = True
isOne _ = False

isTwo :: Typ -> Bool
isTwo Two{} = True
isTwo _ = False

isProductN :: Typ -> Bool
isProductN ProductN{} = True
isProductN _ = False

isSumN :: Typ -> Bool
isSumN SumN{} = True
isSumN _ = False

concert :: Typ -> Bool
concert = or <$> sequence [isZero, isOne, isTwo, isProductN, isSumN]

genValue :: (Typ -> Bool) -> Gen (Typ, Either Typ Value)
genValue f =
    genTypAndValue
        f
        unsupported
        WithoutConstraints
        Concrete
        6

unsupported :: Typ -> Bool
unsupported = \case
    Two FiniteSupport _ _ -> True
    Two PartialFunction _ _ -> True
    _ -> False
