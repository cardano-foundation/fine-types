{-# LANGUAGE LambdaCase #-}

module Language.FineTypes.Export.OpenAPI.ValueSpec where

import Prelude

import Control.Exception (assert)
import Language.FineTypes.Export.OpenAPI.Value.FromJSON (valueFromJson)
import Language.FineTypes.Export.OpenAPI.Value.ToJSON (jsonFromValue)
import Language.FineTypes.Typ
    ( OpTwo (FiniteSupport, PartialFunction)
    , Typ (..)
    )
import Language.FineTypes.Typ.Gen
    ( Mode (..)
    , WithConstraints (..)
    , genTypFiltered
    )
import Language.FineTypes.Value (Value, hasTyp)
import Language.FineTypes.Value.Gen (genTypValue)
import Test.Hspec (Spec, describe, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, classify, forAll, suchThat)

roundTrip :: Typ -> Value -> Either String Value
roundTrip t = valueFromJson t . jsonFromValue t

roundTripProp :: String -> (Typ -> Bool) -> Spec
roundTripProp s f = prop ("should roundtrip " <> s)
    $ forAll (genValue f)
    $ \(typ, evalue) ->
        classify (depth typ > 3) "3-deep" $ do
            case evalue of
                Left _ -> error "should not happen"
                Right value ->
                    assert (hasTyp value typ)
                        $ roundTrip typ value `shouldBe` Right value

spec :: Spec
spec = describe "JSON client" $ do
    roundTripProp "Zeros" isZero
    roundTripProp "Ones" isOne
    roundTripProp "Two" isTwo
    roundTripProp "ProductN" isProductN
    roundTripProp "SumN" isSumN
    roundTripProp "all together" concert

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
genValue f = do
    typ <- genTypFiltered unsupported WithoutConstraints Concrete 6 `suchThat` f
    r <- genTypValue typ
    pure (typ, r)

unsupported :: Typ -> Bool
unsupported = \case
    Two FiniteSupport _ _ -> True
    Two PartialFunction _ _ -> True
    _ -> False

depth :: Typ -> Int
depth = \case
    Zero{} -> 0
    One _ a -> 1 + depth a
    Two _ a b -> 1 + max (depth a) (depth b)
    ProductN fields -> 1 + maximum (fmap (depth . snd) fields)
    SumN constructors -> 1 + maximum (fmap (depth . snd) constructors)
    Constrained a _ -> depth a
    Abstract -> 0
    Var _ -> 0
