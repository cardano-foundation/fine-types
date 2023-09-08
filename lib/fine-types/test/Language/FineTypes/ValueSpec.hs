module Language.FineTypes.ValueSpec where

import Prelude

import Language.FineTypes.Typ (Typ)
import Language.FineTypes.Typ.Gen (Mode (..), WithConstraints (..))
import Language.FineTypes.Value (Value, hasTyp)
import Language.FineTypes.Value.Gen (genTypAndValue)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, forAll)

genValue :: Gen (Typ, Either Typ Value)
genValue = genTypAndValue (const False) WithoutConstraints Concrete 6

spec :: Spec
spec = do
    describe "Generated values" $ do
        prop "typecheck"
            $ forAll genValue
            $ \(typ, evalue) ->
                case evalue of
                    Left _ -> error "should not happen"
                    Right value -> value `hasTyp` typ
