module Language.FineTypes.ValueSpec where

import Language.FineTypes.Value (hasTyp)
import Language.FineTypes.Value.Gen
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Prelude

spec :: Spec
spec = do
    describe "Generated values" $ do
        prop "typecheck"
            $ forAll (genValue 6)
            $ \(typ, evalue) ->
                case evalue of
                    Left _ -> error "should not happen"
                    Right value -> value `hasTyp` typ
