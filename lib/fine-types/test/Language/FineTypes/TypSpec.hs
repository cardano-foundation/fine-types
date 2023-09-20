module Language.FineTypes.TypSpec where

import Prelude

import Language.FineTypes.Typ (OpTwo (..), Typ (..), TypConst, everything)
import Language.FineTypes.Typ.Gen (Mode (..), WithConstraints (..), genTyp)
import Language.FineTypes.Typ.Rewrite.Maps (rewriteMapsAsTuples)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (counterexample, forAll)
import Test.QuickCheck.Gen (Gen)
import Text.Pretty.Simple (pShow)

import qualified Data.Text.Lazy as TL

spec :: Spec
spec = do
    describe "Maps-rewritten generated values" $ do
        prop "have no maps"
            $ forAll genTyp'
            $ everything (&&) isNotAMap . rewriteMapsAsTuples
        prop "preserve typ constants"
            $ forAll genTyp'
            $ \t ->
                let t' = rewriteMapsAsTuples t
                in  counterexample (TL.unpack $ pShow (t, t'))
                        $ collectConst t == collectConst (rewriteMapsAsTuples t)

genTyp' :: Gen Typ
genTyp' = genTyp (const False) WithConstraints Concrete 6

isNotAMap :: Typ -> Bool
isNotAMap (Two PartialFunction _ _) = False
isNotAMap (Two FiniteSupport _ _) = False
isNotAMap _ = True

collectConst :: Typ -> [TypConst]
collectConst = everything (++) go
  where
    go (Zero c) = [c]
    go _ = []
