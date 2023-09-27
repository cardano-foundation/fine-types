{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.FineTypes.Export.Haskell.ValueSpec where

import Prelude

import Control.Monad (join)
import Data.ByteString.Char8 (ByteString)
import Data.Map (Map)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Text (Text)
import Language.FineTypes.Export.Haskell.Value.Runtime
    ( FromValue
    , ToTyp (..)
    , ToValue (..)
    , fromValue
    )
import Language.FineTypes.Typ.Gen (logScale)
import Language.FineTypes.Value (hasTyp)
import Language.FineTypes.Value.Gen (genTypValue)
import Numeric.Natural (Natural)
import Test.Hspec (SpecWith, describe, it)
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , Testable (..)
    , conjoin
    , elements
    , forAllBlind
    , listOf
    , oneof
    , suchThat
    )

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

spec :: SpecWith ()
spec = do
    describe "toValue and toTyp"
        $ it "should agree"
        $ conjoin
            [ testToValueToTyp (genValue 0)
            , testToValueToTyp (genValue 2)
            , testToValueToTyp (genValue 5)
            ]
    describe "toValue and fromValue"
        $ it "should roundrip as identity"
        $ conjoin
            [ testFromValueToValue (genValue 0)
            , testFromValueToValue (genValue 2)
            , testFromValueToValue (genValue 5)
            ]
    describe "fromValue and toValue"
        $ it "should roundrip as identity"
        $ conjoin
            [ testToValueFromValue (genValue 0)
            , testToValueFromValue (genValue 2)
            , testToValueFromValue (genValue 5)
            ]

testFromValueToValue :: Gen T -> Property
testFromValueToValue = testOnT $ \x ->
    property $ fromValue (toValue x) == x

testToValueFromValue :: Gen T -> Property
testToValueFromValue = testOnT $ \(_ :: t) -> property $ do
    ev <- genTypValue $ toTyp (Proxy :: Proxy t)
    case ev of
        Left _ -> error "testToValueFromValue: impossible"
        Right v -> pure $ toValue (fromValue v :: t) == v

testToValueToTyp :: Gen T -> Property
testToValueToTyp = testOnT $ \(x :: t) ->
    property $ toValue x `hasTyp` toTyp (Proxy :: Proxy t)

{-----------------------------------------------------------------------------
    Generators
------------------------------------------------------------------------------}

genUnit :: Gen ()
genUnit = pure ()

genNatural :: Gen Natural
genNatural = fromIntegral <$> (arbitrary `suchThat` (>= 0) :: Gen Integer)

genInteger :: Gen Integer
genInteger = arbitrary

genBool :: Gen Bool
genBool = arbitrary

genByteString :: Gen ByteString
genByteString = B.pack <$> arbitrary

genText :: Gen Text
genText = T.pack <$> arbitrary

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe g = oneof [pure Nothing, Just <$> g]

genEither :: Gen a -> Gen b -> Gen (Either a b)
genEither g1 g2 = oneof [Left <$> g1, Right <$> g2]

genTuple :: Gen a -> Gen b -> Gen (a, b)
genTuple g1 g2 = (,) <$> g1 <*> g2

genList :: Gen a -> Gen [a]
genList = logScale 2 . listOf

genSet :: Ord a => Gen a -> Gen (Set a)
genSet g = Set.fromList <$> logScale 2 (listOf g)

genMap :: Ord a => Gen a -> Gen b -> Gen (Map a b)
genMap g1 g2 = Map.fromList <$> logScale 2 (listOf ((,) <$> g1 <*> g2))

data T = forall a. (ToValue a, FromValue a, Show a) => T {getT :: Gen a}

testOnT
    :: ( forall a
          . (ToValue a, FromValue a, Show a)
         => a
         -> Property
       )
    -> Gen T
    -> Property
testOnT f g = forAllBlind g $ \(T a) -> property $ f <$> a

applyT :: (forall a. (FromValue a, ToValue a, Show a) => Gen a -> T) -> T -> T
applyT f (T g) = f g

applyT2
    :: ( forall a b
          . (FromValue a, ToValue a, Show a, FromValue b, ToValue b, Show b)
         => Gen a
         -> Gen b
         -> T
       )
    -> T
    -> T
    -> T
applyT2 f (T g1) (T g2) = f g1 g2

zeroes :: [T]
zeroes =
    [ T genUnit
    , T genNatural
    , T genInteger
    , T genBool
    , T genByteString
    , T genText
    ]

genValue :: Int -> Gen T
genValue 0 = elements zeroes
genValue n =
    join
        $ let gv = genValue (n - 1)
          in  elements
                $ [ applyT (T . genMaybe) <$> gv
                  , applyT2 (\x y -> T $ genEither x y) <$> gv <*> gv
                  , applyT2 (\x y -> T $ genTuple x y) <$> gv <*> gv
                  , applyT (T . genList) <$> gv
                  , applyT (T . genSet) <$> gv
                  , applyT2 (\x y -> T $ genMap x y) <$> gv <*> gv
                  ]
                    <> fmap pure zeroes
