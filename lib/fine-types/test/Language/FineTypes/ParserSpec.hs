{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Language.FineTypes.ParserSpec
    ( spec
    ) where

import Prelude

import Data.Foldable (toList)
import Data.Maybe
    ( isJust
    )
import Language.FineTypes.Module
    ( Module (..)
    , collectNotInScope
    )
import Language.FineTypes.Module.Gen (genModule, shrinkModule)
import Language.FineTypes.Module.PrettyPrinter (prettyPrintModule)
import Language.FineTypes.Parser
    ( parseFineTypes
    , parseFineTypes'
    )
import Language.FineTypes.Typ (Typ (..), everything)
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldSatisfy
    )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (classify, counterexample, forAllShrink, property)

import qualified Data.Set as Set

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
spec :: Spec
spec = do
    describe "parseFineTypes applied to ParseTestBabbage.fine" $ do
        it "parses the file" $ do
            file <- readFile "test/data/ParseTestBabbage.fine"
            parseFineTypes file `shouldSatisfy` isJust
        it "detects constants" $ do
            file <- readFile "test/data/ParseTestBabbage.fine"
            Just m <- pure $ parseFineTypes file
            collectNotInScope m `shouldBe` Set.empty
    describe "pretty print + parse idempotency" $ do
        it "holds for the ParseTestBabbage.fine file" $ do
            file <- readFile "test/data/ParseTestBabbage.fine"
            Just m <- pure $ parseFineTypes file
            let output = prettyPrintModule m
                m' = parseFineTypes output
            m' `shouldBe` Just m
        prop "holds on random generated modules"
            $ forAllShrink genModule shrinkModule
            $ \m ->
                let output = prettyPrintModule m
                    m' = parseFineTypes output
                in  classify (allPositive $ countTyps m) "fat"
                        $ property
                        $ counterexample (show (m', m))
                        $ counterexample output
                        $ counterexample (show $ parseFineTypes' output)
                        $ m' == Just m

{-----------------------------------------------------------------------------
    Counting
------------------------------------------------------------------------------}

countTyps :: Module -> Counting
countTyps Module{moduleDeclarations} =
    mconcat $ everything (<>) counting <$> toList moduleDeclarations

data Counting = Counting
    { abstract :: Int
    , var :: Int
    , zero :: Int
    , one :: Int
    , two :: Int
    , productN :: Int
    , sumN :: Int
    }
    deriving (Eq, Show)

allPositive :: Counting -> Bool
allPositive Counting{..} =
    abstract > 0
        && var > 0
        && zero > 0
        && one > 0
        && two > 0
        && productN > 0
        && sumN > 0

instance Semigroup Counting where
    Counting a b c d e f g <> Counting a' b' c' d' e' f' g' =
        Counting
            (a + a')
            (b + b')
            (c + c')
            (d + d')
            (e + e')
            (f + f')
            (g + g')

instance Monoid Counting where
    mempty = Counting 0 0 0 0 0 0 0

counting :: Typ -> Counting
counting Abstract = Counting 1 0 0 0 0 0 0
counting Var{} = Counting 0 1 0 0 0 0 0
counting Zero{} = Counting 0 0 1 0 0 0 0
counting One{} = Counting 0 0 0 1 0 0 0
counting Two{} = Counting 0 0 0 0 1 0 0
counting ProductN{} = Counting 0 0 0 0 0 1 0
counting SumN{} = Counting 0 0 0 0 0 0 1
