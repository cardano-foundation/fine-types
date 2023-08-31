{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Language.FineTypes.ParserSpec
    ( spec
    ) where

import Prelude

import Data.Either (isRight)
import Data.Foldable (toList)
import Data.TreeDiff.QuickCheck (ediffEq)
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
import Language.FineTypes.Typ
    ( Typ (..)
    , everything
    )
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
    describe "parseFineTypes" $ do
        specParserOnFile "test/data/ParseTestBabbage.fine"
        specParserOnFile "test/data/HaskellUTxO.fine"

    describe "pretty print + parse idempotency" $ do
        specPrettyOnFile "test/data/ParseTestBabbage.fine"
        specPrettyOnFile "test/data/HaskellUTxO.fine"
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
                        $ ediffEq m'
                        $ Just m

specParserOnFile :: FilePath -> Spec
specParserOnFile fp = do
    describe ("on file " <> fp) $ do
        it "parses the file" $ do
            file <- readFile fp
            parseFineTypes' file `shouldSatisfy` isRight
        it "detects undefined names" $ do
            file <- readFile fp
            Just m <- pure $ parseFineTypes file
            collectNotInScope m `shouldBe` Set.empty

specPrettyOnFile :: FilePath -> Spec
specPrettyOnFile fp = do
    it ("holds for file" <> fp) $ do
        file <- readFile "test/data/ParseTestBabbage.fine"
        Just m <- pure $ parseFineTypes file
        let output = prettyPrintModule m
            m' = parseFineTypes output
        m' `shouldBe` Just m

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
    , constrained :: Int
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
        && constrained > 0

instance Semigroup Counting where
    Counting a b c d e f g h <> Counting a' b' c' d' e' f' g' h' =
        Counting
            (a + a')
            (b + b')
            (c + c')
            (d + d')
            (e + e')
            (f + f')
            (g + g')
            (h + h')

instance Monoid Counting where
    mempty = Counting 0 0 0 0 0 0 0 0

counting :: Typ -> Counting
counting Abstract = Counting 1 0 0 0 0 0 0 0
counting Var{} = Counting 0 1 0 0 0 0 0 0
counting Zero{} = Counting 0 0 1 0 0 0 0 0
counting One{} = Counting 0 0 0 1 0 0 0 0
counting Two{} = Counting 0 0 0 0 1 0 0 0
counting ProductN{} = Counting 0 0 0 0 0 1 0 0
counting SumN{} = Counting 0 0 0 0 0 0 1 0
counting Constrained{} = Counting 0 0 0 0 0 0 0 1
