{-# LANGUAGE LambdaCase #-}

module Language.FineTypes.Documentation.Gen where

import Prelude

import Control.Monad (forM)
import Data.Char
    ( isAlphaNum
    , isPunctuation
    , isSpace
    , isSymbol
    )
import Data.List (isInfixOf)
import Language.FineTypes.Documentation
    ( Documentation (..)
    , Identifier (..)
    , Place (..)
    , document
    )
import QuickCheck.GenT (GenT, suchThat)
import Test.QuickCheck
    ( Gen
    , arbitrary
    , elements
    , listOf1
    )

import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Writer (Writer)
import Data.Set (Set)
import Language.FineTypes.Typ (TypName)

import qualified Data.Map as Map

genDocumentation :: Identifier -> Gen Documentation
genDocumentation i = do
    places <- elements placesChoices
    fmap mconcat . forM places $ \place -> do
        docs <-
            fmap (take 3)
                $ listOf1 (arbitrary `suchThat` allowedChars)
                    `suchThat` noHeadingSpace
                    `suchThat` correctInside place
        pure $ document i $ Map.singleton place docs

placesChoices :: [[Place]]
placesChoices =
    [ []
    , [After]
    , [Before]
    , [BeforeMultiline]
    , [Before, After]
    , [BeforeMultiline, After]
    ]

allowedChars :: Char -> Bool
allowedChars x = isSpace x || isAlphaNum x || isPunctuation x || isSymbol x

noHeadingSpace :: String -> Bool
noHeadingSpace = \case
    [] -> True
    (x : _) -> not $ isSpace x

correctInside :: Place -> [Char] -> Bool
correctInside = \case
    BeforeMultiline -> not . ("-}" `isInfixOf`)
    _ -> not . ('\n' `elem`)

type GenerateUniqueWithDocs = GenT (StateT (Set TypName) (Writer Documentation))
