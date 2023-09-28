{-# LANGUAGE OverloadedStrings #-}

module Language.FineTypes.Parser.Lexer where

import Prelude

import Language.FineTypes.Parser.Types (Parser)
import Text.Megaparsec
    ( MonadParsec (notFollowedBy, takeWhileP, try)
    , anySingle
    , between
    , manyTill
    , satisfy
    )

import Control.Monad (void)
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

{-----------------------------------------------------------------------------
    Lexer
------------------------------------------------------------------------------}

-- | Parse the rest of a line, without the newline character.
line :: Parser String
line = takeWhileP (Just "character") (/= '\n')

lineComment :: Parser ()
lineComment =
    try start <* line
  where
    start = C.string "--" *> notFollowedBy (satisfy (`elem` ("^|" :: String)))

blockComment :: Parser ()
blockComment =
    void (try start *> manyTill anySingle (C.string "-}"))
  where
    start = C.string "{-" *> notFollowedBy (C.char '|')

space :: Parser ()
space = L.space C.space1 lineComment blockComment

symbol :: String -> Parser String
symbol = L.symbol space

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

sumBraces :: Parser a -> Parser a
sumBraces = between (symbol "Σ{") (symbol "}")
