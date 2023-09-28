module Language.FineTypes.Parser.Common where

import Prelude

import Language.FineTypes.Parser.Lexer (space)
import Language.FineTypes.Parser.Types (Parser)
import Language.FineTypes.Typ (TypName)
import Text.Megaparsec (many, satisfy, (<|>))

import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

typName :: Parser TypName
typName =
    L.lexeme space
        $ (:)
            <$> C.upperChar
            <*> many (C.alphaNumChar <|> satisfy (`elem` "_^-"))
