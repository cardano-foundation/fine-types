module Language.FineTypes.Parser.Types where

import Data.String (String)
import Data.Void (Void)
import Text.Megaparsec (Parsec)

type Parser = Parsec Void String
