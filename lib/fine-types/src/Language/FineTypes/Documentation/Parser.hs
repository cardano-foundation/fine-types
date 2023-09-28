module Language.FineTypes.Documentation.Parser where

import Prelude

import Control.Applicative ((<|>))
import Data.Map (Map)
import Language.FineTypes.Documentation (DocString, Place (..))
import Language.FineTypes.Parser.Lexer (line, space)
import Language.FineTypes.Parser.Types (Parser)
import Text.Megaparsec
    ( MonadParsec (try)
    , anySingle
    , manyTill
    , skipMany
    )

import qualified Data.Map as Map
import qualified Text.Megaparsec.Char as C

documentationPre :: Parser (Map Place DocString)
documentationPre =
    try (Map.singleton BeforeMultiline <$> blockDocumentationPre)
        <|> (Map.singleton Before <$> lineDocumentationPre)
        <|> mempty

documentationPost :: Parser (Map Place DocString)
documentationPost =
    (Map.singleton After <$> lineDocumentationPost) <|> mempty

lineDocumentationPre :: Parser DocString
lineDocumentationPre =
    C.string "--|" *> skipMany C.space1 *> line <* space

lineDocumentationPost :: Parser DocString
lineDocumentationPost =
    C.string "--^" *> skipMany C.space1 *> line <* space

blockDocumentationPre :: Parser DocString
blockDocumentationPre =
    start *> manyTill anySingle (C.string "-}") <* space
  where
    start = C.string "{-|" *> skipMany C.space1
