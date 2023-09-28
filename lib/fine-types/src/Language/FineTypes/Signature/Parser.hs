-- | Parser for a FineTypes 'Signature'.
module Language.FineTypes.Signature.Parser
    ( parseSignature
    , parseSignature'
    , ErrParseSignature
    )
where

import Prelude

import Data.Map (Map)
import Data.Void
    ( Void
    )
import Language.FineTypes.Documentation.Parser
    ( documentationPost
    , documentationPre
    )
import Language.FineTypes.Module
    ( DocString
    , ModuleName
    , Place (..)
    , document
    )
import Language.FineTypes.Parser.Common (typName)
import Language.FineTypes.Parser.Lexer (space, symbol)
import Language.FineTypes.Parser.Types (Parser)
import Language.FineTypes.Signature
    ( DocumentedDeclarations
    , Signature (Signature)
    )
import Language.FineTypes.Typ
    ( TypName
    )
import Text.Megaparsec
    ( ParseErrorBundle
    , many
    , parse
    , parseMaybe
    , satisfy
    , (<|>)
    )

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Language.FineTypes.Module as Module
import qualified Language.FineTypes.Signature as Signature
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

{-----------------------------------------------------------------------------
    Exported functions
------------------------------------------------------------------------------}

-- | Parse a 'String' containing mathematical types,
-- as they appears in the Cardano ledger specification.
parseSignature :: String -> Maybe Signature
parseSignature = parseMaybe signatureFull

parseSignature' :: String -> Either ErrParseSignature Signature
parseSignature' = parse signatureFull ""

type ErrParseSignature = ParseErrorBundle String Void

{-----------------------------------------------------------------------------
    Parser
------------------------------------------------------------------------------}
{- Note
For the design patterns used when implementing this parser, see
  J. Willis, N. Wu, Design Patterns for Parser Combinators (Functional Pearl)
  https://dl.acm.org/doi/10.1145/3471874.3472984
-}

signatureFull :: Parser Signature
signatureFull = space *> signature'

signature' :: Parser Signature
signature' = do
    _ <- symbol "signature"
    n <- signatureName
    _ <- symbol "where"
    (decls, docs) <- declarations
    pure
        Signature
            { Signature.signatureName = n
            , Signature.signatureDeclarations = decls
            , Signature.signatureDocumentation = docs
            }

declarations :: Parser DocumentedDeclarations
declarations = mconcat <$> many declaration

-- | Parse a single declaration, with documentation comments.
declaration :: Parser DocumentedDeclarations
declaration =
    mkDocumentedDeclaration
        <$> documentationPre
        <*> typName
        <* symbol ";"
        <*> documentationPost

mkDocumentedDeclaration
    :: Map Place DocString
    -> TypName
    -> Map Place DocString
    -> DocumentedDeclarations
mkDocumentedDeclaration doc1 name doc2 =
    (decl, docs)
  where
    decl = Set.singleton name
    docs =
        mconcat
            [ document (Module.Typ name) d
            | d <- [doc1, doc2]
            , not (Map.null d)
            ]

signatureName :: Parser ModuleName
signatureName =
    L.lexeme space
        $ (:)
            <$> C.upperChar
            <*> many (C.alphaNumChar <|> satisfy (`elem` "."))
