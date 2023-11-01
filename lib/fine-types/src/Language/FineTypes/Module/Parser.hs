{-# LANGUAGE DeriveGeneric #-}

-- | Parser for a FineTypes 'Module'.
module Language.FineTypes.Module.Parser
    ( parseModule
    , ErrParseModule (..)
    , moduleName
    , parseModule'
    ) where

import Prelude

import Data.Bifunctor (first)
import Data.Map (Map)
import Data.TreeDiff (ToExpr (..))
import Data.Void
    ( Void
    )
import GHC.Generics (Generic)
import Language.FineTypes.Documentation
    ( DocString
    , Documentation
    , Place (..)
    , document
    )
import Language.FineTypes.Documentation.Parser
    ( documentationPost
    , documentationPre
    )
import Language.FineTypes.Module
    ( Declarations
    , Import (..)
    , Imports
    , Module (Module)
    , ModuleName
    )
import Language.FineTypes.Parser.Common (typName)
import Language.FineTypes.Parser.Lexer
    ( parens
    , space
    , symbol
    )
import Language.FineTypes.Typ
    ( TypName
    )
import Language.FineTypes.Typ.Parser (DocumentedTyp (..), parseTyp)
import Text.Megaparsec
    ( ParseErrorBundle
    , Parsec
    , endBy
    , many
    , parse
    , parseMaybe
    , satisfy
    , sepBy
    , (<|>)
    )

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Language.FineTypes.Documentation as Documentation
import qualified Language.FineTypes.Module as Module
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

{-----------------------------------------------------------------------------
    Exported functions
------------------------------------------------------------------------------}

-- | Parse a 'String' containing mathematical types,
-- as they appears in the Cardano ledger specification.
parseModule :: String -> Maybe Module
parseModule = parseMaybe moduleFull

parseModule' :: String -> Either ErrParseModule Module
parseModule' = first ErrParseModule . parse moduleFull ""

newtype ErrParseModule = ErrParseModule
    {parseErrorBundle :: ParseErrorBundle String Void}
    deriving (Eq, Show, Generic)

instance ToExpr ErrParseModule where
    toExpr _ = toExpr "ErrParseModule"

{-----------------------------------------------------------------------------
    Parser
------------------------------------------------------------------------------}
{- Note
For the design patterns used when implementing this parser, see
  J. Willis, N. Wu, Design Patterns for Parser Combinators (Functional Pearl)
  https://dl.acm.org/doi/10.1145/3471874.3472984
-}

type Parser = Parsec Void String

moduleFull :: Parser Module
moduleFull = space *> module'

module' :: Parser Module
module' = do
    _ <- symbol "module"
    n <- moduleName
    _ <- symbol "where"
    i <- imports
    (decls, docs) <- declarations
    pure
        Module
            { Module.moduleName = n
            , Module.moduleImports = i
            , Module.moduleDeclarations = decls
            , Module.moduleDocumentation = docs
            }

imports :: Parser Imports
imports = mconcat <$> (import' `endBy` symbol ";")

import' :: Parser Imports
import' =
    Map.singleton
        <$ symbol "import"
        <*> moduleName
        <*> (toImport <$> importedNames)
  where
    importedNames = parens (typName `sepBy` symbol ",")
    toImport = ImportNames . Set.fromList

type DocumentedDeclarations = (Declarations TypName, Documentation)

declarations :: Parser DocumentedDeclarations
declarations = mconcat <$> many declaration

-- | Parse a single declaration, with documentation comments.
declaration :: Parser DocumentedDeclarations
declaration =
    mkDocumentedDeclaration
        <$> documentationPre
        <*> typName
        <* symbol "="
        <*> parseTyp
        <* symbol ";"
        <*> documentationPost

mkDocumentedDeclaration
    :: Map Place DocString
    -> TypName
    -> DocumentedTyp
    -> Map Place DocString
    -> DocumentedDeclarations
mkDocumentedDeclaration doc1 name (DocumentedTyp typ fs cs) doc2 =
    (decl, docs)
  where
    decl = Map.singleton name typ
    docs =
        mconcat
            [ document (Documentation.Typ name) d
            | d <- [doc1, doc2]
            , not (Map.null d)
            ]
            <> mconcat
                [ document (Documentation.Field name f) d
                | (f, d) <- fs
                ]
            <> mconcat
                [ document (Documentation.Constructor name c) d
                | (c, d) <- cs
                ]

moduleName :: Parser ModuleName
moduleName =
    L.lexeme space
        $ (:)
            <$> C.upperChar
            <*> many (C.alphaNumChar <|> satisfy (`elem` "."))
