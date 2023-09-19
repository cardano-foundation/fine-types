-- | Parser for a FineTypes 'Module'.
module Language.FineTypes.Parser
    ( parseFineTypes
    , ErrParseModule
    , parseFineTypes'

      -- * Exported lexers
    , space
    , symbol
    , moduleName
    , typName
    ) where

import Prelude

import Control.Monad (void)
import Data.Char (isSpace)
import Data.Map (Map)
import Data.Void
    ( Void
    )
import Language.FineTypes.Module
    ( Declarations
    , DocString
    , Documentation
    , Import (..)
    , Imports
    , Module (Module)
    , ModuleName
    , Place (..)
    , document
    )
import Language.FineTypes.Typ
    ( Constraint
    , Constraint1 (..)
    , ConstructorName
    , FieldName
    , OpOne (..)
    , OpTwo (..)
    , Typ (..)
    , TypConst (..)
    , TypName
    )
import Text.Megaparsec
    ( ParseErrorBundle
    , Parsec
    , anySingle
    , between
    , endBy
    , many
    , manyTill
    , notFollowedBy
    , parse
    , parseMaybe
    , satisfy
    , sepBy
    , skipMany
    , some
    , takeWhileP
    , try
    , (<?>)
    , (<|>)
    )

import qualified Control.Monad.Combinators.Expr as Parser.Expr
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Language.FineTypes.Module as Module
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

{-----------------------------------------------------------------------------
    Exported functions
------------------------------------------------------------------------------}

-- | Parse a 'String' containing mathematical types,
-- as they appears in the Cardano ledger specification.
parseFineTypes :: String -> Maybe Module
parseFineTypes = parseMaybe moduleFull

parseFineTypes' :: String -> Either ErrParseModule Module
parseFineTypes' = parse moduleFull ""

type ErrParseModule = ParseErrorBundle String Void

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

type DocumentedDeclarations = (Declarations, Documentation)

declarations :: Parser DocumentedDeclarations
declarations = mconcat <$> many declaration

-- | Parse a single declaration, with documentation comments.
declaration :: Parser DocumentedDeclarations
declaration =
    mkDocumentedDeclaration
        <$> documentationPre
        <*> typName
        <* symbol "="
        <*> rhs
        <* symbol ";"
        <*> documentationPost
  where
    rhs :: Parser DocumentedTyp
    rhs =
        try (notDocumented <$> abstract)
            <|> try productN
            <|> try sumN
            <|> (notDocumented <$> expr)

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
            [ document (Module.Typ name) d
            | d <- [doc1, doc2]
            , not (Map.null d)
            ]
            <> mconcat [document (Module.Field name f) d | (f, d) <- fs]
            <> mconcat [document (Module.Constructor name c) d | (c, d) <- cs]

constrained :: Parser Typ
constrained = braces $ do
    _ <- varName
    _ <- symbol ":"
    typ <- zeroVar
    _ <- symbol "|"
    Constrained typ <$> constraint

abstract :: Parser Typ
abstract = Abstract <$ symbol "_"

data DocumentedTyp
    = DocumentedTyp
        Typ
        [(FieldName, Map Place DocString)]
        [(ConstructorName, Map Place DocString)]

notDocumented :: Typ -> DocumentedTyp
notDocumented typ = DocumentedTyp typ [] []

mkProductN :: [DocumentedField] -> DocumentedTyp
mkProductN fields = DocumentedTyp typ doc []
  where
    typ = ProductN [(name, t) | DocumentedField name t _ <- fields]
    doc = [(name, d) | DocumentedField name _ d <- fields, not (Map.null d)]

-- | Parse a disjoint product with field names.
productN :: Parser DocumentedTyp
productN = mkProductN <$> braces (field `sepBy` symbol ",")

data DocumentedField
    = DocumentedField FieldName Typ (Map Place DocString)

field :: Parser DocumentedField
field =
    mkDocumentedField
        <$> documentationPre
        <*> fieldName
        <* symbol ":"
        <*> expr
        <*> documentationPost
  where
    mkDocumentedField a b c d =
        DocumentedField b c (a <> d)

mkSumN :: [DocumentedConstructor] -> DocumentedTyp
mkSumN cs = DocumentedTyp typ [] doc
  where
    typ = SumN [(name, t) | DocumentedConstructor name t _ <- cs]
    doc = [(name, d) | DocumentedConstructor name _ d <- cs, not (Map.null d)]

-- | Parse a disjoint sum with constructor names.
sumN :: Parser DocumentedTyp
sumN = mkSumN <$> sumBraces (constructor `sepBy` symbol ",")

data DocumentedConstructor
    = DocumentedConstructor ConstructorName Typ (Map Place DocString)

constructor :: Parser DocumentedConstructor
constructor =
    mkDocumentedConstructor
        <$> documentationPre
        <*> constructorName
        <* symbol ":"
        <*> expr
        <*> documentationPost
  where
    mkDocumentedConstructor a b c d =
        DocumentedConstructor b c (a <> d)

-- | Parse an expression.
expr :: Parser Typ
expr = Parser.Expr.makeExprParser atom tableOfOperators <?> "expression"

constraint :: Parser Constraint
constraint = some constraint1
  where
    constraint1 :: Parser Constraint1
    constraint1 = try (Braces <$> braces constraint) <|> (Token <$> token)

    token :: Parser String
    token = L.lexeme space
        $ some
        $ satisfy
        $ \c -> not (c `elem` "{}" || isSpace c)

zeroVar :: Parser Typ
zeroVar = try (Zero <$> constants) <|> (Var <$> typName)

atom :: Parser Typ
atom =
    parens expr
        <|> zeroVar
        <|> constrained
        <?> "atom"

constants :: Parser TypConst
constants =
    (Bool <$ symbol "Bool")
        <|> (Bytes <$ symbol "Bytes")
        <|> (Integer <$ symbol "ℤ")
        <|> (Natural <$ symbol "ℕ")
        <|> (Rational <$ symbol "ℚ")
        <|> (Text <$ symbol "Text")
        <|> (Unit <$ symbol "Unit")

tableOfOperators :: [[Parser.Expr.Operator Parser Typ]]
tableOfOperators =
    [
        [ postfix "*" (One Sequence)
        , postfix "∗" (One Sequence)
        , postfix "?" (One Option)
        ]
    ,
        [ prefix "ℙ" (One PowerSet)
        ]
    ,
        [ binaryR "×" (Two Product2)
        ]
    ,
        [ binaryR "⊎" (Two Sum2)
        , binaryR "+" (Two Sum2)
        ]
    ,
        [ binaryR "→∗" (Two FiniteSupport)
        , binaryR "↦0" (Two FiniteSupport)
        , binaryR "↦" (Two PartialFunction)
        , binaryR "→" (Two PartialFunction)
        ]
    ]

binaryR :: String -> (a -> a -> a) -> Parser.Expr.Operator Parser a
binaryR name f = Parser.Expr.InfixR (f <$ symbol name)

prefix
    , postfix
        :: String -> (a -> a) -> Parser.Expr.Operator Parser a
prefix name f = Parser.Expr.Prefix (f <$ symbol name)
postfix name f = Parser.Expr.Postfix (f <$ symbol name)

documentationPre :: Parser (Map Place DocString)
documentationPre =
    try (Map.singleton BeforeMultiline <$> blockDocumentationPre)
        <|> (Map.singleton Before <$> lineDocumentationPre)
        <|> mempty

documentationPost :: Parser (Map Place DocString)
documentationPost =
    (Map.singleton After <$> lineDocumentationPost) <|> mempty

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
    start = C.string "--" *> notFollowedBy (satisfy (`elem` "^|"))

lineDocumentationPre :: Parser DocString
lineDocumentationPre =
    C.string "--|" *> skipMany C.space1 *> line <* space

lineDocumentationPost :: Parser DocString
lineDocumentationPost =
    C.string "--^" *> skipMany C.space1 *> line <* space

blockComment :: Parser ()
blockComment =
    void (try start *> manyTill anySingle (C.string "-}"))
  where
    start = C.string "{-" *> notFollowedBy (C.char '|')

blockDocumentationPre :: Parser DocString
blockDocumentationPre =
    start *> manyTill anySingle (C.string "-}") <* space
  where
    start = C.string "{-|" *> skipMany C.space1

space :: Parser ()
space = L.space C.space1 lineComment blockComment

symbol :: String -> Parser String
symbol = L.symbol space

moduleName :: Parser ModuleName
moduleName =
    L.lexeme space
        $ (:)
            <$> C.upperChar
            <*> many (C.alphaNumChar <|> satisfy (`elem` "."))

typName :: Parser TypName
typName =
    L.lexeme space
        $ (:)
            <$> C.upperChar
            <*> many (C.alphaNumChar <|> satisfy (`elem` "_^-"))

constructorName :: Parser ConstructorName
constructorName = fieldName

fieldName :: Parser FieldName
fieldName =
    L.lexeme space
        $ many (C.alphaNumChar <|> satisfy (`elem` "_^-"))

type VarName = String

varName :: Parser VarName
varName =
    L.lexeme space
        $ (:)
            <$> C.lowerChar
            <*> many (C.alphaNumChar <|> satisfy (`elem` "_^-"))

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

sumBraces :: Parser a -> Parser a
sumBraces = between (symbol "Σ{") (symbol "}")
