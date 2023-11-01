-- | Parser for a FineTypes 'Module'.
module Language.FineTypes.Typ.Parser
    ( parseTyp
    , DocumentedTyp (..)
    )
where

import Prelude

import Data.Char (isSpace)
import Data.Map (Map)
import Language.FineTypes.Documentation
    ( DocString
    , Place (..)
    )
import Language.FineTypes.Documentation.Parser
    ( documentationPost
    , documentationPre
    )
import Language.FineTypes.Parser.Common (typName)
import Language.FineTypes.Parser.Lexer
    ( braces
    , parens
    , space
    , sumBraces
    , symbol
    )
import Language.FineTypes.Parser.Types (Parser)
import Language.FineTypes.Typ
    ( Constraint
    , Constraint1 (..)
    , ConstructorName
    , FieldName
    , OpOne (..)
    , OpTwo (..)
    , Typ (..)
    , TypConst (..)
    , VarName
    , var
    )
import Text.Megaparsec
    ( many
    , satisfy
    , sepBy
    , some
    , try
    , (<?>)
    , (<|>)
    )

import qualified Control.Monad.Combinators.Expr as Parser.Expr
import qualified Data.Map.Strict as Map
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

parseTyp :: Parser DocumentedTyp
parseTyp =
    try productN
        <|> try sumN
        <|> (notDocumented <$> expr)

constrained :: Parser Typ
constrained = braces $ do
    v <- varName
    _ <- symbol ":"
    typ <- zeroVar
    _ <- symbol "|"
    Constrained v typ <$> constraint

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
zeroVar = try (Zero <$> constants) <|> (var <$> typName)

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

constructorName :: Parser ConstructorName
constructorName = fieldName

fieldName :: Parser FieldName
fieldName =
    L.lexeme space
        $ many (C.alphaNumChar <|> satisfy (`elem` "_^-"))

varName :: Parser VarName
varName =
    L.lexeme space
        $ (:)
            <$> C.lowerChar
            <*> many (C.alphaNumChar <|> satisfy (`elem` "_^-"))
