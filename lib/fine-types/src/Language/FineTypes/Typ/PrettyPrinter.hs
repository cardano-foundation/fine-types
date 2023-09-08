{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.FineTypes.Typ.PrettyPrinter
    ( prettyTyp
    , QueryDocumentation
    , predoc
    , postdoc
    , (</>)
    , addDocs
    ) where

import Prelude

import Data.Functor ((<&>))
import Language.FineTypes.Module
    ( Identifier (..)
    , IdentifierDocumentation
    , Place (..)
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
import Prettyprinter
    ( Doc
    , Pretty (pretty)
    , concatWith
    , encloseSep
    , indent
    , line
    , (<+>)
    )

import Control.Applicative ((<|>))
import qualified Data.Map as Map
import qualified Data.Text as T

-- | Query documentation for an identifier.
type QueryDocumentation = Identifier -> IdentifierDocumentation

prettyText :: T.Text -> Doc ann
prettyText = pretty

requireParens :: Typ -> Bool
requireParens = \case
    Zero _ -> False
    One _ _ -> True
    Two{} -> True
    ProductN _ -> False
    SumN _ -> False
    Var _ -> False
    Abstract -> False
    Constrained _ _ -> False

parens :: Doc ann -> Doc ann
parens doc = encloseSep "(" ")" " " [doc]

withParens :: (Typ -> Doc ann) -> Typ -> Doc ann
withParens f x = if requireParens x then parens (f x) else f x

prettyConstrainedTyp
    :: (?docs :: QueryDocumentation, ?typname :: TypName)
    => Typ
    -> Constraint
    -> Doc ann
prettyConstrainedTyp typ [] = prettyTyp typ
prettyConstrainedTyp typ constraint =
    "{ x :"
        <+> prettyTyp typ
        <+> prettyText "|"
        <+> prettyConstraint constraint
        <+> "}"

prettyConstraint :: Constraint -> Doc ann
prettyConstraint = foldr ((<+>) . prettyConstraint1) ""

prettyConstraint1 :: Constraint1 -> Doc ann
prettyConstraint1 = \case
    Braces x -> prettyText "{" <+> prettyConstraint x <+> prettyText "}"
    Token x -> prettyText $ T.pack x

getDocs
    :: (?docs :: QueryDocumentation)
    => Place
    -> Identifier
    -> (String -> Doc ann)
    -> Maybe (Doc ann)
getDocs k name f = fmap f $ Map.lookup k $ ?docs name

-- | Query documentation that goes after an identifier.
postdoc
    :: (?docs :: QueryDocumentation)
    => Identifier
    -> (Doc ann -> Doc ann -> Doc ann)
    -> Doc ann
    -> Doc ann
postdoc name f = maybe id f $ getDocs After name $ \postdocs ->
    concatWith
        (<>)
        [ prettyText "--^ "
        , pretty postdocs
        ]

-- | Query documentation that goes before for an identifier.
predoc
    :: (?docs :: QueryDocumentation)
    => Identifier
    -> (Doc ann -> Doc ann -> Doc ann)
    -> Doc ann
    -> Doc ann
predoc name f = maybe id f $ multi <|> single
  where
    single = getDocs Before name $ \predocs ->
        concatWith
            (<>)
            [ prettyText "--| "
            , pretty predocs
            ]
    multi = getDocs BeforeMultiline name $ \predocs ->
        concatWith
            (<>)
            [ prettyText "{-| "
            , pretty predocs
            , prettyText "-}"
            ]

addDocs :: (?docs :: QueryDocumentation) => Identifier -> Doc ann -> Doc ann
addDocs i = postdoc i (flip (<+>)) . predoc i (</>)

-- | Concatenate two 'Doc's with a line break in between.
(</>) :: Doc ann -> Doc ann -> Doc ann
d </> d' = d <> line <> d'

-- | Pretty print a 'Typ'.
prettyTyp
    :: (?docs :: QueryDocumentation, ?typname :: TypName) => Typ -> Doc ann
prettyTyp = \case
    Zero tc -> prettyConst tc
    One op typ -> prettyOpOne op $ withParens prettyTyp typ
    Two op typ1 typ2 ->
        withParens prettyTyp typ1
            <+> prettyOpTwo op
            <+> withParens prettyTyp typ2
    ProductN fields -> prettyProductN fields
    SumN constructors -> prettySumN constructors
    Var name -> pretty name
    Abstract -> prettyText "_"
    Constrained typ c -> prettyConstrainedTyp typ c

structures
    :: (?docs :: QueryDocumentation, ?typname :: TypName)
    => (TypName -> String -> Identifier)
    -> Doc ann
    -> Doc ann
    -> [(String, Typ)]
    -> Doc ann
structures field o c xs = line <> content <> line <> c
  where
    content = indent 4 $ o <+> concatWith (\q w -> q </> "," <+> w) ds
    ds =
        xs <&> \(fn, typ) ->
            addDocs (field ?typname fn)
                $ pretty fn
                    <+> prettyText ":"
                    <+> prettyTyp typ

prettyProductN
    :: (?docs :: QueryDocumentation, ?typname :: TypName)
    => [(FieldName, Typ)]
    -> Doc ann
prettyProductN = structures Field "{" "}"

prettySumN
    :: (?docs :: QueryDocumentation, ?typname :: TypName)
    => [(ConstructorName, Typ)]
    -> Doc ann
prettySumN = structures Constructor "Σ{ " "}"

prettyOpTwo :: OpTwo -> Doc ann
prettyOpTwo = \case
    Sum2 -> prettyText "+"
    Product2 -> prettyText "×"
    PartialFunction -> prettyText "↦"
    FiniteSupport -> prettyText "↦0"

prettyOpOne :: OpOne -> Doc ann -> Doc ann
prettyOpOne op x = case op of
    Option -> x <> prettyText "?"
    Sequence -> x <> prettyText "*"
    PowerSet -> prettyText "ℙ" <+> x

prettyConst :: TypConst -> Doc ann
prettyConst = \case
    Bool -> prettyText "Bool"
    Bytes -> prettyText "Bytes"
    Integer -> prettyText "ℤ"
    Natural -> prettyText "ℕ"
    Text -> prettyText "Text"
    Unit -> prettyText "Unit"
    Rational -> prettyText "ℚ"
