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

import Control.Applicative ((<|>))
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
    , VarName
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
    Constrained{} -> False

parens :: Doc ann -> Doc ann
parens doc = encloseSep "(" ")" " " [doc]

withParens :: (Typ -> Doc ann) -> Typ -> Doc ann
withParens f x = if requireParens x then parens (f x) else f x

prettyConstrainedTyp
    :: QueryDocumentation
    -> TypName
    -> VarName
    -> Typ
    -> Constraint
    -> Doc ann
prettyConstrainedTyp docs typname _v typ [] = prettyTyp docs typname typ
prettyConstrainedTyp docs typname v typ constraint =
    "{"
        <+> pretty v
        <+> ":"
        <+> prettyTyp docs typname typ
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
    :: QueryDocumentation
    -> Place
    -> Identifier
    -> (String -> Doc ann)
    -> Maybe (Doc ann)
getDocs docs k name f = fmap f $ Map.lookup k $ docs name

-- | Query documentation that goes after an identifier.
postdoc
    :: QueryDocumentation
    -> Identifier
    -> (Doc ann -> Doc ann -> Doc ann)
    -> Doc ann
    -> Doc ann
postdoc docs name f = maybe id f $ getDocs docs After name $ \postdocs ->
    concatWith
        (<>)
        [ prettyText "--^ "
        , pretty postdocs
        ]

-- | Query documentation that goes before for an identifier.
predoc
    :: QueryDocumentation
    -> Identifier
    -> (Doc ann -> Doc ann -> Doc ann)
    -> Doc ann
    -> Doc ann
predoc docs name f = maybe id f $ multi <|> single
  where
    single = getDocs docs Before name $ \predocs ->
        concatWith
            (<>)
            [ prettyText "--| "
            , pretty predocs
            ]
    multi = getDocs docs BeforeMultiline name $ \predocs ->
        concatWith
            (<>)
            [ prettyText "{-| "
            , pretty predocs
            , prettyText "-}"
            ]

addDocs :: QueryDocumentation -> Identifier -> Doc ann -> Doc ann
addDocs docs i = postdoc docs i (flip (<+>)) . predoc docs i (</>)

-- | Concatenate two 'Doc's with a line break in between.
(</>) :: Doc ann -> Doc ann -> Doc ann
d </> d' = d <> line <> d'

-- | Pretty print a 'Typ'.
prettyTyp
    :: QueryDocumentation -> TypName -> Typ -> Doc ann
prettyTyp docs typname = \case
    Zero tc -> prettyConst tc
    One op typ -> prettyOpOne op $ withParens prettyTyp' typ
    Two op typ1 typ2 ->
        withParens prettyTyp' typ1
            <+> prettyOpTwo op
            <+> withParens prettyTyp' typ2
    ProductN fields -> prettyProductN docs typname fields
    SumN constructors -> prettySumN docs typname constructors
    Var name -> pretty name
    Abstract -> prettyText "_"
    Constrained v typ c -> prettyConstrainedTyp docs typname v typ c
  where
    prettyTyp' = prettyTyp docs typname

structures
    :: QueryDocumentation
    -> TypName
    -> (TypName -> String -> Identifier)
    -> Doc ann
    -> Doc ann
    -> [(String, Typ)]
    -> Doc ann
structures docs typname field o c xs = line <> content <> line <> c
  where
    content = indent 4 $ o <+> concatWith (\q w -> q </> "," <+> w) ds
    ds =
        xs <&> \(fn, typ) ->
            addDocs docs (field typname fn)
                $ pretty fn
                    <+> prettyText ":"
                    <+> prettyTyp docs typname typ

prettyProductN
    :: QueryDocumentation
    -> TypName
    -> [(FieldName, Typ)]
    -> Doc ann
prettyProductN docs typname = structures docs typname Field "{" "}"

prettySumN
    :: QueryDocumentation
    -> TypName
    -> [(ConstructorName, Typ)]
    -> Doc ann
prettySumN docs typname = structures docs typname Constructor "Σ{ " "}"

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
