{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.FineTypes.Typ.PrettyPrinter
    ( prettyTyp
    ) where

import Prelude

import Data.Functor ((<&>))
import Language.FineTypes.Typ
    ( Constraint
    , Constraint1 (..)
    , ConstructorName
    , FieldName
    , OpOne (..)
    , OpTwo (..)
    , Typ (..)
    , TypConst (..)
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

import qualified Data.Text as T

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

prettyConstrainedTyp :: Typ -> Constraint -> Doc ann
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

prettyTyp :: Typ -> Doc ann
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

structures :: Doc ann -> Doc ann -> [(String, Typ)] -> Doc ann
structures o c xs = line <> content <> line <> c
  where
    content = indent 4 $ o <+> concatWith (\q w -> q <> line <> "," <+> w) ds
    ds = xs <&> \(fn, typ) -> pretty fn <+> prettyText ":" <+> prettyTyp typ

prettyProductN :: [(FieldName, Typ)] -> Doc ann
prettyProductN = structures "{" "}"

prettySumN :: [(ConstructorName, Typ)] -> Doc ann
prettySumN = structures "Σ{ " "}"

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
