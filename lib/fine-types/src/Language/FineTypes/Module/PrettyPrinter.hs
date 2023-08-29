{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.FineTypes.Module.PrettyPrinter
    ( prettyPrintModule
    , prettyModule
    , prettyTyp
    ) where

import Prelude

import Language.FineTypes.Module (Declarations, Module (..))
import Language.FineTypes.Typ
    ( ConstructorName
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
    , defaultLayoutOptions
    , encloseSep
    , indent
    , layoutPretty
    , line
    , vsep
    , (<+>)
    )
import Prettyprinter.Render.Text (renderStrict)

import Data.Functor ((<&>))
import qualified Data.Map as Map
import qualified Data.Text as T

prettyText :: T.Text -> Doc ann
prettyText = pretty

requireParens :: Typ -> Bool
requireParens = \case
    Zero _ -> False
    One _ _ -> False
    Two{} -> True
    ProductN _ -> False
    SumN _ -> False
    Var _ -> False
    Abstract -> False

parens :: Doc ann -> Doc ann
parens doc = encloseSep "(" ")" " " [doc]

withParens :: (Typ -> Doc ann) -> Typ -> Doc ann
withParens f x = if requireParens x then parens (f x) else f x

prettyPrintModule :: Module -> String
prettyPrintModule =
    T.unpack
        . renderStrict
        . layoutPretty defaultLayoutOptions
        . prettyModule

-- | Pretty print a 'Module'.
prettyModule :: Module -> Doc ann
prettyModule Module{moduleName, moduleDeclarations} =
    vsep
        [ prettyText "module" <+> pretty moduleName <+> prettyText "where"
        , prettyText ""
        , prettyDeclarations moduleDeclarations
        ]

prettyDeclarations :: Declarations -> Doc ann
prettyDeclarations = vsep . map prettyDeclaration . Map.toList

prettyDeclaration :: (TypName, Typ) -> Doc ann
prettyDeclaration (name, typ) =
    pretty name
        <+> prettyText "="
        <+> prettyTyp typ
        <> prettyText ";"

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
