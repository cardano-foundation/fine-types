{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.FineTypes.Commands.Check.PrettyPrinter where

import Prelude

import Data.Foldable (toList)
import Language.FineTypes.Package
    ( ErrAddModule (..)
    , ErrCompilePackage (..)
    , ErrIncludePackage (..)
    , ErrParsePackage
    )
import Prettyprinter
    ( Doc
    , Pretty (pretty)
    , concatWith
    , defaultLayoutOptions
    , indent
    , layoutSmart
    , line
    , (<+>)
    )
import Prettyprinter.Render.String (renderString)
import Text.Megaparsec (errorBundlePretty)

prettyString :: String -> Doc ann
prettyString = pretty

prettyPrintCompilePackage :: ErrCompilePackage -> Doc ann
prettyPrintCompilePackage = \case
    ErrFile ioE ->
        prettyString "Error while reading a file:"
            <+> pretty (show ioE)
    ErrParseModuleError mn e ->
        prettyString "Error while parsing module"
            <+> pretty mn
                </> indent 4 (pretty (errorBundlePretty e))
    ErrIncludeParsePackageError pn e ->
        prettyString "Error while parsing included package"
            <+> pretty pn
                </> pretty (errorBundlePretty e)
    ErrIncludeCompilePackage pn e ->
        prettyString "Error while compiling included package"
            <+> pretty pn
                </> indent 4 (prettyPrintCompilePackage e)
    ErrIncludePackage pn e ->
        prettyString "Error while including package"
            <+> pretty pn
                </> indent 4 (prettyPrintIncludePackage e)
    ErrIncludePackageNameMismatch pn pn' ->
        prettyString "Package name mismatch in include statement:"
            <+> pretty pn
            <+> prettyString "vs"
            <+> pretty pn'
    ErrAddModule mn e ->
        prettyString "Error while adding module"
            <+> pretty mn
                </> indent 4 (prettyPrintAddModule e)
    ErrAddModuleNameMismatch mn mn' ->
        prettyString "Module name mismatch in module statement:"
            <+> pretty mn
            <+> prettyString "vs"
            <+> pretty mn'

prettyPrintAddModule :: ErrAddModule -> Doc ann
prettyPrintAddModule = \case
    ErrModuleAlreadyInScope ->
        prettyString "Module already in scope"
    ErrImportNotInScope is ->
        concatWith
            (</>)
            [ "Module" <+> pretty m <+> "does not export name" <+> pretty n
            | (m, n) <- toList is
            ]
    ErrNamesNotInScope ns ->
        prettyString "Names not in scope:" <+> pretty (toList ns)

prettyPrintIncludePackage :: ErrIncludePackage -> Doc ann
prettyPrintIncludePackage = \case
    ErrModulesAlreadyInScope ms ->
        prettyString "Modules already in scope:" <+> pretty (toList ms)

(</>) :: Doc ann -> Doc ann -> Doc ann
x </> y = x <> line <> y

renderCompilePackageError :: ErrCompilePackage -> String
renderCompilePackageError =
    renderString
        . layoutSmart defaultLayoutOptions
        . prettyPrintCompilePackage

renderParsePackageError :: ErrParsePackage -> String
renderParsePackageError =
    renderString
        . layoutSmart defaultLayoutOptions
        . pretty
        . errorBundlePretty
