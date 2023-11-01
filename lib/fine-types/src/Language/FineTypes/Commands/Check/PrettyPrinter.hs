{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.FineTypes.Commands.Check.PrettyPrinter where

import Prelude

import Data.Foldable (toList)
import Language.FineTypes.Module.Parser (ErrParseModule (..))
import Language.FineTypes.Package
    ( ErrAddModule (..)
    , ErrAssertion (..)
    , ErrCompilePackage (..)
    , ErrIncludePackage (..)
    , ErrParsePackage (..)
    )
import Language.FineTypes.Package.PrettyPrinter (prettyAssertion)
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

import qualified Data.Map as Map
import qualified Language.FineTypes.Package as Pkg

prettyString :: String -> Doc ann
prettyString = pretty

prettyPrintCompilePackage :: ErrCompilePackage -> Doc ann
prettyPrintCompilePackage = \case
    ErrFile ioE ->
        prettyString "Error while reading a file:"
            <+> pretty (show ioE)
    ErrParseModuleError mn (ErrParseModule e) ->
        prettyString "Error while parsing module"
            <+> pretty mn
                </> indent 4 (pretty (errorBundlePretty e))
    ErrIncludeParsePackageError pn (ErrParsePackage e) ->
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
    ErrAssertFailed assertion e ->
        prettyString "Assertion failed:"
            <+> prettyAssertion assertion
                </> indent 4 (prettyPrintCheckAssertion e)

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
    ErrDuplicatedImports is ->
        concatWith
            (</>)
            [ "Duplicated imports of name" <+> pretty n
                <> ":"
                    </> indent
                        4
                        ( concatWith
                            (</>)
                            [ pretty m
                            | m <- toList ms
                            ]
                        )
            | (n, ms) <- Map.toList is
            ]

prettyPrintIncludePackage :: ErrIncludePackage -> Doc ann
prettyPrintIncludePackage = \case
    ErrModulesAlreadyInScope ms ->
        prettyString "Modules already in scope:" <+> pretty (toList ms)

prettyPrintCheckAssertion :: ErrAssertion -> Doc ann
prettyPrintCheckAssertion = \case
    ErrNameNotInScope n ->
        prettyString "Module name not in scope:" <+> pretty n
    ErrUnequal ida idb ->
        prettyString "Module identities are not equal:"
            </> indent
                4
                ( pretty (show ida)
                    </> prettyString "=/="
                    </> pretty (show idb)
                )

{-----------------------------------------------------------------------------
    Utilities
------------------------------------------------------------------------------}
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
        . Pkg.parseErrorBundle
