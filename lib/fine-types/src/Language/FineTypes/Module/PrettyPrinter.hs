{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.FineTypes.Module.PrettyPrinter
    ( prettyPrintModule
    , prettyModule
    ) where

import Prelude

import Language.FineTypes.Module
    ( Declarations
    , Documentation (..)
    , Identifier (..)
    , Import (..)
    , Imports
    , Module (..)
    , ModuleName
    )
import Language.FineTypes.Typ
    ( Typ (..)
    , TypName
    )
import Language.FineTypes.Typ.PrettyPrinter
    ( QueryDocumentation
    , addDocs
    , prettyTyp
    )
import Prettyprinter
    ( Doc
    , Pretty (pretty)
    , comma
    , concatWith
    , defaultLayoutOptions
    , group
    , layoutPretty
    , line
    , line'
    , nest
    , space
    , vsep
    , (<+>)
    )
import Prettyprinter.Render.Text (renderStrict)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

prettyText :: T.Text -> Doc ann
prettyText = pretty

prettyPrintModule :: Module -> String
prettyPrintModule =
    T.unpack
        . renderStrict
        . layoutPretty defaultLayoutOptions
        . prettyModule

-- | Pretty print a 'Module'.
prettyModule :: Module -> Doc ann
prettyModule
    Module
        { moduleName
        , moduleDeclarations
        , moduleImports
        , moduleDocumentation = Documentation docs'
        } =
        let docs i = Map.findWithDefault mempty i docs'
        in  vsep
                [ prettyText "module"
                    <+> pretty moduleName
                    <+> prettyText "where"
                , prettyText ""
                , prettyImports moduleImports
                , prettyDeclarations docs moduleDeclarations
                ]

prettyImports :: Imports -> Doc ann
prettyImports m =
    if Map.null m
        then mempty
        else (vsep . map prettyImport $ Map.toList m) <> line

prettyImport :: (ModuleName, Import) -> Doc ann
prettyImport (name, ImportNames names) =
    "import" <+> pretty name <> nest 4 listing <> ";"
  where
    docs = map pretty $ Set.toList names
    listing = group (line <> "(" <+> list <> line <> ")")
    list = concatWith (\a b -> a <> line' <> comma <> space <> b) docs

prettyDeclarations :: QueryDocumentation -> Declarations -> Doc ann
prettyDeclarations docs = vsep . map (prettyDeclaration docs) . Map.toList

prettyDeclaration :: QueryDocumentation -> (TypName, Typ) -> Doc ann
prettyDeclaration docs (name, typ) =
    addDocs docs (Typ name)
        $ pretty name
            <+> prettyText "="
            <+> prettyTyp docs name typ
            <> prettyText ";"
