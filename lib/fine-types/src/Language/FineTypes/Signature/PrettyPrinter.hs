{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.FineTypes.Signature.PrettyPrinter
    ( prettyPrintSignature
    , prettySignature
    ) where

import Prelude

import Data.Foldable (toList)
import Language.FineTypes.Documentation (Documentation (..))
import Language.FineTypes.Signature
    ( Declarations
    , Signature (..)
    )
import Language.FineTypes.Typ
    ( TypName
    )
import Language.FineTypes.Typ.PrettyPrinter
    ( QueryDocumentation
    , addDocs
    )
import Prettyprinter
    ( Doc
    , Pretty (pretty)
    , defaultLayoutOptions
    , layoutPretty
    , vsep
    , (<+>)
    )
import Prettyprinter.Render.Text (renderStrict)

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Language.FineTypes.Documentation as Documentation

prettyText :: T.Text -> Doc ann
prettyText = pretty

prettyPrintSignature :: Signature -> String
prettyPrintSignature =
    T.unpack
        . renderStrict
        . layoutPretty defaultLayoutOptions
        . prettySignature

-- | Pretty print a 'Signature'.
prettySignature :: Signature -> Doc ann
prettySignature
    Signature
        { signatureName
        , signatureDeclarations
        , signatureDocumentation = Documentation docs'
        } =
        let docs i = Map.findWithDefault mempty i docs'
        in  vsep
                [ prettyText "signature"
                    <+> pretty signatureName
                    <+> prettyText "where"
                , prettyText ""
                , prettyDeclarations docs signatureDeclarations
                ]

prettyDeclarations :: QueryDocumentation -> Declarations -> Doc ann
prettyDeclarations docs = vsep . map (prettyDeclaration docs) . toList

prettyDeclaration :: QueryDocumentation -> TypName -> Doc ann
prettyDeclaration docs name =
    addDocs docs (Documentation.Typ name)
        $ pretty name
            <> prettyText ";"
