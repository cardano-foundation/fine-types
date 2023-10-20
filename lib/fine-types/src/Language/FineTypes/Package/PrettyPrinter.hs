{-# LANGUAGE OverloadedStrings #-}

module Language.FineTypes.Package.PrettyPrinter
    ( prettyAssertion
    ) where

import Language.FineTypes.Package.Description
    ( Assertion (..)
    )
import Prettyprinter
    ( Doc
    , Pretty (pretty)
    , (<+>)
    )

-- | Pretty print an 'Assertion'.
prettyAssertion :: Assertion -> Doc ann
prettyAssertion (Equal a b) =
    pretty a <+> "==" <+> pretty b
