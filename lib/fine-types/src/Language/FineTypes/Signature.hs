{-# LANGUAGE DeriveGeneric #-}

module Language.FineTypes.Signature
    ( Signature (..)
    , Declarations
    , DocumentedDeclarations
    )
where

import Prelude

import Data.Set (Set)
import Data.TreeDiff (ToExpr)
import GHC.Generics (Generic)
import Language.FineTypes.Module (Documentation, ModuleName)
import Language.FineTypes.Typ (TypName)

-- | A 'Signature' is a collection of 'Typ' names and documentation.
data Signature = Signature
    { signatureName :: ModuleName
    , signatureDeclarations :: Set TypName
    , signatureDocumentation :: Documentation
    }
    deriving (Eq, Show, Generic)

instance ToExpr Signature