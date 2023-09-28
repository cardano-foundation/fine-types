{-# LANGUAGE DeriveGeneric #-}

-- | A 'Module' is a collection of 'Typ' definitions.
module Language.FineTypes.Documentation
    ( -- * Documentation texts
      Identifier (..)
    , DocString
    , IdentifierDocumentation
    , Documentation (..)
    , Place (..)
    , document
    ) where

import Prelude

import Data.Map
    ( Map
    )
import Data.TreeDiff (ToExpr)
import GHC.Generics (Generic)
import Language.FineTypes.Typ
    ( ConstructorName
    , FieldName
    , TypName
    )

import qualified Data.Map as Map

{-----------------------------------------------------------------------------
    Documentation texts
------------------------------------------------------------------------------}

-- | An 'Identifier' refers to a specific 'TypName'
-- or one of its fields or constructors.
data Identifier
    = Typ TypName
    | Field TypName FieldName
    | Constructor TypName ConstructorName
    deriving (Eq, Ord, Show, Generic)

instance ToExpr Identifier

type DocString = String

-- | Documentation texts associated to one identifier.
type IdentifierDocumentation = Map Place DocString

-- | Documentation texts associated to 'Identifiers'.
newtype Documentation = Documentation
    {getDocumentation :: Map Identifier IdentifierDocumentation}
    deriving (Eq, Show, Generic)

instance ToExpr Documentation

-- | Place for putting documentation
data Place = BeforeMultiline | Before | After
    deriving (Eq, Ord, Show, Generic)

instance ToExpr Place

document :: Identifier -> Map Place DocString -> Documentation
document i = Documentation . Map.singleton i

-- | Concatenates texts when multiple texts are associated
-- with the same identifier.
instance Semigroup Documentation where
    (Documentation a) <> (Documentation b) =
        Documentation (Map.unionWith (Map.unionWith (<>)) a b)

instance Monoid Documentation where
    mempty = Documentation mempty
