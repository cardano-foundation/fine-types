{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use bimap" #-}

-- | Export values of Haskell types to 'Value'.
module Language.FineTypes.Export.Haskell.Value.Runtime
    ( ToValue (..)
    , V.Value (..)
    ) where

import Prelude ((.))

import qualified Language.FineTypes.Value as V

import qualified Data.ByteString
import qualified Data.Map
import qualified Data.Set
import qualified Data.Text
import qualified Numeric.Natural
import qualified Prelude

{-----------------------------------------------------------------------------
    Runtime definitions
------------------------------------------------------------------------------}

-- | Class of types that can be converted to 'Value'.
--
-- Note: The 'Ord' constraint is necessary to deal with 'Set'.
class Prelude.Ord a => ToValue a where
    toValue :: a -> V.Value

z :: V.ZeroF -> V.Value
z = V.Zero

instance ToValue Prelude.Bool where toValue = z . V.Bool
instance ToValue Data.ByteString.ByteString where toValue = z . V.Bytes
instance ToValue Prelude.Integer where toValue = z . V.Integer
instance ToValue Numeric.Natural.Natural where toValue = z . V.Natural
instance ToValue Data.Text.Text where toValue = z . V.Text
instance ToValue () where toValue _ = z V.Unit

instance ToValue a => ToValue (Prelude.Maybe a) where
    toValue = V.One . V.Option . Prelude.fmap toValue

instance ToValue a => ToValue [a] where
    toValue = V.One . V.Sequence . Prelude.fmap toValue

instance ToValue a => ToValue (Data.Set.Set a) where
    toValue = V.One . V.PowerSet . Data.Set.map toValue

instance (ToValue a, ToValue b) => ToValue (Prelude.Either a b) where
    toValue (Prelude.Left a) = V.Sum 0 (toValue a)
    toValue (Prelude.Right b) = V.Sum 1 (toValue b)

instance (ToValue a, ToValue b) => ToValue (a, b) where
    toValue (a, b) = V.Product [toValue a, toValue b]

instance (ToValue a, ToValue b) => ToValue (Data.Map.Map a b) where
    toValue =
        V.Two
            . V.FiniteMap
            . Data.Map.fromList
            . Prelude.map (\(k, v) -> (toValue k, toValue v))
            . Data.Map.toList
