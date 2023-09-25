{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Export values of Haskell types to 'Value'.
module Language.FineTypes.Export.Haskell.Value.Runtime
    ( ToValue (..)
    , V.Value (..)
    , ToTyp (..)
    ) where

import Prelude ((.))

import Data.Bifunctor (bimap)
import Data.Proxy (Proxy (..))
import Language.FineTypes.Typ (Typ)

import qualified Data.ByteString
import qualified Data.Map
import qualified Data.Set
import qualified Data.Text
import qualified Language.FineTypes.Typ as Typ
import qualified Language.FineTypes.Value as V
import qualified Numeric.Natural
import qualified Prelude

{-----------------------------------------------------------------------------
    Runtime definitions
------------------------------------------------------------------------------}

class ToTyp a where
    toTyp :: Proxy a -> Typ

instance ToTyp Prelude.Bool where toTyp _ = Typ.Zero Typ.Bool
instance ToTyp Data.ByteString.ByteString where toTyp _ = Typ.Zero Typ.Bytes
instance ToTyp Prelude.Integer where toTyp _ = Typ.Zero Typ.Integer
instance ToTyp Numeric.Natural.Natural where toTyp _ = Typ.Zero Typ.Natural
instance ToTyp Data.Text.Text where toTyp _ = Typ.Zero Typ.Text
instance ToTyp () where toTyp _ = Typ.Zero Typ.Unit
instance forall a. (ToTyp a) => ToTyp (Prelude.Maybe a) where
    toTyp _ = Typ.One Typ.Option (toTyp (Proxy @a))
instance forall a. (ToTyp a) => ToTyp [a] where
    toTyp _ = Typ.One Typ.Sequence (toTyp (Proxy @a))
instance forall a. (ToTyp a) => ToTyp (Data.Set.Set a) where
    toTyp _ = Typ.One Typ.PowerSet (toTyp (Proxy @a))
instance forall a b. (ToTyp a, ToTyp b) => ToTyp (Prelude.Either a b) where
    toTyp _ = Typ.Two Typ.Sum2 (toTyp (Proxy @a)) (toTyp (Proxy @b))
instance forall a b. (ToTyp a, ToTyp b) => ToTyp (a, b) where
    toTyp _ = Typ.Two Typ.Product2 (toTyp (Proxy @a)) (toTyp (Proxy @b))
instance forall a b. (ToTyp a, ToTyp b) => ToTyp (Data.Map.Map a b) where
    toTyp _ = Typ.Two Typ.PartialFunction (toTyp (Proxy @a)) (toTyp (Proxy @b))

-- | Class of types that can be converted to 'Value'.
--
-- Note: The 'Ord' constraint is necessary to deal with 'Set'.
class (Prelude.Ord a, ToTyp a) => ToValue a where
    toValue :: a -> V.Value

z :: V.ZeroF -> V.Value
z = V.Zero

instance ToValue Prelude.Bool where toValue = z . V.Bool
instance ToValue Data.ByteString.ByteString where toValue = z . V.Bytes
instance ToValue Prelude.Integer where toValue = z . V.Integer
instance ToValue Numeric.Natural.Natural where toValue = z . V.Natural
instance ToValue Data.Text.Text where toValue = z . V.Text
instance ToValue () where toValue _ = z V.Unit

instance (ToValue a) => ToValue (Prelude.Maybe a) where
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
    toValue :: Data.Map.Map a b -> V.Value
    toValue =
        V.Two
            . V.FiniteMap
            . Data.Map.fromList
            . Prelude.map (bimap toValue toValue)
            . Data.Map.toList
