{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Export values of Haskell types to 'Value'.
module Language.FineTypes.Export.Haskell.Value.Runtime
    ( ToValue (..)
    , V.Value (..)
    , ToTyp (..)
    , FromValue (..)
    ) where

import Prelude (Ord, error, (.))

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

class (Ord a, ToTyp a) => FromValue a where
    fromValue :: V.Value -> a

instance FromValue Prelude.Bool where
    fromValue (V.Zero (V.Bool b)) = b
    fromValue _ = error "fromValue: Prelude.Bool"

instance FromValue Data.ByteString.ByteString where
    fromValue (V.Zero (V.Bytes b)) = b
    fromValue _ = error "fromValue: Data.ByteString.ByteString"

instance FromValue Prelude.Integer where
    fromValue (V.Zero (V.Integer b)) = b
    fromValue _ = error "fromValue: Prelude.Integer"

instance FromValue Numeric.Natural.Natural where
    fromValue (V.Zero (V.Natural b)) = b
    fromValue _ = error "fromValue: Numeric.Natural.Natural"

instance FromValue Data.Text.Text where
    fromValue (V.Zero (V.Text b)) = b
    fromValue _ = error "fromValue: Data.Text.Text"

instance FromValue () where
    fromValue (V.Zero V.Unit) = ()
    fromValue _ = error "fromValue: ()"

instance FromValue a => FromValue (Prelude.Maybe a) where
    fromValue (V.One (V.Option Prelude.Nothing)) = Prelude.Nothing
    fromValue (V.One (V.Option (Prelude.Just a))) = Prelude.Just (fromValue a)
    fromValue _ = error "fromValue: Prelude.Maybe"

instance FromValue a => FromValue [a] where
    fromValue (V.One (V.Sequence as)) = Prelude.fmap fromValue as
    fromValue _ = error "fromValue: [a]"

instance FromValue a => FromValue (Data.Set.Set a) where
    fromValue (V.One (V.PowerSet as)) = Data.Set.map fromValue as
    fromValue _ = error "fromValue: Data.Set.Set"

instance (FromValue a, FromValue b) => FromValue (Prelude.Either a b) where
    fromValue (V.Sum 0 a) = Prelude.Left (fromValue a)
    fromValue (V.Sum 1 b) = Prelude.Right (fromValue b)
    fromValue _ = error "fromValue: Prelude.Either"

instance (FromValue a, FromValue b) => FromValue (a, b) where
    fromValue (V.Product [a, b]) = (fromValue a, fromValue b)
    fromValue _ = error "fromValue: (a, b)"

instance (FromValue a, FromValue b) => FromValue (Data.Map.Map a b) where
    fromValue (V.Two (V.FiniteMap m)) =
        Data.Map.fromList
            ( Prelude.map (bimap fromValue fromValue) (Data.Map.toList m)
            )
    fromValue _ = error "fromValue: Data.Map.Map"
