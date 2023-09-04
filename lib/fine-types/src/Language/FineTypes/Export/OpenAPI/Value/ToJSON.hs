{-# LANGUAGE OverloadedStrings #-}

module Language.FineTypes.Export.OpenAPI.Value.ToJSON
    ( jsonFromValue
    ) where

import Prelude

import Data.Aeson
    ( (.=)
    )
import Data.Base16.Types
    ( extractBase16
    )
import Data.ByteString
    ( ByteString
    )
import Data.Text
    ( Text
    )
import Language.FineTypes.Typ
    ( ConstructorName
    , FieldName
    , Typ
    )
import Language.FineTypes.Value
    ( Ix
    , OneF (..)
    , Value (..)
    , ZeroF (..)
    , hasTyp
    )
import Text.Pretty.Simple (pShow)

import qualified Data.Aeson as JS
import qualified Data.Aeson.Key as JS.Key
import qualified Data.ByteString.Base16 as B
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Language.FineTypes.Typ as Typ

{-----------------------------------------------------------------------------
    JSON export
------------------------------------------------------------------------------}

-- | Convert a 'Value' with a 'Typ' to a JSON value.
--
-- We need the 'Typ' of the 'Value' in order to add field names.
--
-- Note: We assume that the 'Value' has the given 'Typ'.
reportTypeValue :: (Show t, Show v) => t -> v -> String
reportTypeValue typ value =
    TL.unpack $ pShow (("type:" :: Text, typ), ("value:" :: Text, value))

impossibleCase :: (Show t, Show v) => t -> v -> String -> a
impossibleCase typ value prefix =
    error $ prefix <> ": " <> reportTypeValue typ value

jsonFromValue :: Typ -> Value -> JS.Value
jsonFromValue = go
  where
    go :: Typ -> Value -> JS.Value
    go (Typ.Two Typ.Product2 ta tb) (Product [a, b]) =
        JS.object ["0" .= go ta a, "1" .= go tb b]
    go (Typ.ProductN fields) (Product ps) =
        jsonFromProduct fields ps
    go (Typ.Two Typ.Sum2 ta _) (Sum 0 a) =
        JS.object ["0" .= go ta a]
    go (Typ.Two Typ.Sum2 _ tb) (Sum 1 b) =
        JS.object ["1" .= go tb b]
    go (Typ.SumN constructors) (Sum ix a) =
        jsonFromSum constructors ix a
    go Typ.Abstract v =
        impossibleCase Typ.Abstract v "Only concrete types"
    go (Typ.Var _) _ = error "Var not supported"
    go (Typ.One op t) (One v) = go1 op t v
    go t@Typ.Two{} v@(Two _) =
        impossibleCase t v "FiniteMapV is not supported by JSON"
    go (Typ.Zero t) (Zero v) = go0 t v
    go t v = impossibleCase t v "Value does not type check"

    go0 :: Typ.TypConst -> ZeroF -> JS.Value
    go0 Typ.Bool (Bool b) = JS.toJSON b
    go0 Typ.Bytes (Bytes s) = JS.toJSON $ toHex s
    go0 Typ.Integer (Integer i) = JS.toJSON i
    go0 Typ.Natural (Natural n) = JS.toJSON n
    go0 Typ.Text (Text t) = JS.toJSON t
    go0 Typ.Unit Unit = JS.Null
    go0 Typ.Rational (Rational r) = JS.toJSON r
    go0 t v = impossibleCase t v "Value does not type check"

    go1 :: Typ.OpOne -> Typ -> OneF Value -> JS.Value
    go1 Typ.Option t (Option (Just x)) = JS.object ["0" .= go t x]
    go1 Typ.Option _ (Option Nothing) = JS.object []
    go1 Typ.Sequence t (Sequence xs) = JS.toJSON $ map (go t) xs
    go1 Typ.PowerSet t (PowerSet xs) = JS.toJSON $ map (go t) $ Set.toList xs
    go1 t _ v = impossibleCase t v "Value does not type check"

jsonFromProduct :: [(FieldName, Typ)] -> [Value] -> JS.Value
jsonFromProduct fields xs
    | length fields == length xs =
        JS.object
            [ key (T.pack field) .= jsonFromValue typ2 x2
            | ((field, typ), x) <- zip fields xs
            , omitNothingOption x
            , let (typ2, x2) = skipJustOption (typ, x)
            ]
    | otherwise = impossibleCase fields xs "Fields missing"
  where
    omitNothingOption = (One (Option Nothing) /=)

    skipJustOption :: (Typ, Value) -> (Typ, Value)
    skipJustOption (Typ.One Typ.Option typ, One (Option (Just x))) = (typ, x)
    skipJustOption y = y

jsonFromSum :: [(ConstructorName, Typ)] -> Ix -> Value -> JS.Value
jsonFromSum constructors ix a
    | 0 <= ix && ix < length constructors =
        JS.object [key (T.pack name) .= jsonFromValue typ a]
    | otherwise =
        impossibleCase constructors ix "Index out of bounds"
  where
    (name, typ) = constructors !! ix

{-----------------------------------------------------------------------------
    Utilities
------------------------------------------------------------------------------}
toHex :: ByteString -> Text
toHex = extractBase16 . B.encodeBase16

key :: Text -> JS.Key
key = JS.Key.fromText
