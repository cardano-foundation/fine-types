{-# LANGUAGE OverloadedStrings #-}

module Language.FineTypes.Export.OpenAPI.Value
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
    , TwoF (..)
    , Value (..)
    , ZeroF (..)
    )

import qualified Data.Aeson as JS
import qualified Data.Aeson.Key as JS.Key
import qualified Data.ByteString.Base16 as B
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Language.FineTypes.Typ as Typ

{-----------------------------------------------------------------------------
    JSON export
------------------------------------------------------------------------------}

-- | Convert a 'Value' with a 'Typ' to a JSON value.
--
-- We need the 'Typ' of the 'Value' in order to add field names.
--
-- Note: We assume that the 'Value' has the given 'Typ'.
jsonFromValue :: Typ -> Value -> JS.Value
jsonFromValue = go
  where
    go :: Typ -> Value -> JS.Value
    go (Typ.Two Typ.Product2 ta tb) (Product [a, b]) =
        JS.object ["0" .= go ta a, "1" .= go tb b]
    go (Typ.ProductN fields) x@(Product _) =
        jsonFromProduct fields
            $ flattenBinaryProduct (length fields) x
    -- TODO: Don't flatten automatically when Embedding get better.

    go (Typ.Two Typ.Sum2 ta _) (Sum 0 a) =
        JS.object ["0" .= go ta a]
    go (Typ.Two Typ.Sum2 _ tb) (Sum 1 b) =
        JS.object ["1" .= go tb b]
    go (Typ.SumN constructors) (Sum ix a) =
        jsonFromSum constructors ix a
    go Typ.Abstract _ = error "jsonFromValue: Typ may not be abstract."
    go (Typ.Var _) (Zero v) = go0 v
    go (Typ.One op t) (One v) = go1 op t v
    go Typ.Two{} (Two v) = go2 v
    go _ _ = error "jsonFromValue: Typ error"

    go0 :: ZeroF -> JS.Value
    go0 (Bool b) = JS.toJSON b
    go0 (Bytes s) = JS.toJSON $ toHex s
    go0 (Integer i) = JS.toJSON i
    go0 (Natural n) = JS.toJSON n
    go0 (Text t) = JS.toJSON t
    go0 Unit = JS.Null

    go1 :: Typ.OpOne -> Typ -> OneF Value -> JS.Value
    go1 Typ.Option t (Option (Just x)) = JS.object ["0" .= go t x]
    go1 Typ.Option _ (Option Nothing) = JS.object []
    go1 Typ.Sequence t (Sequence xs) = JS.toJSON $ map (go t) xs
    go1 Typ.PowerSet t (PowerSet xs) = JS.toJSON $ map (go t) $ Set.toList xs
    go1 _ _ _ = error "jsonFromValue: Typ error"

    go2 :: TwoF Value Value -> JS.Value
    go2 (FiniteMap _) =
        error "FiniteMapV is not supported by JSON"

-- | Flatten a chain of @n@ binary products to a single 'Product'.
flattenBinaryProduct :: Int -> Value -> [Value]
flattenBinaryProduct = flatten
  where
    flatten :: Int -> Value -> [Value]
    flatten 1 x = [x]
    flatten n (Product [x, y]) = x : flatten (n - 1) y
    flatten _ x = [x]

jsonFromProduct :: [(FieldName, Typ)] -> [Value] -> JS.Value
jsonFromProduct fields xs
    | length fields == length xs =
        JS.object
            [ key (T.pack field) .= jsonFromValue typ2 x2
            | ((field, typ), x) <- zip fields xs
            , omitNothingOption x
            , let (typ2, x2) = skipJustOption (typ, x)
            ]
    | otherwise =
        error "jsonFromRecord: field count of Value does not match Typ"
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
        error "jsonFromSum: index of Value does not match Typ"
  where
    (name, typ) = constructors !! ix

{-----------------------------------------------------------------------------
    Utilities
------------------------------------------------------------------------------}
toHex :: ByteString -> Text
toHex = extractBase16 . B.encodeBase16

key :: Text -> JS.Key
key = JS.Key.fromText
