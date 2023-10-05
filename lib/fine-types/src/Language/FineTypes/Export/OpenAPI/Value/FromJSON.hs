{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.FineTypes.Export.OpenAPI.Value.FromJSON
    ( valueFromJson
    )
where

import Prelude

import Data.ByteString (ByteString)
import Data.Foldable (find, toList)
import Data.Text (Text)
import Data.Traversable (forM)
import Language.FineTypes.Typ (ConstructorName, FieldName, Typ)
import Language.FineTypes.Value (OneF (..), Value (..), ZeroF (..))

import qualified Data.Aeson.Key as JS.Key
import qualified Data.Aeson.KeyMap as KMap
import qualified Data.Aeson.Types as JS
import qualified Data.Base16.Types as B
import qualified Data.ByteString.Base16 as B
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Language.FineTypes.Typ as Typ

type Parser a = JS.Value -> JS.Parser a

-- | Convert a JSON value to a 'Value' of a given 'Typ'.
valueFromJson :: Typ -> JS.Value -> Either String Value
valueFromJson = JS.parseEither . parseTyp

-- | build a Parser for a 'Typ' recursively
parseTyp :: Typ.Typ -> Parser Value
parseTyp = \case
    Typ.Var _ -> const $ fail "Var not supported"
    Typ.Zero tc -> fmap Zero . parseZero tc
    Typ.One op t -> fmap One . parseOne op t
    Typ.Two op t1 t2 -> parseTwo op t1 t2
    Typ.ProductN fields -> parseProductN fields
    Typ.SumN fields -> parseSumN fields
    Typ.Constrained{} -> const $ fail "Constrained not supported"

-- | contextualize parser errors
parsing :: String -> Parser a -> Parser a
parsing s f x = JS.prependFailure ("In " <> s <> ": ") $ f x

-- | match objects with exactly one key-value pair
oneKeyPair :: ((JS.Key, JS.Value) -> JS.Parser a) -> Parser a
oneKeyPair f = onObject $ \case
    [pair] -> f pair
    _ -> fail "expected exactly one key-value pair"

-- | match objects
onObject :: ([(JS.Key, JS.Value)] -> JS.Parser a) -> Parser a
onObject f = \case
    JS.Object obj -> f $ KMap.toAscList obj
    _ -> fail "expected object"

parseSumN :: [(ConstructorName, Typ)] -> Parser Value
parseSumN cs = parsing "Sum" $ oneKeyPair $ \(key, value) -> do
    case find (\(_, (k, _)) -> keyFromString k == key) (zip [0 ..] cs) of
        Nothing -> fail $ "missing constructor " <> keyToString key
        Just (ix, (_, t)) -> Sum ix <$> parseTyp t value

parseProductN :: [(FieldName, Typ)] -> Parser Value
parseProductN fs = parsing "Product" $ onObject $ \obj -> do
    fmap Product $ forM fs $ \(f, t) -> do
        case lookup (keyFromString f) obj of
            Nothing ->
                case hasDefault t of
                    Nothing -> fail $ "missing field " <> f
                    Just d -> pure d
            Just v -> case shortcut t of
                Just (t', amend) -> amend <$> parseTyp t' v
                Nothing -> parseTyp t v

hasDefault :: Typ -> Maybe Value
hasDefault = \case
    Typ.One Typ.Option _ -> Just $ One $ Option Nothing
    Typ.One Typ.Sequence _ -> Just $ One $ Sequence []
    Typ.One Typ.PowerSet _ -> Just $ One $ PowerSet Set.empty
    _ -> Nothing

shortcut :: Typ -> Maybe (Typ, Value -> Value)
shortcut = \case
    Typ.One Typ.Option t -> Just (t, One . Option . Just)
    _ -> Nothing

parseTwo :: Typ.OpTwo -> Typ -> Typ -> Parser Value
parseTwo = \case
    Typ.Sum2 -> \t1 t2 -> parsing "Sum" $ oneKeyPair $ \(key, value) -> do
        case fromKey key of
            "0" -> Sum 0 <$> parseTyp t1 value
            "1" -> Sum 1 <$> parseTyp t2 value
            _ -> fail "expected 0 or 1 as the key"
    Typ.Product2 -> \t1 t2 -> parsing "Product" $ onObject $ \case
        [(key0, value0), (key1, value1)] -> do
            case (fromKey key0, fromKey key1) of
                ("0", "1") -> do
                    a <- parseTyp t1 value0
                    b <- parseTyp t2 value1
                    pure $ Product [a, b]
                _ -> fail "expected 0 and 1 as the keys"
        _ -> fail "expected exactly two key-value pairs"
    Typ.PartialFunction -> \_ _ _ -> fail "PartialFunction not supported"
    Typ.FiniteSupport -> \_ _ _ -> fail "FiniteSupport not supported"

parseOne :: Typ.OpOne -> Typ -> Parser (OneF Value)
parseOne = \case
    Typ.Option -> \t -> parsing "Option" $ onObject $ \case
        [] -> pure $ Option Nothing
        [(key, value)] -> do
            case fromKey key of
                "0" -> Option . Just <$> parseTyp t value
                _ -> fail "expected 0 as the key"
        _ -> fail "expected exactly one or zero key-value pair"
    Typ.Sequence -> \t -> JS.withArray "Sequence" $ \arr -> do
        fmap Sequence <$> mapM (parseTyp t) $ toList arr
    Typ.PowerSet -> \t -> JS.withArray "PowerSet" $ \arr -> do
        fmap (PowerSet . Set.fromList)
            <$> mapM (parseTyp t)
            $ toList arr

parseZero :: Typ.TypConst -> Parser ZeroF
parseZero = \case
    Typ.Bool -> JS.withBool "Bool" $ pure . Bool
    Typ.Bytes -> JS.withText "Bytes" $ pure . Bytes . fromHex
    Typ.Integer -> JS.withScientific "Integer" $ \r ->
        let
            (integer, fraction) = properFraction r
        in
            if fraction == 0
                then pure $ Integer integer
                else fail "expected integer"
    Typ.Natural -> JS.withScientific "Natural" $ \r ->
        let
            (integer, fraction) = properFraction r
        in
            if fraction == 0 && integer >= 0
                then pure $ Natural $ fromInteger integer
                else fail "expected natural"
    Typ.Text -> JS.withText "Text" $ pure . Text
    Typ.Unit -> \case
        JS.Null -> pure Unit
        _ -> fail "expected null"
    Typ.Rational -> JS.withScientific "Rational" $ pure . Rational . toRational

{-----------------------------------------------------------------------------
    Utilities
------------------------------------------------------------------------------}

fromHex :: Text -> ByteString
fromHex = B.decodeBase16' . B.assertBase16

fromKey :: JS.Key -> Text
fromKey = JS.Key.toText

keyFromString :: String -> JS.Key
keyFromString = JS.Key.fromText . T.pack

keyToString :: KMap.Key -> String
keyToString = T.unpack . JS.Key.toText
