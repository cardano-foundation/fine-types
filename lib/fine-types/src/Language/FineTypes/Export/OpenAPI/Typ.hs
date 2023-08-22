{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Export type definitions to OpenAPI JSON schemas.
--
-- https://www.openapis.org
module Language.FineTypes.Export.OpenAPI.Typ
    ( OpenAPISchema (..)
    , schemaFromModule
    , supportsJSON
    , convertToJSON
    ) where

import Prelude

import Control.DeepSeq
    ( NFData
    )
import Data.Aeson
    ( (.=)
    )
import Data.Text
    ( Text
    )
import GHC.Generics
    ( Generic
    )
import Language.FineTypes.Module
    ( Declarations
    , Module (..)
    , resolveVars
    )
import Language.FineTypes.Typ
    ( ConstructorName
    , FieldName
    , OpOne (..)
    , OpTwo (..)
    , Typ (..)
    , TypConst (..)
    , everything
    , everywhere
    )

import qualified Data.Aeson as JS
import qualified Data.Aeson.Key as JS.Key
import qualified Data.Aeson.Types as JS
import qualified Data.Map as Map
import qualified Data.Text as T

{-----------------------------------------------------------------------------
    OpenAPI
------------------------------------------------------------------------------}
newtype OpenAPISchema = OpenAPISchema {getOpenAPISchema :: JS.Value}
    deriving (Eq, Ord, Show, Generic)

instance NFData OpenAPISchema

-- | Export
--
-- Assumes that the argument satisfies 'supportsJSON'.
schemaFromModule :: Module -> OpenAPISchema
schemaFromModule m =
    OpenAPISchema
        $ wrapSchemasInHeader
            (T.pack $ moduleName m)
            [ (T.pack name, schemaFromTyp typ)
            | (name, typ) <- Map.toList declarations
            ]
  where
    declarations = moduleDeclarations m

-- | Test whether a 'Module' only uses types supported by JSON.
--
-- JSON does not support finite maps such as @↦@, @↦0@, @→∗@.
supportsJSON :: Module -> Bool
supportsJSON =
    and . Map.map isSupportedTyp . moduleDeclarations
  where
    isSupportedTyp = everything (&&) isSupported
    isSupported (Two fun _ _) =
        fun `notElem` [PartialFunction, FiniteSupport]
    isSupported _ = True

-- | Convert 'Typ' definitions to JSON.
--
-- The result satisfies 'supportsJSON'.
--
-- Note: We don't recommend that you use this function,
-- because it does a lot of conversions under the hood.
-- Instead, if you want to export a 'Typ' to JSON,
-- we recommend that you explicitly define a second 'Typ'
-- which is apparently compatible with JSON,
-- and show that the first 'Typ' can be embedded into the second 'Typ'.
convertToJSON :: Declarations -> Declarations
convertToJSON declarations = Map.map (jsonify declarations) declarations

{-----------------------------------------------------------------------------
    Convert Typ to JSON schema
------------------------------------------------------------------------------}
wrapSchemasInHeader :: Text -> [(Text, JS.Value)] -> JS.Value
wrapSchemasInHeader title xs =
    object
        [ "openapi" .= s "3.0.3"
        , "info"
            .= object
                [ "title" .= s title
                , "version" .= s "1"
                ]
        , "components"
            .= object
                [ "schemas"
                    .= object
                        [ key name .= x
                        | (name, x) <- xs
                        ]
                ]
        , "paths" .= object []
        ]

schemaFromTyp :: Typ -> JS.Value
schemaFromTyp = go
  where
    go Abstract =
        object
            ["type" .= s "object"]
    go (Var name) =
        object
            ["$ref" .= s (T.pack $ "#/components/schemas/" <> name)]
    go (Zero Bool) =
        object
            ["type" .= s "boolean"]
    go (Zero Bytes) =
        object
            [ "type" .= s "string"
            , "format" .= s "base16"
            ]
    go (Zero Integer) =
        object
            ["type" .= s "integer"]
    go (Zero Natural) =
        object
            [ "type" .= s "integer"
            , "minimum" .= JS.toJSON (0 :: Int)
            ]
    go (Zero Text) =
        object
            ["type" .= s "string"]
    go (Zero Unit) =
        object
            ["type" .= s "null"]
    go (One Option a) =
        object
            [ "type" .= s "object"
            , "properties" .= object ["0" .= go a]
            ]
    go (One Sequence a) =
        object
            [ "type" .= s "array"
            , "items" .= go a
            ]
    go (One PowerSet a) =
        go (One Sequence a)
    go (Two Sum2 a b) =
        schemaFromSumN [("0", a), ("1", b)]
    go (Two Product2 a b) =
        object
            [ "type" .= s "object"
            , "properties" .= object ["0" .= go a, "1" .= go b]
            , "required" .= array [s "0", s "1"]
            , "additionalProperties" .= false
            ]
    go (Two PartialFunction _ _) =
        error "PartialFunction is not supported by JSON schema"
    go (Two FiniteSupport _ _) =
        error "FiniteSupport is not supported by JSON schema"
    go (ProductN fields) =
        schemaFromProductN fields
    go (SumN constructors) =
        schemaFromSumN constructors

-- | Map a record type to a JSON schema.
--
-- Field that are option types (@?@) will be mapped to optional fields.
schemaFromProductN :: [(FieldName, Typ)] -> JS.Value
schemaFromProductN fields =
    object
        [ "type" .= s "object"
        , "properties"
            .= object
                [ key (T.pack name) .= schemaFromTyp (stripOption typ)
                | (name, typ) <- fields
                ]
        , "required" .= array required
        , "additionalProperties" .= false
        ]
  where
    required =
        [ s (T.pack name)
        | (name, typ) <- fields
        , not (isOption typ)
        ]

stripOption :: Typ -> Typ
stripOption (One Option a) = a
stripOption a = a

isOption :: Typ -> Bool
isOption (One Option _) = True
isOption _ = False

-- | Map a union type to a JSON.
--
-- The encoding corresponds to the 'ObjectWithSingleField' encoding.
schemaFromSumN :: [(ConstructorName, Typ)] -> JS.Value
schemaFromSumN constructors =
    object ["oneOf" .= array (map fromConstructor constructors)]
  where
    fromConstructor (name, typ) =
        object
            [ "type" .= s "object"
            , "title" .= s (T.pack name)
            , "properties"
                .= object [key (T.pack name) .= schemaFromTyp typ]
            , "required" .= array [s (T.pack name)]
            , "additionalProperties" .= false
            ]

{-----------------------------------------------------------------------------
    Preprocessing
------------------------------------------------------------------------------}

-- | Modify the 'Typ' to be closer to JSON.
jsonify :: Declarations -> Typ -> Typ
jsonify declarations =
    mergeRecords . representFiniteMaps . resolveVars declarations

representFiniteMaps :: Typ -> Typ
representFiniteMaps = everywhere represent
  where
    represent x@(Two op a b)
        | op == FiniteSupport || op == PartialFunction =
            One Sequence (Two Product2 a b)
        | otherwise =
            x
    represent x = x

mergeRecords :: Typ -> Typ
mergeRecords = everywhere merge
  where
    merge (Two Product2 (ProductN a) (ProductN b)) =
        ProductN (a <> b)
    merge x = x

{-----------------------------------------------------------------------------
    JSON helpers
------------------------------------------------------------------------------}
key :: Text -> JS.Key
key = JS.Key.fromText

s :: Text -> JS.Value
s = JS.String

object :: [JS.Pair] -> JS.Value
object = JS.object

array :: [JS.Value] -> JS.Value
array = JS.toJSON

false :: JS.Value
false = JS.toJSON False
