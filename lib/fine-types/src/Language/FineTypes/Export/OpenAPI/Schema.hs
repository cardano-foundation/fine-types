{-# LANGUAGE OverloadedStrings #-}

-- | Export type definitions to OpenAPI JSON schemas.
--
-- https://www.openapis.org
module Language.FineTypes.Export.OpenAPI.Schema
    ( schemaFromModule
    , schemaFromTyp
    , supportsJSON
    , convertToJSON
    ) where

import Prelude

import Data.OpenApi
    ( AdditionalProperties (AdditionalPropertiesAllowed)
    , Components (_componentsSchemas)
    , Info (_infoTitle)
    , OpenApi (_openApiComponents, _openApiInfo)
    , OpenApiItems (OpenApiItemsObject)
    , OpenApiType
        ( OpenApiArray
        , OpenApiBoolean
        , OpenApiInteger
        , OpenApiNull
        , OpenApiNumber
        , OpenApiObject
        , OpenApiString
        )
    , Reference (Reference)
    , Referenced (Inline, Ref)
    , Schema
        ( _schemaAdditionalProperties
        , _schemaAllOf
        , _schemaFormat
        , _schemaItems
        , _schemaMinimum
        , _schemaOneOf
        , _schemaProperties
        , _schemaRequired
        , _schemaTitle
        , _schemaType
        )
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

import qualified Data.HashMap.Strict.InsOrd as MH
import qualified Data.Map as Map
import qualified Data.Text as T

{-----------------------------------------------------------------------------
    OpenAPI
------------------------------------------------------------------------------}

-- | Export
--
-- Assumes that the argument satisfies 'supportsJSON'.
schemaFromModule :: Module -> OpenApi
schemaFromModule m = mempty{_openApiComponents = ds, _openApiInfo = info'}
  where
    info' =
        mempty
            { _infoTitle = T.pack $ moduleName m
            }
    ds = mapDeclarations $ moduleDeclarations m

mapDeclarations :: Declarations -> Components
mapDeclarations ds = mkComponents $ do
    (typName, typ) <- Map.toList ds
    pure (T.pack typName, schemaFromTyp typ)
  where
    mkComponents xs = mempty{_componentsSchemas = MH.fromList xs}

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
    isSupported (Constrained _ _) =
        False
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

schemaFromTyp :: Typ -> Schema
schemaFromTyp = go
  where
    go Abstract =
        mempty
            { _schemaType = Just OpenApiObject
            }
    go (Var name') =
        mempty
            { _schemaAllOf = Just [Ref $ Reference $ T.pack name']
            }
    go (Zero Bool) =
        mempty
            { _schemaType = Just OpenApiBoolean
            }
    go (Zero Bytes) =
        mempty
            { _schemaType = Just OpenApiString
            , _schemaFormat = Just "base16"
            }
    go (Zero Integer) =
        mempty
            { _schemaType = Just OpenApiInteger
            }
    go (Zero Natural) =
        mempty
            { _schemaType = Just OpenApiInteger
            , _schemaMinimum = Just 0
            }
    go (Zero Text) =
        mempty
            { _schemaType = Just OpenApiString
            }
    go (Zero Unit) =
        mempty
            { _schemaType = Just OpenApiNull
            }
    go (Zero Rational) =
        mempty
            { _schemaType = Just OpenApiNumber
            }
    go (One Option a) =
        mempty
            { _schemaType = Just OpenApiObject
            , _schemaProperties = MH.fromList [("0", Inline $ schemaFromTyp a)]
            }
    go (One Sequence a) =
        mempty
            { _schemaType = Just OpenApiArray
            , _schemaItems =
                Just
                    $ OpenApiItemsObject
                    $ Inline
                    $ schemaFromTyp a
            }
    go (One PowerSet a) =
        go (One Sequence a)
    go (Two Sum2 a b) = schemaFromSumN [("0", a), ("1", b)]
    go (Two Product2 a b) =
        mempty
            { _schemaType = Just OpenApiObject
            , _schemaProperties =
                MH.fromList
                    [ ("0", Inline $ schemaFromTyp a)
                    , ("1", Inline $ schemaFromTyp b)
                    ]
            , _schemaRequired = ["0", "1"]
            , _schemaAdditionalProperties =
                Just $ AdditionalPropertiesAllowed False
            }
    go (Two PartialFunction _ _) =
        error "PartialFunction is not supported by JSON schema"
    go (Two FiniteSupport _ _) =
        error "FiniteSupport is not supported by JSON schema"
    go (ProductN fields) =
        schemaFromProductN fields
    go (SumN constructors) =
        schemaFromSumN constructors
    go (Constrained _ _) =
        error "ConstrainedTyp is not supported by JSON schema"

-- | Map a record type to a JSON schema.
--
-- Field that are option types (@?@) will be mapped to optional fields.
schemaFromProductN :: [(FieldName, Typ)] -> Schema
schemaFromProductN fields =
    mempty
        { _schemaType = Just OpenApiObject
        , _schemaProperties =
            MH.fromList
                [ (T.pack name', Inline $ schemaFromTyp (stripOption typ))
                | (name', typ) <- fields
                ]
        , _schemaRequired = required'
        , _schemaAdditionalProperties =
            Just $ AdditionalPropertiesAllowed False
        }
  where
    required' =
        [ T.pack name'
        | (name', typ) <- fields
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
schemaFromSumN :: [(ConstructorName, Typ)] -> Schema
schemaFromSumN constructors =
    mempty
        { _schemaOneOf =
            Just $ do
                (name', typ) <- constructors
                let title' = T.pack name'
                pure
                    $ Inline
                    $ mempty
                        { _schemaType = Just OpenApiObject
                        , _schemaTitle = Just title'
                        , _schemaProperties =
                            MH.fromList [(title', Inline $ schemaFromTyp typ)]
                        , _schemaRequired = [title']
                        , _schemaAdditionalProperties =
                            Just $ AdditionalPropertiesAllowed False
                        }
        }

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
