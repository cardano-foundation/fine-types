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

import Data.Foldable (fold)
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
        , _schemaDescription
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
import Language.FineTypes.Documentation
    ( DocString
    , Documentation (Documentation)
    , Place
    )
import Language.FineTypes.Module
    ( Declarations
    , Module (..)
    , resolveVars
    )
import Language.FineTypes.Typ
    ( Constraint
    , ConstructorName
    , FieldName
    , OpOne (..)
    , OpTwo (..)
    , TypConst (..)
    , TypName
    , TypV (..)
    , VarName
    , everything
    , everywhere
    )
import Language.FineTypes.Typ.PrettyPrinter (prettyTyp)
import Prettyprinter (layoutCompact)
import Prettyprinter.Render.String (renderString)

import qualified Data.HashMap.Strict.InsOrd as MH
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Language.FineTypes.Documentation as Documentation

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
    ds = mapDeclarations (moduleDocumentation m) (moduleDeclarations m)

mapDeclarations :: Documentation -> Declarations TypName -> Components
mapDeclarations doc ds = mkComponents $ do
    (typName, typ) <- Map.toList ds
    let schema0 = schemaFromTyp typ
        schema1 = addDocumentation doc (typName, typ) schema0
    pure (T.pack typName, schema1)
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
convertToJSON :: Declarations TypName -> Declarations TypName
convertToJSON declarations = Map.map (jsonify declarations) declarations

{-----------------------------------------------------------------------------
    Convert Typ to JSON schema
------------------------------------------------------------------------------}

schemaFromTyp :: TypV TypName -> Schema
schemaFromTyp = go
  where
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
    go (Constrained v t c) =
        schemaFromConstraint v t c

schemaFromConstraint :: VarName -> TypV TypName -> Constraint -> Schema
schemaFromConstraint v t c = (schemaFromTyp t){_schemaFormat = Just format}
  where
    format =
        T.pack
            $ renderString
            $ layoutCompact
            $ prettyTyp (const mempty) "no-name"
            $ Constrained v t c

-- | Map a record type to a JSON schema.
--
-- Field that are option types (@?@) will be mapped to optional fields.
schemaFromProductN :: [(FieldName, TypV TypName)] -> Schema
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

stripOption :: TypV var -> TypV var
stripOption (One Option a) = a
stripOption a = a

isOption :: TypV var -> Bool
isOption (One Option _) = True
isOption _ = False

-- | Map a union type to a JSON.
--
-- The encoding corresponds to the 'ObjectWithSingleField' encoding.
schemaFromSumN :: [(ConstructorName, TypV TypName)] -> Schema
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
    Documentation
------------------------------------------------------------------------------}

-- | Postprocess a 'Schema' to add documentation where appropriate.
addDocumentation
    :: Documentation -> (TypName, TypV TypName) -> Schema -> Schema
addDocumentation (Documentation doc) (tname, typ) =
    describeTyp . decribeConstructorsOrFields
  where
    describeTyp =
        maybe id addDescription $ Map.lookup (Documentation.Typ tname) doc

    decribeConstructorsOrFields schema = case typ of
        ProductN fields ->
            foldr
                ($)
                schema
                [ adjustProperty (addDescription d) fname
                | (fname, _) <- fields
                , Just d <- [Map.lookup (Documentation.Field tname fname) doc]
                ]
        SumN constructors ->
            foldr
                ($)
                schema
                [ adjustProperty (addDescription d) cname
                | (cname, _) <- constructors
                , Just d <-
                    [Map.lookup (Documentation.Constructor tname cname) doc]
                ]
        _ -> schema

-- | Adjust a specific entry in the @_schemaProperties@.
adjustProperty
    :: (Schema -> Schema) -> String -> (Schema -> Schema)
adjustProperty f name s =
    s{_schemaProperties = MH.adjust g (T.pack name) (_schemaProperties s)}
  where
    g (Inline a) = Inline (f a)
    g x@(Ref _) = x

-- | Add a @_schemaDescription@ from documentation.
addDescription :: Map.Map Place DocString -> Schema -> Schema
addDescription doc schema =
    schema{_schemaDescription = Just $ T.pack description}
  where
    description = fold doc

{-----------------------------------------------------------------------------
    Preprocessing
------------------------------------------------------------------------------}

-- | Modify the 'Typ' to be closer to JSON.
jsonify :: Declarations TypName -> TypV TypName -> TypV TypName
jsonify declarations =
    mergeRecords . representFiniteMaps . resolveVars declarations

representFiniteMaps :: TypV var -> TypV var
representFiniteMaps = everywhere represent
  where
    represent x@(Two op a b)
        | op == FiniteSupport || op == PartialFunction =
            One Sequence (Two Product2 a b)
        | otherwise =
            x
    represent x = x

mergeRecords :: TypV var -> TypV var
mergeRecords = everywhere merge
  where
    merge (Two Product2 (ProductN a) (ProductN b)) =
        ProductN (a <> b)
    merge x = x
