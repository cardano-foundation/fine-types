{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Options.Convert where

import Prelude

import Data.Foldable (asum)
import Data.Maybe (catMaybes)
import Options.Applicative
    ( InfoMod
    , Parser
    , command
    , flag
    , flag'
    , header
    , help
    , hsubparser
    , info
    , long
    , optional
    , progDesc
    )
import Options.Common (inputOption, outputOption)

data Filter = RewriteMaps | RemoveConstraints

data Format = Json | Yaml
data Schema = JsonSchema Format | HaskellSchema | FineTypeSchema [Filter]

data ConvertOptions = ConvertOptions
    { optInput :: Maybe FilePath
    , optOutput :: Maybe FilePath
    , optSchema :: Schema
    }

convertDescr :: InfoMod a
convertDescr =
    mconcat
        [ progDesc "Convert a fine-types module to a json schema"
        , header "fine-types convert - convert a fine-types module to a schema"
        ]

convertOptions :: Parser ConvertOptions
convertOptions = do
    input <- inputOption
    output <- outputOption
    schema <- schemaP
    pure $ ConvertOptions input output schema

format :: Parser Format
format =
    asum
        [ flag' Json
            $ mconcat
                [ long "json"
                , help "output in json format"
                ]
        , flag Yaml Yaml
            $ mconcat
                [ long "yaml"
                , help "output in yaml format (default format)"
                ]
        ]

schemaP :: Parser Schema
schemaP =
    hsubparser
        $ mconcat
            [ command "haskell"
                $ info (pure HaskellSchema) haskellDescr
            , command "json"
                $ info (JsonSchema <$> format) jsonDescr
            , command "fine-types"
                $ info (FineTypeSchema <$> filterP) fineTypesDescr
            ]

filterP :: Parser [Filter]
filterP = do
    rm <-
        optional
            $ flag' RewriteMaps
            $ mconcat
                [ long "rewrite-maps"
                , help "rewrite maps to lists of tuples"
                ]
    rc <-
        optional
            $ flag' RemoveConstraints
            $ mconcat
                [ long "remove-constraints"
                , help "remove constraints annotations"
                ]
    pure $ catMaybes [rm, rc]

fineTypesDescr :: InfoMod Schema
fineTypesDescr =
    mconcat
        [ progDesc "Output a fine-types schema"
        , header
            "fine-types convert fine-types --rewrite-maps - output a \
            \ fine-types schema where the maps are rewritten to lists"
        ]

jsonDescr :: InfoMod Schema
jsonDescr =
    mconcat
        [ progDesc "Output a json schema"
        , header "fine-types convert json - output a json schema"
        ]

haskellDescr :: InfoMod Schema
haskellDescr =
    mconcat
        [ progDesc "Output a Haskell module"
        , header "fine-types convert haskell - output a Haskell module"
        ]
