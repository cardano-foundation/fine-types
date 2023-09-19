{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Options.Convert where

import Prelude

import Data.Foldable (asum)
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
    , progDesc
    )
import Options.Common (inputOption, outputOption)

data Format = Json | Yaml
data Schema = JsonSchema Format | HaskellSchema

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
