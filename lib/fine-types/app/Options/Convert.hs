{-# LANGUAGE ApplicativeDo #-}

module Options.Convert where

import Prelude

import Data.Foldable (asum)
import Options.Applicative
    ( InfoMod
    , Parser
    , flag
    , flag'
    , header
    , help
    , long
    , progDesc
    )
import Options.Common (inputOption, outputOption)

data Format = Json | Yaml

data ConvertOptions = ConvertOptions
    { optInput :: Maybe FilePath
    , optOutput :: Maybe FilePath
    , optFormat :: Format
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
    format <-
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
    pure $ ConvertOptions input output format
