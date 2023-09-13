{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Options where

import Prelude

import Data.Foldable (asum)
import Options.Applicative
    ( InfoMod
    , Parser
    , ReadM
    , command
    , eitherReader
    , execParser
    , flag
    , flag'
    , fullDesc
    , header
    , help
    , helper
    , hsubparser
    , info
    , long
    , metavar
    , option
    , progDesc
    , short
    , strOption
    , value
    , (<**>)
    )

data Options = Options
    { optCommand :: Commands
    , optLogFile :: LogFile
    }

newtype Commands = Convert ConvertOptions

data Format = Json | Yaml

data ConvertOptions = ConvertOptions
    { optInput :: Maybe FilePath
    , optOutput :: Maybe FilePath
    , optFormat :: Format
    }

data LogFile = LogFile FilePath | StdOut | StdErr

parseOptions :: IO Options
parseOptions =
    execParser
        $ info (options <**> helper)
        $ fullDesc
            <> progDesc "Run fine-types commands"
            <> header "fine-types - a tool for working with fine-types modules"
  where
    options = Options <$> commands <*> optionLogFile

    commands =
        hsubparser
            $ mconcat
                [ command "convert"
                    $ info (Convert <$> convertOptions) convertDescr
                    -- other commands go here
                ]

convertDescr :: InfoMod Commands
convertDescr =
    mconcat
        [ progDesc "Convert a fine-types module to a json schema"
        , header "fine-types convert - convert a fine-types module to a schema"
        ]

convertOptions :: Parser ConvertOptions
convertOptions = do
    input <-
        option fileReader
            $ mconcat
                [ long "input"
                , short 'i'
                , metavar "FILE"
                , help "input file (stdin if not specified or --)"
                , value Nothing
                ]
    output <-
        option fileReader
            $ mconcat
                [ long "output"
                , short 'o'
                , metavar "FILE"
                , help "output file (stdout if not specified or --)"
                , value Nothing
                ]
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

fileReader :: ReadM (Maybe FilePath)
fileReader = eitherReader $ \case
    "--" -> Right Nothing
    file -> Right $ Just file

optionLogFile :: Parser LogFile
optionLogFile =
    asum
        [ fmap LogFile
            $ strOption
            $ mconcat
                [ long "log-file"
                , metavar "FILE"
                , help "log file"
                ]
        , flag' StdOut
            $ mconcat
                [ long "log-stdout"
                , help "log to stdout"
                ]
        , flag StdErr StdErr
            $ mconcat
                [ long "log-stderr"
                , help "log to stderr (default log destination)"
                ]
        ]
