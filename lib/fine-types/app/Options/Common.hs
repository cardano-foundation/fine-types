{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Options.Common where

import Prelude

import Options.Applicative
    ( Parser
    , ReadM
    , eitherReader
    , help
    , long
    , metavar
    , option
    , short
    , strOption
    , value
    )

inputOption :: Parser (Maybe FilePath)
inputOption =
    option fileReader
        $ mconcat
            [ long "input"
            , short 'i'
            , metavar "FILE"
            , help "input file (stdin if not specified)"
            , value Nothing
            ]

outputOption :: Parser (Maybe FilePath)
outputOption =
    option fileReader
        $ mconcat
            [ long "output"
            , short 'o'
            , metavar "FILE"
            , help "output file (stdout if not specified)"
            , value Nothing
            ]

fileReader :: ReadM (Maybe FilePath)
fileReader = eitherReader $ \case
    file -> Right $ Just file

dirOption :: Parser FilePath
dirOption =
    strOption
        ( mconcat
            [ long "dir"
            , short 'd'
            , metavar "DIR"
            , help "Directory where modules are located"
            , value "."
            ]
        )
