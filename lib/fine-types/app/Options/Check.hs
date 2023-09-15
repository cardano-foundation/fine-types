{-# LANGUAGE ApplicativeDo #-}

module Options.Check where

import Prelude

import Options.Applicative
    ( InfoMod
    , Parser
    , header
    , help
    , long
    , metavar
    , progDesc
    , short
    , strOption
    , value
    )
import Options.Common (inputOption)

data CheckOptions = CheckOptions
    { optInput :: Maybe FilePath
    , optDir :: FilePath
    }

checkDescr :: InfoMod a
checkDescr =
    mconcat
        [ progDesc "Check a fine-types package for errors"
        , header "fine-types check - check a fine-types package for errors"
        ]

checkOptions :: Parser CheckOptions
checkOptions = CheckOptions <$> inputOption <*> dirOption

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
