{-# LANGUAGE DuplicateRecordFields #-}

module Options where

import Prelude

import Options.Applicative
    ( Parser
    , command
    , execParser
    , fullDesc
    , header
    , helper
    , hsubparser
    , info
    , progDesc
    , (<**>)
    )
import Options.Check (CheckOptions, checkDescr, checkOptions)
import Options.Convert (ConvertOptions, convertDescr, convertOptions)
import Options.Lint (LintOptions (..), lintDescr, lintOptions)
import Options.Log (LogFile, optionLogFile)

data Options = Options
    { optCommand :: Commands
    , optLogFile :: LogFile
    }

data Commands = Convert ConvertOptions | Check CheckOptions | Lint LintOptions

parseOptions :: IO Options
parseOptions =
    execParser
        $ info (options <**> helper)
        $ fullDesc
            <> progDesc "Run fine-types commands"
            <> header "fine-types - a tool for working with fine-types modules"
  where
    options = Options <$> commands <*> optionLogFile

commands :: Parser Commands
commands =
    hsubparser
        $ mconcat
            [ command "convert"
                $ info (Convert <$> convertOptions) convertDescr
            , command "check"
                $ info (Check <$> checkOptions) checkDescr
            , command "lint"
                $ info (Lint <$> lintOptions) lintDescr
                -- other commands go here
            ]
