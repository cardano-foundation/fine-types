{-# LANGUAGE ApplicativeDo #-}

module Options.Log where

import Prelude

import Data.Foldable (asum)
import Options.Applicative
    ( Parser
    , flag
    , flag'
    , help
    , long
    , metavar
    , strOption
    )

data LogFile = LogFile FilePath | StdOut | StdErr

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
