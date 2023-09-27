{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Options.Lint
    ( LintOptions (..)
    , lintDescr
    , lintOptions
    )
where

import Prelude

import Language.FineTypes.Commands.Lint (LintOptions (..))
import Options.Applicative
    ( InfoMod
    , Parser
    , header
    , progDesc
    )
import Options.Common (dirOption, inputOption)

lintDescr :: InfoMod a
lintDescr =
    mconcat
        [ progDesc "Lint a fine-types package"
        , header "fine-types lint - lint a fine-types package"
        ]

lintOptions :: Parser LintOptions
lintOptions = do
    input <- inputOption
    dir <- dirOption
    pure $ LintOptions input dir
