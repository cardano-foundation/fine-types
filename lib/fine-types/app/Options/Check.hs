{-# LANGUAGE ApplicativeDo #-}

module Options.Check (CheckOptions, checkDescr, checkOptions)
where

import Prelude

import Language.FineTypes.Commands.Check (CheckOptions (..))
import Options.Applicative
    ( InfoMod
    , Parser
    , header
    , progDesc
    )
import Options.Common (dirOption, inputOption)

checkDescr :: InfoMod a
checkDescr =
    mconcat
        [ progDesc "Check a fine-types package for errors"
        , header "fine-types check - check a fine-types package for errors"
        ]

checkOptions :: Parser CheckOptions
checkOptions = CheckOptions <$> inputOption <*> dirOption
