module Language.FineTypes.Commands.Convert.Haskell.Cabal
    ( fineTypes
    ) where

import Prelude

import Control.Tracer (stdoutTracer)
import Distribution.PackageDescription
    ( BuildInfo
    )
import Distribution.Simple.LocalBuildInfo
    ( ComponentLocalBuildInfo
    , LocalBuildInfo
    )
import Distribution.Simple.PreProcess
    ( PPSuffixHandler
    , PreProcessor (..)
    , mkSimplePreProcessor
    , unsorted
    )
import Language.FineTypes.Commands.Convert
    ( ConvertOptions (..)
    , Schema (..)
    , convert
    )

fineTypes :: PPSuffixHandler
fineTypes = ("fine", ppFineTypesModule)

-- | Preprocess a @.fine@ module into a Haskell module.
ppFineTypesModule
    :: BuildInfo -> LocalBuildInfo -> ComponentLocalBuildInfo -> PreProcessor
ppFineTypesModule _ _ _ =
    PreProcessor
        { platformIndependent = True
        , runPreProcessor = mkSimplePreProcessor $ \inFile outFile _verbosity ->
            convert
                stdoutTracer
                ConvertOptions
                    { optInput = Just inFile
                    , optOutput = Just outFile
                    , optSchema = HaskellSchema
                    }
        , ppOrdering = unsorted
        }
