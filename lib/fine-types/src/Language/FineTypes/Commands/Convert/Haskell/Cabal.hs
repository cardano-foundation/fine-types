module Language.FineTypes.Commands.Convert.Haskell.Cabal (generateSources) where

import Prelude

import Control.Monad (forM_)
import Control.Tracer (stdoutTracer, traceWith)
import Distribution.Simple.BuildPaths (autogenComponentModulesDir)
import Distribution.Simple.LocalBuildInfo
    ( LocalBuildInfo
    , allComponentsInBuildOrder
    )
import Language.FineTypes.Commands.Convert
    ( ConvertOptions (..)
    , Schema (..)
    , convert
    )
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))

generateSources :: LocalBuildInfo -> IO ()
generateSources lbi =
    forM_ (allComponentsInBuildOrder lbi) $ \clbi -> do
        let dir = autogenComponentModulesDir lbi clbi
            file = dir </> "Language/FineTypes/Test/UTxO.hs"
        trace $ concat ["*** Creating ", file, "."]
        createDirectoryIfMissing True (takeDirectory file)
        convert
            stdoutTracer
            ConvertOptions
                { optInput = Just "test/data/HaskellUTxO.fine"
                , optOutput = Just file
                , optSchema = HaskellSchema
                }
  where
    trace = traceWith stdoutTracer
