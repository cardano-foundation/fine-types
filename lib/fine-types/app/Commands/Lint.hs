{-# LANGUAGE RecordWildCards #-}

module Commands.Lint where

import Prelude

import Commands.Check.PrettyPrinter (renderParsePackageError)
import Commands.Common (readInput)
import Commands.Log (inside)
import Control.Tracer (Tracer, traceWith)
import Data.Maybe (fromMaybe)
import Language.FineTypes.Package (parsePackageDescription)
import Options.Lint (LintOptions (..))
import System.Exit (exitFailure)

lint :: Tracer IO String -> LintOptions -> IO ()
lint tracer LintOptions{..} = do
    let trace = traceWith tracer
    m <- readInput (inside "readInput" tracer) optInput
    trace
        $ "Linting "
            <> fromMaybe "<stdin>" optInput
    case parsePackageDescription m of
        Left e -> do
            trace "Failed to parse input file:"
            trace $ renderParsePackageError e
            exitFailure
        Right _pd -> do
            trace "Linting..."
            trace "Not implemented yet!"

-- trace "Success!"
