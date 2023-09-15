{-# LANGUAGE RecordWildCards #-}

module Commands.Check where

import Prelude

import Commands.Check.PrettyPrinter
    ( renderCompilePackageError
    , renderParsePackageError
    )
import Commands.Common (readInput)
import Commands.Log (inside)
import Control.Tracer (Tracer, traceWith)
import Language.FineTypes.Package
    ( compilePackageDescription
    , parsePackageDescription
    )
import Options.Check (CheckOptions (..))
import System.Exit (exitFailure)

check :: Tracer IO String -> CheckOptions -> IO ()
check tracer CheckOptions{..} = do
    m <- readInput (inside "readInput" tracer) optInput
    case parsePackageDescription m of
        Left e -> do
            trace "Failed to parse input file:"
            trace $ renderParsePackageError e
            exitFailure
        Right pd -> do
            trace "Checking..."
            r <- compilePackageDescription optDir pd
            case r of
                Left e -> do
                    trace "Failed to compile package description:"
                    trace $ renderCompilePackageError e
                    exitFailure
                Right _p -> pure ()
    trace "Success!"
  where
    trace = traceWith tracer
