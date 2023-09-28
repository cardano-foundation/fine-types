{-# LANGUAGE RecordWildCards #-}

module Language.FineTypes.Commands.Check where

import Prelude

import Control.Tracer (Tracer, traceWith)
import Language.FineTypes.Commands.Check.PrettyPrinter
    ( renderCompilePackageError
    , renderParsePackageError
    )
import Language.FineTypes.Commands.Common (readInput)
import Language.FineTypes.Commands.Log (inside)
import Language.FineTypes.Package
    ( compilePackageDescription
    , parsePackageDescription
    )
import System.Exit (exitFailure)

data CheckOptions = CheckOptions
    { optInput :: Maybe FilePath
    , optDir :: FilePath
    }

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
