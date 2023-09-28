module Language.FineTypes.Commands.Common where

import Prelude

import Control.Exception (catch)
import Control.Tracer (Tracer, traceWith)
import System.Exit (exitFailure)

readInput :: Tracer IO String -> Maybe FilePath -> IO String
readInput tracer optInput =
    case optInput of
        Nothing -> do
            traceWith tracer "Reading from stdin"
            getContents
        Just inputFile ->
            readFile inputFile `catch` \e -> do
                traceWith tracer
                    $ "Failed to read input file "
                        <> inputFile
                        <> " : "
                        <> show (e :: IOError)
                exitFailure
