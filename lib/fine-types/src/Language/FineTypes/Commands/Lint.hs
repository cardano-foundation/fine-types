{-# LANGUAGE RecordWildCards #-}

module Language.FineTypes.Commands.Lint
    ( lint
    , LintOptions (..)
    ) where

import Prelude

import Control.Tracer (Tracer, traceWith)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Language.FineTypes.Commands.Check.PrettyPrinter
    ( renderCompilePackageError
    , renderParsePackageError
    )
import Language.FineTypes.Commands.Common (readInput)
import Language.FineTypes.Commands.Log (inside)
import Language.FineTypes.Module (redundantImports)
import Language.FineTypes.Package
    ( Package (..)
    , compilePackageDescription
    , parsePackageDescription
    )
import System.Exit (exitFailure)

import qualified Data.Map as Map

data LintOptions = LintOptions
    { optInput :: Maybe FilePath
    , optDir :: FilePath
    }

lint :: Tracer IO String -> LintOptions -> IO ()
lint tracer LintOptions{..} = do
    let trace = traceWith tracer
    package <- readInput (inside "readInput" tracer) optInput
    trace
        $ "Linting "
            <> fromMaybe "<stdin>" optInput
    case parsePackageDescription package of
        Left e -> do
            trace "Failed to parse input file:"
            trace $ renderParsePackageError e
            exitFailure
        Right pd -> do
            r <- compilePackageDescription optDir pd
            case r of
                Left e -> do
                    trace "Failed to compile package description:"
                    trace $ renderCompilePackageError e
                    exitFailure
                Right (Package ms) -> do
                    rs <- sequence $ do
                        (mname, module') <- Map.assocs ms
                        (imodule, iname) <- toList $ redundantImports module'
                        pure
                            $ traceWith
                                (inside ("module " <> mname) tracer)
                            $ "Redundant import: "
                                <> imodule
                                <> "("
                                <> iname
                                <> ")"
                    case rs of
                        [] -> trace "Success!"
                        _ -> exitFailure
