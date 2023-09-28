{-# LANGUAGE RecordWildCards #-}

module Language.FineTypes.Commands.Convert.Haskell.Cabal
    ( generateSources
    , addFineTypes
    , stdoutTracer
    ) where

import Prelude

import Control.Monad (forM_)
import Control.Tracer (Tracer, stdoutTracer, traceWith)
import Distribution.ModuleName (ModuleName, toFilePath)
import Distribution.PackageDescription
    ( BuildInfo
    , Executable (buildInfo)
    , PackageDescription (..)
    , TestSuite (..)
    , autogenModules
    , benchmarkBuildInfo
    , libBuildInfo
    )
import Distribution.Simple (UserHooks (..))
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
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (<.>), (</>))

generateSources
    :: Tracer IO String
    -> FilePath
    -> PackageDescription
    -> LocalBuildInfo
    -> IO ()
generateSources tracer dirFines pd lbi =
    forM_ (allComponentsInBuildOrder lbi) $ \clbi -> do
        let dirHaskell = autogenComponentModulesDir lbi clbi
            conversions = allComponents pd >>= selectModules dirHaskell dirFines
        forM_ conversions $ \(fine, haskell) -> do
            trace $ concat ["*** Creating ", haskell, "."]
            createDirectoryIfMissing True (takeDirectory haskell)
            guardFine tracer fine
                $ convert
                    tracer
                    ConvertOptions
                        { optInput = Just fine
                        , optOutput = Just haskell
                        , optSchema = HaskellSchema
                        }
  where
    trace = traceWith tracer

guardFine :: Tracer IO String -> FilePath -> IO () -> IO ()
guardFine tracer fp action = do
    exists <- doesFileExist fp
    if exists
        then action
        else
            traceWith tracer
                $ concat ["*** Skipping ", fp, " (file not found)."]

selectModules :: FilePath -> FilePath -> BuildInfo -> [(FilePath, FilePath)]
selectModules dirHaskell dirFines =
    fmap (paths dirHaskell dirFines) . autogenModules

allComponents :: PackageDescription -> [BuildInfo]
allComponents PackageDescription{..} =
    maybe [] (\x -> [libBuildInfo x]) library
        ++ fmap buildInfo executables
        ++ fmap testBuildInfo testSuites
        ++ fmap benchmarkBuildInfo benchmarks

paths :: FilePath -> FilePath -> ModuleName -> (FilePath, FilePath)
paths dirHaskell dirFines x =
    (dirFines </> path <.> "fine", dirHaskell </> path <.> "hs")
  where
    path = toFilePath x

addFineTypes :: Tracer IO String -> UserHooks -> FilePath -> UserHooks
addFineTypes tracer hooks config =
    hooks
        { postConf = \a b c d ->
            postConf hooks a b c d
                >> generateSources tracer config c d
        }
