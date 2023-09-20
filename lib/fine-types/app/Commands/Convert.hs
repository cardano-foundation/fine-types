{-# LANGUAGE RecordWildCards #-}

module Commands.Convert where

import Prelude

import Commands.Common (readInput)
import Commands.Log (inside)
import Control.Tracer (Tracer, traceWith)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Language.FineTypes.Export.OpenAPI.Schema (schemaFromModule)
import Language.FineTypes.Module (Module, fmapTyp)
import Language.FineTypes.Module.PrettyPrinter (prettyPrintModule)
import Language.FineTypes.Parser (parseFineTypes')
import Language.FineTypes.Typ.Rewrite.Constraints (removeConstraints)
import Language.FineTypes.Typ.Rewrite.Maps (rewriteMapsAsTuples)
import Options.Convert
    ( ConvertOptions (..)
    , Filter (..)
    , Format (..)
    , Schema (..)
    )
import Text.Megaparsec.Error (errorBundlePretty)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Yaml.Pretty as Y
import qualified Language.FineTypes.Export.Haskell.Typ as Hs

convert :: Tracer IO String -> ConvertOptions -> IO ()
convert tracer ConvertOptions{..} = do
    let trace = traceWith tracer
    m <- readInput (inside "readInput" tracer) optInput
    trace
        $ "Converting "
            <> fromMaybe "<stdin>" optInput
            <> " to "
            <> fromMaybe "<stdout>" optOutput

    case parseFineTypes' m of
        Left e -> do
            trace "Failed to parse input file:"
            trace $ errorBundlePretty e
        Right m' -> do
            case optSchema of
                JsonSchema format -> do
                    let schema = schemaFromModule m'
                    case format of
                        Json ->
                            encodePretty schema
                                & maybe BL.putStr BL.writeFile optOutput
                        Yaml ->
                            Y.encodePretty Y.defConfig schema
                                & maybe B.putStr B.writeFile optOutput
                HaskellSchema ->
                    Hs.prettyPrint (Hs.haskellFromModule m')
                        & maybe putStr writeFile optOutput
                FineTypeSchema filters -> do
                    let m'' = foldl' (flip filterModule) m' filters
                    prettyPrintModule m''
                        & maybe putStr writeFile optOutput
            trace "Success!"

filterModule :: Filter -> Module -> Module
filterModule RewriteMaps = fmapTyp rewriteMapsAsTuples
filterModule RemoveConstraints = fmapTyp removeConstraints
