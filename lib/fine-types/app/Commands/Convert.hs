{-# LANGUAGE RecordWildCards #-}

module Commands.Convert where

import Prelude

import Commands.Common (readInput)
import Commands.Log (inside)
import Control.Tracer (Tracer, traceWith)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Language.FineTypes.Export.OpenAPI.Schema (schemaFromModule)
import Language.FineTypes.Parser (parseFineTypes')
import Options.Convert (ConvertOptions (..), Format (Json, Yaml))
import Text.Megaparsec.Error (errorBundlePretty)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Yaml.Pretty as Y

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
            let schema = schemaFromModule m'
            case optFormat of
                Json ->
                    encodePretty schema
                        & maybe BL.putStr BL.writeFile optOutput
                Yaml ->
                    Y.encodePretty Y.defConfig schema
                        & maybe B.putStr B.writeFile optOutput
            trace "Success!"
