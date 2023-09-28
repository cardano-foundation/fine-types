{-# LANGUAGE RecordWildCards #-}

module Language.FineTypes.Commands.Convert where

import Prelude

import Control.Tracer (Tracer, traceWith)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Language.FineTypes.Commands.Common (readInput)
import Language.FineTypes.Commands.Log (inside)
import Language.FineTypes.Export.OpenAPI.Schema (schemaFromModule)
import Language.FineTypes.Module.Parser (parseFineTypes')
import Text.Megaparsec.Error (errorBundlePretty)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Yaml.Pretty as Y
import qualified Language.FineTypes.Export.Haskell.Typ as Hs

data Format = Json | Yaml
data Schema = JsonSchema Format | HaskellSchema

data ConvertOptions = ConvertOptions
    { optInput :: Maybe FilePath
    , optOutput :: Maybe FilePath
    , optSchema :: Schema
    }

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
            case optSchema of
                JsonSchema Json ->
                    encodePretty schema
                        & maybe BL.putStr BL.writeFile optOutput
                JsonSchema Yaml ->
                    Y.encodePretty Y.defConfig schema
                        & maybe B.putStr B.writeFile optOutput
                HaskellSchema ->
                    Hs.prettyPrint (Hs.haskellFromModule m')
                        & maybe putStr writeFile optOutput
            trace "Success!"
