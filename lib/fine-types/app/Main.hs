{-# LANGUAGE RecordWildCards #-}

import Prelude

import Control.Exception (catch)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Language.FineTypes.Export.OpenAPI.Schema (schemaFromModule)
import Language.FineTypes.Parser (parseFineTypes')
import Main.Utf8 (withUtf8)
import Options
    ( Commands (..)
    , Options (..)
    , parseOptions
    )
import System.Exit (exitFailure)
import System.IO (Handle, IOMode (..), hPutStrLn, stderr, stdout, withFile)
import Text.Megaparsec (errorBundlePretty)

import Options.Convert (ConvertOptions (..), Format (..))
import Options.Log (LogFile (..))

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Yaml.Pretty as Y

main :: IO ()
main = withUtf8 $ do
    Options{..} <- parseOptions
    case optCommand of
        Convert co -> convert optLogFile co
        Check _ -> error "Not implemented"

withLogHandle :: LogFile -> (Handle -> IO a) -> IO a
withLogHandle StdOut f = f stdout
withLogHandle StdErr f = f stderr
withLogHandle (LogFile fp) f = withFile fp WriteMode f

withLogPutLn :: LogFile -> ((String -> IO ()) -> IO a) -> IO a
withLogPutLn q f = withLogHandle q $ f . hPutStrLn

convert :: LogFile -> ConvertOptions -> IO ()
convert logFile ConvertOptions{..} = do
    withLogPutLn logFile $ \logMe -> do
        m <- case optInput of
            Nothing -> do
                logMe "Reading from stdin"
                getContents
            Just inputFile ->
                readFile inputFile `catch` \e -> do
                    logMe
                        $ "Failed to read input file "
                            <> inputFile
                            <> " : "
                            <> show (e :: IOError)
                    exitFailure
        logMe
            $ "Converting "
                <> fromMaybe "<stdin>" optInput
                <> " to "
                <> fromMaybe "<stdout>" optOutput
        case parseFineTypes' m of
            Left e -> do
                logMe "Failed to parse input file:"
                logMe $ errorBundlePretty e
            Right m' -> do
                let schema = schemaFromModule m'
                case optFormat of
                    Json ->
                        encodePretty schema
                            & maybe BL.putStr BL.writeFile optOutput
                    Yaml ->
                        Y.encodePretty Y.defConfig schema
                            & maybe B.putStr B.writeFile optOutput
                logMe "Success!"
