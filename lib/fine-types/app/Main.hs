{-# LANGUAGE RecordWildCards #-}

import Prelude

import Main.Utf8 (withUtf8)
import Options
    ( Commands (..)
    , Options (..)
    , parseOptions
    )

import Commands.Convert (convert)
import Commands.Log (inside, withLogPutLn)

main :: IO ()
main = withUtf8 $ do
    Options{..} <- parseOptions
    withLogPutLn optLogFile $ \tracer ->
        case optCommand of
            Convert co -> convert (inside "convert" tracer) co
            Check _ -> error "Not implemented"
