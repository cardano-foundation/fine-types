{-# LANGUAGE RecordWildCards #-}

import Prelude

import Commands.Check (check)
import Commands.Convert (convert)
import Commands.Lint (lint)
import Commands.Log (inside, withLogPutLn)
import Main.Utf8 (withUtf8)
import Options
    ( Commands (..)
    , Options (..)
    , parseOptions
    )

main :: IO ()
main = withUtf8 $ do
    Options{..} <- parseOptions
    withLogPutLn optLogFile $ \tracer ->
        case optCommand of
            Convert co -> convert (inside "convert" tracer) co
            Check co -> check (inside "check" tracer) co
            Lint co -> lint (inside "lint" tracer) co
