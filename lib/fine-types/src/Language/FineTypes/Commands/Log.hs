{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.FineTypes.Commands.Log where

import Prelude

import Control.Tracer (Contravariant (..), Tracer (Tracer), emit)
import System.IO (Handle, IOMode (..), hPutStrLn, stderr, stdout, withFile)

data LogFile = LogFile FilePath | StdOut | StdErr

withLogHandle :: LogFile -> (Handle -> IO a) -> IO a
withLogHandle StdOut f = f stdout
withLogHandle StdErr f = f stderr
withLogHandle (LogFile fp) f = withFile fp WriteMode f

withLogPutLn :: LogFile -> (Tracer IO String -> IO a) -> IO a
withLogPutLn q f = withLogHandle q $ f . Tracer . emit . hPutStrLn

inside :: String -> Tracer IO String -> Tracer IO String
inside x = contramap ((x <> ": ") <>)
