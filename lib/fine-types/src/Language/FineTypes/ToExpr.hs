{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.FineTypes.ToExpr where

import Prelude

import Data.Void (Void)
import Text.Megaparsec
    ( ParseErrorBundle (..)
    )

import Data.TreeDiff (ToExpr (..))
import GHC.IO.Exception (IOException)
import Text.Megaparsec.Error (ParseError (..))

instance ToExpr (ParseErrorBundle String Void) where
    toExpr _ = toExpr "ParseErrorBundle"
instance ToExpr (ParseError String Void) where
    toExpr _ = toExpr "ParseError"

instance ToExpr IOException where
    toExpr _ = toExpr "IOException"
