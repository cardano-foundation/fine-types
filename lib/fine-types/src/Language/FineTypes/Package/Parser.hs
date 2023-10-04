{-# LANGUAGE DeriveGeneric #-}

module Language.FineTypes.Package.Parser
    ( ErrParsePackage (..)
    , parsePackageDescription
    ) where

import Prelude

import Data.Bifunctor (first)
import Data.TreeDiff (ToExpr (..))
import Data.Void (Void)
import GHC.Generics (Generic)
import Language.FineTypes.Module.Parser (moduleName)
import Language.FineTypes.Package.Description
    ( PackageDescription (PackageDescription)
    , PackageName
    , Source (..)
    , Statement (..)
    )
import Language.FineTypes.Parser.Common (typName)
import Language.FineTypes.Parser.Lexer (space, symbol)
import Text.Megaparsec
    ( ParseErrorBundle
    , Parsec
    , between
    , endBy
    , parse
    , takeWhileP
    , (<|>)
    )

import qualified Text.Megaparsec.Char as C

{-----------------------------------------------------------------------------
    Exported functions
------------------------------------------------------------------------------}
parsePackageDescription :: String -> Either ErrParsePackage PackageDescription
parsePackageDescription = first ErrParsePackage . parse packageFull ""

newtype ErrParsePackage = ErrParsePackage
    {parseErrorBundle :: ParseErrorBundle String Void}
    deriving (Eq, Show, Generic)

instance ToExpr ErrParsePackage where
    toExpr _ = toExpr "ErrParsePackage"

{-----------------------------------------------------------------------------
    Parser
------------------------------------------------------------------------------}

type Parser = Parsec Void String

packageFull :: Parser PackageDescription
packageFull = space *> package

package :: Parser PackageDescription
package =
    PackageDescription
        <$ symbol "package"
        <*> packageName
        <* symbol "where"
        <*> statements

statements :: Parser [Statement]
statements = statement `endBy` symbol ";"

statement :: Parser Statement
statement =
    (Include <$ symbol "include" <*> packageName <*> source)
        <|> (Module <$ symbol "module" <*> moduleName <*> source)
        <|> (Signature <$ symbol "signature" <*> moduleName <*> source)
        <|> (Assert () <$ symbol "assert")

source :: Parser Source
source =
    File
        <$ symbol "from"
        <*> filePath

{-----------------------------------------------------------------------------
    Lexer
------------------------------------------------------------------------------}
packageName :: Parser PackageName
packageName = typName

filePath :: Parser FilePath
filePath =
    between (C.string "\"") (C.string "\"")
        $ takeWhileP (Just "character") (`notElem` "\"\n")
