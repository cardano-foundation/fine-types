{-# LANGUAGE DeriveGeneric #-}

-- | A package description is a small program which evaluates to a package.
module Language.FineTypes.Package.Compile
    ( compilePackageDescription
    , ErrCompilePackage (..)
    ) where

import Prelude

import Control.Monad (foldM)
import Control.Monad.Trans.Except
    ( ExceptT (..)
    , except
    , runExceptT
    , throwE
    , withExceptT
    )
import Data.Maybe (fromMaybe)
import Data.TreeDiff (ToExpr)
import GHC.Generics (Generic)
import Language.FineTypes.Module (ModuleName, moduleName)
import Language.FineTypes.Module.Parser (ErrParseModule (..), parseModule')
import Language.FineTypes.Package.Content
    ( ErrAddModule
    , ErrAssertion
    , ErrIncludePackage
    , Package (..)
    , addModule
    , addSignature
    , checkAssertion
    , emptyPackage
    , includePackage
    )
import Language.FineTypes.Package.Description
    ( Assertion (..)
    , PackageDescription (..)
    , PackageName
    , Source (..)
    , Statement (..)
    )
import Language.FineTypes.Package.Parser
    ( ErrParsePackage
    , parsePackageDescription
    )
import Language.FineTypes.Parser.Common ()
import Language.FineTypes.Signature (Signature (signatureName))
import Language.FineTypes.Signature.Parser (parseSignature')
import Language.FineTypes.ToExpr ()
import System.FilePath (takeDirectory, (</>))
import System.IO.Error (catchIOError)

{-----------------------------------------------------------------------------
    Package compiler
------------------------------------------------------------------------------}

data ErrCompilePackage
    = -- | Error while reading a file.
      ErrFile IOError
    | -- | Error while parsing a module.
      ErrParseModuleError ModuleName ErrParseModule
    | -- | Error while parsing an included package description.
      ErrIncludeParsePackageError PackageName ErrParsePackage
    | -- | Error while compiling an included package.
      ErrIncludeCompilePackage PackageName ErrCompilePackage
    | -- | Error while including a package.
      ErrIncludePackage PackageName ErrIncludePackage
    | -- | The package name in the include statement does
      -- not match the included package name
      ErrIncludePackageNameMismatch PackageName PackageName
    | -- | Error while adding a module.
      ErrAddModule ModuleName ErrAddModule
    | -- | The module name in the module statement does
      -- not match the included module name
      ErrAddModuleNameMismatch ModuleName ModuleName
    | -- | Checking the given assertion failed.
      ErrAssertFailed Assertion ErrAssertion
    deriving (Eq, Show, Generic)

instance ToExpr ErrCompilePackage

-- | Compile a package description to a 'Package' or
-- return a descriptive error message.
--
-- The first argument is a directory which is used as base
-- for resolving relative filenames.
compilePackageDescription
    :: FilePath
    -> PackageDescription
    -> IO (Either ErrCompilePackage Package)
compilePackageDescription dir =
    runExceptT
        . foldM (execStatement dir) emptyPackage
        . packageStatements

execStatement
    :: FilePath
    -> Package
    -> Statement
    -> ExceptT ErrCompilePackage IO Package
execStatement dir pkg (Include includeName source) = do
    file <- loadSource dir source
    description <-
        exceptT (ErrIncludeParsePackageError includeName)
            $ parsePackageDescription file
    guardExceptT
        (ErrIncludePackageNameMismatch includeName $ packageName description)
        $ includeName == packageName description
    include <-
        withExceptT (ErrIncludeCompilePackage includeName)
            $ ExceptT
            $ compilePackageDescription
                (changeDirectory dir source)
                description
    exceptT (ErrIncludePackage includeName)
        $ includePackage include pkg
execStatement dir pkg (Module modName mOriginalName source) = do
    file <- loadSource dir source
    let originalName = fromMaybe modName mOriginalName
    m <-
        exceptT (ErrParseModuleError originalName)
            $ parseModule' file
    guardExceptT
        (ErrAddModuleNameMismatch originalName $ moduleName m)
        $ originalName == moduleName m
    exceptT (ErrAddModule modName)
        $ addModule modName m pkg
execStatement dir pkg (Signature modName source) = do
    file <- loadSource dir source
    m <-
        exceptT (ErrParseModuleError modName . ErrParseModule)
            $ parseSignature' file
    guardExceptT
        (ErrAddModuleNameMismatch modName $ signatureName m)
        $ modName == signatureName m
    exceptT (ErrAddModule modName)
        $ addSignature m pkg
execStatement _ pkg (Assert assertion) = do
    exceptT (ErrAssertFailed assertion)
        $ checkAssertion pkg assertion
    pure pkg

loadSource
    :: FilePath
    -> Source
    -> ExceptT ErrCompilePackage IO String
loadSource dir (File path) = withExceptT ErrFile . ExceptT $ do
    (Right <$> readFile (dir </> path)) `catchIOError` (pure . Left)

-- | Change the current directory to use the 'Source' as base.
changeDirectory :: FilePath -> Source -> FilePath
changeDirectory dir (File path) = dir </> takeDirectory path

{-----------------------------------------------------------------------------
    Utilities
------------------------------------------------------------------------------}
guardExceptT :: Monad m => e -> Bool -> ExceptT e m ()
guardExceptT e b = if b then pure () else throwE e

exceptT :: (e -> e') -> Either e a -> ExceptT e' IO a
exceptT f = withExceptT f . except
