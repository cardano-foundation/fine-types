-- | A package description is a small program which evaluates to a package.
module Language.FineTypes.Package.Compile
    ( compilePackageDescription
    , ErrCompilePackage
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
import Language.FineTypes.Module (ModuleName, moduleName)
import Language.FineTypes.Package.Content
    ( ErrAddModule
    , ErrIncludePackage
    , Package (..)
    , addModule
    , emptyPackage
    , includePackage
    )
import Language.FineTypes.Package.Description
    ( PackageDescription (..)
    , PackageName
    , Source (..)
    , Statement (..)
    )
import Language.FineTypes.Package.Parser
    ( ErrParsePackage
    , parsePackageDescription
    )
import Language.FineTypes.Parser (ErrParseModule, parseFineTypes')
import System.FilePath (takeDirectory, (</>))
import System.IO.Error (catchIOError)

{-----------------------------------------------------------------------------
    Package compiler
------------------------------------------------------------------------------}

data ErrCompilePackage
    = ErrFile IOError
    | ErrParseModuleError ModuleName ErrParseModule
    | ErrParsePackageError PackageName ErrParsePackage
    | ErrCompilePackage PackageName ErrCompilePackage
    | ErrIncludePackage PackageName ErrIncludePackage
    | ErrIncludePackageNameMismatch PackageName PackageName
    | ErrAddModule ModuleName ErrAddModule
    | ErrAddModuleNameMismatch ModuleName ModuleName
    deriving (Eq, Show)

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
        exceptT (ErrParsePackageError includeName)
            $ parsePackageDescription file
    guardExceptT
        (ErrIncludePackageNameMismatch includeName $ packageName description)
        $ includeName == packageName description
    include <-
        withExceptT (ErrCompilePackage includeName)
            $ ExceptT
            $ compilePackageDescription
                (changeDirectory dir source)
                description
    exceptT (ErrIncludePackage includeName)
        $ includePackage include pkg
execStatement dir pkg (Module modName source) = do
    file <- loadSource dir source
    m <-
        exceptT (ErrParseModuleError modName)
            $ parseFineTypes' file
    guardExceptT
        (ErrAddModuleNameMismatch modName $ moduleName m)
        $ modName == moduleName m
    exceptT (ErrAddModule modName)
        $ addModule m pkg
execStatement _ pkg (Assert _) =
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
