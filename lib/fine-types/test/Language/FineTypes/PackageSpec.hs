{-# LANGUAGE OverloadedLists #-}

module Language.FineTypes.PackageSpec
    ( spec
    ) where

import Prelude

import Control.Monad (unless)
import Data.Function (on)
import Data.TreeDiff (ToExpr (..), ediff, prettyEditExprCompact)
import Language.FineTypes.Documentation (Documentation (..), Place (..))
import Language.FineTypes.Module (Import (..), Module (..))
import Language.FineTypes.Package
    ( ErrAddModule (ErrNamesNotInScope)
    , ErrCompilePackage (..)
    , ErrParsePackage (..)
    , Package (..)
    , PackageDescription (..)
    , Source (..)
    , compilePackageDescription
    , parsePackageDescription
    )
import Language.FineTypes.Signature (Signature (..))
import Language.FineTypes.Typ
    ( OpTwo (Product2, Sum2)
    , Typ (Two, Var, Zero)
    , TypConst (Bytes, Integer, Rational)
    )
import System.FilePath
    ( (</>)
    )
import Test.HUnit (assertFailure)
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Text.PrettyPrint (render)

import qualified Language.FineTypes.Documentation as Doc
import qualified Language.FineTypes.Package as Pkg
    ( Statement (Include, Signature)
    )

{-----------------------------------------------------------------------------
    Tests lib
------------------------------------------------------------------------------}

assertWithEDiff :: (ToExpr a) => (a -> a -> Bool) -> a -> a -> IO ()
assertWithEDiff eq x y =
    unless (eq x y)
        $ assertFailure
        $ render
        $ prettyEditExprCompact
        $ ediff x y
assertWithEDiffOnEq :: (ToExpr a, Eq a) => a -> a -> IO ()
assertWithEDiffOnEq = assertWithEDiff (==)

assertWithEDiffOnEqExpr :: (ToExpr a) => a -> a -> IO ()
assertWithEDiffOnEqExpr = assertWithEDiff ((==) `on` toExpr)

assertWithEDiffOfEither
    :: (ToExpr a, ToExpr b, Eq b)
    => Either a b
    -> Either a b
    -> IO ()
assertWithEDiffOfEither (Left x) (Left y) = assertWithEDiffOnEqExpr x y
assertWithEDiffOfEither (Right x) (Right y) = assertWithEDiffOnEq x y
assertWithEDiffOfEither x y = assertWithEDiff (\_x _y -> False) x y

parserSpec
    :: FilePath
    -> FilePath
    -> Either ErrParsePackage PackageDescription
    -> Spec
parserSpec dir filename pkgConf =
    describe ("on package " <> fp) $ do
        it "parses" $ do
            file <- readFile fp
            assertWithEDiffOfEither (parsePackageDescription file) pkgConf
  where
    fp = dir </> filename

compilationSpec
    :: FilePath
    -> FilePath
    -> Either ErrCompilePackage Package
    -> Spec
compilationSpec dir filename compiledPackage =
    describe ("on package " <> fp) $ do
        it "compiles" $ do
            file <- readFile fp
            Right pkg <- pure $ parsePackageDescription file
            epkg <- compilePackageDescription dir pkg
            assertWithEDiffOfEither epkg compiledPackage
  where
    fp = dir </> filename

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}

spec :: Spec
spec = do
    describe "positive" $ do
        positiveOnPackageTest
    describe "negative" $ do
        failToParse
        failToParseAnImport
        failToParseAnInclude
        failToCompileAnInclude

failToParse :: Spec
failToParse =
    parserSpec
        "test/data/package/failure"
        "FailToParse.pkg.fine"
        (Left $ ErrParsePackage undefined)

failToParseAnImport :: Spec
failToParseAnImport =
    compilationSpec
        "test/data/package/failure"
        "FailToParseAnImport.pkg.fine"
        (Left $ ErrParseModuleError "FailToParse" undefined)

failToParseAnInclude :: Spec
failToParseAnInclude =
    compilationSpec
        "test/data/package/failure"
        "FailToParseAnInclude.pkg.fine"
        (Left $ ErrIncludeParsePackageError "FailToParse" undefined)

failToCompileAnInclude :: Spec
failToCompileAnInclude = do
    compilationSpec
        "test/data/package/failure"
        "FailToCompileAnInclude.pkg.fine"
        ( Left
            $ ErrIncludeCompilePackage "FailToCompile"
            $ ErrParseModuleError "FailToParse" undefined
        )
    compilationSpec
        "test/data/package/failure"
        "FailToCompileAnInclude2.pkg.fine"
        ( Left
            $ ErrIncludeCompilePackage "FailToCompile2"
            $ ErrAddModule "FailToCompile"
            $ ErrNamesNotInScope ["B"]
        )

positiveOnPackageTest :: Spec
positiveOnPackageTest = do
    parserSpec
        "test/data/"
        "PackageTest.fine"
        (Right packageTestPackageDescription)
    compilationSpec
        "test/data/"
        "PackageTest.fine"
        (Right packageTestCompiledPackage)
  where
    packageTestPackageDescription :: PackageDescription
    packageTestPackageDescription =
        PackageDescription
            { packageName = "PackageTest"
            , packageStatements =
                [ Pkg.Signature "S" (File "package/S.fine")
                , Pkg.Include "P2" (File "package/P2.fine")
                ]
            }

    packageTestCompiledPackage :: Package
    packageTestCompiledPackage =
        Package
            { packageModules =
                [
                    ( "S"
                    , Left
                        ( Signature
                            { signatureName = "S"
                            , signatureDeclarations = ["SType"]
                            , signatureDocumentation =
                                Documentation
                                    { getDocumentation =
                                        [
                                            ( Doc.Typ "SType"
                                            ,
                                                [
                                                    ( Before
                                                    , "The type of the state of the mind"
                                                    )
                                                ]
                                            )
                                        ]
                                    }
                            }
                        )
                    )
                ,
                    ( "X"
                    , Right
                        ( Module
                            { moduleName = "X"
                            , moduleImports = []
                            , moduleDeclarations =
                                [ ("A", Zero Integer)
                                , ("B", Two Sum2 (Var "A") (Zero Bytes))
                                ]
                            , moduleDocumentation =
                                Documentation{getDocumentation = []}
                            }
                        )
                    )
                ,
                    ( "Y"
                    , Right
                        ( Module
                            { moduleName = "Y"
                            , moduleImports =
                                [
                                    ( "X"
                                    , ImportNames
                                        { getImportNames =
                                            ["A", "B"]
                                        }
                                    )
                                ]
                            , moduleDeclarations =
                                [
                                    ( "C"
                                    , Two Product2 (Zero Rational) (Var "B")
                                    )
                                ]
                            , moduleDocumentation =
                                Documentation{getDocumentation = []}
                            }
                        )
                    )
                ]
            }
