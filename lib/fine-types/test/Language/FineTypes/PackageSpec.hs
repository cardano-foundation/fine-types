{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Language.FineTypes.PackageSpec
    ( spec
    ) where

import Prelude

import Control.Monad (unless)
import Data.TreeDiff (ToExpr (..), ediff, prettyEditExprCompact)
import Language.FineTypes.Documentation (Documentation (..), Place (..))
import Language.FineTypes.Module (Import (..), Module (..))
import Language.FineTypes.Module.Identity (Identity (..))
import Language.FineTypes.Module.Instance (ModuleInstance (..))
import Language.FineTypes.Package
    ( ErrAddModule (..)
    , ErrCompilePackage (..)
    , ErrIncludePackage (..)
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
import Test.HUnit (Assertion, assertFailure)
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
success :: Assertion
success = pure ()

prettyFailure :: ToExpr a => a -> a -> Assertion
prettyFailure x y = assertFailure . render . prettyEditExprCompact $ ediff x y

shouldRelateWithDiff :: (ToExpr a) => (a -> a -> Bool) -> a -> a -> Assertion
shouldRelateWithDiff eq x y = unless (eq x y) $ prettyFailure x y

shouldBeWithDiff :: (ToExpr a, Eq a) => a -> a -> Assertion
shouldBeWithDiff = shouldRelateWithDiff (==)

shouldBeWithDiff'
    :: (ToExpr a, ToExpr b, Eq b, Eq a)
    => Maybe (a -> Bool)
    -> Either a b
    -> Either a b
    -> Assertion
shouldBeWithDiff' m (Left x) (Left y) = case m of
    Nothing -> shouldBeWithDiff x y
    Just match ->
        if match x
            then success
            else prettyFailure x y
shouldBeWithDiff' _m (Right x) (Right y) = shouldBeWithDiff x y
shouldBeWithDiff' _m x y = prettyFailure x y

parserSpec
    :: FilePath
    -> FilePath
    -> Either ErrParsePackage PackageDescription
    -> Maybe (ErrParsePackage -> Bool)
    -> Spec
parserSpec dir filename pkgConf match =
    describe ("on package " <> fp) $ do
        it "parses" $ do
            file <- readFile fp
            shouldBeWithDiff' match (parsePackageDescription file) pkgConf
  where
    fp = dir </> filename

compilationSpec
    :: FilePath
    -> FilePath
    -> Either ErrCompilePackage Package
    -> Maybe (ErrCompilePackage -> Bool)
    -> Spec
compilationSpec dir filename compiledPackage match =
    describe ("on package " <> fp) $ do
        it "compiles" $ do
            file <- readFile fp
            Right pkg <- pure $ parsePackageDescription file
            epkg <- compilePackageDescription dir pkg
            shouldBeWithDiff' match epkg compiledPackage
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
        failToInclude
        failToIncludeWithNameMismatch
        failToAddModuleModuleAlreadyInScope
        failToAddModuleImportNotInScope
        failToAddModuleNamesNotInScope
        failToAddModuleNameMismatch
        failToAddModuleDuplicatedImports

failToParse :: Spec
failToParse =
    parserSpec
        "test/data/package/failure"
        "FailToParse.pkg.fine"
        (Left $ ErrParsePackage undefined)
        $ Just
        $ \(ErrParsePackage _) -> True

failToParseAnImport :: Spec
failToParseAnImport =
    compilationSpec
        "test/data/package/failure"
        "FailToParseAnImport.pkg.fine"
        (Left $ ErrParseModuleError "FailToParse" undefined)
        $ Just
        $ \case
            (ErrParseModuleError "FailToParse" _) -> True
            _ -> False

failToParseAnInclude :: Spec
failToParseAnInclude =
    compilationSpec
        "test/data/package/failure"
        "FailToParseAnInclude.pkg.fine"
        (Left $ ErrIncludeParsePackageError "FailToParse" undefined)
        $ Just
        $ \case
            (ErrIncludeParsePackageError "FailToParse" _) -> True
            _ -> False

failToCompileAnInclude :: Spec
failToCompileAnInclude = do
    compilationSpec
        "test/data/package/failure"
        "FailToCompileAnInclude.pkg.fine"
        ( Left
            $ ErrIncludeCompilePackage "FailToCompile"
            $ ErrParseModuleError "FailToParse" undefined
        )
        $ Just
        $ \case
            ( ErrIncludeCompilePackage
                    "FailToCompile"
                    (ErrParseModuleError "FailToParse" _)
                ) -> True
            _ -> False

    compilationSpec
        "test/data/package/failure"
        "FailToCompileAnIncludeNamesNotInScope.pkg.fine"
        ( Left
            $ ErrIncludeCompilePackage "FailToCompileNamesNotInScope"
            $ ErrAddModule "FailToCompile"
            $ ErrNamesNotInScope ["B"]
        )
        Nothing

failToInclude :: Spec
failToInclude =
    compilationSpec
        "test/data/package/failure"
        "FailToInclude.pkg.fine"
        ( Left
            $ ErrIncludePackage "Works"
            $ ErrModulesAlreadyInScope ["Works"]
        )
        Nothing

failToIncludeWithNameMismatch :: Spec
failToIncludeWithNameMismatch =
    compilationSpec
        "test/data/package/failure"
        "FailToIncludeWithNameMismatch.pkg.fine"
        (Left $ ErrIncludePackageNameMismatch "NameMismatch" "MameMismatch")
        Nothing

failToAddModuleModuleAlreadyInScope :: Spec
failToAddModuleModuleAlreadyInScope =
    compilationSpec
        "test/data/package/failure"
        "FailToAddModuleModuleAlreadyInScope.pkg.fine"
        (Left $ ErrAddModule "Works" ErrModuleAlreadyInScope)
        Nothing

failToAddModuleImportNotInScope :: Spec
failToAddModuleImportNotInScope =
    compilationSpec
        "test/data/package/failure"
        "FailToAddModuleImportNotInScope.pkg.fine"
        ( Left
            $ ErrAddModule "Importing"
            $ ErrImportNotInScope [("Works", "Absent")]
        )
        Nothing

failToAddModuleNamesNotInScope :: Spec
failToAddModuleNamesNotInScope =
    compilationSpec
        "test/data/package/failure"
        "FailToAddModuleNamesNotInScope.pkg.fine"
        (Left $ ErrAddModule "NamesNotInScope" $ ErrNamesNotInScope ["B"])
        Nothing

failToAddModuleNameMismatch :: Spec
failToAddModuleNameMismatch =
    compilationSpec
        "test/data/package/failure"
        "FailToAddModuleNameMismatch.pkg.fine"
        (Left $ ErrAddModuleNameMismatch "NameMismatch" "MameMismatch")
        Nothing

failToAddModuleDuplicatedImports :: Spec
failToAddModuleDuplicatedImports =
    compilationSpec
        "test/data/package/failure"
        "FailToAddModuleDuplicatedImports.pkg.fine"
        ( Left
            $ ErrAddModule
                "DuplicatedImports"
            $ ErrDuplicatedImports
                [("A", ["Works", "WorksCopy"])]
        )
        Nothing

positiveOnPackageTest :: Spec
positiveOnPackageTest = do
    parserSpec
        "test/data/"
        "PackageTest.fine"
        (Right packageTestPackageDescription)
        Nothing
    compilationSpec
        "test/data/"
        "PackageTest.fine"
        (Right packageTestCompiledPackage)
        Nothing
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
                    , Right moduleInstanceX
                    )
                ,
                    ( "Xoops"
                    , Right moduleInstanceX
                    )
                ,
                    ( "Y"
                    , Right
                        ( ModuleInstance
                            { content =
                                Module
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
                                            , Two
                                                Product2
                                                (Zero Rational)
                                                (var moduleIdentityX "B")
                                            )
                                        ]
                                    , moduleDocumentation =
                                        Documentation{getDocumentation = []}
                                    }
                            , identity = moduleIdentityY
                            }
                        )
                    )
                ,
                    ( "Z"
                    , Right
                        ( ModuleInstance
                            { content =
                                Module
                                    { moduleName = "Z"
                                    , moduleImports =
                                        [
                                            ( "Xoops"
                                            , ImportNames
                                                { getImportNames =
                                                    ["A", "B"]
                                                }
                                            )
                                        ]
                                    , moduleDeclarations =
                                        [
                                            ( "C"
                                            , Two
                                                Sum2
                                                (var moduleIdentityXoops "A")
                                                (var moduleIdentityXoops "B")
                                            )
                                        ]
                                    , moduleDocumentation =
                                        Documentation{getDocumentation = []}
                                    }
                            , identity = moduleIdentityZ
                            }
                        )
                    )
                ]
            }

    moduleInstanceX =
        ( ModuleInstance
            { content =
                Module
                    { moduleName = "X"
                    , moduleImports = []
                    , moduleDeclarations =
                        [ ("A", Zero Integer)
                        ,
                            ( "B"
                            , Two
                                Sum2
                                (var moduleIdentityX "A")
                                (Zero Bytes)
                            )
                        ]
                    , moduleDocumentation =
                        Documentation{getDocumentation = []}
                    }
            , identity = moduleIdentityX
            }
        )

    var mid typname = Var (Just mid, typname)

    moduleIdentityX =
        Const "X"
    moduleIdentityXoops =
        moduleIdentityX
    moduleIdentityY =
        Apply (Const "Y") [Const "X"]
    moduleIdentityZ =
        Apply (Const "Z") [moduleIdentityXoops]
