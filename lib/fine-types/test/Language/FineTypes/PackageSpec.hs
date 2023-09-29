{-# LANGUAGE OverloadedLists #-}

module Language.FineTypes.PackageSpec
    ( spec
    ) where

import Prelude

import Control.Monad (when)
import Data.TreeDiff (ediff, prettyEditExprCompact)
import Language.FineTypes.Documentation (Documentation (..), Place (..))
import Language.FineTypes.Module (Import (..), Module (..))
import Language.FineTypes.Package
    ( ErrCompilePackage
    , ErrParsePackage
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
    , shouldBe
    )
import Text.PrettyPrint (render)

import qualified Language.FineTypes.Documentation as Doc
import qualified Language.FineTypes.Package as Pkg

{-----------------------------------------------------------------------------
    Tests lib
------------------------------------------------------------------------------}

specOnFile
    :: FilePath
    -> FilePath
    -> Either ErrParsePackage PackageDescription
    -> Either ErrCompilePackage Package
    -> Spec
specOnFile dir filename pkgConf compiledPackage =
    describe ("on package " <> fp) $ do
        it "parses" $ do
            file <- readFile fp
            parsePackageDescription file
                `shouldBe` pkgConf

        it "compiles" $ do
            file <- readFile fp
            Right pkg <- pure $ parsePackageDescription file
            epkg <- compilePackageDescription dir pkg
            when (epkg /= compiledPackage)
                $ assertFailure
                $ render
                $ prettyEditExprCompact
                $ ediff epkg compiledPackage
  where
    fp = dir </> filename

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}

spec :: Spec
spec = do
    describe "positive" $ do
        positiveOnPackageTest

positiveOnPackageTest :: Spec
positiveOnPackageTest =
    specOnFile
        "test/data/"
        "PackageTest.fine"
        (Right packageTestPackageDescription)
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
