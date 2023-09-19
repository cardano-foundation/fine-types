-- | Utilities concerning the Haskell language.
module Language.FineTypes.Export.Haskell.Language
    ( raiseFirstLetter
    , hsType
    , hsList
    , hsUnit
    , hsPair
    , hsImportQualified
    , hsImportQualifiedAs
    ) where

import Prelude

import Language.FineTypes.Typ
    ( TypName
    )

import qualified Data.Char
import qualified Language.Haskell.Exts.Simple as Hs

{-----------------------------------------------------------------------------
    Haskell language utilities
------------------------------------------------------------------------------}
raiseFirstLetter :: String -> String
raiseFirstLetter [] = []
raiseFirstLetter (c : cs) = Data.Char.toUpper c : cs

hsType :: TypName -> Hs.Type
hsType = Hs.TyCon . Hs.UnQual . Hs.Ident

hsList :: Hs.Type
hsList = Hs.TyCon $ Hs.Special Hs.ListCon

hsPair :: Hs.Type
hsPair = Hs.TyCon $ Hs.Special $ Hs.TupleCon Hs.Boxed 2

hsUnit :: Hs.Type
hsUnit = Hs.TyCon $ Hs.Special Hs.UnitCon

hsImportQualified :: String -> Hs.ImportDecl
hsImportQualified name =
    Hs.ImportDecl
        { Hs.importModule = Hs.ModuleName name
        , Hs.importQualified = True
        , Hs.importSrc = False
        , Hs.importSafe = False
        , Hs.importPkg = Nothing
        , Hs.importAs = Nothing
        , Hs.importSpecs = Nothing
        }

hsImportQualifiedAs :: String -> String -> Hs.ImportDecl
hsImportQualifiedAs name as =
    Hs.ImportDecl
        { Hs.importModule = Hs.ModuleName name
        , Hs.importQualified = True
        , Hs.importSrc = False
        , Hs.importSafe = False
        , Hs.importPkg = Nothing
        , Hs.importAs = Just $ Hs.ModuleName as
        , Hs.importSpecs = Nothing
        }
