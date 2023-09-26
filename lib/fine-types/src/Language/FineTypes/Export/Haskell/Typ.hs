-- | Export 'Typ' definitions to Haskell type definitions.
module Language.FineTypes.Export.Haskell.Typ
    ( HsModule (..)
    , prettyPrint
    , haskellFromModule
    ) where

import Prelude

import Language.FineTypes.Export.Haskell.Language
    ( hsImportQualified
    , hsImportQualifiedAs
    , raiseFirstLetter
    )
import Language.FineTypes.Export.Haskell.Value.Compiletime
    ( declareInstanceToTyp
    , declareInstanceToValue
    , declareToTypFunProduct
    , declareToTypFunSum
    , declareToValueFunProduct
    , declareToValueFunSum
    , typeFromTyp
    )
import Language.FineTypes.Module
    ( Module (..)
    )
import Language.FineTypes.Typ
    ( ConstructorName
    , FieldName
    , Typ (..)
    , TypName
    )

import qualified Data.Map as Map
import qualified Language.Haskell.Exts.Simple as Hs

{-----------------------------------------------------------------------------
    Haskell
------------------------------------------------------------------------------}
newtype HsModule = HsModule {getHsModule :: Hs.Module}

prettyPrint :: HsModule -> String
prettyPrint = Hs.prettyPrint . getHsModule

haskellFromModule :: Module -> HsModule
haskellFromModule m =
    HsModule
        $ Hs.Module (Just moduleHead) modulePragmas imports declarations
  where
    moduleHead =
        Hs.ModuleHead (Hs.ModuleName $ moduleName m) Nothing Nothing
    modulePragmas =
        optionsGHC <> languageExtensions
    -- TODO: Filter set of imported modules in order to avoid switching
    -- off the warning -Wno-unused-imports.
    optionsGHC =
        [Hs.OptionsPragma (Just Hs.GHC) "-Wno-unused-imports"]
    languageExtensions =
        map
            (Hs.LanguagePragma . (: []) . Hs.Ident)
            [ "DeriveGeneric"
            , "DerivingStrategies" -- see Note [Deriving]
            , "DuplicateRecordFields"
            , "NoImplicitPrelude"
            ]
    imports =
        map
            hsImportQualified
            [ "Prelude"
            , "Data.ByteString"
            , "Data.Map"
            , "Data.Set"
            , "Data.Text"
            , "GHC.Generics"
            , "Numeric.Natural"
            , "Data.Proxy"
            ]
            <> [ hsImportQualifiedAs
                    "Language.FineTypes.Export.Haskell.Value.Runtime"
                    "FineTypes.Value"
               , hsImportQualifiedAs
                    "Language.FineTypes.Typ"
                    "FineTypes.Typ"
               ]
    declarations =
        concat
            [ declarationFromTyp name typ
            | (name, typ) <- Map.toList (moduleDeclarations m)
            ]

{-----------------------------------------------------------------------------
    Convert Typ to Haskell declaration
------------------------------------------------------------------------------}
declarationFromTyp :: TypName -> Typ -> [Hs.Decl]
declarationFromTyp name typ = case typ of
    ProductN fields ->
        [ Hs.DataDecl
            Hs.DataType
            Nothing
            declaredName
            (declareProduct name fields)
            derivingEqOrdGeneric
        , declareInstanceToValue
            name
            (declareToValueFunProduct name fields)
        , declareInstanceToTyp
            name
            (declareToTypFunProduct name fields)
        ]
    SumN constructors ->
        [ Hs.DataDecl
            Hs.DataType
            Nothing
            declaredName
            (declareSum name constructors)
            derivingEqOrdGeneric
        , declareInstanceToValue
            name
            (declareToValueFunSum name constructors)
        , declareInstanceToTyp
            name
            (declareToTypFunSum name constructors)
        ]
    _ ->
        [Hs.TypeDecl declaredName (typeFromTyp typ)]
  where
    declaredName = Hs.DHead $ Hs.Ident name

{- Note [Deriving]

haskell-src-exts version 1.23.1 is not able to represent
the Haskell98 deriving clause (`deriving (Eq,Ord,Generic)`).
Instead, the generated code needs the `DerivingStrategies` extension.

-}
derivingEqOrdGeneric :: [Hs.Deriving]
derivingEqOrdGeneric =
    map
        derivingClass
        [ "Prelude.Eq"
        , "Prelude.Ord"
        , "Prelude.Show"
        , "GHC.Generics.Generic"
        ]
  where
    derivingClass c =
        Hs.Deriving
            Nothing
            [Hs.IRule Nothing Nothing (instanceHead c)]
    instanceHead = Hs.IHCon . Hs.UnQual . Hs.Ident

declareProduct
    :: TypName
    -> [(FieldName, Typ)]
    -> [Hs.QualConDecl]
declareProduct name fields =
    [ Hs.QualConDecl Nothing Nothing
        $ Hs.RecDecl
            (Hs.Ident name)
            [Hs.FieldDecl [Hs.Ident n] (typeFromTyp t) | (n, t) <- fields]
    ]

-- | Declare a Haskell type with constructors, corresponding to
-- a disjoint sum of types.
--
-- TODO: What about enumerations, i.e. when the arguments are 'Unit'?
declareSum
    :: TypName
    -> [(ConstructorName, Typ)]
    -> [Hs.QualConDecl]
declareSum _ constructors =
    [ Hs.QualConDecl Nothing Nothing
        $ Hs.ConDecl (Hs.Ident (raiseFirstLetter cons)) arguments
    | (cons, typ) <- constructors
    , let arguments = [typeFromTyp typ]
    ]
