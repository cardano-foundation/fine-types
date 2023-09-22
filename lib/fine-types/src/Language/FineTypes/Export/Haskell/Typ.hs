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
    , hsList
    , hsPair
    , hsType
    , hsUnit
    , raiseFirstLetter
    )
import Language.FineTypes.Export.Haskell.Value.Compiletime
    ( declareInstanceToValue
    , declareToValueFunProduct
    , declareToValueFunSum
    )
import Language.FineTypes.Module
    ( Module (..)
    )
import Language.FineTypes.Typ
    ( ConstructorName
    , FieldName
    , OpOne (..)
    , OpTwo (..)
    , Typ (..)
    , TypConst (..)
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
            ]
            <> [ hsImportQualifiedAs
                    "Language.FineTypes.Export.Haskell.Value.Runtime"
                    "FineTypes.Value"
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

typeFromTyp :: Typ -> Hs.Type
typeFromTyp = go
  where
    go Abstract = error "Abstract is not supported by Haskell"
    go (Var name) = hsType name
    go (Zero c) = case c of
        Bool -> hsType "Prelude.Bool"
        Bytes -> hsType "Data.ByteString.ByteString"
        Integer -> hsType "Prelude.Integer"
        Natural -> hsType "Numeric.Natural.Natural"
        Rational -> hsType "Prelude.Rational"
        Text -> hsType "Data.Text.Text"
        Unit -> hsUnit
    go (One fun a) = fun1 `Hs.TyApp` go a
      where
        fun1 = case fun of
            Option -> hsType "Prelude.Maybe"
            Sequence -> hsList
            PowerSet -> hsType "Data.Set.Set"
    go (Two fun a b) = (fun2 `Hs.TyApp` go a) `Hs.TyApp` go b
      where
        fun2 = case fun of
            Sum2 -> hsType "Prelude.Either"
            Product2 -> hsPair
            PartialFunction -> hsType "Data.Map.Map"
            -- FIXME: FiniteSupport is bogus, we need to
            -- associate a selected few types with default values.
            -- Use Jonathan's 'monoidmap' package?
            FiniteSupport -> hsType "Data.Map.Map"
    go (ProductN _) =
        error "Nested Product is not supported by Haskell"
    go (SumN _) =
        error "Nested Sum is not supported by Haskell"
    go (Constrained _ typ _) =
        -- FIXME: Emit a warning.
        -- TODO: Add Liquid Haskell support for top-level definitions? 😲
        -- Currently no good representation for comments / Liquid Haskell
        -- in haskell-src-exts, though.
        go typ
