-- | Export 'Typ' definitions to Haskell type definitions.
module Language.FineTypes.Export.Haskell.Typ
    ( HsModule (..)
    , prettyPrint
    , haskellFromModule
    ) where

import Prelude

import Language.FineTypes.Export.Haskell.Language
    ( Annotation
    , hsImportQualified
    , hsList
    , hsPair
    , hsType
    , hsUnit
    , l
    , raiseFirstLetter
    , tyApp
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
import qualified Language.Haskell.Exts as Hs

{-----------------------------------------------------------------------------
    Haskell
------------------------------------------------------------------------------}
newtype HsModule = HsModule {getHsModule :: Hs.Module Annotation}

prettyPrint :: HsModule -> String
prettyPrint = Hs.prettyPrint . getHsModule

haskellFromModule :: Module -> HsModule
haskellFromModule m =
    HsModule
        $ Hs.Module l (Just moduleHead) modulePragmas imports declarations
  where
    moduleHead =
        Hs.ModuleHead l (Hs.ModuleName l $ moduleName m) Nothing Nothing
    modulePragmas =
        optionsGHC <> languageExtensions
    -- TODO: Filter set of imported modules in order to avoid switching
    -- off the warning -Wno-unused-imports.
    optionsGHC =
        [Hs.OptionsPragma l (Just Hs.GHC) "-Wno-unused-imports"]
    languageExtensions =
        map
            (Hs.LanguagePragma l . (: []) . Hs.Ident l)
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
    declarations =
        [ declarationFromTyp name typ
        | (name, typ) <- Map.toList (moduleDeclarations m)
        ]

{-----------------------------------------------------------------------------
    Convert Typ to Haskell declaration
------------------------------------------------------------------------------}
declarationFromTyp :: TypName -> Typ -> Hs.Decl Annotation
declarationFromTyp name typ = case typ of
    ProductN fields ->
        Hs.DataDecl
            l
            (Hs.DataType l)
            Nothing
            declaredName
            (declareRecord name fields)
            derivingEqOrdGeneric
    SumN constructors ->
        Hs.DataDecl
            l
            (Hs.DataType l)
            Nothing
            declaredName
            (declareUnion name constructors)
            derivingEqOrdGeneric
    _ ->
        Hs.TypeDecl l declaredName (typeFromTyp typ)
  where
    declaredName = Hs.DHead l $ Hs.Ident l name

{- Note [Deriving]

haskell-src-exts version 1.23.1 is not able to represent
the Haskell98 deriving clause (`deriving (Eq,Ord,Generic)`).
Instead, the generated code needs the `DerivingStrategies` extension.

-}
derivingEqOrdGeneric :: [Hs.Deriving Annotation]
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
            l
            Nothing
            [Hs.IRule l Nothing Nothing (instanceHead c)]
    instanceHead = Hs.IHCon l . Hs.UnQual l . Hs.Ident l

declareRecord
    :: TypName
    -> [(FieldName, Typ)]
    -> [Hs.QualConDecl Annotation]
declareRecord name fields =
    [ Hs.QualConDecl l Nothing Nothing
        $ Hs.RecDecl
            l
            (Hs.Ident l name)
            [Hs.FieldDecl l [Hs.Ident l n] (typeFromTyp t) | (n, t) <- fields]
    ]

-- | Declare a Haskell type with constructors, corresponding to
-- a disjoint union of types.
--
-- TODO: What about enumerations, i.e. when the arguments are 'Unit'?
declareUnion
    :: TypName
    -> [(ConstructorName, Typ)]
    -> [Hs.QualConDecl Annotation]
declareUnion _ constructors =
    [ Hs.QualConDecl l Nothing Nothing
        $ Hs.ConDecl l (Hs.Ident l (raiseFirstLetter cons)) arguments
    | (cons, typ) <- constructors
    , let arguments = [typeFromTyp typ]
    ]

typeFromTyp :: Typ -> Hs.Type Annotation
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
    go (One fun a) = fun1 `tyApp` go a
      where
        fun1 = case fun of
            Option -> hsType "Prelude.Maybe"
            Sequence -> hsList
            PowerSet -> hsType "Data.Set.Set"
    go (Two fun a b) = (fun2 `tyApp` go a) `tyApp` go b
      where
        fun2 = case fun of
            Sum2 -> hsType "Prelude.Either"
            Product2 -> hsPair
            PartialFunction -> hsType "Data.Map.Map"
            -- FIXME: FiniteSupport is ambiguous. Use Jonathan's 'monoidmap' package.
            FiniteSupport -> hsType "Data.Map.Map"
    go (ProductN _) =
        error "Nested Record is not supported by Haskell"
    go (SumN _) =
        error "Nested Union is not supported by Haskell"
    go (Constrained _ _) =
        -- TODO: Support using Liquid Haskell? 😲
        error "Constrained is (currently) not supported by Haskell"
