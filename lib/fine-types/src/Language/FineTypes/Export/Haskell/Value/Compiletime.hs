-- | Export values of Haskell types to 'Value', generated at compile time.
module Language.FineTypes.Export.Haskell.Value.Compiletime
    ( declareInstanceToValue
    , declareToValueFunProduct
    , declareToValueFunSum
    , declareToTypFunSum
    , declareToTypFunProduct
    , declareInstanceToTyp
    , typeFromTyp
    ) where

import Prelude

import Language.FineTypes.Export.Haskell.Language
    ( hsList
    , hsPair
    , hsType
    , hsUnit
    , raiseFirstLetter
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

import qualified Language.Haskell.Exts.Simple as Hs

{-----------------------------------------------------------------------------
    Compile time definitions
------------------------------------------------------------------------------}
declareInstance :: String -> TypName -> Hs.Decl -> Hs.Decl
declareInstance className name declaration =
    Hs.InstDecl Nothing instanceRule (Just instanceDecls)
  where
    instanceRule = Hs.IRule Nothing Nothing instanceHead
    instanceHead = Hs.IHApp (Hs.IHCon $ runtime className) (hsType name)
    instanceDecls = [Hs.InsDecl declaration]

-- | Declare an @instance ToTyp@ for a record type.
declareInstanceToTyp
    :: TypName -> Hs.Decl -> Hs.Decl
declareInstanceToTyp = declareInstance "ToTyp"

-- | Declare an @instance ToValue@ for a record type.
declareInstanceToValue
    :: TypName -> Hs.Decl -> Hs.Decl
declareInstanceToValue = declareInstance "ToValue"

-- | Declare the function `toValue` for a record type.
declareToValueFunProduct
    :: TypName
    -> [(FieldName, Typ)]
    -> Hs.Decl
declareToValueFunProduct constructor fields =
    Hs.FunBind
        [Hs.Match (Hs.Ident "toValue") [pat] rhs Nothing]
  where
    pat =
        Hs.PApp
            (Hs.UnQual $ Hs.Ident constructor)
            [ Hs.PVar (Hs.Ident $ field <> "_pat")
            | (field, _) <- fields
            ]
    rhs = Hs.UnGuardedRhs $ productV `Hs.App` Hs.List arguments
    arguments =
        [ Hs.Var (runtime "toValue") `Hs.App` var (field <> "_pat")
        | (field, _) <- fields
        ]
    productV = Hs.Con (runtime "Product")

-- | Declare the function `toTyp` for a record type.
declareToTypFunProduct
    :: TypName
    -> [(FieldName, Typ)]
    -> Hs.Decl
declareToTypFunProduct _ fields =
    Hs.FunBind
        [ Hs.Match
            (Hs.Ident "toTyp")
            [Hs.PWildCard]
            rhs
            Nothing
        ]
  where
    rhs = Hs.UnGuardedRhs $ productN `Hs.App` l
    l =
        Hs.List
            [ Hs.Tuple
                Hs.Boxed
                [Hs.Lit $ Hs.String c, t typ]
            | (c, typ) <- fields
            ]
    t typ = Hs.Var (runtime "toTyp") `Hs.App` q typ
    q typ =
        Hs.ExpTypeSig (Hs.Con proxy)
            $ Hs.TyApp (Hs.TyCon proxy)
            $ typeFromTyp typ
    proxy = Hs.Qual (Hs.ModuleName "Data.Proxy") (Hs.Ident "Proxy")
    productN = Hs.Con (runtimeTyp "ProductN")

-- | Declare the function `toValue` for a sum type.
declareToValueFunSum
    :: TypName
    -> [(ConstructorName, Typ)]
    -> Hs.Decl
declareToValueFunSum _ constructors =
    Hs.FunBind
        [ Hs.Match
            (Hs.Ident "toValue")
            [pat constructor]
            (rhs ix)
            Nothing
        | (ix, (constructor, _)) <- zip [0 ..] constructors
        ]
  where
    pat constructor =
        Hs.PApp
            (Hs.UnQual $ Hs.Ident $ raiseFirstLetter constructor)
            [Hs.PVar (Hs.Ident "x")]
    rhs ix =
        Hs.UnGuardedRhs
            $ (sumV `Hs.App` int ix)
                `Hs.App` (Hs.Var (runtime "toValue") `Hs.App` var "x")
    int ix = Hs.Lit $ Hs.Int ix
    sumV = Hs.Con (runtime "Sum")

-- | Declare the function `toTyp` for a sum type.
declareToTypFunSum
    :: TypName
    -> [(ConstructorName, Typ)]
    -> Hs.Decl
declareToTypFunSum _ constructors =
    Hs.FunBind
        [ Hs.Match
            (Hs.Ident "toTyp")
            [Hs.PWildCard]
            rhs
            Nothing
        ]
  where
    rhs = Hs.UnGuardedRhs $ sumN `Hs.App` l
    l =
        Hs.List
            [ Hs.Tuple
                Hs.Boxed
                [Hs.Lit $ Hs.String c, t typ]
            | (c, typ) <- constructors
            ]
    t typ = Hs.Var (runtime "toTyp") `Hs.App` q typ
    q typ =
        Hs.ExpTypeSig (Hs.Con proxy)
            $ Hs.TyApp (Hs.TyCon proxy)
            $ typeFromTyp typ
    proxy = Hs.Qual (Hs.ModuleName "Data.Proxy") (Hs.Ident "Proxy")
    sumN = Hs.Con (runtimeTyp "SumN")

{-----------------------------------------------------------------------------
    Expression utilities
------------------------------------------------------------------------------}
runtime :: String -> Hs.QName
runtime = Hs.Qual (Hs.ModuleName "FineTypes.Value") . Hs.Ident

runtimeTyp :: String -> Hs.QName
runtimeTyp = Hs.Qual (Hs.ModuleName "FineTypes.Typ") . Hs.Ident

var :: String -> Hs.Exp
var = Hs.Var . Hs.UnQual . Hs.Ident

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
