-- | Export values of Haskell types to 'Value', generated at compile time.
module Language.FineTypes.Export.Haskell.Value.Compiletime
    ( declareInstanceToValue
    , declareToValueFunProduct
    , declareToValueFunSum
    ) where

import Prelude

import Language.FineTypes.Export.Haskell.Language
    ( hsType
    , raiseFirstLetter
    )
import Language.FineTypes.Typ
    ( ConstructorName
    , FieldName
    , Typ
    , TypName
    )

import qualified Language.Haskell.Exts.Simple as Hs

{-----------------------------------------------------------------------------
    Compile time definitions
------------------------------------------------------------------------------}

-- | Declare an @instance ToValue@ for a record type.
declareInstanceToValue
    :: TypName -> Hs.Decl -> Hs.Decl
declareInstanceToValue name toValueDeclaration =
    Hs.InstDecl Nothing instanceRule (Just instanceDecls)
  where
    instanceRule = Hs.IRule Nothing Nothing instanceHead
    instanceHead = Hs.IHApp (Hs.IHCon className) (hsType name)
    className = runtime "ToValue"
    instanceDecls = [Hs.InsDecl toValueDeclaration]

-- | Declare the funcion `toValue` for a record type.
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

-- | Declare the funcion `toValue` for a sum type.
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

{-----------------------------------------------------------------------------
    Expression utilities
------------------------------------------------------------------------------}
runtime :: String -> Hs.QName
runtime = Hs.Qual (Hs.ModuleName "FineTypes.Value") . Hs.Ident

var :: String -> Hs.Exp
var = Hs.Var . Hs.UnQual . Hs.Ident
