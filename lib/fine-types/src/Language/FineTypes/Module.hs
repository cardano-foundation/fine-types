-- | A 'Module' is a collection of 'Typ' definitions.
module Language.FineTypes.Module
    ( Module (..)
    , Declarations
    , collectNotInScope
    , resolveVars
    ) where

import Prelude

import Data.Map
    ( Map
    )
import Data.Set
    ( Set
    )
import Language.FineTypes.Typ
    ( Typ (..)
    , TypName
    , everything
    , everywhere
    )

import qualified Data.Map as Map
import qualified Data.Set as Set

{-----------------------------------------------------------------------------
    Module type
------------------------------------------------------------------------------}

-- | A 'Module' is a collection of 'Typ' definitions and documentation.
data Module = Module
    { moduleName :: String
    , moduleDeclarations :: Declarations
    }
    deriving (Eq, Show)

type Declarations = Map TypName Typ

{-----------------------------------------------------------------------------
    Module functions
------------------------------------------------------------------------------}

-- | Resolve all variables in a 'Typ' that can be resolved
-- using the given declarations.
--
-- This function will loop in the case of recursive definitions.
resolveVars :: Declarations -> Typ -> Typ
resolveVars declarations = everywhere resolve
  where
    resolve (Var name) = case Map.lookup name declarations of
        Nothing -> Var name
        Just typ -> everywhere resolve typ
    resolve a = a

-- | Collect all 'Typ' names that have not been defined in the 'Module'.
collectNotInScope :: Module -> Set TypName
collectNotInScope Module{moduleDeclarations = declarations} =
    rhs Set.\\ lhs
  where
    lhs = Map.keysSet declarations
    rhs = mconcat . map collectVars $ Map.elems declarations

-- | Collect all 'Var' in a 'Typ'.
collectVars :: Typ -> Set TypName
collectVars = everything (<>) vars
  where
    vars (Var name) = Set.singleton name
    vars _ = Set.empty
