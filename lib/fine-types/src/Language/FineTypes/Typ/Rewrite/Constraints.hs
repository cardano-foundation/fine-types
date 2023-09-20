module Language.FineTypes.Typ.Rewrite.Constraints where

import Language.FineTypes.Typ (Typ (..), everywhere)

removeConstraints :: Typ -> Typ
removeConstraints = everywhere removeConstraint
  where
    removeConstraint :: Typ -> Typ
    removeConstraint (Constrained typ _) = typ
    removeConstraint typ = typ
