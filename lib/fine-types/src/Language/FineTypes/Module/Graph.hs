{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Language.FineTypes.Module.Graph where

import Prelude

import Control.Lens (Field1 (_1), view)
import Data.Graph (Graph, Tree, Vertex, dff, graphFromEdges)
import Data.Map (Map)
import Data.Set (Set)
import Language.FineTypes.Module (Module (..))
import Language.FineTypes.Typ (Typ (Var), TypName, everything)

import qualified Data.Map as Map
import qualified Data.Set as Set

type DependsMap = Map TypName (Set TypName)

mkDependsMap :: Module -> DependsMap
mkDependsMap Module{..} = Map.unionsWith (<>) $ do
    (k, v) <- Map.toList moduleDeclarations
    pure $ Map.singleton k $ vars v

vars :: Typ -> Set TypName
vars = everything (<>) $ \case
    Var x -> Set.singleton x
    _ -> mempty

data DependsData = Node TypName [DependsData]
    deriving (Eq, Ord, Show)

mkGraph
    :: DependsMap
    -> ( Graph
       , Vertex
         -> (TypName, TypName, [TypName])
       , TypName -> Maybe Vertex
       )
mkGraph =
    graphFromEdges
        . fmap (\(k, v) -> (k, k, Set.toList v))
        . Map.toList

forestOfTypes :: DependsMap -> [Tree TypName]
forestOfTypes dm =
    let
        (g, solve, _) = mkGraph dm
    in
        fmap (view _1 . solve) <$> dff g
