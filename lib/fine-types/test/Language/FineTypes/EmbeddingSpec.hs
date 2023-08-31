module Language.FineTypes.EmbeddingSpec
    ( spec
    ) where

import Prelude

import Language.FineTypes.Embedding
    ( assocR
    , exponential
    , first'
    , map1
    , representMap
    , second'
    , typecheck
    , unit0
    )
import Language.FineTypes.Module
    ( Module (..), resolveVars )
import Language.FineTypes.Parser
    ( parseFineTypes )
import Language.FineTypes.Typ
    ( OpTwo (..), TypName, Typ (..), everywhere )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )

import qualified Data.Map as Map
import qualified Language.FineTypes.Value as Value

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
spec :: Spec
spec = do
    describe "Embedding" $ do
        it "works on UTxO types" $ do
            Just hs <- parseFineTypes <$> readFile "test/data/HaskellUTxO.fine"
            Just js <- parseFineTypes <$> readFile "test/data/JsonUTxO.fine"

            let hsValue = forgetNames $ resolve hs "Value"
                jsValue = forgetNames $ resolve js "Value"

                zero = Value.Zero (Value.Integer 0)
                emb = second' (map1 assocR <> representMap)
                    <> first' (unit0 zero)
                    <> exponential

            typecheck emb hsValue `shouldBe` Just jsValue

-- | Resolve the 'Typ' corresponding to a name.
-- The result will not contain 'Var'.
resolve :: Module -> TypName -> Typ
resolve m name = resolveVars declarations typ
  where
    typ = declarations Map.! name
    declarations = moduleDeclarations m

-- | Forget all field and constructor names.
forgetNames :: Typ -> Typ
forgetNames = everywhere forget
  where
    forget (ProductN nas@(_:_)) =
        foldr (Two Product2 . snd) (snd $ last nas) (init nas)
    forget (SumN  nas@(_:_)) =
        foldr (Two Sum2 . snd) (snd $ last nas) (init nas)
    forget x = x
