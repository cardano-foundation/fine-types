module Language.FineTypes.Signature.ParserSpec
    ( spec
    ) where

import Prelude

import Data.TreeDiff.QuickCheck (ediffEq)
import Language.FineTypes.Signature.Gen (genSignature, shrinkSignature)
import Language.FineTypes.Signature.Parser
    ( parseSignature
    , parseSignature'
    )
import Language.FineTypes.Signature.PrettyPrinter (prettyPrintSignature)
import Test.Hspec
    ( Spec
    , describe
    )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
    ( counterexample
    , forAllShrink
    , property
    )

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
spec :: Spec
spec = do
    describe "pretty print + parse idempotency" $ do
        prop "holds on random generated signatures"
            $ forAllShrink genSignature shrinkSignature
            $ \m ->
                let output = prettyPrintSignature m
                    m' = parseSignature output
                in  property
                        $ counterexample (show (m', m))
                        $ counterexample output
                        $ counterexample (show $ parseSignature' output)
                        $ ediffEq m'
                        $ Just m
