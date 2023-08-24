{-# LANGUAGE LambdaCase #-}

module Language.FineTypes.Typ.Gen where

import Prelude

import Data.Char (toUpper)
import Data.List (nub)
import Data.Traversable (forM)
import Language.FineTypes.Typ
    ( ConstructorName
    , FieldName
    , OpOne (..)
    , OpTwo (..)
    , Typ (Abstract, One, ProductN, SumN, Two, Var, Zero)
    , TypConst (..)
    , TypName
    )
import Test.QuickCheck
    ( Gen
    , elements
    , frequency
    , listOf1
    , oneof
    , scale
    , vectorOf
    )

-- | If the generated 'Typ' should be concrete or not. 'Concrete' will not contain
-- 'Abstract' or 'Var' leaves
data Concrete = Complete | Concrete

patchNoData :: Concrete -> [Gen Typ] -> [Gen Typ]
patchNoData Concrete = id
patchNoData Complete =
    (<>)
        [ pure Abstract
        , Var <$> genVarName
        ]

-- | Minimum depth of the generated 'Typ'. Shorter than depth branches are still
-- possible if the actual  'Typ' is Zero or Abstract or Var
type DepthGen = Int

willBranch :: DepthGen -> Gen Bool
willBranch n = frequency [(1, pure True), (max 0 $ negate n, pure False)]

-- | Generate a random 'Typ'.
genTyp :: Concrete -> DepthGen -> Gen Typ
genTyp f n = do
    b <- willBranch n
    oneof
        $ patchNoData f
        $ if b
            then
                [ Zero <$> genConst
                , One <$> genOne <*> genTyp'
                , Two <$> genTwo <*> genTyp' <*> genTyp'
                , ProductN <$> genTagged genFields
                , SumN <$> genTagged genConstructors
                ]
            else [Zero <$> genConst]
  where
    genTyp' = genTyp f $ n - 1
    genTagged :: Gen [a] -> Gen [(a, Typ)]
    genTagged gen = do
        names <- gen
        forM names $ \name -> (,) name <$> genTyp'

genTwo :: Gen OpTwo
genTwo =
    elements
        [ Sum2
        , Product2
        , PartialFunction
        , FiniteSupport
        ]

genOne :: Gen OpOne
genOne =
    elements
        [ Option
        , Sequence
        , PowerSet
        ]

genConst :: Gen TypConst
genConst =
    elements
        [ Bool
        , Bytes
        , Integer
        , Natural
        , Text
        , Unit
        ]

genNames :: Gen [String]
genNames =
    fmap nub
        $ logScale 2
        $ listOf1
            genName

genName :: Gen [Char]
genName =
    vectorOf 4
        $ elements ['a' .. 'z']

genConstructors :: Gen [ConstructorName]
genConstructors = fmap capitalise <$> genNames

genVarName :: Gen TypName
genVarName = capitalise <$> genName

capitalise :: [Char] -> [Char]
capitalise = \case
    [] -> []
    (x : xs) -> toUpper x : xs

genFields :: Gen [FieldName]
genFields = genNames

logScale :: Double -> Gen a -> Gen a
logScale n = scale logN
  where
    logN x = floor $ logBase n $ 1 + fromIntegral x
