{-# LANGUAGE LambdaCase #-}

module Language.FineTypes.Typ.Gen where

import Prelude hiding (round)

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
    , suchThat
    , vectorOf
    )

-- | If the generated 'Typ' should be concrete or not. 'Concrete' will not contain
-- 'Abstract' or 'Var' leaves
data Mode = Complete | Concrete

onComplete :: (Monoid p) => Mode -> p -> p
onComplete Complete f = f
onComplete Concrete _ = mempty

-- | Minimum depth of the generated 'Typ'. Shorter than depth branches are still
-- possible if the actual  'Typ' is Zero or Abstract or Var
type DepthGen = Int

-- | Whether the generated 'Typ' will be Zero or more complex
data Branch = Wont | Will

willBranch :: DepthGen -> Gen Branch
willBranch n =
    frequency
        [ (1, pure Will)
        , (max 0 $ negate n, pure Wont)
        ]

onWill :: (Monoid p) => Branch -> p -> p
onWill Will f = f
onWill Wont _ = mempty

-- | Whether the generated 'Typ' is the top one in the tree or not
data Round = Top | Rest

onTop :: (Monoid p) => Round -> p -> p
onTop Top f = f
onTop Rest _ = mempty

-- | Generate a random 'Typ'.
genTyp :: Mode -> DepthGen -> Gen Typ
genTyp = go Top
  where
    go round mode depth = do
        branching <- onWill <$> willBranch depth
        let top = onTop round
            complete = onComplete mode
            always = id
        oneof
            $ []
                <> always [Zero <$> genConst]
                <> branching [Two <$> genTwoOpen <*> go' <*> go']
                <> top
                    [ One <$> genOne <*> go'
                    , Two <$> genTwoClose <*> go' <*> go'
                    , ProductN <$> genTagged genFields go'
                    , SumN <$> genTagged genConstructors go'
                    ]
                <> complete [Var <$> genVarName]
                <> (top . complete) [pure Abstract]
      where
        go' = go Rest mode $ depth - 1

genTagged :: Gen [a] -> Gen Typ -> Gen [(a, Typ)]
genTagged gen f = do
    names <- gen
    forM names $ \name -> (,) name <$> f

genTwoOpen :: Gen OpTwo
genTwoOpen = elements [Sum2, Product2]

genTwoClose :: Gen OpTwo
genTwoClose = elements [PartialFunction, FiniteSupport]

genOne :: Gen OpOne
genOne = elements [Option, Sequence, PowerSet]

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

notPrivate :: String -> Bool
notPrivate = not . flip elem ["Text", "Bytes", "Bool", "Unit"]

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
genConstructors = genNames

genVarName :: Gen TypName
genVarName = (capitalise <$> genName) `suchThat` notPrivate

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

shrinkTyp :: Typ -> [Typ]
shrinkTyp = \case
    Zero _ -> []
    One _ typ -> typ : shrinkTyp typ
    Two _ typ1 typ2 -> typ1 : typ2 : shrinkTyp typ1 ++ shrinkTyp typ2
    ProductN fields -> map snd fields
    SumN constructors -> map snd constructors
    Var _ -> []
    Abstract -> []
