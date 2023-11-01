{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Language.FineTypes.Typ.Gen where

import Prelude hiding (round)

import Data.Char
    ( isAlphaNum
    , isPunctuation
    , isSymbol
    , toUpper
    )
import Data.List (isInfixOf, nub)
import Data.Traversable (forM)
import Language.FineTypes.Typ
    ( Constraint
    , Constraint1 (..)
    , ConstructorName
    , FieldName
    , OpOne (..)
    , OpTwo (..)
    , TypConst (..)
    , TypName
    , TypV (..)
    )
import Test.QuickCheck
    ( Gen
    , arbitrary
    , choose
    , elements
    , frequency
    , listOf1
    , oneof
    , scale
    , shrink
    , shrinkList
    , suchThat
    , vectorOf
    )

-- | If the generated 'Typ' should be concrete or not.
-- 'Concrete' will not contain 'Var' leaves
data Mode v where
    Complete :: Mode TypName
    Concrete :: Mode v

genVar :: Mode v -> [Gen (TypV v)]
genVar Complete = [Var <$> genVarName]
genVar Concrete = []

-- | Minimum depth of the generated 'Typ'. Shorter than depth branches are still
-- possible if the actual 'Typ' is Zero or Var
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

-- | Whether the generated 'Typ' will have constraints or not
data WithConstraints = WithConstraints | WithoutConstraints

onConstraints :: (Monoid p) => WithConstraints -> p -> p
onConstraints WithConstraints f = f
onConstraints WithoutConstraints _ = mempty

-- | Whether the generated 'Typ' is the top one in the tree or not
data Round = Top | Rest

onTop :: (Monoid p) => Round -> p -> p
onTop Top f = f
onTop Rest _ = mempty

-- | Generate a random 'Typ'.
genTyp
    :: (TypV var -> Bool)
    -- ^ Whether the generated 'Typ' should be filtered out
    -> WithConstraints
    -- ^ Whether the generated 'Typ' can have constraints or not
    -> Mode var
    -- ^ Whether the generated 'Typ' should be concrete or not
    -> DepthGen
    -- ^ Maximum depth of the generated 'Typ'
    -> Gen (TypV var)
genTyp out = go Top
  where
    go round hasC mode depth = do
        branching <- onWill <$> willBranch depth
        let top = onTop round
            always = id
            constrained = onConstraints hasC
            expansion =
                []
                    <> always [Zero <$> genConst]
                    <> branching
                        [ Two <$> genTwoOpen <*> go' <*> go'
                        , One <$> genOne <*> go'
                        ]
                    <> top
                        [ One <$> genOne <*> go'
                        , Two <$> genTwoClose <*> go' <*> go'
                        , ProductN <$> genTagged genFields go'
                        , SumN <$> genTagged genConstructors go'
                        ]
                    <> genVar mode
                    <> constrained [genConstrainedTyp mode]
        oneof expansion `suchThat` (not . out)
      where
        go' = go Rest hasC mode (depth - 1)

genConstraint :: Int -> Gen Constraint
genConstraint 0 = pure []
genConstraint n = listOf1 $ genConstraint1 $ n - 1

genConstraint1 :: Int -> Gen Constraint1
genConstraint1 n =
    oneof
        $ [Braces <$> genConstraint n | n > 0]
            <> [ Token
                    <$> listOf1 (arbitrary `suchThat` goodChar)
                        `suchThat` goodToken
               ]

goodToken :: String -> Bool
goodToken = not . ("--" `isInfixOf`)

goodChar :: Char -> Bool
goodChar x =
    (isSymbol x || isAlphaNum x || isPunctuation x) && x `notElem` " {}"

genConstrainedTyp :: Mode v -> Gen (TypV v)
genConstrainedTyp mode = do
    constraintDepth <- choose (1, 2)
    c <- genConstraint constraintDepth
    typ <- genTyp'
    v <- genName
    pure $ Constrained v typ c
  where
    always = id
    genTyp' =
        oneof
            $ []
                <> always [Zero <$> genConst]
                <> genVar mode

genTagged :: Gen [a] -> Gen (TypV var) -> Gen [(a, TypV var)]
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

shrinkTyp :: TypV var -> [TypV var]
shrinkTyp = \case
    Zero _ -> []
    One _ typ -> typ : shrinkTyp typ
    Two _ typ1 typ2 -> typ1 : typ2 : shrinkTyp typ1 ++ shrinkTyp typ2
    ProductN fields ->
        map snd fields
            <> (ProductN <$> shrinkList shrinkNamed fields)
    SumN constructors ->
        map snd constructors
            <> (SumN <$> shrinkList shrinkNamed constructors)
    Var _ -> []
    Constrained v typ c ->
        [typ]
            <> [Constrained v typ' c | typ' <- shrinkTyp typ]
            <> [Constrained v typ c' | c' <- shrinkConstraint c]

shrinkNamed :: (t, TypV var) -> [(t, TypV var)]
shrinkNamed (f, t) = (f,) <$> shrinkTyp t

shrinkConstraint :: Constraint -> [Constraint]
shrinkConstraint = shrinkList shrinkConstraint1

shrinkConstraint1 :: Constraint1 -> [Constraint1]
shrinkConstraint1 = \case
    Braces c -> Braces <$> shrinkConstraint c
    Token xs -> Token <$> shrink xs
