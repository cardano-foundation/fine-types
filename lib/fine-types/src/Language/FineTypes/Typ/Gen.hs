{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Language.FineTypes.Typ.Gen where

import Prelude hiding (round)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (MonadWriter (tell))
import Data.Char
    ( isAlphaNum
    , isPunctuation
    , isSpace
    , isSymbol
    , toUpper
    )
import Data.Foldable (forM_)
import Data.List (isInfixOf, nub)
import Data.Traversable (forM)
import Language.FineTypes.Module
    ( Documentation (..)
    , Identifier (..)
    , Place (..)
    )
import Language.FineTypes.Typ
    ( Constraint
    , Constraint1 (..)
    , ConstructorName
    , FieldName
    , OpOne (..)
    , OpTwo (..)
    , Typ (..)
    , TypConst (..)
    , TypName
    )
import QuickCheck.GenT
    ( GenT
    , arbitrary'
    , choose
    , elements
    , frequency
    , listOf1
    , oneof
    , scale
    , shrink
    , suchThat
    , vectorOf
    )
import Test.QuickCheck (shrinkList)

import qualified Data.Map as Map

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

willBranch :: Monad m => DepthGen -> GenT m Branch
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
    :: (MonadWriter Documentation m, ?typname :: TypName)
    => (Typ -> Bool)
    -- ^ Whether the generated 'Typ' should be filtered out
    -> WithConstraints
    -- ^ Whether the generated 'Typ' can have constraints or not
    -> Mode
    -- ^ Whether the generated 'Typ' should be concrete or not
    -> DepthGen
    -- ^ Maximum depth of the generated 'Typ'
    -> GenT m Typ
genTyp out = go Top
  where
    go round hasC mode depth = do
        branching <- onWill <$> willBranch depth
        let top = onTop round
            complete = onComplete mode
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
                    <> (top . complete) [pure Abstract]
                    <> complete [Var <$> genVarName]
                    <> constrained [genConstrainedTyp mode]
        oneof expansion `suchThat` (not . out)
      where
        go' = go Rest hasC mode (depth - 1)

genConstraint :: Monad m => Int -> GenT m Constraint
genConstraint 0 = pure []
genConstraint n = listOf1 $ genConstraint1 $ n - 1

genConstraint1 :: Monad m => Int -> GenT m Constraint1
genConstraint1 n =
    oneof
        $ [Braces <$> genConstraint n | n > 0]
            <> [ Token
                    <$> listOf1 (arbitrary' `suchThat` goodChar)
                        `suchThat` goodToken
               ]

goodToken :: String -> Bool
goodToken = not . ("--" `isInfixOf`)

goodChar :: Char -> Bool
goodChar x =
    (isSymbol x || isAlphaNum x || isPunctuation x) && x `notElem` " {}"

genConstrainedTyp :: Monad m => Mode -> GenT m Typ
genConstrainedTyp mode = do
    constraintDepth <- choose (1, 2)
    c <- genConstraint constraintDepth
    typ <- genTyp'
    pure $ Constrained typ c
  where
    complete = onComplete mode
    always = id
    genTyp' =
        oneof
            $ []
                <> always [Zero <$> genConst]
                <> complete [Var <$> genVarName]

genTagged :: Monad m => GenT m [a] -> GenT m Typ -> GenT m [(a, Typ)]
genTagged gen f = do
    names <- gen
    forM names $ \name -> (,) name <$> f

genTwoOpen :: Monad m => GenT m OpTwo
genTwoOpen = elements [Sum2, Product2]

genTwoClose :: Monad m => GenT m OpTwo
genTwoClose = elements [PartialFunction, FiniteSupport]

genOne :: Monad m => GenT m OpOne
genOne = elements [Option, Sequence, PowerSet]

genConst :: Monad m => GenT m TypConst
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

genNames :: Monad m => GenT m [String]
genNames =
    fmap nub
        $ logScale 2
        $ listOf1
            genName

genName :: Monad m => GenT m [Char]
genName =
    vectorOf 4
        $ elements ['a' .. 'z']

genConstructors
    :: (?typname :: TypName, MonadWriter Documentation m)
    => GenT m [ConstructorName]
genConstructors = do
    ns <- genNames
    forM_ ns $ \n -> genDocumentation (Constructor ?typname n)
    pure ns

genVarName :: Monad m => GenT m TypName
genVarName = (capitalise <$> genName) `suchThat` notPrivate

capitalise :: [Char] -> [Char]
capitalise = \case
    [] -> []
    (x : xs) -> toUpper x : xs

genFields
    :: (MonadWriter Documentation m, ?typname :: TypName)
    => GenT m [FieldName]
genFields = do
    ns <- genNames
    forM_ ns $ \n -> genDocumentation (Field ?typname n)
    pure ns

logScale :: Monad m => Double -> GenT m a -> GenT m a
logScale n = scale logN
  where
    logN x = floor $ logBase n $ 1 + fromIntegral x

shrinkTyp :: Typ -> [Typ]
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
    Abstract -> []
    Constrained typ c ->
        [typ]
            <> [Constrained typ' c | typ' <- shrinkTyp typ]
            <> [Constrained typ c' | c' <- shrinkConstraint c]

shrinkNamed :: (t, Typ) -> [(t, Typ)]
shrinkNamed (f, t) = (f,) <$> shrinkTyp t

shrinkConstraint :: Constraint -> [Constraint]
shrinkConstraint = shrinkList shrinkConstraint1

shrinkConstraint1 :: Constraint1 -> [Constraint1]
shrinkConstraint1 = \case
    Braces c -> Braces <$> shrinkConstraint c
    Token xs -> Token <$> shrink xs

genDocumentation :: MonadWriter Documentation m => Identifier -> GenT m ()
genDocumentation i = do
    places <- elements placesChoices
    forM_ places $ \place -> do
        docs <-
            fmap (take 3)
                $ listOf1 (arbitrary' `suchThat` allowedChars)
                    `suchThat` noHeadingSpace
                    `suchThat` correctInside place
        lift $ tell $ Documentation $ Map.singleton i $ Map.singleton place docs

placesChoices :: [[Place]]
placesChoices =
    [ []
    , [After]
    , [Before]
    , [BeforeMultiline]
    , [Before, After]
    , [BeforeMultiline, After]
    ]

allowedChars :: Char -> Bool
allowedChars x = isSpace x || isAlphaNum x || isPunctuation x || isSymbol x

noHeadingSpace :: String -> Bool
noHeadingSpace = \case
    [] -> True
    (x : _) -> not $ isSpace x

correctInside :: Place -> [Char] -> Bool
correctInside = \case
    BeforeMultiline -> not . ("-}" `isInfixOf`)
    _ -> not . ('\n' `elem`)
