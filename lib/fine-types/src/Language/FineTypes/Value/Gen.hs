{-# LANGUAGE TypeApplications #-}

-- | Generate random 'Value's of a given 'Typ'.
module Language.FineTypes.Value.Gen
    ( genTypValue
    , genTypValue'
    )
where

import Prelude

import Control.Monad (replicateM)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Language.FineTypes.Typ (Typ)
import Language.FineTypes.Value
    ( OneF (..)
    , TwoF (..)
    , Value (..)
    , ZeroF (..)
    )
import Test.QuickCheck
    ( Arbitrary (arbitrary)
    , Gen
    , Positive (Positive)
    , choose
    , getSize
    , listOf
    , oneof
    )

import qualified Data.ByteString as B
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Language.FineTypes.Typ as Typ

-- | Generate a random 'Text'.
genText :: Gen Text
genText = T.pack <$> listOf arbitrary

-- | Generate a random 'ByteString'.
genBytes :: Gen ByteString
genBytes = B.pack <$> listOf arbitrary

-- | Generate a random 'Value' of the given 'Typ' and fail if it is not possible.
exceptGenValue :: Typ -> ExceptT Typ Gen Value
exceptGenValue = ExceptT . genTypValue

-- | Generate a random list of the given length under a monad transformer.
listOfT :: (Monad (t Gen), MonadTrans t) => t Gen a -> t Gen [a]
listOfT f = do
    l <- lift getSize
    replicateM l f

-- | Generate a random 'Value' of the given 'Typ' or report the first 'Typ' that
-- cannot be generated down the tree.
genTypValue :: Typ -> Gen (Either Typ Value)
genTypValue typ =
    case typ of
        Typ.Zero typ' ->
            pure . Zero <$> case typ' of
                Typ.Bool -> Bool <$> arbitrary
                Typ.Bytes -> Bytes <$> genBytes
                Typ.Integer -> Integer <$> arbitrary
                Typ.Natural -> do
                    Positive n <- arbitrary
                    pure $ Natural $ fromIntegral @Int n
                Typ.Text -> Text <$> genText
                Typ.Unit -> pure Unit
        Typ.One op typ' -> case op of
            Typ.Option -> runExceptT $ do
                v <- exceptGenValue typ'
                lift
                    $ One . Option
                        <$> oneof
                            [ pure Nothing
                            , pure $ Just v
                            ]
            Typ.Sequence -> runExceptT $ do
                One . Sequence <$> listOfT (exceptGenValue typ')
            Typ.PowerSet -> runExceptT $ do
                One . PowerSet . Set.fromList <$> listOfT (exceptGenValue typ')
        Typ.Two op typ1 typ2 -> case op of
            Typ.Sum2 -> runExceptT $ do
                ix <- lift $ choose (0, 1)
                let typ' = [typ1, typ2] !! ix
                Sum ix <$> exceptGenValue typ'
            Typ.Product2 -> runExceptT $ do
                Product
                    <$> do
                        x <- exceptGenValue typ1
                        y <- exceptGenValue typ2
                        pure [x, y]
            Typ.PartialFunction -> runExceptT $ do
                Two . FiniteMap . Map.fromList
                    <$> listOfT ((,) <$> exceptGenValue typ1 <*> exceptGenValue typ2)
            Typ.FiniteSupport ->
                genTypValue
                    $ Typ.Two Typ.PartialFunction typ1 typ2
        Typ.ProductN fields -> runExceptT $ do
            fmap Product
                $ sequence
                $ do
                    (_fn, typ') <- fields
                    pure $ exceptGenValue typ'
        Typ.SumN constructors -> runExceptT $ do
            ix <- lift $ choose (0, length constructors - 1)
            let (_cn, typ') = constructors !! ix
            Sum ix <$> exceptGenValue typ'
        typ' -> pure $ Left typ'

-- | Generate a random 'Value' of the given 'Typ' and fail if it is not possible.
genTypValue' :: Typ -> Gen Value
genTypValue' typ = do
    r <- genTypValue typ
    case r of
        Left typ' -> error $ "typeValueGenE: " <> show typ'
        Right v -> pure v
