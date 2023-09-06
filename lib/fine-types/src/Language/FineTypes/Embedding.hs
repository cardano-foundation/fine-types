{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Language.FineTypes.Embedding where

import Prelude

import Control.Monad
    ((>=>))
import Language.FineTypes.Typ
    ( Typ )
import Language.FineTypes.Value

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Language.FineTypes.Typ as Typ

{-----------------------------------------------------------------------------
    Embedding
------------------------------------------------------------------------------}
-- | An 'Embedding' from a type A into a type B
-- is a many-to-one correspondence from the second type to the first.
--
-- > from . to = id
--
-- See Wadler's introduction to Agda
--  https://plfa.github.io/20.07/Isomorphism/#embedding
data Embedding a b = Embedding
    { to :: a -> b
    , from :: b -> a
    }

-- | An 'EmbeddingV' is a dynamically typed embedding of 'Value' into 'Value'.
--
-- Specifically, if @a `hasTyp` ta@ and @typecheck e ta == Just tb@,
-- then @to e a == b@ and @b `hasTyp tab@.
--
-- The result of @to e a@ can be 'undefined' if the value @a@ does
-- not have the expected 'Typ', that is @typecheck e ta == Nothing tb@.
data EmbeddingTyp = EmbeddingTyp
    { embed :: Embedding Value Value
        -- ^ Embedding of 'Value's from one 'Typ' into the other.
    , typecheck :: Typ -> Maybe Typ
        -- ^ Check whether the 'Embedding' works on the given 'Typ'.
    }

-- Design Question: Return the embedding as part of the type check?

{-----------------------------------------------------------------------------
    Functorial operations
------------------------------------------------------------------------------}
-- | Composition of embeddings. Right-to-left.
--
-- > to (embed (ebc <> eab)) = to (embed ebc) . to (embed eab)
instance Semigroup EmbeddingTyp where
    ebc <> eab = EmbeddingTyp
        { embed = Embedding
            { to = to (embed ebc) . to (embed eab)
            , from = from (embed eab) . from (embed ebc)
            }
        , typecheck = typecheck eab >=> typecheck ebc
        }

instance Monoid EmbeddingTyp where
    mempty = EmbeddingTyp
        { embed = Embedding { to = id, from = id }
        , typecheck = Just
        }

-- | Operate on the argument of an unary operation.
map1 :: EmbeddingTyp -> EmbeddingTyp
map1 ee = EmbeddingTyp
    { embed = Embedding
        { to = \a -> case a of
            One x -> One (fmap' (to e) x)
            _ -> error "Typ(e) error in: map1, to"
        , from = \b -> case b of
            One x -> One (fmap' (from e) x)
            _ -> error "Typ(e) error in: map1, from"
        }
    , typecheck = \t -> case t of
        Typ.One op a -> Typ.One op <$> typecheck ee a
        _ -> Nothing
    }
  where
    e = embed ee

    fmap' :: (Value -> Value) -> OneF Value -> OneF Value
    fmap' f (Option a) = Option (f <$> a)
    fmap' f (Sequence a) = Sequence (f <$> a)
    fmap' f (PowerSet a) = PowerSet (Set.map f a)


-- | Operate on the first argument of a 'Product' or 'Sum'.
first' :: EmbeddingTyp -> EmbeddingTyp
first' ee = EmbeddingTyp
    { embed = Embedding
        { to = \a -> case a of
            Product [x,y] -> Product [to e x, y]
            Sum 0 x -> Sum 0 $ to e x
            Sum 1 _ -> a
            _ -> error "Typ(e) error in: first', to"
        , from = \b -> case b of
            Product [x2,y2] -> Product [from e x2, y2]
            Sum 0 x2 -> Sum 0 $ from e x2
            Sum 1 _ -> b
            _ -> error "Typ(e) error in: first', from"
        }
    , typecheck = \t -> case t of
        Typ.Two fun a0 b
            | fun == Typ.Sum2 || fun == Typ.Product2
                -> (\a1 -> Typ.Two fun a1 b) <$> typecheck ee a0
        _ -> Nothing
    }
  where
    e = embed ee

-- | Operate on the first argument of a 'Product' or 'Sum'.
second' :: EmbeddingTyp -> EmbeddingTyp
second' ee = EmbeddingTyp
    { embed = Embedding
        { to = \a -> case a of
            Product [x,y] -> Product [x, to e y]
            Sum 0 _ -> a
            Sum 1 y -> Sum 1 $ to e y
            _ -> error "Typ(e) error in: second', to"
        , from = \b -> case b of
            Product [x2, y2] -> Product [x2, from e y2]
            Sum 0 _ -> b
            Sum 1 y2 -> Sum 1 $ from e y2
            _ -> error "Typ(e) error in: second', from"
        }
    , typecheck = \t -> case t of
        Typ.Two fun a b0
            | fun == Typ.Sum2 || fun == Typ.Product2
                -> Typ.Two fun a <$> typecheck ee b0
        _ -> Nothing
    }
  where
    e = embed ee

-- | Associative law for products.
--
-- > (A × B) × C  =>  A × (B × C)
assocR :: EmbeddingTyp
assocR = EmbeddingTyp
    { embed = Embedding
        { to = \t -> case t of
            Product [Product [a,b],c] -> Product [a, Product [b,c]]
            _ -> error "Typ(e) error in: assocR, to"
        , from = \t -> case t of
            Product [a, Product [b,c]] -> Product [Product [a,b],c]
            _ -> error "Typ(e) error in: assocR, from"
        }
    , typecheck = \t -> case t of
        Typ.Two Typ.Product2 (Typ.Two Typ.Product2 a b) c
            -> Just $ Typ.Two Typ.Product2 a (Typ.Two Typ.Product2 b c)
        _ -> Nothing
    }

{-----------------------------------------------------------------------------
    Basic algebra
------------------------------------------------------------------------------}
-- | Unit law for a monoid.
--
-- > () ↦0 A  =>  A
unit0 :: Value -> EmbeddingTyp
unit0 zero = EmbeddingTyp
    { embed = Embedding
        { to = \a -> case a of
            Two (FiniteMap m)
                -> Map.findWithDefault zero (Zero Unit) m
            _ -> error "Typ(e) error in: unit0, to"
        , from =
            Two . FiniteMap . Map.singleton (Zero Unit)
        }
    , typecheck = \t -> case t of
        Typ.Two Typ.FiniteSupport (Typ.Zero Typ.Unit) s -> Just s
        _ -> Nothing
    }

-- | Exponential law(s)
-- 
-- > (A ⊎ B) ↦0 C  =>  (A ↦0 C) × (B ↦0 C)
-- > (A ⊎ B) ↦  C  =>  (A ↦ C)  × (B ↦ C)
exponential :: EmbeddingTyp
exponential = EmbeddingTyp
    { embed = Embedding
        { to = \a -> case a of
            Two (FiniteMap m) ->
                Product
                    [ Two $ FiniteMap (left m)
                    , Two $ FiniteMap (right m)
                    ]
            _ -> error "Typ(e) error in: exponential, to"
        , from = \b -> case b of
            Product
                [ Two (FiniteMap ml)
                , Two (FiniteMap mr)
                ]
                -> Two $ FiniteMap (plus ml mr)
            _ -> error "Typ(e) error in: exponential, from"
        }
    , typecheck = \t -> case t of
        Typ.Two fun (Typ.Two Typ.Sum2 a b) c
            | fun == Typ.FiniteSupport || fun == Typ.PartialFunction
            -> Just $ Typ.Two Typ.Product2 (Typ.Two fun a c) (Typ.Two fun b c)
        _ -> Nothing
    }
  where
    plus ml mr
        = Map.mapKeys (Sum 0) ml <> Map.mapKeys (Sum 1) mr

    left = withKeys matchLeft
    right = withKeys matchRight

    withKeys f
        = Map.mapKeys fromSum
        . Map.mapMaybeWithKey f

    fromSum (Sum _ x) = x
    fromSum _ = error "exponential: expected Sum"

    matchLeft (Sum 0 _) v = Just v
    matchLeft _ _ = Nothing

    matchRight (Sum 1 _) v = Just v
    matchRight _ _ = Nothing

{-----------------------------------------------------------------------------
    Conversions
------------------------------------------------------------------------------}
-- | Representation of finite maps as sequences of pairs.
--
-- > A ↦ B   =>  (A × B)*
-- > A ↦0 B   =>  (A × B)*
representMap :: EmbeddingTyp
representMap = EmbeddingTyp
    { embed = Embedding
        { to = \a -> case a of
            Two (FiniteMap m) -> valueFromList (Map.toList m)
            _ -> error "Typ(e) error in: representMap, to"
        , from = \b -> case b of
            One (Sequence xs) ->
                Two $ FiniteMap
                    $ Map.fromList [ (x,y) | Product [x,y] <- xs ]
            _ -> error "Typ(e) error in: representMap, from"
        }
    , typecheck = \t -> case t of
        Typ.Two Typ.PartialFunction a b
            -> Just $ Typ.One Typ.Sequence (Typ.Two Typ.Product2 a b)
        Typ.Two Typ.FiniteSupport a b
            -> Just $ Typ.One Typ.Sequence (Typ.Two Typ.Product2 a b)
        _ -> Nothing
    }
  where
    valueFromList = One . Sequence . map (\(a,b) -> Product [a,b])
