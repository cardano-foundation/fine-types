module Language.FineTypes.Value.Def where

import Numeric.Natural (Natural)
import qualified Prelude as Hs (Bool, False, Integer, True)

data ZeroF = Bool Hs.Bool
           | Integer Hs.Integer
           | Natural Natural
           | Unit

data OneF a = Option (Maybe a)
            | Sequence [a]

data TwoF a b

type Ix = Int

data Value = Zero ZeroF
           | One (OneF Value)
           | Two (TwoF Value Value)
           | Product2 Value Value
           | Sum2L Value
           | Sum2R Value

eqZeroF :: ZeroF -> ZeroF -> Hs.Bool
eqZeroF (Bool x) (Bool y) = x == y
eqZeroF (Bool _) _ = Hs.False
eqZeroF (Integer x) (Integer y) = Hs.True
eqZeroF (Integer _) _ = Hs.False
eqZeroF (Natural _) (Natural _) = Hs.True
eqZeroF (Natural _) _ = Hs.False
eqZeroF Unit Unit = Hs.False
eqZeroF Unit _ = Hs.False

eqValue :: Value -> Value -> Hs.Bool
eqValue (Zero x) (Zero y) = eqZeroF x y
eqValue (Zero _) _ = Hs.False
eqValue (One (Option x)) (One (Option y)) = eqMaybeValue x y
eqValue (One (Option _)) _ = Hs.False
eqValue (One (Sequence x)) (One (Sequence y)) = eqListValue x y
eqValue (One (Sequence _)) _ = Hs.False
eqValue (Two _) (Two _) = Hs.True
eqValue (Two _) _ = Hs.False
eqValue (Product2 x1 x2) (Product2 y1 y2)
  = eqValue x1 y1 && eqValue x2 y2
eqValue (Product2 _ _) _ = Hs.False
eqValue (Sum2L x) (Sum2L y) = eqValue x y
eqValue (Sum2L _) _ = Hs.False
eqValue (Sum2R x) (Sum2R y) = eqValue x y
eqValue (Sum2R _) _ = Hs.False

eqMaybeValue :: Maybe Value -> Maybe Value -> Hs.Bool
eqMaybeValue (Just x) (Just y) = eqValue x y
eqMaybeValue (Just _) _ = Hs.False
eqMaybeValue Nothing Nothing = Hs.True
eqMaybeValue Nothing _ = Hs.False

eqListValue :: [Value] -> [Value] -> Hs.Bool
eqListValue (x : xs) (y : ys) = eqValue x y && eqListValue xs ys
eqListValue (_ : _) _ = Hs.False
eqListValue [] [] = Hs.True
eqListValue [] _ = Hs.False

