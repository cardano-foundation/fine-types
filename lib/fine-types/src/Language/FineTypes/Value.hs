-- | 'Value' represents the values that can inhabit 'Typ'.
module Language.FineTypes.Value
    ( -- * 'Value' data type.
      Value (..)
    , Ix
    , ZeroF (..)
    , OneF (..)
    , TwoF (..)

      -- * Type checking
    , hasTyp
    , hasTyp1
    , hasTyp2
    ) where

import Prelude

import Data.ByteString
    ( ByteString
    )
import Data.Text
    ( Text
    )
import Language.FineTypes.Typ
    ( Typ
    )
import Numeric.Natural
    ( Natural
    )

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Language.FineTypes.Typ as Typ

{-----------------------------------------------------------------------------
    Values corresponding to Typ
------------------------------------------------------------------------------}
type Ix = Int

data Value
    = Zero ZeroF
    | One (OneF Value)
    | Two (TwoF Value Value)
    | -- | N-ary products.
      Product [Value]
    | -- | N-ary sums.
      Sum Ix Value
    deriving (Eq, Ord, Show)

-- | Values for predefined 'Typ's.
data ZeroF
    = Bool Bool
    | Bytes ByteString
    | Integer Integer
    | Natural Natural
    | Text Text
    | Unit
    | Rational Rational
    deriving (Eq, Ord, Show)

-- | Values for unary operations on 'Typ'.
data OneF a
    = Option (Maybe a)
    | Sequence [a]
    | PowerSet (Set.Set a)
    deriving (Eq, Ord, Show)

-- | Values for binary operations on 'Typ'.
newtype TwoF a b
    = FiniteMap (Map.Map a b)
    deriving (Eq, Ord, Show)

{-----------------------------------------------------------------------------
    Type checking
------------------------------------------------------------------------------}

-- | Check whether a 'Value' inhabits the given 'Typ'.
hasTyp :: Value -> Typ -> Bool
hasTyp (Zero a) (Typ.Zero b) =
    typOf0 a == b
hasTyp (Zero _) _ =
    False
hasTyp (One a) (Typ.One op typ) =
    hasTyp1 a op typ
hasTyp (One _) _ =
    False
hasTyp (Product [a, b]) (Typ.Two Typ.Product2 ta tb) =
    (a `hasTyp` ta) && (b `hasTyp` tb)
hasTyp (Product as) (Typ.ProductN fields)
    | length as == length fields =
        and (zipWith hasTyp as $ map snd fields)
hasTyp (Product _) _ =
    False
hasTyp (Sum 0 a) (Typ.Two Typ.Sum2 ta _) =
    a `hasTyp` ta
hasTyp (Sum 1 b) (Typ.Two Typ.Sum2 _ tb) =
    b `hasTyp` tb
hasTyp (Sum ix a) (Typ.SumN constructors)
    | 0 <= ix && ix < length constructors =
        a `hasTyp` snd (constructors !! ix)
hasTyp (Sum _ _) _ =
    False
hasTyp (Two a) (Typ.Two op typ1 typ2) =
    hasTyp2 a op typ1 typ2
hasTyp (Two _) _ =
    False

typOf0 :: ZeroF -> Typ.TypConst
typOf0 a = case a of
    Bool{} -> Typ.Bool
    Bytes{} -> Typ.Bytes
    Integer{} -> Typ.Integer
    Natural{} -> Typ.Natural
    Text{} -> Typ.Text
    Unit -> Typ.Unit
    Rational{} -> Typ.Rational

hasTyp1 :: OneF Value -> Typ.OpOne -> Typ -> Bool
hasTyp1 (Option a) Typ.Option t =
    all (`hasTyp` t) a
hasTyp1 (Option _) _ _ =
    False
hasTyp1 (Sequence as) Typ.Sequence t =
    all (`hasTyp` t) as
hasTyp1 (Sequence _) _ _ =
    False
hasTyp1 (PowerSet as) Typ.PowerSet t =
    all (`hasTyp` t) as
hasTyp1 (PowerSet _) _ _ =
    False

hasTyp2 :: TwoF Value Value -> Typ.OpTwo -> Typ -> Typ -> Bool
hasTyp2 (FiniteMap m) Typ.PartialFunction ta tb =
    all (`hasTyp` ta) (Map.keys m)
        && all (`hasTyp` tb) (Map.elems m)
hasTyp2 (FiniteMap m) Typ.FiniteSupport ta tb =
    hasTyp2 (FiniteMap m) Typ.PartialFunction ta tb
hasTyp2 (FiniteMap _) _ _ _ =
    False
