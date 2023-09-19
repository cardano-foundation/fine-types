module Language.FineTypes.Typ.Rewrite.Maps where

import Prelude

import Language.FineTypes.Typ
    ( OpOne (Sequence)
    , OpTwo (..)
    , Typ (..)
    )

rewriteMapsOnTwo :: OpTwo -> Typ -> Typ -> Typ
rewriteMapsOnTwo PartialFunction t1 t2 =
    One
        Sequence
        (Two Product2 t1 t2)
rewriteMapsOnTwo FiniteSupport t1 t2 =
    One
        Sequence
        (Two Product2 t1 t2)
rewriteMapsOnTwo op t1 t2 = Two op t1 t2

rewriteMapsAsTuples :: Typ -> Typ
rewriteMapsAsTuples (One op t) =
    One op (rewriteMapsAsTuples t)
rewriteMapsAsTuples (Two op t1 t2) =
    rewriteMapsOnTwo op (rewriteMapsAsTuples t1) (rewriteMapsAsTuples t2)
rewriteMapsAsTuples (ProductN ts) =
    ProductN (fmap rewriteMapsAsTuples <$> ts)
rewriteMapsAsTuples (SumN ts) =
    SumN (fmap rewriteMapsAsTuples <$> ts)
rewriteMapsAsTuples t = t
