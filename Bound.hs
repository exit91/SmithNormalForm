module Bound where

import Prelude hiding (abs)

data Bound a = NegativeInfinity | PositiveInfinity | Bound a

instance Eq a => Eq (Bound a) where
    NegativeInfinity == NegativeInfinity = True
    PositiveInfinity == PositiveInfinity = True
    Bound a == Bound b = a == b
    _ == _ = False

instance Ord a => Ord (Bound a) where
    compare PositiveInfinity PositiveInfinity = EQ
    compare NegativeInfinity NegativeInfinity = EQ
    compare PositiveInfinity _ = GT
    compare NegativeInfinity _ = LT
    compare _ PositiveInfinity = LT
    compare _ NegativeInfinity = GT
    compare (Bound a) (Bound b) = compare a b

bseqL :: (Bound a, b) -> Bound (a,b)
bseqL (Bound a, b) = Bound (a,b)
bseqL (PositiveInfinity, _) = PositiveInfinity
bseqL (NegativeInfinity, _) = NegativeInfinity


instance Show a => Show (Bound a) where
    show PositiveInfinity = "infty"
    show NegativeInfinity = "-infty"
    show (Bound a)   = show a
