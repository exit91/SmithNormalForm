module Algebra where

import qualified Prelude as P
import Prelude (Integer, Int)

class Plus t where
    zero :: t
    (+) :: t -> t -> t

class Mult t where
    one :: t
    (*) :: t -> t -> t

class Div t where
    -- remainder > 0
    quotRem :: t -> t -> (t,t)
    -- quotient > 0
    divMod  :: t -> t -> (t,t)

class Neg t where
    negate :: t -> t

(-) :: (Neg t, Plus t) => t -> t -> t
a - b = a + negate b

class (Plus a, Mult a, Div a, Neg a) => DivRing a where

class Abs t where
    abs :: t -> t


-- | Integer

instance Plus Integer where
    zero = 0
    x + y = x P.+ y

instance Mult Integer where
    one = 1
    x * y = x P.* y

instance Div Integer where
    quotRem x y = P.quotRem x y
    divMod  x y = P.divMod x y

instance Neg Integer where
    negate x = P.negate x

instance DivRing Integer

instance Abs Integer where
    abs = P.abs


-- | Int

instance Plus Int where
    zero = 0
    x + y = x P.+ y

instance Neg Int where
    negate = P.negate

instance Abs Int where
    abs = P.abs


-- | Tuple

instance (Abs a, Abs b) => Abs (a,b) where
    abs (a,b) = (abs a, abs b)
