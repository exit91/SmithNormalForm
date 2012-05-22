module Algebra where

import qualified Prelude as P
import Prelude (Integer, Int, Maybe(..))

import Natural
import Tuple

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

class (Plus a, Mult a, Div a, Neg a, Abs a) => DivRing a

class Abs t where
    abs :: t -> Maybe Natural
    -- law
    -- abs 0 = Nothing


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
    abs 0 = Nothing
    abs x = natural (P.abs x)


-- | Int

instance Plus Int where
    zero = 0
    x + y = x P.+ y

instance Neg Int where
    negate = P.negate

instance Abs Int where
    abs 0 = Nothing
    abs x = (natural P.. P.fromIntegral P.. P.abs) x


-- | Natural

instance Plus Natural where
    zero = natural_exn 0
    Natural x + Natural y = Natural (x+y)


instance Mult Natural where
    one = natural_exn 1
    Natural x * Natural y = Natural (x*y)

instance Abs Natural where
    abs (Natural 0) = Nothing
    abs n           = Just n

instance Div Natural where
    quotRem (Natural x) (Natural y) = (Natural *** Natural) (quotRem x y)
    divMod  (Natural x) (Natural y) = (Natural *** Natural) (divMod  x y)

