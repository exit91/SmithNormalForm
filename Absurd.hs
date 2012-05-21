module Absurd where

absurd :: a
absurd = error "absurd"

absurd_val :: a -> a
absurd_val x = x
