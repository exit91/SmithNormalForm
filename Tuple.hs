module Tuple where

(***) :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
(***) f g (x,y) = (f x, g y)
