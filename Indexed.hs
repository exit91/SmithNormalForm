module Indexed where

import Data.List (unfoldr)

-- | an integer indexed map
ixmap :: Integral i => (i -> a -> b) -> [a] -> [b]
{- ixmap fi = map (uncurry fi) . zip [0..]  (where zip is defined as an unfoldr)-}
ixmap fi xs = unfoldr gen seed
    where
    seed = (0, xs)
    gen (_,[]) = Nothing
    gen (n,x:xr) = Just (fi n x, (n+1,xr))


enumerate :: Integral i => [a] -> [(i,a)]
enumerate
  {- = zip [0..] -}
  {- = ixmap (,) -}
     = ixmap (\i val -> (i,val))


enumerate2d :: Integral i => [[a]] -> [[((i,i),a)]]
enumerate2d = ixmap (\r ->
              ixmap (\c ->
                \val -> ((r,c),val)))


enumerate3d :: Integral i => [[[a]]] -> [[[((i,i,i),a)]]]
enumerate3d = ixmap (\x ->
              ixmap (\y ->
              ixmap (\z ->
                \element -> ((x,y,z),element))))
