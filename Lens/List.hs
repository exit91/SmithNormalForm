module Lens.List where

import Lens

listL :: Int -> Lens [a] a
listL i = Lens $ \xs ->
  let (as,b:bs) = splitAt i xs
  in  Store (\b' -> as ++ b' : bs) b


mapL :: Lens a b -> Lens [a] [b]
mapL l = Lens $ \xs ->
  let stores = map (lens l) xs
  in  Store (zipWith (\s x' -> restorepoke x' s) stores) (map peek stores)
