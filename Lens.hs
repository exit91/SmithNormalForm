module Lens where

import Control.Category
import Prelude hiding (id,(.))

data Store s a = Store (a -> s) a

smap :: (a -> a) -> Store s a -> Store s a
smap g (Store h x) = Store h (g x)

peek :: Store s a -> a
peek (Store _ x) = x

poke :: a -> Store s a -> Store s a
poke x (Store h _) = Store h x

restore :: Store s a -> s
restore (Store h x) = h x

restorepoke :: a -> Store s a -> s
restorepoke x = restore . poke x


newtype Lens s a = Lens { lens :: s -> Store s a }

get :: Lens s a -> s -> a
get l = peek . lens l

set :: Lens s a -> a -> s -> s
set l x = restore . poke x . lens l

modify :: Lens s a -> (a -> a) -> (s -> s)
modify l g = restore . smap g . lens l

replace :: Lens s a -> a -> (s -> s)
replace l a = modify l (const a)


instance Category Lens where
    id = Lens (\s -> Store id s)
    Lens g . Lens f = Lens $ \s ->
      let w1 = f s
          w2 = g (peek w1)
          x  = peek w2
      in  Store (\x' -> restorepoke (restorepoke x' w2) w1) x


-- | external swap
swapExtL :: Lens s a -> (s,s) -> (s,s)
swapExtL l (s1,s2) =
  let w1 = lens l s1
      w2 = lens l s2
  in  (restorepoke (peek w2) w1, restorepoke (peek w1) w2)

-- | internal swap
swapIntL :: Lens s a -> Lens s a -> s -> s
swapIntL l1 l2 s =
  let x1 = get l1 s
      x2 = get l2 s
  in  (set l2 x1 . set l1 x2) s
