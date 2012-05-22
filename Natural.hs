module Natural where

newtype Natural = Natural Integer


natural :: Integer -> Maybe Natural
natural i | i <= 0    = Nothing
          | otherwise = Just (Natural i)


natural_exn :: Integer -> Natural
natural_exn i | i <= 0    = error "negative natural"
              | otherwise = Natural i


instance Eq Natural where
    Natural x == Natural y = x == y

instance Ord Natural where
    Natural x `compare` Natural y = compare x y

instance Show Natural where
    show (Natural x) = show x
