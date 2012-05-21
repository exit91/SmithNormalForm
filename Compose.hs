module Compose where

compose :: [a -> a] -> (a -> a)
compose = foldr (flip (.)) id
