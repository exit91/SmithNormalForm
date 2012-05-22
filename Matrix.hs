module Matrix where

import Prelude hiding (id,(.),(*),(+))
import Control.Category

import Absurd
import Algebra
import Data.List
import Lens
import Lens.List


data M a = M { unM :: [[a]], dim :: (Int,Int) }

mkMat :: (Int,Int) -> [[a]] -> M a
mkMat (x,y) xs
  | length xs == x && all ((==y) . length) xs = M xs (x,y)
  | otherwise = error "dimensional unfit"

mat_zero :: (Eq a, Plus a) => M a -> Bool
mat_zero (M xs _) = all (all (==zero)) xs

line_zero :: (Eq a, Plus a) => [a] -> Bool
line_zero = all (==zero)

instance Show a => Show (M a) where
    show (M xs _) = "[" ++ (intercalate ";\n " . map (intercalate "\t" . map show)) xs ++ "]"


-- lenses

repL :: Lens (M a) [[a]]
repL = Lens (\(M xs d) -> Store (\xs' -> M xs' d) xs)

rowL :: Int -> Lens (M a) [a]
rowL i = listL i . repL

colL :: Int -> Lens (M a) [a]
colL i = mapL (listL i) . repL


diagL :: Lens (M a) [a]
diagL = l . repL
    where
    l = Lens $ \xxs ->
        let 
          n = if null xxs then 0 else min (length xxs) (length (head xxs))
          (diag,rest) = extract n 0 [] [] xxs
        in  Store (\diag' -> weave 0 diag' rest) diag


    extract _ _ diag rest [] = (reverse diag, reverse rest)
    extract n i diag rest (xs:xxr) =
        if i >= n
           then (reverse diag, reverse rest ++ xxr)
           else let (as,b:bs) = splitAt i xs
                in  extract n (i+1) (b:diag) ((as++bs) : rest) xxr

    weave _ [] rest = rest
    weave i (x:xs) (xr:xxr) =
        let (as,bs) = splitAt i xr
        in  (as ++ x:bs) : weave (i+1) xs xxr
    weave _ _ _ = absurd

entryL :: Int -> Int -> Lens (M a) a
entryL i j = listL j . listL i . repL

-- | think of row / column
data RC = R | C

lineL :: RC -> Int -> Lens (M a) [a]
lineL R = rowL
lineL C = colL

colineL :: RC -> Int -> Lens (M a) [a]
colineL = lineL . toggle

toggle :: RC -> RC
toggle R = C
toggle C = R

-- matrix operations

mat_muladd :: (Mult a, Plus a) => RC -> Int -> Int -> a -> M a -> M a
mat_muladd rc i_from i_to a mat =
  let l = map (*a) $ get (lineL rc i_from) mat
  in  modify (lineL rc i_to) (zipWith (+) l) mat

mat_add :: Plus a => RC -> Int -> Int -> M a -> M a
mat_add rc i_from i_to mat =
  let l = get (lineL rc i_from) mat
  in  modify (lineL rc i_to) (zipWith (+) l) mat

mat_mul :: Mult a => RC -> Int -> a -> M a -> M a
mat_mul rc i a = modify (lineL rc i) (map (*a))

mat_swap :: RC -> Int -> Int -> M a -> M a
mat_swap rc i j = swapIntL (lineL rc i) (lineL rc j)
