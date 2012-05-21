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

showMat :: Show a => M a -> String
showMat (M xs _) = "[" ++ (intercalate ";\n " . map (intercalate "\t" . map show)) xs ++ "]"

printMat :: Show a => M a -> IO ()
printMat = putStrLn . showMat


-- | basic lensing

repL :: Lens (M a) [[a]]
repL = Lens (\m@(M xs d) -> Store (\xs' -> M xs' d) xs)

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


    extract n _ diag rest [] = (reverse diag, reverse rest)
    extract n i diag rest (xs:xxr) =
        if i >= n
           then (reverse diag, reverse rest ++ xxr)
           else let (as,b:bs) = splitAt i xs
                in  extract n (i+1) (b:diag) ((as++bs) : rest) xxr

    weave i [] rest = rest
    weave i (x:xs) (xr:xxr) =
        let (as,bs) = splitAt i xr
        in  (as ++ x:bs) : weave (i+1) xs xxr
    weave _ _ _ = absurd


-- | intermediate lensing

type Proj a = Int -> Lens (M a) [a]

gswap :: Proj a -> Int -> Int -> M a -> M a
gswap p i j = swapIntL (p i) (p j)

gmul :: Mult a => Proj a -> Int -> a -> M a -> M a
gmul p i a = modify (p i) (map (*a))

gadd :: Plus a => Proj a -> Int -> Int -> M a -> M a
gadd p i_from i_to m =
  let line = get (p i_from) m
  in  modify (p i_to) (\line' -> zipWith (+) line line') m

gmuladd :: (Plus a, Mult a) => Proj a -> Int -> Int -> a -> M a -> M a
gmuladd p i_from i_to a m =
  let line = map (*a) $ get (p i_from) m
  in  modify (p i_to) (\line' -> zipWith (+) line line') m

col_swap, row_swap :: Int -> Int -> M a -> M a
col_swap = gswap colL
row_swap = gswap rowL

col_mul, row_mul :: Mult a => Int -> a -> M a -> M a
col_mul = gmul colL
row_mul = gmul rowL

col_add, row_add :: Plus a => Int -> Int -> M a -> M a
col_add = gadd colL
row_add = gadd rowL

col_muladd, row_muladd :: (Mult a, Plus a) => Int -> Int -> a -> M a -> M a
col_muladd = gmuladd colL
row_muladd = gmuladd rowL


entry :: Int -> Int -> Lens (M a) a
entry i j = listL j . listL i . repL
