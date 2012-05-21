import qualified Prelude as P
import Prelude (Integer, Int, error, Eq(..), (&&), otherwise, (.), Show(..), String, IO, putStrLn, undefined, Ord(..),flip,Bool(..), Ordering(..), (||),(&&), and, snd, ($), not)

import Data.List
import Data.Monoid
import Control.Monad


import Absurd
import Algebra
import Bound
import Lens
import Lens.List
import Matrix


a,b :: M Integer
a = mkMat (3,4) [[4,5,6,0],[7,8,10,3],[40,30,1,11]]
b = mkMat (3,4) [[3,6,9,0],[12,15,-3,12],[6,-6,18,-27]]

-- | main algorithm
smith :: (Eq a, Ord a, Abs a, DivRing a) => M a -> M a
smith m@(M _ (x,y))
  | isSmith m = m
  | otherwise = let r = min x y
                    smith' = (appEndo . mconcat) [Endo (step (r-i-1)) | i <- [0..r-1]]
                in smith' m


-- | a generic function which eliminates the i-th column or row
gelim :: (Eq a, DivRing a)
       => (Int -> Lens (M a) [a])
       -> (Int -> Lens (M a) [a])
       -> Int -> M a -> M a
gelim lineL coLineL i mat =
  let
    pivot_line = get (coLineL i) mat
    pivot = get (listL i) pivot_line

    elim_entries = drop (i+1) (zip [0..] (get (lineL i) mat))
    elim_function = appEndo . mconcat $
     do (j,x) <- elim_entries
        let (p,_) = quotRem x pivot
        return $ Endo $ modify (coLineL j) (\coLine' -> zipWith (+) coLine' (map (*(negate p)) pivot_line))
  in elim_function mat

row_elim :: (Eq a, DivRing a) => Int -> M a -> M a
row_elim = gelim rowL colL

col_elim :: (Eq a, DivRing a) => Int -> M a -> M a
col_elim = gelim colL rowL

positify :: (Mult a, Plus a, Neg a, Ord a) => Int -> M a -> M a
positify i mat =
  if get (entry i i) mat < zero 
     then modify (rowL i) (map (*(negate one))) mat
     else mat

-- | this function clears the i-th row and column, assuming all the 
--   preceding lines and columns have been cleaned up
--   
--   the Smith algorithm is just this function repeatedly (min(m,n) times) applied
--   on an mxn-dimensional matrix (from i=0 until i=min(m,n)-1)
step :: (DivRing a, Eq a, Ord a, Abs a) => Int -> M a -> M a
step i mat
  | not (row_clear i mat) = step i (row_step i mat)
  | not (col_clear i mat) = step i (col_step i mat)
  | (get (entry i i) mat) < zero = positify i mat
  | otherwise             = mat


-- generic iterative elimination step
gstep :: (DivRing a, Eq a, Ord a, Abs a)
      => (Int -> M a -> Bool)
      -> (Int -> M a -> M a)
      -> Int -> M a -> M a
gstep line_clear line_elim i mat =
  P.until (line_clear i) (\mat' ->
          let (x,y) = snd $ mat_find_minimum_nonzero i mat'
          in  line_elim i
            . positify i
            . col_swap i y
            . row_swap i x
            $ mat'
            ) mat

row_step :: (DivRing a, Eq a, Ord a, Abs a) => Int -> M a -> M a
row_step = gstep row_clear row_elim

row_clear :: (Plus a, Eq a) => Int -> M a -> Bool
row_clear i = line_zero . drop (i+1) . get (rowL i)

col_step :: (DivRing a, Eq a, Ord a, Abs a) => Int -> M a -> M a
col_step = gstep col_clear col_elim

col_clear :: (Plus a, Eq a) => Int -> M a -> Bool
col_clear i = line_zero . drop (i+1) . get (colL i)

-- | check for smithness
isSmith :: (Eq a, Plus a, Div a, Neg a) => M a -> Bool
isSmith mat@(M _ (m,n)) =
     m == 0
  || n == 0
  || mat_zero mat
  || ( and [get (entry i j) mat == zero || i == j | i <- [0..m-1], j <- [0..n-1]]
      && validDiag (get diagL mat))
  where
    validDiag (x:y:xs) = (y == zero || (x /= zero && snd (quotRem y x) == zero)) && validDiag (y:xs)
    validDiag _ = True


-- | find absolute minimum, which is nonzero
mat_find_minimum_nonzero :: (Ord a, Abs a, Plus a) => Int -> M a -> (a, (Int, Int))
mat_find_minimum_nonzero i mat =
  case (min_nonzero_in_line . map (min_nonzero_in_line . zeros_to_infty) . map (drop i) . drop i) (get repL mat) of
       Bound ((a,y),x) -> (a,(x+i,y+i))
       PositiveInfinity -> error "zero matrix"
       NegativeInfinity -> absurd

min_nonzero_in_line :: (Ord a, Abs a)  => [Bound a] -> Bound (a, Int)
min_nonzero_in_line = foldr min PositiveInfinity . map bseqL . flip zip [0..] . map abs

-- map zero to upper bound
zeros_to_infty :: (Ord a, Plus a) => [a] -> [Bound a]
zeros_to_infty = map (\x -> if x == zero then PositiveInfinity else Bound x)
