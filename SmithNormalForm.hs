import qualified Prelude as P
import Prelude (Integer, Int, error, Eq(..), (&&), otherwise, (.), Show(..), String, IO, putStrLn, undefined, Ord(..),flip,Bool(..), Ordering(..), (||),(&&), and, snd, ($), not, id)

import Data.List
import Data.Monoid
import Control.Monad


import Absurd
import Algebra
import Bound
import Compose
import Lens
import Lens.List
import Matrix


a,b :: M Integer
a = mkMat (3,4) [[4,5,6,0],[7,8,10,3],[40,30,1,11]]
b = mkMat (3,4) [[3,6,9,0],[12,15,-3,12],[6,-6,18,-27]]


-- | The Algorithm
--   
--   If the input matrix is already is Smith normal form, nothing needs to be done.
--   Otherwise the result matrix is recursively calculated, by calculating the first
--   row and column and then solving the smaller problem for the (1,1)-minor
--
--   For an (MxN)-matrix this takes min(M,N) recursive steps.
--
smith :: (Eq a, Ord a, Abs a, DivRing a) => M a -> M a
smith m@(M _ (x,y))
  | isSmith m = m
  | otherwise = compose (map step [0 .. min x y - 1]) m


-- | Recursive Step function
--
--   this function clears the i-th row and column, assuming all the 
--   preceding lines and columns have been cleaned up
--   
step :: (DivRing a, Eq a, Ord a, Abs a) => Int -> M a -> M a
step i mat
  | not (clear R i mat) = step i (elim R i mat)
  | not (clear C i mat) = step i (elim C i mat)
  | (get (entryL i i) mat) < zero = positify i mat
  | otherwise           = mat

  where
  clear rc i = line_zero . drop (i+1) . get ((lineL rc) i)

  elim rc i = P.until (clear rc i) $ \mat' ->
                let (x,y) = snd $ mat_find_minimum_nonzero i mat'
                in  (elim_step rc i . positify i . mat_swap C i y . mat_swap R i x) mat'

-- | Elimination algorithm
--
--   a generic function which eliminates the i-th column or row
--
elim_step :: (Eq a, DivRing a)
       => RC -> Int -> M a -> M a
elim_step rc i mat =
  let
    pivot = get (entryL i i) mat
    elim_entries = drop (i+1) (zip [0..] (get (lineL rc i) mat))

    elim_function = compose $
     do (j,x) <- elim_entries
        let (p,_) = quotRem x pivot
        return $ mat_muladd (toggle rc) i j (negate p)
  in elim_function mat


-- | If the (i,i) element is negative multiply the i-th line by -1
--
positify :: (Mult a, Plus a, Neg a, Ord a) => Int -> M a -> M a
positify i mat =
  if get (entryL i i) mat < zero 
     then modify (rowL i) (map (*(negate one))) mat
     else mat

-- | check for Smithness
--
isSmith :: (Eq a, Plus a, Div a, Neg a) => M a -> Bool
isSmith mat@(M _ (m,n)) =
     m == 0
  || n == 0
  || mat_zero mat
  || ( and [get (entryL i j) mat == zero || i == j | i <- [0..m-1], j <- [0..n-1]]
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

  where
  min_nonzero_in_line :: (Ord a, Abs a)  => [Bound a] -> Bound (a, Int)
  min_nonzero_in_line = foldr min PositiveInfinity . map bseqL . flip zip [0..] . map abs

  -- map zero to upper bound
  zeros_to_infty :: (Ord a, Plus a) => [a] -> [Bound a]
  zeros_to_infty = map (\x -> if x == zero then PositiveInfinity else Bound x)
