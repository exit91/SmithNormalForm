import qualified Prelude as P
import Prelude (Integer, Int, Eq(..), (&&), otherwise, (.), Show(..), String, IO, putStrLn, undefined, Ord(..),flip,Bool(..), Ordering(..), (||),(&&), and, snd, ($), not, id, fst, Maybe(..))

import Control.Monad
import Data.List
import Data.Maybe (isJust)
import Data.Ord


import Algebra
import Compose
import Indexed
import Lens
import Matrix
import Tuple


test1, test2 :: M Integer
test1 = mkMat (3,4) [[4,5,6,0],[7,8,10,3],[40,30,1,11]]
test2 = mkMat (3,4) [[3,6,9,0],[12,15,-3,12],[6,-6,18,-27]]


-- | The Algorithm
--   
--   The result matrix is recursively calculated, by calculating the first
--   row and column and then solving the smaller problem for the (1,1)-minor
--
--   For an (MxN)-matrix this takes min(M,N) recursive steps.
--
--   Let r = min(M,N), then smith yields just
--      
--      step r . step (r-1) . … . step 2 . step 1
--
smith :: (Ord a, DivRing a) => M a -> M a
smith m@(M _ (x,y)) = compose (map step [0 .. min x y - 1]) m


-- | Recursive Step function
--
--   this function clears the i-th row and column, assuming all the 
--   preceding lines and columns have been cleaned up
--   
step :: (DivRing a, Ord a) => Int -> M a -> M a
step i mat
  | not (clear R i mat) = step i (elim R i mat)
  | not (clear C i mat) = step i (elim C i mat)
  | (get (entryL i i) mat) < zero = positify i mat
  | otherwise           = mat

  where
  clear rc j = line_zero . drop (j+1) . get ((lineL rc) j)

  elim rc j = P.until (clear rc i) $ \mat' ->
                case minimum_by_abs i mat' of
                     Just (x,y) -> (elim_step rc j . positify j . mat_swap C j y . mat_swap R j x) mat'
                     Nothing    -> mat'

-- | Elimination algorithm
--
--   a generic function which eliminates the i-th column or row
--
elim_step :: DivRing a => RC -> Int -> M a -> M a
elim_step rc i mat =
  let
    pivot = get (entryL i i) mat
    elim_entries = drop (i+1) (zip [0..] (get (lineL rc i) mat))

    elim_function = compose $
     do (j,x) <- elim_entries
        let (q,_) = quotRem x pivot
        return $ mat_muladd (toggle rc) i j (negate q)
  in elim_function mat


-- | If the (i,i) element is negative multiply the i-th line by -1
--
positify :: (Mult a, Plus a, Neg a, Ord a) => Int -> M a -> M a
positify i mat
  | get (entryL i i) mat < zero = mat_mul R i (negate one) mat
  | otherwise                   = mat

-- | find absolute minimum, which is nonzero
--   if the submatrix is zero then Nothing is returned
minimum_by_abs :: Abs a => Int -> M a -> Maybe (Int, Int)
minimum_by_abs i mat =
  case (filter (isJust . snd) . map (id *** abs) . concat . restrict_search i . enumerate2d) (get repL mat) of
       [] -> Nothing
       xs -> (Just . fst . minimumBy (comparing snd)) xs

  where
  restrict_search :: Int -> [[a]] -> [[a]]
  restrict_search j = map (drop j) . drop j
