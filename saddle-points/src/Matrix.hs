module Matrix (saddlePoints) where

import Data.Array (Array, indices, bounds, (!))

saddlePoints :: Array (Int, Int) Int -> [(Int, Int)]
saddlePoints ar = filter isSaddle inds
  where
    inds = indices ar
    ((x, y), (w, h)) = bounds ar

    row n = map (\m -> ar ! (n, m)) [y..h]
    col n = map (\m -> ar ! (m, n)) [x..w]

    isSaddle ind@(i, j) = 
      let el = ar ! ind 
      in all (<= el) (row i) && all (>= el) (col j)
