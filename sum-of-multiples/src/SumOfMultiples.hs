module SumOfMultiples (sumOfMultiples) where

import Data.List (nub)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum . nub $ multiplies
  where multiplies = factors >>= \x -> takeWhile (< limit) $ [x, 2 * x ..]
