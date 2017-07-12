module Sieve (primesUpTo) where

primesUpTo :: Integer -> [Integer]
primesUpTo n
  | n < 2 = []
  | otherwise = reverse $ f [2..n] []
    where
      f [x] ps = x : ps
      f (x:xs) ps = f (filter (\a -> a `rem` x /= 0) xs) (x : ps)
