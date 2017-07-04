module Series (slices) where

import Data.Char (digitToInt)

slices :: Int -> String -> [[Int]]
slices n 
  | n == 0 = const [[]]
  | n == 1 = map (return . digitToInt)
  | otherwise = fst . foldr f ([] :: [[Int]], [] :: [Int])
    where
      f x (acc, []) = (acc, [digitToInt x])
      f x (acc, stack@(y:ys))
        | val == y - 1 =
            if length stack == n - 1
            then ((val : stack) : acc, val : init stack)
            else (acc, val : stack)
        | otherwise = (acc, []) 
          where
            val = digitToInt x
  
