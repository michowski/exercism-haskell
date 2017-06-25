module Series (largestProduct) where

import Data.Char (digitToInt, isDigit)

largestProduct :: Int -> String -> Maybe Integer
largestProduct size digits
  | size < 0 = Nothing
  | size > length digits = Nothing
  | all isDigit digits == False = Nothing
  | otherwise = Just . toInteger $ maximum products
    where
      products :: [Int]
      products = foldr (\xs -> (:) . product $ xs) [] groups

      groups :: [[Int]]
      groups = flip map [0 .. length digits - size] $ \n -> 
        map digitToInt . sublist n (n + size) $ digits

      sublist :: Int -> Int -> [a] -> [a]
      sublist from to = drop from . take to
