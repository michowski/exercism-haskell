module Raindrops (convert) where

convert :: Int -> String
convert n = if null result then show n else result
  where
    result = [(3, "Pling"), (5, "Plang"), (7, "Plong")] >>= f
    f (factor, str)
      | n `mod` factor == 0 = str
      | otherwise = ""
