module Grains (square, total) where

import Data.Maybe (fromMaybe)

-- I want to avoid known fact that: 2^0 + 2^1 + 2^2 + ... + 2^n = 2^n+1 - 1

sq :: Integer -> Integer
sq n = 2 ^ (n - 1)

square :: Integer -> Maybe Integer
square n
  | n >= 1 && n <= 64 = Just (sq n)
  | otherwise = Nothing

total :: Integer
total = sum . map sq $ [1 .. 64]
