module Triangle (rows) where

row :: Int -> [Integer]
row 1 = [1]
row x = 1 : zipWith (+) prev (tail prev ++ [0])
  where
    prev = row (x - 1)

rows :: Int -> [[Integer]]
rows x = map row [1 .. x]
