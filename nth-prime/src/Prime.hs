module Prime (nth) where

nth :: Int -> Maybe Integer
nth n
  | n < 1 = Nothing
  | otherwise = Just (sieve [2..] !! (n - 1))
    where
      sieve (p:xs) = p : sieve (filter (\x -> rem x p /= 0) xs)
