module PrimeFactors (primeFactors) where

primes = 2 : filter (\n -> head (primeFactors n) == n) [3, 5..]

primeFactors :: Integer -> [Integer]
primeFactors = f (head primes) (tail primes)
  where
    f p ps x
      | x < 2 = []
      | x < p ^ 2 = [x]
      | mod x p == 0 = p : f p ps (div x p)
      | otherwise = f (head ps) (tail ps) x
