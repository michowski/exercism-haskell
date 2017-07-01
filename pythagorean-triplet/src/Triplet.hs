module Triplet (isPythagorean, mkTriplet, pythagoreanTriplets) where

import Data.List (sort)

isPythagorean :: (Int, Int, Int) -> Bool
isPythagorean (x, y, z) = (\(x:y:z:_) -> x ^ 2 + y ^ 2 == z ^ 2) $ sort [x, y, z]

mkTriplet :: Int -> Int -> Int -> (Int, Int, Int)
mkTriplet a b c = (a, b, c)

pythagoreanTriplets :: Int -> Int -> [(Int, Int, Int)]
pythagoreanTriplets x y = map toTriplet . filter (isValidThird . thirdSide) $ pairs
  where
    thirdSide :: (Int, Int) -> Float
    thirdSide (a, b) = sqrt $ fromIntegral a ** 2 + fromIntegral b ** 2

    toTriplet :: (Int, Int) -> (Int, Int, Int)
    toTriplet p@(a, b) = (a, b, round . thirdSide $ p)

    isInt :: Float -> Bool
    isInt x = (fromInteger . round $ x) == x

    isValidThird :: Float -> Bool
    isValidThird t = isInt t && t <= fromIntegral y

    pairs :: [(Int, Int)]
    pairs = [x .. y - 1] >>= \a ->
      flip map [a + 1 .. y - 1] $ \b -> (a, b)
