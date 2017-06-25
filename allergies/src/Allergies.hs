module Allergies (Allergen(..), allergies, isAllergicTo) where

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Enum)

allergies :: Int -> [Allergen]
allergies = foldl f [] . zip [0..] . toBinReversed
  where
    toBinReversed :: Int -> [Int]
    toBinReversed 0 = [0]
    toBinReversed 1 = [1]
    toBinReversed n
        | mod n 2 == 0 = 0 : toBinReversed (div n 2)
        | otherwise = 1 : toBinReversed (div n 2)

    f acc (_, 0) = acc
    f acc (n, 1)
      | n < 8 = (toEnum n :: Allergen) : acc
      | otherwise = acc

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo x = elem x . allergies
