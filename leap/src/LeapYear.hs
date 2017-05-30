module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year
  | divBy 400 = True
  | divBy 100 = False
  | divBy 4   = True
  | otherwise = False
  where divBy x = mod year x == 0
