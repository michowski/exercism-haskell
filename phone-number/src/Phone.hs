module Phone (number) where

import Data.Char (isDigit)

ifMaybe :: Bool -> a -> Maybe a
ifMaybe True x = Just x
ifMaybe False _ = Nothing

number :: String -> Maybe String
number nr
  | len == 10 = ifMaybe (validCountry clean) clean
  | len == 11 = ifMaybe
    (head clean == '1' && (validCountry $ tail clean))
    (tail clean)
  | otherwise = Nothing
  where 
    clean = filter isDigit nr
    len = length clean
    validCountry code = all (flip elem ['2'..'9']) [code !! 0, code !! 3]
