module Roman (numerals) where

import Data.Char (digitToInt)

letters = "IVXLCDM"

numerals :: Integer -> Maybe String
numerals x
  | x > 3999 = Nothing
  | otherwise =
    let
      digits = map digitToInt . show $ x
      len = length digits
      nDigits = zip [len - 1, len - 2 .. 0] digits
      l n = letters !! n
      convert (n, d)
        | d == 0 = []
        | d <= 3 = replicate d $ l (n * 2)
        | d <= 5 = replicate (5 - d) (l (n * 2)) ++ [l (n * 2 + 1)]
        | d <= 8 = l (n * 2 + 1) : replicate (d - 5) (l (n * 2))
        | d == 9 = [l (n * 2), l (n * 2 + 2)]
    in
      Just $ foldl (\acc -> (acc ++) . convert) [] nDigits
