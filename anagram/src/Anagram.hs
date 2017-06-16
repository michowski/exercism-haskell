module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toUpper)

anagramsFor :: String -> [String] -> [String]
anagramsFor str =
  let
    upper = map toUpper
    upperStr = upper str
    upperSortedStr = sort upperStr
  in
    filter $ \x -> upper x /= upperStr && (sort . upper) x == upperSortedStr
