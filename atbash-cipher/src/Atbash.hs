module Atbash (decode, encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (elemIndex, intercalate)

alfphabet :: [Char]
alfphabet = ['a'..'z']

alfphabetLength :: Int
alfphabetLength = length alfphabet

shiftChar :: Char -> Char
shiftChar c = case elemIndex c alfphabet of
  Just ind -> alfphabet !! (alfphabetLength - 1 - ind)
  Nothing  -> c

decode :: String -> String
decode = map shiftChar . filter isAlphaNum

encode :: String -> String
encode = intercalate " " . group5 . map (shiftChar . toLower) . filter isAlphaNum
  where
    group5 :: [a] -> [[a]]
    group5 [] = []
    group5 xs = take 5 xs : group5 (drop 5 xs)
