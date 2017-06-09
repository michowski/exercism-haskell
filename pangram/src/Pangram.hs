module Pangram (isPangram) where

import Data.Char (toLower)

type LetterMap = [(Char, Bool)]

emptyLetterMap :: LetterMap
emptyLetterMap = map (\x -> (x, False)) ['a'..'z']

insertLetter :: Char -> LetterMap -> LetterMap
insertLetter l = map (\t@(x, y) -> if x == toLower l then (x, True) else t)

isPangram :: String -> Bool
isPangram = all snd . foldr insertLetter emptyLetterMap
