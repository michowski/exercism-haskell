module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toUpper)
import Data.Maybe (fromMaybe)

scoreMap :: [(String, Integer)]
scoreMap =
  [ ("AEIOULNRST", 1)
  , ("DG", 2)
  , ("BCMP", 3)
  , ("FHWVY", 4)
  , ("K", 5)
  , ("JX", 8)
  , ("QZ", 10)
  ]

scoreLetter :: Char -> Integer
scoreLetter x = fromMaybe 0 $ lookup (toUpper x) scoreForLetter
  where
    scoreForLetter :: [(Char, Integer)]
    scoreForLetter = scoreMap >>= 
      \(xs, score) -> map (\x -> (x, score)) xs

scoreWord :: String -> Integer
scoreWord = foldr (\x -> (+ scoreLetter x)) 0 
