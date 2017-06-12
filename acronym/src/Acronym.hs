module Acronym (abbreviate) where

import Data.Char (isUpper, toUpper, isAlphaNum, isSpace)

abbreviate :: String -> String
abbreviate = wordsToAcronim . words . (map hyphenToSpace) . (takeWhile while)
  where
    while = (/= ':')
    
    hyphenToSpace '-' = ' '
    hyphenToSpace x = x

    wordsToAcronim words = case words of
      [x] -> x
      xs -> xs >>= wordAcronim

    wordAcronim t@(x:xs)
      | all isUpper t = [x]
      | otherwise = (toUpper x) : (filter isUpper xs)
