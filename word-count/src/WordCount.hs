module WordCount (wordCount) where

import Data.Char (toLower)

isIgnoredChar :: Char -> Bool
isIgnoredChar = flip elem ".:,!&@$%^"

wordCount :: String -> [(String, Int)]
wordCount = foldr insertToMap [] . map removeQuotes . words . removeIgnored
  where
    removeIgnored = map (\c -> if isIgnoredChar c then ' ' else toLower c)

    removeQuotes ('\'' : x) = removeQuotes x
    removeQuotes x = if last x == '\'' then init x else x

    insertToMap x m = case lookup x m of
      Nothing -> (x, 1) : m
      Just _  -> map (\t@(k, v) -> if k == x then (k, v + 1) else t) m
