module Bob (responseFor) where

import Data.Char
import Data.List

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

hasOnlyUpperLetters :: String -> Bool
hasOnlyUpperLetters xs = (not . null) xs' && all isUpper xs'
  where 
    xs' = filter isLetter xs

responseFor :: String -> String
responseFor xs
  | null xs'                = "Fine. Be that way!"
  | hasOnlyUpperLetters xs' = "Whoa, chill out!"  
  | last xs' == '?'         = "Sure."
  | otherwise               = "Whatever."
  where 
    xs' = trim xs
