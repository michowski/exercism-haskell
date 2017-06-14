module Beer (song) where

import Data.List (intercalate)
import Data.Char (toUpper)

line :: Int -> String
line x =
  capitalize n ++ " of beer on the wall, " ++ n ++ " of beer.\n"
  ++ action x ++ ", " ++ m ++ " of beer on the wall.\n"
  where
    n = bottles x
    m = bottles (x - 1)
    bottles (-1) = "99 bottles"
    bottles 0 = "no more bottles"
    bottles 1 = "1 bottle"
    bottles x = show x ++ " bottles"
    action 0 = "Go to the store and buy some more"
    action 1 = "Take it down and pass it around"
    action x = "Take one down and pass it around"
    capitalize (x:xs) = toUpper x : xs

song :: String
song = intercalate "\n" $ map line [99,98..0]

