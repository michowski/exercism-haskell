module SecretHandshake (handshake) where

import Numeric (showIntAtBase)
import Data.Char (intToDigit)

handshake :: Int -> [String]
handshake = action . reverse . toBin
  where
    toBin :: Int -> String
    toBin = ($ "") . (showIntAtBase 2 intToDigit)

    action :: [Char] -> [String]
    action xs
      | length xs > 4 =
          let
            order = if xs !! 4 == '1' then reverse else id
          in
            order . action . take 4 $ xs
      | otherwise = (foldr f []) . (zip [0..]) $ xs

    f :: (Int, Char) -> [String] -> [String]
    f (_, '0') = id
    f (n, '1') = (:) $ case n of
      0 -> "wink"
      1 -> "double blink"
      2 -> "close your eyes"
      3 -> "jump"
