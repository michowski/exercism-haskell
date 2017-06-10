module RunLength (decode, encode) where

import Data.Char

decode :: String -> String
decode = fst . foldl f ("", "")
  where
    f (res, counter) x
      | isDigit x = (res, counter ++ [x])
      | otherwise =
        let
          n = if null counter then 1 else read counter
        in
          (res ++ replicate n x, "")

encode :: String -> String
encode = result . foldl f ("", 0, Nothing) . (++ "/")
  where
    result (x, _, _) = x  
    f (res, _, Nothing) x = (res, 1, Just x)
    f (res, counter, Just char) x
      | x == char = (res, counter + 1, Just char)
      | otherwise =
        let
          n = if counter > 1 then show counter else ""
        in
          (res ++ n ++ [char], 1, Just x)
