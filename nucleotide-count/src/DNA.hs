module DNA (nucleotideCounts) where

import Data.Map (Map)
import qualified Data.Map as Map

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts = foldr f (Right $ Map.fromList empty)
  where
    empty = map (\x -> (x, 0)) nucleotides
    f _ (Left e) = Left e
    f x (Right map)
      | elem x nucleotides == True = Right $ Map.insertWith (+) x 1 map
      | otherwise = Left $ "There is no such a nucleotide: " ++ [x]
    nucleotides = "ACGT"
