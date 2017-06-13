module ETL (transform) where

import Data.Map (Map)
import Data.Char (toLower)
import qualified Data.Map as Map

transform :: Map a String -> Map Char a
transform = Map.foldlWithKey f Map.empty
  where
    f map score = foldr (insertScoreForChar score) map
    insertScoreForChar :: a -> Char -> Map Char a -> Map Char a 
    insertScoreForChar score char = Map.insert (toLower char) score
