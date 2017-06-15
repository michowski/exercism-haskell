module Garden
    ( Plant (..)
    , defaultGarden
    , garden
    , lookupPlants
    ) where

import Data.List (sort)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Data.Map (Map)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

plantForChar :: Char -> Plant
plantForChar 'C' = Clover
plantForChar 'G' = Grass
plantForChar 'R' = Radishes
plantForChar 'V' = Violets

defaultStudents :: [String]
defaultStudents = 
  [ "Alice"
  , "Bob"
  , "Charlie"
  , "David"
  , "Eve"
  , "Fred"
  , "Ginny"
  , "Harriet"
  , "Ileana"
  , "Joseph"
  , "Kincaid"
  , "Larry"
  ]

defaultGarden :: String -> Map String [Plant]
defaultGarden = garden defaultStudents

garden :: [String] -> String -> Map String [Plant]
garden students str = case words str of
  [x, y] -> f x y
  _ -> Map.empty
  where
    f x y = parseCharGroups . groupBy4 $ joinRows x y

    flatten :: [[a]] -> [a]
    flatten = (>>= id)

    joinRows :: String -> String -> String
    joinRows x y = flatten $ zipWith (\i j -> [i, j]) x y

    groupBy4 :: [a] -> [[a]]
    groupBy4 = foldr groupFn []
    groupFn x [] = [[x]]
    groupFn x t@(y:ys)
      | length y < 4 = (x:y):ys
      | otherwise = [x]:t    

    parseCharGroups :: [String] -> Map String [Plant]
    parseCharGroups = (foldr addPlantsForStudent Map.empty) . (zip $ sort students)

    addPlantsForStudent :: (String, [Char]) -> Map String [Plant] -> Map String [Plant]
    addPlantsForStudent (student, [a, b, c, d]) =
      Map.insert student (map plantForChar [a, c, b, d])

lookupPlants :: String -> Map String [Plant] -> [Plant]
lookupPlants x y = fromMaybe [] $ Map.lookup x y
