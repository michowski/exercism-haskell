module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    ) where

import qualified Data.List as L
import Data.Vector (Vector)
import qualified Data.Vector as V

type Row a = Vector a
data Matrix a = Matrix { getRows :: Vector (Row a) } deriving (Eq, Show)

cols :: Matrix a -> Int
cols x
  | V.length v == 0 = 0
  | otherwise = V.length . (V.! 0) $ v
    where
      v = getRows x

column :: Int -> Matrix a -> Vector a
column x = V.map (V.! x) . getRows

flatten :: Matrix a -> Vector a
flatten = V.concat . V.toList . getRows

fromList :: [[a]] -> Matrix a
fromList = Matrix . V.fromList . map V.fromList

fromString :: Read a => String -> Matrix a
fromString = fromList . map (map read . words) . lines

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (h, w) = fromList . V.foldr f [] . flatten
  where
    f x [] = [[x]]
    f x t@(y:head)
      | length y >= w = [x] : t
      | otherwise = (x : y) : head

row :: Int -> Matrix a -> Vector a
row x = (V.! x) . getRows

rows :: Matrix a -> Int
rows = V.length . getRows

shape :: Matrix a -> (Int, Int)
shape x = (rows x, cols x)

transpose :: Matrix a -> Matrix a
transpose = fromList . L.transpose . toList . getRows
  where
    toList :: Vector (Vector a) -> [[a]]
    toList = V.toList . V.map V.toList

