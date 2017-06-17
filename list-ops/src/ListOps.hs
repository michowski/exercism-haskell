module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f s [] = s
foldl' f s (x:xs) =
  let s' = f s x
  in seq s' $ foldl' f s' xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ s [] = s
foldr f s (x:xs) = f x (foldr f s xs)

length :: [a] -> Int
length = foldr (const (+ 1)) 0

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

map :: (a -> b) -> [a] -> [b]
map f = foldr (\x -> (f x :)) []

filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr (\x acc -> if f x then x : acc else acc) []

(++) :: [a] -> [a] -> [a]
(++) = flip $ foldr (:)

concat :: [[a]] -> [a]
concat = foldr (++) []
