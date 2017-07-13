module Brackets (arePaired) where

brackets =
  [ ('{', '}')
  , ('(', ')')
  , ('[', ']')
  ]

opening = map fst brackets
closing = map snd brackets

arePaired :: String -> Bool
arePaired = (== (Just "")) . foldr f (Just "")
  where
    f _ Nothing = Nothing
    f c (Just xs)
      | elem c closing = Just (c : xs)
      | elem c opening =
          if (not . null $ xs) && (lookup c brackets == Just (head xs))
            then Just $ tail xs
            else Nothing
      | otherwise = Just xs
