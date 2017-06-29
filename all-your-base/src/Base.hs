module Base (rebase) where

toTen :: Integral a => a -> [a] -> Maybe a
toTen base
  | base <= 1 = const Nothing
  | otherwise = foldr f (Just 0) . zip [0..] . reverse
    where
      f _ Nothing = Nothing
      f (n, x) (Just acc)
        | x >= base || x < 0 = Nothing
        | otherwise = Just $ acc + x * (base ^ n)

fromTen :: Integral a => a -> a -> Maybe [a]
fromTen base
  | base <= 1 = const Nothing
  | otherwise = Just . (f [])
    where
      f xs y
        | y > 0 = f (mod y base : xs) (div y base)
        | otherwise = xs

rebase :: Integral a => a -> a -> [a] -> Maybe [a]
rebase inputBase outputBase = (>>= fromTen outputBase) . toTen inputBase
