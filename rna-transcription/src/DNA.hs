module DNA (toRNA) where

toRNA :: String -> Maybe String
toRNA = foldr f (Just [])
  where
    f _ Nothing = Nothing
    f x (Just xs) = (:xs) <$> dnaToRna x
    dnaToRna :: Char -> Maybe Char
    dnaToRna = flip lookup $
      [ ('G', 'C')
      , ('C', 'G')
      , ('T', 'A')
      , ('A', 'U')
      ]
