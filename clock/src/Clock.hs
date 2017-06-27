module Clock (clockHour, clockMin, fromHourMin, toString) where

data Clock = Clock
  { hour :: Integer
  , mins :: Integer
  } deriving (Eq, Show)

instance Num Clock where
  fromInteger = fromHourMin 0 . fromIntegral
  x + y = fromInteger $ toMin x + toMin y
  x - y = fromInteger $ toMin x - toMin y
  negate = fromInteger . (1440 -) . toMin

toMin :: Clock -> Integer
toMin x = hour x * 60 + mins x

clockHour :: Clock -> Int
clockHour = fromIntegral . hour

clockMin :: Clock -> Int
clockMin = fromIntegral . mins

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock
  { hour = fromIntegral $ mod (hour + div min 60) 24
  , mins = fromIntegral $ mod min 60
  }

toString :: Clock -> String
toString clock = toStr (hour clock) ++ ":" ++ toStr (mins clock)
  where
    toStr x
      | x < 10 = '0' : show x
      | otherwise = show x
