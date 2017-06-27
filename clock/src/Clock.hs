module Clock (clockHour, clockMin, fromHourMin, toString) where

data Clock = Clock { getMins :: Int } deriving (Eq, Show)

instance Num Clock where
  fromInteger = Clock . fromIntegral
  x + y = Clock $ getMins x + getMins y
  x - y = Clock $ getMins x - getMins y
  negate = Clock . (1440 -) . getMins

clockHour :: Clock -> Int
clockHour = (`mod` 24) . (`div` 60) . getMins

clockMin :: Clock -> Int
clockMin = (`mod` 60) . getMins

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock $ mod ((60 * hour) + min) 1440

toString :: Clock -> String
toString clock = toStr (clockHour clock) ++ ":" ++ toStr (clockMin clock)
  where
    toStr x
      | x < 10 = '0' : show x
      | otherwise = show x
