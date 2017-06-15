module Robot
( Bearing(East,North,South,West)
, bearing
, coordinates
, mkRobot
, simulate
, turnLeft
, turnRight
) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show, Enum)

data Robot = Robot
  { direction :: Bearing
  , position :: (Integer, Integer)
  }

bearing :: Robot -> Bearing
bearing = direction

coordinates :: Robot -> (Integer, Integer)
coordinates = position

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

simulate :: Robot -> String -> Robot
simulate = foldl move
  where
    move robot@(Robot { position = (x, y), direction = d }) action
      | action == 'A' = robot
        { position = case d of
            North -> (x, y + 1)
            East -> (x + 1, y)
            South -> (x, y - 1)
            West -> (x - 1, y)
        }
      | otherwise = robot
        { direction = (if action == 'L' then turnLeft else turnRight) d }

turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft x = pred x

turnRight :: Bearing -> Bearing
turnRight West = North
turnRight x = succ x
