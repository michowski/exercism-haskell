module SpaceAge (Planet(..), ageOn) where

import Data.List
import Data.Maybe

data Planet = Mercury
  | Venus
  | Earth
  | Mars
  | Jupiter
  | Saturn
  | Uranus
  | Neptune
  deriving (Eq)

type PlanetPeriod = (Planet, Float)

planetPeriodMap :: [PlanetPeriod]
planetPeriodMap =
  [ (Mercury, 0.2408467)
  , (Venus, 0.61519726)
  , (Earth, 1)
  , (Mars, 1.8808158)
  , (Jupiter, 11.862615)
  , (Saturn, 29.447498)
  , (Uranus, 84.016846)
  , (Neptune, 164.79132)
  ]

ageOn :: Planet -> Float -> Float
ageOn planet = secondsToYears . planetRatio
  where
    secondsToYears :: Float -> Float
    secondsToYears = (/ (3600 * 24 * 365.25))
    planetPeriod :: Maybe PlanetPeriod
    planetPeriod = find ((planet ==) . fst) planetPeriodMap
    planetRatio :: Float -> Float
    planetRatio = (/ (fromMaybe 1 $ snd <$> planetPeriod))

