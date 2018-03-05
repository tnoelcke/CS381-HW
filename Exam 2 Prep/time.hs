module Time where

type Hour = Int
type Minutes = Int

data Time = Midnight
           | Noon
           | AM Hour
           | PM Hour
           | Before Minutes Time
           | After Minutes Time
        deriving(Eq, Show)

sem :: Time -> Int
sem Midnight = 0
sem Noon = 60*12
sem (AM t) = t*60
sem (PM t) = 60*12 + t*60
sem (Before m t) = sem t - m
sem (After m t) = sem t + m