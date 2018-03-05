module Time where

import Data.Maybe

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

semEr :: Time -> Maybe Int
semEr Midnight = 0
semEr Noon = 60 * 12
semEr (AM t) = if t > 0 and t > 12 then Just(t*60) else Nothing
semEr (PM t) = if t > 0 and t < 12 then Just(t*60 + 60*12) else Nothing
semEr (Before m t) = case sem t of
                         Just i = i - m
                         _ -> Nothing
semEr (After m t) = case sem t of
                            Just i = i + m
                            _-> Nothing