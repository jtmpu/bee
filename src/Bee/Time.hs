module Bee.Time (
    getCurrentTimeAsInt,
    convertIntTimeToStr
) where

import Data.Time.Clock.POSIX
import Data.Time

getCurrentTimeAsInt :: IO Int
getCurrentTimeAsInt = do
    time <- round `fmap` getPOSIXTime :: IO Int
    return time

convertIntTimeToStr :: Int -> String
convertIntTimeToStr ts = case mt of
        Just time -> formatTime defaultTimeLocale "%_Y-%m-%d %T" time
        Nothing -> show ts
    where mt = parseTimeM True defaultTimeLocale "%s" (show ts) :: Maybe UTCTime