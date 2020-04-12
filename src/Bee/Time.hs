module Bee.Time (
    getCurrentTimeAsInt
) where

import Data.Time.Clock.POSIX

getCurrentTimeAsInt :: IO Int
getCurrentTimeAsInt = do
    time <- round `fmap` getPOSIXTime :: IO Int
    return time