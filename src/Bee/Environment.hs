module Bee.Environment (
    BeeEnvironment (..),
    mkBeeEnvironment,
    mkDefaultBeeEnvironment
) where

import System.FilePath ((</>))
import System.Directory

data BeeEnvironment = BeeEnvironment {
    rootFolder :: String,
    fileExtension :: String
} deriving (Show)

-- An IO function as it retrieves a users home folder
-- which can fail
mkDefaultBeeEnvironment :: IO BeeEnvironment
mkDefaultBeeEnvironment = do
    home <- getHomeDirectory
    let defaultFolder = home </> ".beehive"
    return $ BeeEnvironment defaultFolder "hny"

mkBeeEnvironment :: String -> String -> BeeEnvironment
mkBeeEnvironment = BeeEnvironment

