module Bee.Config (
    Config,
    parseConfig,
    beeRootFolder
) where

data Config = Config {
    beeRootFolder :: String
}

parseConfig :: String -> Either String Config
parseConfig value = Right (Config "/home/user/.bee")