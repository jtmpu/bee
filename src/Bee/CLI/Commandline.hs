module Bee.CLI.Commandline (
    CLIArgument (..),
    parse,
    hasFlag
) where

import Data.List

data CLIArgument = CLIArgument { argumentString :: String, argumentValue :: String } | CLIFlag { flagString :: String } | CLIProgram { programList :: [String] } deriving Show

hasFlag :: String -> [CLIArgument] -> Bool
hasFlag flag [] = False
hasFlag flag (CLIFlag other : xs) = flag == other
hasFlag flag (x:xs) = hasFlag flag xs

parse :: [String] -> [String] -> [String] -> Either String [CLIArgument]
parse _ _ [] = Right []
parse possibleFlags possibleArgs args = if "-" `isPrefixOf` value
    then if value `elem` possibleFlags
        then case parse possibleFlags possibleArgs (tail args) of
            (Left error) -> Left error
            (Right vals) -> Right (CLIFlag value : vals)
        else if value `elem` possibleArgs 
            then if not $ null (tail args)
                then case parse possibleFlags possibleArgs (tail $ tail args) of
                    (Left error) -> Left error
                    (Right vals) -> Right (CLIArgument value (head (tail args)) : vals)
                else Left ("Missing value for parameter '" ++ value ++ "'")
            else Left ("Unknown parameter '" ++ value ++ "'")
    else Right [CLIProgram args]
    where value = head args