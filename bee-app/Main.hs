module Main where

import System.Environment (getArgs)

import qualified Bee.CLI.Commandline as BC
import qualified Bee.Config as BCFG
import qualified Bee.Environment as E
import qualified Bee.Engine.Process as P

main :: IO ()
main = do
    args <- getArgs
    let resultArgs = BC.parse possibleFlags possibleArgs args
    let resultCfg = BCFG.parseConfig ""
    case resultCfg of
        (Left error) -> putStrLn ("[!] Error in config: " ++ error)
        (Right config) -> case resultArgs of 
            (Left error) -> putStrLn ("[!] Error in command line: " ++ error)
            (Right validArgs) -> case determineCommand config validArgs of
                (Left error) -> putStrLn ("[!] Error parsing command: " ++ error)
                (Right command) -> runBee config command

runBee :: BCFG.Config -> BeeCommand -> IO ()
runBee _ HelpCommand = printHelp
runBee cfg (ExecuteCommand target shouldGather) = do
    env <- buildEnvironment cfg
    processExecution <- P.mkProcessEnvironment env target shouldGather
    P.runCatch processExecution
    return ()

buildEnvironment :: BCFG.Config -> IO E.BeeEnvironment
buildEnvironment cfg = E.mkDefaultBeeEnvironment

data BeeCommand = HelpCommand | ExecuteCommand { programArgs :: [String], shouldGather :: Bool }
determineCommand :: BCFG.Config -> [BC.CLIArgument] -> Either String BeeCommand
determineCommand config args = if BC.hasFlag "-h" args || BC.hasFlag "--help" args
    then Right HelpCommand
    else case getTarget args of
        Nothing -> Left "No target specified."
        Just target -> if BC.hasFlag "-g" args || BC.hasFlag "--gather" args 
            then Right (ExecuteCommand target True)
            else Right (ExecuteCommand target False)

getTarget :: [BC.CLIArgument] -> Maybe [String]
getTarget [] = Nothing
getTarget (x:xs) = case x of
    BC.CLIProgram target -> Just target
    _ -> getTarget xs

possibleFlags = ["-h", "--help", "-g", "--gather"]
possibleArgs = []

printHelp :: IO ()
printHelp = do
    putStrLn ""
    putStrLn "Bee - version 0.1"
    putStrLn ""
    putStrLn "      Automatically save the timestamp, command, and it's output."
    putStrLn ""
    putStrLn "bee [args] TARGET"
    putStrLn ""
    putStrLn "Arguments: "
    putStrLn " -h, --help       - Display this help message"
    putStrLn " -g, --gather     - Run program in an isolated working directory, and save all generated files"
    putStrLn ""
    putStrLn "TARGET:"
    putStrLn "The program and all its flags bee should run."
    putStrLn "Example: bee nmap -Pn -n -vvv -sV -p443 127.0.0.1/32"