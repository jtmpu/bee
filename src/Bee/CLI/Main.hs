module Bee.CLI.Main (
    execBeeCLI
) where

import System.IO
import System.Exit
import System.Process
import System.FilePath ((</>))
import System.Directory
import System.Environment

import Control.Monad

import qualified Data.ByteString as BS

import qualified Bee.Time as BT
import qualified Bee.Persistence.Repository as BPR
import qualified Bee.Config as BCFG 
import qualified Bee.Engine.Process as BP
import qualified Bee.CLI.Commandline as BC

execBeeCLI :: IO ()
execBeeCLI = do
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
    let program = head target
    let args = tail target
    -- Generate directories for program execution info
    time <- BT.getCurrentTimeAsInt
    repo <- BPR.createDefaultRepo
    directory <- BPR.createProgramInstanceDirectory repo program time
    let fpStdout = BPR.getStdoutPath directory
    let fpStderr = BPR.getStderrPath directory
    let fpStatus = BPR.getStatusPath directory
    let status = BP.createExecutionInfo target time
    BP.saveExecutionInfo status fpStatus
    -- Get handles for output files
    hStdout <- openFile fpStdout AppendMode
    hStderr <- openFile fpStderr AppendMode
    -- Run program and monitor all outputs, and store them.
    workingDir <- BPR.createRandomFolder
    let cwd = getWorkingDirectory shouldGather workingDir
    (_, Just hout, Just herr, ph) <- createProcess (proc program args) { 
        std_in = Inherit, 
        std_out = CreatePipe, 
        std_err = CreatePipe,
        cwd = cwd }
    ec <- BP.monitor ph hout herr (saveOutput [hStdout, stdout]) (saveOutput [hStderr, stderr])
    -- Retrieve all created files
    when shouldGather $ BPR.moveAllFiles workingDir directory
    -- cleanup
    endTime <- BT.getCurrentTimeAsInt
    let endStatus = BP.updateExecutionInfo status endTime ec 
    BP.saveExecutionInfo endStatus fpStatus
    hClose hStdout
    hClose hStderr
    removeDirectory workingDir

-- Handler which writes the received data to all handles (the file, and stdout/stderr)
saveOutput :: [Handle] -> BS.ByteString -> IO ()
saveOutput handles d = mapM_ (`BS.hPutStr` d) handles

getWorkingDirectory :: Bool -> FilePath -> Maybe FilePath
getWorkingDirectory True fp = Just fp
getWorkingDirectory False fp = Nothing

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