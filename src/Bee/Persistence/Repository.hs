module Bee.Persistence.Repository (
    createDefaultRepo,
    createProgramInstanceDirectory,
    getStdoutPath,
    getStderrPath,
    getStatusPath,
    createRandomFolder,
    moveAllFiles
) where

import System.Directory
import System.FilePath ((</>))
import System.Random

import Control.Monad

data Repository = Repository {
    rootFolder :: String
}

-- Constructs the stdout file's path given the instances folder
getStdoutPath :: FilePath -> FilePath
getStdoutPath folder = folder </> "stdout.hny"

-- Constructs the stderr file's path given the instances folder
getStderrPath :: FilePath -> FilePath
getStderrPath folder = folder </> "stderr.hny"

-- Constructs the programs status file's path given the instances folder
getStatusPath :: FilePath -> FilePath
getStatusPath folder = folder </> "program.hny"

-- Constructs the program result file's path given the instances folder
getResultPath :: FilePath -> FilePath
getResultPath folder = folder </> "result.hny"

createRepo :: String -> Repository
createRepo folder = Repository folder


-- Impure functions galore

createRandomFilePath :: IO FilePath
createRandomFilePath = do
    name <- randomString 10
    let fp = "/tmp/" </> "beefile-" ++ name
    return fp

createRandomFolder :: IO FilePath
createRandomFolder = do
    name <- randomString 10
    let folder = "/tmp/" </> "bee-" ++ name
    ensureDirectoryExists folder
    return folder

randomString :: Int -> IO String
randomString 0 = return ""
randomString count = do
    val <- getStdRandom $ randomR ('a', 'z')
    other <- randomString (count -1)
    return (val : other)

moveAllFiles :: FilePath -> FilePath -> IO ()
moveAllFiles source target = do
    files <- getDirectoryContents source
    move source target files

move :: FilePath -> FilePath -> [FilePath] -> IO ()
move _ _ [] = return ()
move source target (x:xs) = do
    let src = source </> x
    let dst = target </> x
    exists <- doesFileExist src
    if exists
        then do
        copyFile src dst
        removeFile src
        putStrLn $ "Moved " ++ src ++ " to " ++ dst
        move source target xs
        else move source target xs

-- This needs to be in the IO monad due to getHomeDirectory
createDefaultRepo :: IO Repository
createDefaultRepo = do
    home <- getHomeDirectory
    return (Repository (home </> ".bee"))

-- Creates the folder for an instance of a program execution, and returns its path
createProgramInstanceDirectory :: Repository -> String -> Int -> IO FilePath
createProgramInstanceDirectory repo program time = do
    ensureDirectoryExists root
    ensureDirectoryExists progFolder
    let folder = progFolder </> show time
    ensureDirectoryExists folder
    return folder
    where root = rootFolder repo
          progFolder = root </> program


-- Creates the directory if it doesn't exist.
ensureDirectoryExists :: FilePath -> IO ()
ensureDirectoryExists dir = do
    exists <- doesDirectoryExist dir
    if exists
        then return ()
        else createDirectory dir
