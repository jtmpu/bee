{-# LANGUAGE DeriveGeneric #-}

module Bee.Engine.Storage (
    BeeStorage (..),
    BeeStorageUnit (..),
    StorageUnitKey (..),
    allocateStorage,
    getStorageKeys,
    getStorage,
    allocateStorageUnit,
    getStorageUnit,
    getStorageUnits,
    writeTo,
    readFrom,
    createRandomFolder,
    saveFiles,
    clearStorageUnit
) where

import Control.Monad (filterM)
import Control.Exception
import Data.Aeson
import GHC.Generics
import System.IO
import System.Directory (listDirectory, createDirectory, doesFileExist, copyFile, removeFile, doesDirectoryExist)
import System.FilePath ((</>))
import System.Random
import Text.Read (readMaybe)
import qualified Data.ByteString as B

import Bee.Time as T
import Bee.Environment as E

data BeeStorage = BeeStorage {
    directory :: FilePath,
    timestamp :: Int,
    extension :: String
} deriving (Show, Generic)

instance ToJSON BeeStorage
instance FromJSON BeeStorage

data BeeStorageUnit = BeeStorageUnit {
    path :: FilePath,
    maybeHandle :: Maybe Handle
} deriving (Show)

data StorageUnitKey = Info | Status | Stdout | Stderr 

data ConversionException = ConversionException deriving (Show)
instance Exception ConversionException

allocateStorage :: E.BeeEnvironment -> String -> IO BeeStorage
allocateStorage env key = do
    ensureDirectoryExists root
    let progFolder = root </> key
    ensureDirectoryExists progFolder
    timestamp <- T.getCurrentTimeAsInt
    let instanceFolder = progFolder </> show timestamp
    ensureDirectoryExists instanceFolder
    return $ BeeStorage instanceFolder timestamp extension
    where root = E.rootFolder env
          extension = E.fileExtension env

getStorageKeys :: E.BeeEnvironment -> IO [(String, String)]
getStorageKeys env = do
    programs <- listDirectory (rootFolder env) >>= filterM (\prog -> doesDirectoryExist (folder </> prog))
    instances <- mapM (\prog -> listDirectory (folder </> prog)) programs
    let a = zip programs instances
    let b = map (\(prog, inst) -> [(prog, x) | x <- inst]) a
    let keys = concat b
    return keys
    where folder = E.rootFolder env

-- Returns the storage for the specified key-tuple (program, time)
getStorage :: E.BeeEnvironment -> (String, String) -> IO BeeStorage
getStorage env (program, time) =
    -- TODO: Safe parse
    case readMaybe time :: Maybe Int of
        Just val -> return $ BeeStorage path val (E.fileExtension env)
        Nothing -> return $ BeeStorage path 0 (E.fileExtension env)
    where path = E.rootFolder env </> program </> time

allocateStorageUnit :: BeeStorage -> StorageUnitKey -> IO BeeStorageUnit 
allocateStorageUnit store key = do
    let path = getUnitPath store key
    writeFile path ""
    return $ BeeStorageUnit path Nothing

openStorageUnit :: BeeStorage -> StorageUnitKey -> IO BeeStorageUnit
openStorageUnit store key = undefined

getStorageUnit :: BeeStorage -> StorageUnitKey -> BeeStorageUnit
getStorageUnit store key = BeeStorageUnit path Nothing
    where path = getUnitPath store key

getStorageUnits :: BeeStorage -> IO [BeeStorageUnit]
getStorageUnits store = do
    files <- listDirectory (directory store) >>= filterM (\x -> doesFileExist (directory store </> x))
    return $ map (\x -> BeeStorageUnit (directory store </> x) Nothing) files

getUnitPath :: BeeStorage -> StorageUnitKey -> FilePath
getUnitPath bs Stdout =  directory bs </> ("stdout." ++ extension bs) 
getUnitPath bs Stderr =  directory bs </> ("stderr." ++ extension bs) 
getUnitPath bs Info =  directory bs </> ("info." ++ extension bs) 
getUnitPath bs Status =  directory bs </> ("status." ++ extension bs) 

writeTo :: BeeStorageUnit -> B.ByteString -> IO ()
writeTo unit d = case maybeHandle unit of
    Nothing -> do
        h <- openFile (path unit) AppendMode
        B.hPutStr h d
        hClose h
    Just handle -> B.hPutStr handle d

readFrom :: BeeStorageUnit -> IO B.ByteString
readFrom unit = case maybeHandle unit of
    Nothing -> B.readFile (path unit)
    Just h -> B.hGetContents h

clearStorageUnit :: BeeStorageUnit -> IO ()
clearStorageUnit unit = writeFile (path unit) ""

-- Creates the directory if it doesn't exist.
ensureDirectoryExists :: FilePath -> IO ()
ensureDirectoryExists dir = do
    exists <- doesDirectoryExist dir
    if exists
        then return ()
        else createDirectory dir

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

saveFiles :: FilePath -> BeeStorage -> IO ()
saveFiles source target = do
    files <- listDirectory source
    move source target files

move :: FilePath -> BeeStorage -> [FilePath] -> IO ()
move _ _ [] = return ()
move source store (x:xs) = do
    let src = source </> x
    let dst = target </> x
    exists <- doesFileExist src
    if exists
        then do
        copyFile src dst
        removeFile src
        move source store xs
        else move source store xs
    where target = directory store