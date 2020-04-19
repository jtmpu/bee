{-# LANGUAGE DeriveGeneric #-}

module Bee.Engine.Process (
    mkProcessEnvironment,
    runCatch
) where

import Bee.Environment as E
import Bee.Engine.Storage as S
import Bee.Time as T

import Control.Monad
import Data.Aeson
import GHC.Generics
import System.IO
import System.Exit
import System.Process

import System.Posix.Signals
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LBS

data ProcessInfo = ProcessInfo {
    program :: String,
    arguments :: [String]
} deriving (Show, Generic)

instance ToJSON ProcessInfo
instance FromJSON ProcessInfo

mkProcessInfo :: [String] -> ProcessInfo
mkProcessInfo vals = ProcessInfo program args
    where program = head vals
          args = tail vals

data ProcessEnvironment = ProcessEnvironment {
    processInfo :: ProcessInfo,
    processStorage :: S.BeeStorage,
    gatherOuput :: Bool
} deriving (Show, Generic)

instance ToJSON ProcessEnvironment
instance FromJSON ProcessEnvironment

mkProcessEnvironment :: E.BeeEnvironment -> [String] -> Bool -> IO ProcessEnvironment
mkProcessEnvironment env vals gather = do
    storage <- S.allocateStorage env (program info)
    return $ ProcessEnvironment info storage gather
    where info = mkProcessInfo vals

data ProcessExecutionInfo = ProcessExecutionInfo {
    startTimestamp :: Int,
    endTimestamp :: Maybe Int,
    hasFinished :: Bool,
    exitCode :: Maybe Int
} deriving (Show, Generic)
instance FromJSON ProcessExecutionInfo
instance ToJSON ProcessExecutionInfo

-- IO required to get current time.
mkProcessExecutionInfo :: IO ProcessExecutionInfo
mkProcessExecutionInfo = do
    timestamp <- T.getCurrentTimeAsInt
    return $ ProcessExecutionInfo timestamp Nothing False Nothing

doneProcessExecutionInfo :: ProcessExecutionInfo -> Int -> IO ProcessExecutionInfo
doneProcessExecutionInfo old ec = do
    timestamp <- T.getCurrentTimeAsInt 
    return $ ProcessExecutionInfo (startTimestamp old) (Just timestamp) True (Just ec) 

termHandler :: MVar () -> Handler
termHandler v = CatchOnce $ do
    putStrLn "[!] Aborting, saving data."
    putMVar v ()

runCatch :: ProcessEnvironment -> IO ProcessExecutionInfo
runCatch pe = do
    v <- newEmptyMVar
    installHandler sigTERM (termHandler v) Nothing
    installHandler sigINT (termHandler v) Nothing
    run pe v

run :: ProcessEnvironment -> MVar () -> IO ProcessExecutionInfo
run pe exitSignal = do
    -- save process info
    suProcessEnv <- S.allocateStorageUnit store S.Info
    S.writeTo suProcessEnv (LBS.toStrict . encode $ pe)
    -- create monitor callbacks
    suStdout <- S.allocateStorageUnit store S.Stdout
    let monitorOut = monitorCallback suStdout
    suStderr <- S.allocateStorageUnit store S.Stderr
    let monitorErr = monitorCallback suStderr
    -- save process status
    ei <- mkProcessExecutionInfo
    suInfo <- S.allocateStorageUnit store S.Status
    S.writeTo suInfo (LBS.toStrict . encode $ ei)
    -- run and monitor
    workingDirectory <- getWorkingDirectory shouldGather
    (_, Just hout, Just herr, hproc) <- createProcess (proc prog args) {
        std_in = Inherit,
        std_out = CreatePipe,
        std_err = CreatePipe,
        cwd = workingDirectory }
    exitCode <- monitor exitSignal hproc hout herr monitorOut monitorErr
    -- update and save status, and generated files if applicable
    when shouldGather $
        case workingDirectory of
            Just dir -> S.saveFiles dir store
    S.clearStorageUnit suInfo
    case exitCode of
        ExitSuccess -> do
            doneInfo <- doneProcessExecutionInfo ei 0
            S.writeTo suInfo (LBS.toStrict . encode $ doneInfo)
        ExitFailure val -> do
            doneInfo <- doneProcessExecutionInfo ei val
            S.writeTo suInfo (LBS.toStrict . encode $ doneInfo)

    return ei
    where
        prog = program (processInfo pe)
        args = arguments (processInfo pe)
        store = processStorage pe
        shouldGather = gatherOuput pe

getWorkingDirectory :: Bool -> IO (Maybe FilePath)
getWorkingDirectory True = Just <$> S.createRandomFolder
getWorkingDirectory False = return Nothing

monitor :: MVar () -> ProcessHandle -> Handle -> Handle -> (B.ByteString -> IO B.ByteString) -> (B.ByteString -> IO B.ByteString) -> IO ExitCode
monitor exitSignal hProc hOut hErr callbackOut callbackErr = do
    outbs <- B.hGetNonBlocking hOut (64 * 1024)
    errbs <- B.hGetNonBlocking hErr (64 * 1024)
    callbackOut outbs
    callbackErr errbs
    val <- tryTakeMVar exitSignal
    case val of
        Just _ -> return $ ExitFailure 1
        Nothing -> do
            s <- getProcessExitCode hProc
            case s of
                Nothing -> monitor exitSignal hProc hOut hErr callbackOut callbackErr
                Just ec -> do
                    outlast <- B.hGetContents hOut
                    errlast <- B.hGetContents hErr
                    callbackOut outlast
                    callbackErr errlast
                    return ec

monitorCallback :: S.BeeStorageUnit -> B.ByteString -> IO B.ByteString
monitorCallback storage value = writeToStorage storage value >>= writeToStdout

writeToStdout :: B.ByteString -> IO B.ByteString
writeToStdout value = do
    B.putStr value
    return value

writeToStorage :: S.BeeStorageUnit -> B.ByteString -> IO B.ByteString
writeToStorage store value = do
    S.writeTo store value
    return value