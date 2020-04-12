{-# LANGUAGE DeriveGeneric #-}

module Bee.Engine.Process (
    ProcessExecutionInfo(..),
    createExecutionInfo,
    saveExecutionInfo,
    updateExecutionInfo,
    monitor
) where

import System.IO
import System.Exit
import System.Process

import GHC.Generics
import Data.Aeson

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

data ProcessExecutionInfo = ProcessExecutionInfo {
    programString :: String,
    programArguments :: [String],
    programStartTimestamp :: Int,
    programEndTimestamp :: Int,
    programHasFinished :: Bool,
    programExitCode :: Int
} deriving (Show, Generic)
instance FromJSON ProcessExecutionInfo
instance ToJSON ProcessExecutionInfo

createExecutionInfo :: [String] -> Int -> ProcessExecutionInfo
createExecutionInfo target timestamp = ProcessExecutionInfo program args timestamp 0 False 0
    where program = head target
          args = tail target

updateExecutionInfo :: ProcessExecutionInfo -> Int -> ExitCode -> ProcessExecutionInfo
updateExecutionInfo ei endTime ec = case ec of
    ExitSuccess -> ProcessExecutionInfo (programString ei) (programArguments ei) (programStartTimestamp ei) endTime True 0
    ExitFailure val -> ProcessExecutionInfo (programString ei) (programArguments ei) (programStartTimestamp ei) endTime True val

-- Saves the execution info to the specified file, overwrites the contents of the file.
saveExecutionInfo :: ProcessExecutionInfo -> FilePath -> IO ()
saveExecutionInfo info path = LBS.writeFile path (encode info)

monitor :: ProcessHandle -> Handle -> Handle -> (BS.ByteString -> IO()) -> (BS.ByteString -> IO ()) -> IO ExitCode
monitor ph stdout stderr outfn errfn = work 
    where
        work = do
            outbs <- BS.hGetNonBlocking stdout (64 * 1024)
            errbs <- BS.hGetNonBlocking stderr (64 * 1024)
            outfn outbs
            errfn errbs
            s <- getProcessExitCode ph
            case s of
                Nothing -> work
                Just ec -> do
                    outlast <- BS.hGetContents stdout
                    errlast <- BS.hGetContents stderr
                    outfn outlast
                    errfn errlast
                    return (ec)
