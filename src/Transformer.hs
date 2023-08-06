{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Transformer where

import Parser (FileDiff (..), Hunk (..), Line (..))
import System.Exit
import System.Process

transformFileDiff :: (String -> IO String) -> FileDiff -> IO FileDiff
transformFileDiff transformer (FileDiff oldFileName newFileName indexLine hunks) = do
  newHunks <- mapM (transformHunk transformer) hunks
  return $ FileDiff oldFileName newFileName indexLine newHunks

transformHunk :: (String -> IO String) -> Hunk -> IO Hunk
transformHunk transformer (Hunk from to lines) = do
  newLines <- mapM (transformLine transformer) lines
  return $ Hunk from to newLines

transformLine :: (String -> IO String) -> Line -> IO Line
transformLine transformer (AddedLine line) = do
  newLine <- transformer line
  return $ AddedLine newLine
transformLine _ line = return line

runCommand :: String -> String -> IO String
runCommand command input = do
  (exitCode, stdout, stderr) <- readCreateProcessWithExitCode (shell command) input
  case exitCode of
    ExitSuccess -> return stdout
    ExitFailure _ -> error $ "Failed to run `" ++ command ++ "`:\n" ++ stderr
