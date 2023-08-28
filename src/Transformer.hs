{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Transformer where

import Parser (FileDiff (..), Hunk (..), Line (..))
import System.Exit
import System.Process (readCreateProcessWithExitCode, shell)

transformFileDiff :: (String -> IO String) -> FileDiff -> IO FileDiff
transformFileDiff transformer (FileDiff oldFileName newFileName indexLine hunks) = do
  newHunks <- mapM (transformHunk transformer) hunks
  return $ FileDiff oldFileName newFileName indexLine newHunks

transformHunk :: (String -> IO String) -> Hunk -> IO Hunk
transformHunk transformer (Hunk from to lines) = do
  newLines <- transformLines transformer lines
  return $ Hunk from to newLines

-- NOTES
-- All the lines including the context will be formatted
-- If leading whitespace is stripped by formatter it should be returned to the line
-- Option should be provided to do in-place replacement or not

transformLines :: (String -> IO String) -> [Line] -> IO [Line]
transformLines f lines = do
  return lines

runCommand :: String -> String -> IO String
runCommand command input = do
  (exitCode, stdout, stderr) <- readCreateProcessWithExitCode (shell command) input
  case exitCode of
    ExitSuccess -> return stdout
    ExitFailure _ -> error $ "Failed to run `" ++ command ++ "`:\n" ++ stderr
