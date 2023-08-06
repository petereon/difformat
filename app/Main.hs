module Main where

import Parser (parseFileDiff, splitFileDiffs)

main :: IO ()
main = do
  contents <- getContents
  print $ map parseFileDiff (splitFileDiffs contents)
