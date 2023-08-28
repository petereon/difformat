module Main where

import Parser (parseFileDiff, splitFileDiffs)

-- import Transformer (runCommand, transformFileDiff)

main :: IO ()
main = do
  contents <- getContents
  print $ map parseFileDiff (splitFileDiffs contents)

-- res <- mapM (transformFileDiff (runCommand "") . parseFileDiff) (splitFileDiffs contents)
-- print res
