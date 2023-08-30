module Main where

import Parser (JSONifiable (toJSON), parseFileDiff, splitFileDiffs)

-- import Transformer (runCommand, transformFileDiff)

join sep = foldr (\a b -> a ++ if b == "" then b else sep ++ b) ""

main :: IO ()
main = do
  contents <- getContents
  putStrLn $ "[" ++ join "," (map (toJSON . parseFileDiff) (splitFileDiffs contents)) ++ "]"

-- res <- mapM (transformFileDiff (runCommand "") . parseFileDiff) (splitFileDiffs contents)
-- print res
