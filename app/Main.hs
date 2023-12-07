module Main where

import Parser (parseDiff, toRepr)

main :: IO ()
main = do
  contents <- getContents
  putStrLn $ toRepr (parseDiff contents)
