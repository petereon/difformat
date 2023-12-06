module Main where

import Parser (parseDiff)
import ToJSON (toJSON)

main :: IO ()
main = do
  contents <- getContents
  putStrLn $ toJSON (parseDiff contents)
