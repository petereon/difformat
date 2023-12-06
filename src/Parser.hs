{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Parser where

import Data.List (isInfixOf, isPrefixOf)
import Data.List.Split (splitOn)

data LineRange = LineRange Int Int deriving (Show, Eq)

data Line = AddedLine String | RemovedLine String | ContextLine String deriving (Show, Eq)

data Hunk = Hunk LineRange LineRange [Line] deriving (Show, Eq)

data FileDiff = FileDiff
  { oldFileName :: String,
    newFileName :: String,
    indexLine :: String,
    hunks :: [Hunk]
  }
  deriving (Show, Eq)

parsePair :: String -> LineRange
parsePair (_ : rest) = case splitOn "," rest of
  (x : y : _) -> LineRange (read x) (read y)
  (x : _) -> LineRange (read x) 1
  _ -> error "Invalid range input"
parsePair _ = error "Invalid range input"

parseHunkRangeHeader :: String -> (LineRange, LineRange)
parseHunkRangeHeader header = case splitOn " " cleanedHeader of
  (_ : x : y : _) -> (parsePair x, parsePair y)
  _ -> error "Invalid hunk header input"
  where
    cleanedHeader = splitOn "@@" header !! 1

parseHunkLine :: String -> Line
parseHunkLine ('+' : rest) = AddedLine rest
parseHunkLine ('-' : rest) = RemovedLine rest
parseHunkLine (' ' : rest) = ContextLine rest
parseHunkLine line = error $ "Invalid hunk line input `" ++ line ++ "`"

parseHunk :: [String] -> Hunk
parseHunk (header : rest) = Hunk from to (map parseHunkLine rest')
  where
    (from, to) = parseHunkRangeHeader header
    rest' = case last rest of
      ('\\' : _) -> init rest
      _ -> rest
parseHunk [] = error "Invalid hunk input"

splitFileDiffs :: String -> [String]
splitFileDiffs input
  | null input = []
  | otherwise = tail $ map unlines (splitListOnPredicate (lines input) (isInfixOf "diff --git"))

parseFileDiff :: String -> FileDiff
parseFileDiff input = FileDiff (cleanFileName oldFileName) (cleanFileName newFileName) indexLine (map parseHunk hunks)
  where
    (_ : indexLine : oldFileName : newFileName : hunkBlob) = lines input
    hunks = tail $ splitListOnPredicate hunkBlob (isPrefixOf "@@")

cleanFileName :: String -> String
cleanFileName = drop 1 . (\name -> (if "/" `isInfixOf` name then dropWhile (/= '/') name else name)) . drop 3

splitListOnPredicate :: [a] -> (a -> Bool) -> [[a]]
splitListOnPredicate elements pred = foldr f [[]] elements
  where
    f line (x : xs)
      | pred line = [] : (line : x) : xs
      | otherwise = (line : x) : xs
    f _ [] = []

parseDiff :: String -> [FileDiff]
parseDiff = map parseFileDiff . splitFileDiffs
