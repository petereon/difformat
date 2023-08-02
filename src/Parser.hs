module Parser where

import Data.List.Split (splitOn)

data LineRange = LineRange Int Int deriving (Show, Eq)

data Line = AddedLine String | RemovedLine String | ContextLine String deriving (Show, Eq)

data Hunk = Hunk LineRange LineRange [Line] deriving (Show, Eq)

data FileDiff = FileDiff String String [Hunk] deriving (Show, Eq)

removeFirstChar :: String -> String
removeFirstChar [] = [] -- The empty string has no first character
removeFirstChar (_ : xs) = xs

parsePair :: String -> LineRange
parsePair (_ : rest) = case splitOn "," rest of
  (x : y : _) -> LineRange (read x) (read y)
  _ -> error "Invalid range input"
parsePair _ = error "Invalid range input"

parseHunkHeader :: String -> (LineRange, LineRange)
parseHunkHeader header = case splitOn " " header of
  (_ : x : y : _) -> (parsePair x, parsePair y)
  _ -> error "Invalid hunk header input"

parseHunkLine :: String -> Line
parseHunkLine ('+' : rest) = AddedLine rest
parseHunkLine ('-' : rest) = RemovedLine rest
parseHunkLine (' ' : rest) = ContextLine rest
parseHunkLine _ = error "Invalid hunk line input"

parseHunk :: [String] -> Hunk
parseHunk (header : rest) = Hunk from to (map parseHunkLine rest)
  where
    (from, to) = parseHunkHeader header
parseHunk _ = error "Invalid hunk input"
