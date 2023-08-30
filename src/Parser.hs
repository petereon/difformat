{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Parser where

import Data.List (isInfixOf)
import Data.List.Split (splitOn)

class JSONifiable a where
  toJSON :: a -> String

data LineRange = LineRange Int Int deriving (Show, Eq)

instance JSONifiable LineRange where
  toJSON (LineRange from to) = "{\"from\":" ++ show from ++ ",\"range\":" ++ show to ++ "}"

data Line = AddedLine String | RemovedLine String | ContextLine String deriving (Show, Eq)

instance JSONifiable Line where
  toJSON (AddedLine line) = "{\"type\":\"added\",\"line\":" ++ show line ++ "}"
  toJSON (RemovedLine line) = "{\"type\":\"removed\",\"line\":" ++ show line ++ "}"
  toJSON (ContextLine line) = "{\"type\":\"context\",\"line\":" ++ show line ++ "}"

data Hunk = Hunk LineRange LineRange [Line] deriving (Show, Eq)

instance JSONifiable Hunk where
  toJSON (Hunk from to lines) = "{\"oldRange\":" ++ toJSON from ++ ",\"newRange\":" ++ toJSON to ++ ",\"lines\":[" ++ init lines' ++ "]}"
    where
      lines' = foldr (\line acc -> toJSON line ++ "," ++ acc) "" lines

data FileDiff = FileDiff
  { oldFileName :: String,
    newFileName :: String,
    indexLine :: String,
    hunks :: [Hunk]
  }
  deriving (Show, Eq)

instance JSONifiable FileDiff where
  toJSON (FileDiff oldFileName newFileName indexLine hunks) = "{\"oldName\":\"" ++ oldFileName ++ "\",\"newName\":\"" ++ newFileName ++ "\",\"index\":\"" ++ indexLine ++ "\",\"hunks\":[" ++ init hunks' ++ "]}"
    where
      hunks' = foldr (\hunk acc -> toJSON hunk ++ "," ++ acc) "" hunks

removeFirstChar :: String -> String
removeFirstChar [] = [] -- The empty string has no first character
removeFirstChar (_ : xs) = xs

parsePair :: String -> LineRange
parsePair (_ : rest) = case splitOn "," rest of
  (x : y : _) -> LineRange (read x) (read y)
  _ -> error "Invalid range input"
parsePair _ = error "Invalid range input"

parseHunkRangeHeader :: String -> (LineRange, LineRange)
parseHunkRangeHeader header = case splitOn " " header of
  (_ : x : y : _) -> (parsePair x, parsePair y)
  _ -> error "Invalid hunk header input"

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
    hunks = tail $ splitListOnPredicate hunkBlob (isInfixOf "@@")

cleanFileName :: String -> String
cleanFileName = drop 1 . (\name -> (if "/" `isInfixOf` name then dropWhile (/= '/') name else name)) . drop 3

splitListOnPredicate :: [a] -> (a -> Bool) -> [[a]]
splitListOnPredicate elements pred = foldr f [[]] elements
  where
    f line (x : xs)
      | pred line = [] : (line : x) : xs
      | otherwise = (line : x) : xs
    f _ [] = []
