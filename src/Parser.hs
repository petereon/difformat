{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Parser where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Char (isSpace)
import Data.List (isInfixOf, isPrefixOf)
import Data.List.Split (splitOn)
import Flow
import GHC.Generics

toRepr :: ToJSON a => a -> String
toRepr val = BS.unpack (encode val)

data Line = AddedLine String | RemovedLine String | ContextLine String deriving (Show, Eq, Generic)

data LineRange = LineRange
  { start :: Int,
    range :: Int
  }
  deriving (Show, Eq, Generic)

data Hunk = Hunk
  { oldRange :: LineRange,
    newRange :: LineRange,
    lines :: [Line]
  }
  deriving (Show, Eq, Generic)

data FileDiff = FileDiff
  { oldFileName :: Maybe String,
    newFileName :: Maybe String,
    hunks :: [Hunk]
  }
  deriving (Show, Eq, Generic)

instance ToJSON LineRange

instance ToJSON Line

instance ToJSON Hunk

instance ToJSON FileDiff

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil pred (x : xs)
  | pred x = []
  | otherwise = x : takeUntil pred xs

mapJust :: (a -> Maybe b) -> [a] -> [b]
mapJust _ [] = []
mapJust f (x : xs) = case f x of
  Just x' -> x' : mapJust f xs
  Nothing -> mapJust f xs

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

parseHunkLine :: String -> Maybe Line
parseHunkLine ('+' : rest) = Just $ AddedLine rest
parseHunkLine ('-' : rest) = Just $ RemovedLine rest
parseHunkLine (' ' : rest) = Just $ ContextLine rest
parseHunkLine ('\\' : _) = Nothing
parseHunkLine _ = Nothing

parseHunk :: [String] -> Hunk
parseHunk (header : rest) = Hunk from to (mapJust parseHunkLine rest)
  where
    (from, to) = parseHunkRangeHeader header
parseHunk [] = error "Invalid hunk input"

splitFileDiffs :: String -> [String]
splitFileDiffs input
  | null input = []
  | otherwise = tail $ map unlines (splitListOnPredicate (Prelude.lines input) (isPrefixOf "---"))

parseFileDiff :: String -> FileDiff
parseFileDiff input = parseFileDiffLines startFileDiff (Prelude.lines input)
  where
    startFileDiff = FileDiff Nothing Nothing []

parseFileDiffLines :: FileDiff -> [String] -> FileDiff
parseFileDiffLines fileDiff [] = fileDiff
parseFileDiffLines fileDiff (line : rest)
  | "---" `isPrefixOf` line = parseFileDiffLines (fileDiff {oldFileName = Just $ cleanFileName line}) rest
  | "+++" `isPrefixOf` line = parseFileDiffLines (fileDiff {newFileName = Just $ cleanFileName line}) rest
  | "@@" `isPrefixOf` line = fileDiff {hunks = tail $ map parseHunk (splitListOnPredicate (line : rest) (isPrefixOf "@@"))}
  | otherwise = parseFileDiffLines fileDiff rest

cleanFileName :: String -> String
cleanFileName name =
  name
    |> drop 3
    |> (\name -> (if "/" `isInfixOf` name then dropWhile (/= '/') name else name))
    |> drop 1
    |> takeUntil isSpace

splitListOnPredicate :: [a] -> (a -> Bool) -> [[a]]
splitListOnPredicate elements pred = foldr f [[]] elements
  where
    f line (x : xs)
      | pred line = [] : (line : x) : xs
      | otherwise = (line : x) : xs
    f _ [] = []

parseDiff :: String -> [FileDiff]
parseDiff a = map parseFileDiff (splitFileDiffs a)
