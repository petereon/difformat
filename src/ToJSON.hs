{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module ToJSON where

import Parser

class JSONifiable a where
  toJSON :: a -> String

instance JSONifiable LineRange where
  toJSON (LineRange from to) = "{\"from\":" ++ show from ++ ",\"range\":" ++ show to ++ "}"

instance JSONifiable Line where
  toJSON (AddedLine line) = "{\"type\":\"added\",\"line\":" ++ show line ++ "}"
  toJSON (RemovedLine line) = "{\"type\":\"removed\",\"line\":" ++ show line ++ "}"
  toJSON (ContextLine line) = "{\"type\":\"context\",\"line\":" ++ show line ++ "}"

instance JSONifiable Hunk where
  toJSON (Hunk from to lines) = "{\"oldRange\":" ++ toJSON from ++ ",\"newRange\":" ++ toJSON to ++ ",\"lines\":[" ++ init lines' ++ "]}"
    where
      lines' = foldr (\line acc -> toJSON line ++ "," ++ acc) "" lines

instance JSONifiable FileDiff where
  toJSON (FileDiff oldFileName newFileName indexLine hunks) = "{\"oldName\":\"" ++ oldFileName ++ "\",\"newName\":\"" ++ newFileName ++ "\",\"index\":\"" ++ indexLine ++ "\",\"hunks\":[" ++ init hunks' ++ "]}"
    where
      hunks' = foldr (\hunk acc -> toJSON hunk ++ "," ++ acc) "" hunks

instance JSONifiable [FileDiff] where
  toJSON diffs = "[" ++ init diffs' ++ "]"
    where
      diffs' = foldr (\diff acc -> toJSON diff ++ "," ++ acc) "" diffs
