module ParserSpec where

import Parser (Hunk (..), Line (..), LineRange (..), parseHunk, parseHunkHeader, parseHunkLine, parsePair, removeFirstChar)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "parsing hunk" $ do
    it "removes first char from the string" $ do
      removeFirstChar "-1,2" `shouldBe` "1,2"

    it "parses a single pair into integer tuple" $ do
      parsePair "-1,2" `shouldBe` LineRange 1 2
      parsePair "-2,5" `shouldBe` LineRange 2 5

    it "parses hunk header" $ do
      parseHunkHeader "@@ -1,2 +1,2 @@" `shouldBe` (LineRange 1 2, LineRange 1 2)
      parseHunkHeader "@@ -2,5 +3,8 @@" `shouldBe` (LineRange 2 5, LineRange 3 8)

    it "parses hunk line" $ do
      parseHunkLine "+hello" `shouldBe` AddedLine "hello"
      parseHunkLine "-hello" `shouldBe` RemovedLine "hello"
      parseHunkLine " hello" `shouldBe` ContextLine "hello"

    it "parses hunk" $ do
      parseHunk
        [ "@@ -1,2 +1,5 @@",
          "+hello",
          "+new",
          "+hello",
          "+hello",
          "-world",
          " world"
        ]
        `shouldBe` Hunk (LineRange 1 2) (LineRange 1 5) [AddedLine "hello", AddedLine "new", AddedLine "hello", AddedLine "hello", RemovedLine "world", ContextLine "world"]
