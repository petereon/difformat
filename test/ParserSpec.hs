module ParserSpec where

import Parser (Hunk (..), Line (..), LineRange (..), parseHunk, parseHunkHeader, parseHunkLine, parsePair, removeFirstChar, splitFileDiffs)
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

  describe "parsing full diffs" $ do
    it "parses full diff into separate file-based diffs" $
      do
        splitFileDiffs
          ( "diff --git a/README.md b/README.md\n"
              ++ "index 1d0f1a3..f0b2c3d 100644\n"
              ++ "--- a/README.md\n"
              ++ "+++ b/README.md\n"
              ++ "@@ -1,2 +1,2 @@\n"
              ++ "-hello\n"
              ++ "+world\n"
              ++ " world\n"
              ++ "diff --git a/.gitignore b/.gitignore\n"
              ++ "index 1d0f1a3..f0b2c3d 100644\n"
              ++ "--- a/.gitignore\n"
              ++ "+++ b/.gitignore\n"
              ++ "@@ -1,2 +1,2 @@\n"
              ++ "-hello\n"
              ++ "+world\n"
              ++ " world\n"
          )
        `shouldBe` [ "diff --git a/README.md b/README.md\n"
                       ++ "index 1d0f1a3..f0b2c3d 100644\n"
                       ++ "--- a/README.md\n"
                       ++ "+++ b/README.md\n"
                       ++ "@@ -1,2 +1,2 @@\n"
                       ++ "-hello\n"
                       ++ "+world\n"
                       ++ " world\n",
                     "diff --git a/.gitignore b/.gitignore\n"
                       ++ "index 1d0f1a3..f0b2c3d 100644\n"
                       ++ "--- a/.gitignore\n"
                       ++ "+++ b/.gitignore\n"
                       ++ "@@ -1,2 +1,2 @@\n"
                       ++ "-hello\n"
                       ++ "+world\n"
                       ++ " world\n"
                   ]
