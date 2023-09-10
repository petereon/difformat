module ParserSpec where

import Parser (FileDiff (..), Hunk (..), Line (..), LineRange (..), parseFileDiff, parseHunk, parseHunkLine, parseHunkRangeHeader, parsePair, splitFileDiffs, splitListOnPredicate)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "utils" $ do
    it "splits list on predicate" $ do
      splitListOnPredicate [1, 2, 3, 4, 5, 6, 7, 8, 9] (== 5) `shouldBe` [[1, 2, 3, 4], [5, 6, 7, 8, 9]]
  describe "parsing hunk" $ do
    it "parses a single pair into integer tuple" $ do
      parsePair "-1,2" `shouldBe` LineRange 1 2
      parsePair "-2,5" `shouldBe` LineRange 2 5

    it "parses hunk header" $ do
      parseHunkRangeHeader "@@ -1,2 +1,2 @@" `shouldBe` (LineRange 1 2, LineRange 1 2)
      parseHunkRangeHeader "@@ -2,5 +3,8 @@" `shouldBe` (LineRange 2 5, LineRange 3 8)
      parseHunkRangeHeader "@@ -1 +1,2 @@" `shouldBe` (LineRange 1 1, LineRange 1 2)

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
          ( unlines
              [ "diff --git a/README.md b/README.md",
                "index 1d0f1a3..f0b2c3d 100644",
                "--- a/README.md",
                "+++ b/README.md",
                "@@ -1,2 +1,2 @@",
                "-hello",
                "+world",
                " world",
                "diff --git a/.gitignore b/.gitignore",
                "index 1d0f1a3..f0b2c3d 100644",
                "--- a/.gitignore",
                "+++ b/.gitignore",
                "@@ -1,2 +1,2 @@",
                "-hello",
                "+world",
                " world"
              ]
          )
          `shouldBe` [ unlines
                         [ "diff --git a/README.md b/README.md",
                           "index 1d0f1a3..f0b2c3d 100644",
                           "--- a/README.md",
                           "+++ b/README.md",
                           "@@ -1,2 +1,2 @@",
                           "-hello",
                           "+world",
                           " world"
                         ],
                       unlines
                         [ "diff --git a/.gitignore b/.gitignore",
                           "index 1d0f1a3..f0b2c3d 100644",
                           "--- a/.gitignore",
                           "+++ b/.gitignore",
                           "@@ -1,2 +1,2 @@",
                           "-hello",
                           "+world",
                           " world"
                         ]
                     ]
    it "parses diffs into a FileDiff struct" $ do
      parseFileDiff
        ( unlines
            [ "diff --git a/README.md b/README.md",
              "index 1d0f1a3..f0b2c3d 100644",
              "--- a/README.md",
              "+++ b/README.md",
              "@@ -1,2 +1,2 @@",
              "-hello",
              "+world",
              " world"
            ]
        )
        `shouldBe` FileDiff
          { oldFileName = "README.md",
            newFileName = "README.md",
            indexLine = "index 1d0f1a3..f0b2c3d 100644",
            hunks = [Hunk (LineRange 1 2) (LineRange 1 2) [RemovedLine "hello", AddedLine "world", ContextLine "world"]]
          }
