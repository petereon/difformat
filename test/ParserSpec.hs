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
      parseHunkLine "+hello" `shouldBe` Just (AddedLine "hello")
      parseHunkLine "-hello" `shouldBe` Just (RemovedLine "hello")
      parseHunkLine " hello" `shouldBe` Just (ContextLine "hello")
      parseHunkLine "\\ No newline at end of file" `shouldBe` Nothing

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
              [ "--- a/README.md",
                "+++ b/README.md",
                "@@ -1,2 +1,2 @@",
                "-hello",
                "+world",
                " world",
                "--- a/.gitignore",
                "+++ b/.gitignore",
                "@@ -1,2 +1,2 @@",
                "-hello",
                "+world",
                " world"
              ]
          )
          `shouldBe` [ unlines
                         [ "--- a/README.md",
                           "+++ b/README.md",
                           "@@ -1,2 +1,2 @@",
                           "-hello",
                           "+world",
                           " world"
                         ],
                       unlines
                         [ "--- a/.gitignore",
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
            [ "--- a/README.md",
              "+++ b/README.md",
              "@@ -1,2 +1,2 @@",
              "-hello",
              "+world",
              " world"
            ]
        )
        `shouldBe` FileDiff
          { oldFileName = Just "README.md",
            newFileName = Just "README.md",
            hunks = [Hunk (LineRange 1 2) (LineRange 1 2) [RemovedLine "hello", AddedLine "world", ContextLine "world"]]
          }
    it "parses diffs into FileDiff struct" $ do
      parseFileDiff
        ( unlines
            [ "diff --git a/factorial.py b/factorial.py",
              "index abcdef1..1234567 100644",
              "--- a/factorial.py",
              "+++ b/factorial.py",
              "@@ -1,6 +1,7 @@",
              "def factorial(n):",
              "+    # Calculate the factorial of a number recursively",
              "    if n == 0:",
              "        return 1",
              "    else:",
              "-        return n * factorial(n - 1)",
              "+        return n * factorial(n - 1)",
              "",
              "-num = 5",
              "\\ No newline at end of file",
              "+num = 6"
            ]
        )
        `shouldBe` FileDiff
          { oldFileName = Just "factorial.py",
            newFileName = Just "factorial.py",
            hunks =
              [ Hunk
                  (LineRange 1 6)
                  (LineRange 1 7)
                  [ AddedLine "    # Calculate the factorial of a number recursively",
                    ContextLine "   if n == 0:",
                    ContextLine "       return 1",
                    ContextLine "   else:",
                    RemovedLine "        return n * factorial(n - 1)",
                    AddedLine "        return n * factorial(n - 1)",
                    RemovedLine "num = 5",
                    AddedLine "num = 6"
                  ]
              ]
          }
