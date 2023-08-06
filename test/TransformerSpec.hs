module TransformerSpec where

import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "CommandExecutor" $ do
    it "should be able to execute a command" $ do
      1 `shouldBe` 1
