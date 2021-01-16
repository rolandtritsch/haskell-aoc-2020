module Day25Spec where

import Day25
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      (input "./input/Day25p1test.txt") `shouldBe` (5764801, 17807724)

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day25p1test.txt") `shouldBe` 5764801

    it "puzzle" $ do
      part1 (input "./input/Day25p1.txt") `shouldBe` 12578151

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day25p1test.txt") `shouldBe` 5764801

    it "puzzle" $ do
      part2 (input "./input/Day25p1.txt") `shouldBe` 12578151
