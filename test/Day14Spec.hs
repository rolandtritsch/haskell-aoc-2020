module Day14Spec where

import Day14
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      length (input "./input/Day14p1test.txt") `shouldBe` 4

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day14p1test.txt") `shouldBe` 165

    it "puzzle" $ do
      part1 (input "./input/Day14p1.txt") `shouldBe` 7440382076205

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day14p1test.txt") `shouldBe` 4

    it "puzzle" $ do
      part2 (input "./input/Day14p1.txt") `shouldBe` 559
