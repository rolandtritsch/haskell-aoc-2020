module Day20Spec where

import Day20
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      length (input "./input/Day20p1test.txt") `shouldBe` 107

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day20p1test.txt") `shouldBe` 107

    it "puzzle" $ do
      part1 (input "./input/Day20p1.txt") `shouldBe` 1728

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day20p1test.txt") `shouldBe` 107

    it "puzzle" $ do
      part2 (input "./input/Day20p1.txt") `shouldBe` 1728
