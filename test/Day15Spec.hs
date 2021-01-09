module Day15Spec where

import Day15
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      length (input "./input/Day15p1test.txt") `shouldBe` 3

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day15p1test.txt") `shouldBe` 436

    it "puzzle" $ do
      part1 (input "./input/Day15p1.txt") `shouldBe` 232

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day15p1test.txt") `shouldBe` 3

    it "puzzle" $ do
      part2 (input "./input/Day15p1.txt") `shouldBe` 6
