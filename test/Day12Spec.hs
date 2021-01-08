module Day12Spec where

import Day12
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      head (input "./input/Day12p1test.txt") `shouldBe` (Instruction Forward' 10)

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day12p1test.txt") `shouldBe` 25

    it "puzzle" $ do
      part1 (input "./input/Day12p1.txt") `shouldBe` 2297

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day12p1test.txt") `shouldBe` 5

    it "puzzle" $ do
      part2 (input "./input/Day12p1.txt") `shouldBe` 759
