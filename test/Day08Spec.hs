module Day08Spec where

import Day08
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      head (input "./input/Day08p1test.txt") `shouldBe` (Instruction NOP 0)

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day08p1test.txt") `shouldBe` 9

    it "puzzle" $ do
      part1 (input "./input/Day08p1.txt") `shouldBe` 649

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day08p1test.txt") `shouldBe` 9

    it "puzzle" $ do
      part2 (input "./input/Day08p1.txt") `shouldBe` 649
