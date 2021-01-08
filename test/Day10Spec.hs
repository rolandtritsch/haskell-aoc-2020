module Day10Spec where

import Day10
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      head (input "./input/Day10p1test.txt") `shouldBe` 16

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day10p1test.txt") `shouldBe` 35

    it "puzzle" $ do
      part1 (input "./input/Day10p1.txt") `shouldBe` 2664

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day10p1test.txt") `shouldBe` 11

    it "puzzle" $ do
      part2 (input "./input/Day10p1.txt") `shouldBe` 108
