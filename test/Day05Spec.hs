module Day05Spec where

import Day05
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      length (input "./input/Day05p1test.txt") `shouldBe` 4

  describe "toInt" $ do
    it "testcases" $ do
      toInt "0101100101" `shouldBe` 357
      
  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day05p1test.txt") `shouldBe` 820

    it "puzzle" $ do
      part1 (input "./input/Day05p1.txt") `shouldBe` 989

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day05p1test.txt") `shouldBe` 120

    it "puzzle" $ do
      part2 (input "./input/Day05p1.txt") `shouldBe` 548
