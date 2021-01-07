module Day07Spec where

import Day07
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
       let Bags contains isIn = input "./input/Day07p1test.txt"
       length contains `shouldBe` 9
       length isIn `shouldBe` 7

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day07p1test.txt") `shouldBe` 4

    it "puzzle" $ do
      part1 (input "./input/Day07p1.txt") `shouldBe` 278

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day07p1test.txt") `shouldBe` 9

    it "puzzle" $ do
      part2 (input "./input/Day07p1.txt") `shouldBe` 594
