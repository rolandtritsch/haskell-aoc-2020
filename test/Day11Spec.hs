module Day11Spec where

import Day11
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      let (Seats _ _ dimensions) = input "./input/Day11p1test.txt"
      dimensions `shouldBe` (10,10)

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day11p1test.txt") `shouldBe` 37

    it "puzzle" $ do
      part1 (input "./input/Day11p1.txt") `shouldBe` 2289

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day11p1test.txt") `shouldBe` 100

    it "puzzle" $ do
      part2 (input "./input/Day11p1.txt") `shouldBe` 8820
