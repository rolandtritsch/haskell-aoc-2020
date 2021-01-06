module Day06Spec where

import Day06
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      head (input "./input/Day06p1test.txt") `shouldBe` ["abc"]

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day06p1test.txt") `shouldBe` 11

    it "puzzle" $ do
      part1 (input "./input/Day06p1.txt") `shouldBe` 6583

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day06p1test.txt") `shouldBe` 6

    it "puzzle" $ do
      part2 (input "./input/Day06p1.txt") `shouldBe` 3290
