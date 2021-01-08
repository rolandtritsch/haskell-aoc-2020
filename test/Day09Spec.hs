module Day09Spec where

import Day09
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      let (XMAS preamble numbers) = input "./input/Day09p1test.txt"
      preamble `shouldBe` 5
      length numbers `shouldBe` 20

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day09p1test.txt") `shouldBe` 127

    it "puzzle" $ do
      part1 (input "./input/Day09p1.txt") `shouldBe` 85848519

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day09p1test.txt") `shouldBe` 20

    it "puzzle" $ do
      part2 (input "./input/Day09p1.txt") `shouldBe` 1000
