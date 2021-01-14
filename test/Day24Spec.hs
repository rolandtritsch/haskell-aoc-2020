module Day24Spec where

import Day24
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      length (input "./input/Day24p1test.txt") `shouldBe` 20

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day24p1test.txt") `shouldBe` 20

    it "puzzle" $ do
      part1 (input "./input/Day24p1.txt") `shouldBe` 315

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day24p1test.txt") `shouldBe` 20

    it "puzzle" $ do
      part2 (input "./input/Day24p1.txt") `shouldBe` 315
