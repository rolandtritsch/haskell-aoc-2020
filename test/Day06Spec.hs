module Day02Spec where

import Day02
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      head (input "./input/Day02p1test.txt") `shouldBe` (Password 1 3 'a' "abcde")

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day02p1test.txt") `shouldBe` 2

    it "puzzle" $ do
      part1 (input "./input/Day02p1.txt") `shouldBe` 666

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day02p1test.txt") `shouldBe` 1

    it "puzzle" $ do
      part2 (input "./input/Day02p1.txt") `shouldBe` 670
