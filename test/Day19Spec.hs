module Day19Spec where

import Day19
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      let expected = input "./input/Day19p1test.txt"
      messages expected `shouldBe` [[""]]

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day19p1test.txt") `shouldBe` 2

    it "puzzle" $ do
      part1 (input "./input/Day19p1.txt") `shouldBe` 124

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day19p1test.txt") `shouldBe` 5

    it "puzzle" $ do
      part2 (input "./input/Day19p1.txt") `shouldBe` 368
