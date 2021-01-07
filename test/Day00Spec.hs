module Day00Spec where

import Test.Hspec

import Day00

run :: IO ()
run = hspec $ do
  let input' = input "./input/Day00p1.txt"
  describe "input" $ do
    it "input" $ do
      head input' `shouldBe` "Hello"
      last input' `shouldBe` "World"

  describe "part1" $ do
    it "testcases" $ do
      part1 [] `shouldBe` 0

    it "puzzle" $ do
      part1 input' `shouldBe` 2

  describe "part2" $ do
    it "testcases" $ do
      part2 [] `shouldBe` 0

    it "puzzle" $ do
      part2 input' `shouldBe` 2
